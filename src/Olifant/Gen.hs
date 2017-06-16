{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Olifant.Gen where

import           Olifant.Core

import qualified Prelude as P
import           Protolude hiding (Type, mod, local)

import           LLVM.AST
import           LLVM.AST.AddrSpace
import           LLVM.AST.Attribute
import           LLVM.AST.CallingConvention
import           LLVM.AST.Constant
import           LLVM.AST.Type
import           LLVM.AST.Global
import           LLVM.Context (withContext)
import           LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
import           LLVM.Pretty (ppllvm)

-- | Symbol tables maps a name to a LLVM operand.
type SymbolTable = [(Text, Operand)]

-- | State of the complete program
data GenState = GenState
    { blocks :: [BlockState] -- Blocks, ordered and named
    , symtab :: SymbolTable  -- Symbol table
    , counter :: Int
    , mod :: Module          -- The LLVM Module
    } deriving (Show)

-- | State of a single block
--
-- A function definition contains a list of basic blocks, forming the Control
-- Flow Graph. Each basic block may optionally start with a label, contains a
-- list of instructions and ends with a terminator instruction such as a branch
-- or function return.
--
-- As of now, a function contains just one block.
data BlockState = BlockState
  { name :: Text                     -- Name of the block
  , stack :: [Named Instruction]     -- List of operations
  , term :: Maybe (Named Terminator) -- Block terminator
  } deriving (Show)

-- | Codegen state monad
newtype Codegen a = Codegen
    { runCodegen :: State GenState a
    } deriving (Functor, Applicative, Monad, MonadState GenState)

-- | Default `GenState`
genState :: GenState
genState = GenState
           { blocks = []
           , symtab = []
           , mod = defaultModule {moduleName = "calc"}
           , counter = 0
           }

-- | Default `BlockState`
blockState :: Text -> BlockState
blockState n = BlockState {name = n, stack = [], term = Nothing}

-- * Handle `GenState`

-- Add a global definition to the LLVM module
addDefn :: Global -> Codegen ()
addDefn g = do
    st <- get
    modl <- gets mod
    let defs = moduleDefinitions modl ++ [GlobalDefinition g]
    let mod' = modl {moduleDefinitions = defs}
    put $ st {mod = mod'}

-- Add a symbol to the symbol table
addSym :: Text -> Operand -> Codegen ()
addSym symbol op = modify $ \s -> s {symtab = (symbol, op): symtab s}

-- * Handle `BlockState`

-- | Get the current block
current :: Codegen BlockState
current = P.head <$> gets blocks

-- | Push a named instruction to the stack of the active block
push :: Named Instruction -> Codegen ()
push ins = do
    active <- current
    let block = active {stack = stack active ++ [ins]}

    modify $ \s -> s { blocks = replace (blocks s) block}
  where
      -- Replace first block with new block
      replace :: [BlockState] -> BlockState -> [BlockState]
      replace (_:xs) newBlock = newBlock:xs
      replace _ _ = notImplemented

-- | Name an instruction and add to stack.
--
--  - Takes an expression of the form @Add 1 2@
--  - Gets a fresh name for it, @%2@
--  - Adds @%2 = Add 1 2@ to the stack
--  - Returns @%2@
--
unnamed :: Tipe -> Instruction -> Codegen Operand
unnamed t ins = do
    new <- fresh
    push $ new := ins
    return $ LocalReference (native t) new
  where
    -- | Make a fresh unnamed variable; %4 or %5
    fresh :: Codegen Name
    fresh = do
        n <- gets counter
        modify $ \s -> s {counter = n + 1}
        return $ UnName . fromIntegral $ n

-- | Add the instruction to the stack with a specific name.
--
--  - Takes an expression of the form @Add 1 2@ and a name @%foo@
--  - Adds @%foo = Add 1 2@ to the stack
--  - Returns @%foo@
--
named :: Tipe -> Text -> Instruction -> Codegen Operand
named t str ins = push (op := ins) >> return (LocalReference (native t) op)
  where
    op :: Name
    op = Name $ toS str

-- * Primitive wrappers

-- | Variable assignment
--
-- Converts expression of the form @%1 = 2@ to @* %1 = 2@
store :: Operand -> Operand -> Codegen ()
store var val = push $ Do $ Store False (pointer var) val Nothing 0 []

-- | Fetch a variable from memory
load :: Tipe -> Operand -> Codegen Operand
load t var = unnamed t $ Load False (pointer var) Nothing 0 []

-- | Make an `alloca` instruction
alloca :: Tipe -> Maybe Text -> Codegen Operand
alloca t Nothing = unnamed t $ Alloca (native t) Nothing 0 []
alloca t (Just ref) = named t ref $ Alloca (native t) Nothing 0 []

-- | Call a function `fn` with `arg`
call :: Tipe -> Operand -> Operand -> Codegen Operand
call t fn arg' = unnamed t $ Call Nothing C [] fn' args' [] []
  where
    fn' :: CallableOperand
    fn' = Right fn

    args' :: [(Operand, [ParameterAttribute])]
    args' = [(arg', [])]

-- | Add 2 integers
add :: Operand -> Operand -> Instruction
add lhs rhs = LLVM.AST.Add False False lhs rhs []

-- | Create a simple block from a list of instructions and a terminator
basicBlock :: [Named Instruction] -> Named Terminator -> BasicBlock
basicBlock = BasicBlock (Name "entry")

-- | Return the last expression from a block
terminator :: Operand -> Codegen (Named Terminator)
terminator result = return $ Do $ Ret (Just result) []

-- * References

-- | Get a reference operand from a string
local :: Tipe -> Text -> Operand
local t n = LocalReference (native t) $ Name $ toS n

-- | Get a global reference from a string
global :: Tipe -> Text -> Constant
global t n = GlobalReference (native t) $ Name $ toS n

-- | Map from Olifant types to LLVM types
native :: Tipe -> Type
native TInt = i64
native TBool = i1
native (TArrow types) = FunctionType {
    argumentTypes = map native $ P.init types
  , resultType = native $ P.last types
  , isVarArg = False
  }

-- | Get pointer to a local variable
--
-- @i64 f -> i64* f@
pointer :: Operand -> Operand
pointer (LocalReference t name') = LocalReference p name'
  where
    p :: Type
    p = PointerType t $ AddrSpace 0
pointer x = error $ "Cannot make a pointer for " <> show x

-- | Make an operand out of a global function
--
--   %f -> @f
externf :: Tipe -> Text -> Operand
externf t n = ConstantOperand $ global t n

-- | Define a function
define :: Ref Tipe -> Ref Tipe -> [BasicBlock] -> Codegen ()
define (Ref n t) arg' body' = do
    addDefn $
      functionDefaults {
        name          = Name $ toS n
        , parameters  = ([Parameter ty nm [] | (ty, nm) <- params], False)
        , returnType  = native $ ret t
        , basicBlocks = body'
      }

    addSym n $ externf t n
  where
    params :: [(Type, Name)]
    params = case t of
      (TArrow ts) -> [(native t', Name $ toS $ rname arg') | t' <- P.init ts]
      _ -> []

-- * AST Traversal

-- | Step through the AST
--
-- Step should return an operand, which is the LHS of the operand it just dealt
-- with. Step is free to push instructions to the current block.
step :: Core -> Codegen Operand

-- | Find the operand for the variable from the symbol table and return it.
--
step (Var (Ref n t)) = return $ local t n

-- | Make a constant operand out of the constant and return it.
step (Lit (LNumber n)) = return $ ConstantOperand $ Int 64 (toInteger n)
step (Lit (LBool True)) = return $ ConstantOperand $ Int 1 1
step (Lit (LBool False)) = return $ ConstantOperand $ Int 1 0

-- | Top level lambda, lifted before it gets here
step (Lam ref@(Ref n t) arg' body') = do
    -- Make a new block for this function and add to `GenState`
    modify $ \s -> s {blocks = blocks s ++ [blockState n]}

    -- [TODO] - Should do the closure business
    result <- step body'
    term' <- terminator result
    instructions <- stack <$> current

    define ref arg' [basicBlock instructions term']

    return $ local t n

-- Apply the function
--
-- This is a bit too primitive. There are no type checks, or at least ensuring
-- that the function is even defined.
step (App (Lam (Ref n t) _ _) val) = do
    arg' <- step val
    call (ret t) (externf t n) arg'

-- Add a constant global variable
--
-- Lambda assigning an expression with free variables make no sense, handle the
-- case somewhere before code generation.
--
-- Type information is slightly redundant here. It can be figured out from the
-- explicit tags or from the constructors.
step (Let (Ref name' _t1) (Lit val)) = do
    addDefn global'
    return $ ConstantOperand value'

  where
    (value', tipe') = case val of
        (LNumber n) -> (Int 64 (toInteger n), i64)
        (LBool True) -> (Int 1 1, i1)
        (LBool False) -> (Int 1 0, i1)

    global' :: Global
    global' = globalVariableDefaults
      { name = Name $ toS name'
      , initializer = Just value'
      , type' = tipe'
      }

---
--- Code generation
---

compile :: [Core] -> Module
compile prog = mod $ execState (runCodegen (run prog)) genState
  where
    -- | Step through the AST and _throw_ away the results
    run :: [Core] -> Codegen ()
    run = mapM_ step

-- | Generate native code with C++ FFI
toLLVM :: Module -> IO Text
toLLVM modl =
    withContext $ \context -> do
        errOrLLVM <-
            runExceptT $ withModuleFromAST context modl moduleLLVMAssembly
        case errOrLLVM of
            Left err -> return $ toS $ "Error: " ++ err
            Right llvm -> return $ toS llvm

-- | Generate native code with C++ FFI
pretty :: [Core] -> Text
pretty ast = toS . ppllvm $ compile ast

-- | Print compiled LLVM IR to stdout
genNative :: [Core] -> IO Text
genNative ast = toLLVM $ compile ast
