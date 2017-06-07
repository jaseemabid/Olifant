{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Olifant where

import qualified Prelude as P
import           Protolude hiding (Type, mod)

import           LLVM.AST
import           LLVM.AST.AddrSpace
import           LLVM.AST.Constant
import           LLVM.AST.Global
import           LLVM.Context (withContext)
import           LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
import           LLVM.Pretty (ppllvm)

-- * Language definition

-- | Simple integer language with just assignments, numbers, + and -
data Calc
    = Number Integer
    | Plus Calc Calc
    | Binding Text
    | Assignment Text Calc
    deriving (Read, Show)

-- * Types

-- | 64 bit integer
number :: Type
number = IntegerType 64

-- | Pointer to the `number` type
pointer :: Type
pointer = PointerType number $ AddrSpace 0

-- * State types

-- | Symbol tables maps a string to a LLVM operand
type SymbolTable = [(Text, Operand)]

-- | State of the complete program
data GenState = GenState
    { blocks :: [BlockState] -- Blocks, ordered and named
    , symtab :: SymbolTable  -- Symbol table
    , mod :: Module          -- The LLVM Module
    , counter :: Integer
    } deriving (Show)

-- | State of a single block
--
--
-- A function definition contains a list of basic blocks, forming the Control
-- Flow Graph. Each basic block may optionally start with a label, contains a
-- list of instructions and ends with a terminator instruction such as a branch
-- or function return.
--
-- As of now, a function contains just one block.
data BlockState = BlockState
  { name :: Text                   -- Name of the block
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
blockState name' = BlockState {name = name', stack = [], term = Nothing}

-- * Handle `GenState`

-- Add a global definition to the LLVM module
addDefn :: Global -> Codegen Module
addDefn g = do
    st <- get
    modl <- gets mod
    let defs = moduleDefinitions modl ++ [GlobalDefinition g]
    let mod' = modl {moduleDefinitions = defs}
    put $ st {mod = mod'}
    return mod'

-- Add a list of global definitions to the LLVM module
addDefns :: [Global] -> Codegen Module
addDefns = foldr ((>>) . addDefn) (gets mod)

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
unnamed :: Instruction -> Codegen Operand
unnamed ins = do
    new <- fresh
    push $ new := ins
    return $ LocalReference number new
    -- Make a fresh unnamed variable; %4 or %5
  where
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
named :: Text -> Instruction -> Codegen Operand
named str ins = push (op := ins) >> return (LocalReference number op)
  where
    op :: Name
    op = Name $ toS str

-- * Primitive wrappers

-- | Variable assignment
--
-- Converts expression of the form @%1 = 2@ to @* %1 = 2@
--
-- [TODO]: Having to use `error` here is shitty
store :: Operand -> Operand -> Codegen ()
store (LocalReference _type n) val =
    push $ Do $ Store False ptr val Nothing 0 []
  where
    ptr = LocalReference pointer n
store _ _ = error "Cannot store anything other than local reference"

-- | Fetch a variable from memory
--
-- [TODO] - Enforce operand is a pointer type. This is shitty types :/
load :: Operand -> Codegen Operand
load (LocalReference _type n) = unnamed $ Load False ptr Nothing 0 []
  where
    ptr = LocalReference pointer n
load _ = error "Cannot load anything other than local pointer reference"

-- | Make an `alloca` instruction
alloca :: Type -> Maybe Text -> Codegen Operand
alloca ty Nothing = unnamed $ Alloca ty Nothing 0 []
alloca ty (Just ref) = named ref $ Alloca ty Nothing 0 []

-- | Add 2 integers
add :: Operand -> Operand -> Instruction
add lhs rhs = LLVM.AST.Add False False lhs rhs []

-- | Create a simple block from a list of instructions and a terminator
basicBlock :: [Named Instruction] -> Named Terminator -> BasicBlock
basicBlock = BasicBlock (Name "entry")

-- | Return the last expression from a block
terminator :: Operand -> Codegen (Named Terminator)
terminator result = return $ Do $ Ret (Just result) []

-- * AST Traversal

-- | Step through the AST
--
-- Step should return an operand, which is the LHS of the operand it just dealt
-- with. Step is free to push instructions to the current block.
step :: Calc -> Codegen Operand
-- | Make a constant operand out of the constant and return it.
step (Number n) = return $ ConstantOperand $ Int 64 n
-- | Get the reference to the operands on LHS and RH'S. Push to stack the
-- instruction to compute the new value and return the operand that refers to
-- the new value.
step (Plus a b) = do
    lhs <- step a :: Codegen Operand
    rhs <- step b :: Codegen Operand
    unnamed $ add lhs rhs
-- | Assign an operand into a temporary variable
--
-- > @Assignment "x" (1 + 2)@ is roughly translated into
--
-- > %1 = 1 + 2             ; Step function returns %1 for sub tree
-- > %x = alloca i64        ; Allocate a new variable; might be unnecessary
-- > store i64 %0, i64* %x  ; Copy %1 into %x
-- > %2 = load i64, i64* %x ; Load it back
-- > ret i64 %2             ; Return the alias
--
-- This approach feels pretty silly. Passing a name into step function to store
-- the result might be the right way to do this.
--
step (Assignment bind ref) = do
    result <- step ref
    ptr <- alloca number (Just bind) :: Codegen Operand
    store ptr result
    load ptr

-- | Find the operand for the variable from the symbol table and return it.
step (Binding binding) = return (LocalReference number $ Name $ toS binding)

---
--- Code generation
---

compile :: [Calc] -> Module
compile prog = evalState (runCodegen (run prog >>= addDefns)) genState
  where

    run :: [Calc] -> Codegen [Global]
    run = mapM run1

    -- | Compute a single block from one line of source
    --
    -- A top level expression is either a definition (function or variable) or
    -- its an expression, which will be compiled to "main".
    run1 :: Calc -> Codegen Global
    run1 =
        \case
            (Assignment var val) -> run' var val
            val -> run' "main" val
      where
        run' var val = do
            -- Make a new block for this function and add to `GenState`
            modify $ \s -> s {blocks = blocks s ++ [blockState var]}
            result <- step val
            term' <- terminator result
            ins <- stack <$> current
            let block = basicBlock ins term'
            return $ fn var [block]

    fn :: Text -> [BasicBlock] -> Global
    fn fnName blocks' =
        functionDefaults
        {name = Name $ toS fnName, returnType = number, basicBlocks = blocks'}

-- | Generate native code with C++ FFI
toLLVM :: Module -> IO P.String
toLLVM modl =
    withContext $ \context -> do
        errOrLLVM <-
            runExceptT $ withModuleFromAST context modl moduleLLVMAssembly
        case errOrLLVM of
            Left err -> return $ "Error: " ++ err
            Right llvm -> return llvm

-- | Generate native code with C++ FFI
pretty :: [Calc] -> IO ()
pretty ast = putStrLn . ppllvm $ compile ast

-- | Print compiled LLVM IR to stdout
native :: [Calc] -> IO P.String
native ast = toLLVM $ compile ast
