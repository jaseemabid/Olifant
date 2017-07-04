{-|
Module      : Olifant.Gen
Description : LLVM Code generator for Core

Generate LLVM IR from a fully typed Code

__No Symbol Table__

One of the interesting things I learned about the code generator is that a
symbol table is unnecessary.

As far as possible, we use an alternative strategy: a variable is a data
structure that contains all the information about itself. I find this approach
simpler. Each state of the compiler can augment and annotate more information
into the reference accordingly. A simple pass can be made even simpler without
getting into any of the StateT business.

Ref: http://www.aosabook.org/en/ghc.html § No Symbol Table
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Olifant.Gen where

import Olifant.Core

import Prelude   (head)
import Protolude hiding (Type, head, local, mod)

import Data.ByteString.Short      (toShort)
import LLVM.Analysis              (verify)
import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Attribute
import LLVM.AST.CallingConvention
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.Context               (withContext)
import LLVM.Module                (moduleLLVMAssembly, withModuleFromAST)
import LLVM.PassManager

-- | State of the complete program
data GenState = GenState
    { blocks  :: [BlockState]  -- Blocks, ordered and named
    , counter :: Int           -- Number of unnamed variables
    , mod     :: Module        -- The LLVM Module pointer
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
  { bname :: Text                     -- Name of the block
  , stack :: [Named Instruction]      -- List of operations
  , term  :: Maybe (Named Terminator) -- Block terminator
  } deriving (Show)

-- | Codegen monad is Olifant monad with state specialized to `GenState`
--
-- Errors are not expected to be recoverable. A valid type safe `Progn`
-- shouldn't raise an error and there is nothing much to do if the input is
-- wrong.
type Codegen a = Olifant GenState a

-- | Default `GenState`
genState :: GenState
genState = GenState
           { blocks = []
           , mod = defaultModule {moduleName = "calc"}
           , counter = 0
           }

-- | Default `BlockState`
blockState :: Ref -> BlockState
blockState n = BlockState {bname = rname n, stack = [], term = Nothing}

-- * Manipulate `GenState`

-- | Add a global definition to the LLVM module
define :: Global -> Codegen ()
define g = do
    st <- get
    modl <- gets mod
    let defs = moduleDefinitions modl ++ [GlobalDefinition g]
    let mod' = modl {moduleDefinitions = defs}
    put $ st {mod = mod'}

-- * Manipulate `BlockState`

-- | Get the current block
current :: Codegen BlockState
current = head <$> gets blocks

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
      replace _ _             = notImplemented

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
    op = lname str

-- | Helper function to convert a Text -> ByteString -> ShortByteString -> Name
lname :: Text -> Name
lname = Name . toShort . toS

-- * Primitive wrappers

-- | Variable assignment
--
-- Converts expression of the form @%1 = 2@ to @* %1 = 2@
store :: Operand -> Operand -> Codegen ()
store var val = push $ Do $ Store False (pointer var) val Nothing 0 []

-- | Fetch a variable from memory
load :: Tipe -> Operand -> Codegen Operand
load t var = unnamed t $ Load False var Nothing 0 []

-- | Make an `alloca` instruction
alloca :: Tipe -> Maybe Text -> Codegen Operand
alloca t Nothing    = unnamed t $ Alloca (native t) Nothing 0 []
alloca t (Just ref) = named t ref $ Alloca (native t) Nothing 0 []

-- | Call a function `fn` with `arg`
call :: Tipe -> Operand -> Operand -> Codegen Operand
call t fn arg = unnamed t $ Call Nothing C [] fn' args' [] []
  where
    fn' :: CallableOperand
    fn' = Right fn

    args' :: [(Operand, [ParameterAttribute])]
    args' = [(arg, [])]

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
local t n = LocalReference (native t) $ lname n

-- | Get a global reference from a string
global :: Tipe -> Text -> Constant
global t n = GlobalReference (native t) $ lname n

-- | Map from Olifant types to LLVM types
native :: Tipe -> Type
native TUnit = LLVM.AST.Type.void
native TInt = i64
native TBool = i1
native (TArrow ta tb) = FunctionType {
    argumentTypes = [native ta]
  , resultType = native tb
  , isVarArg = False
  }

-- | Get pointer to a local variable
--
-- > i64 f -> i64* f
pointer :: Operand -> Operand
pointer (LocalReference t name') = LocalReference pt name'
  where
    pt :: Type
    pt = PointerType t $ AddrSpace 0
pointer x = error $ "Cannot make a pointer for " <> show x

-- | Make an operand out of a global function
--
-- > %f -> @f
externf :: Tipe -> Text -> Operand
externf t n = ConstantOperand $ global t n

-- | Generate code for a top level binding
--
-- The only allowed top level constructs are global variables and function
-- definitions. Any other term in top level will raise an error.
gen :: Bind -> Codegen Operand

-- | A top level variable could be an alias, but its an error for now
gen (Bind _ (Var ref)) = err $ "Top level alias " <> rname ref

-- | Add a constant global variable
gen (Bind ref lit@(Lit _val)) = do
    op@(ConstantOperand value') <- step lit

    define $  global' value'
    return op

  where
    global' :: Constant -> Global
    global' val = globalVariableDefaults
      { name = Name $ toShort $ toS $ rname ref
      , initializer = Just val
      , type' = native $ tipe lit
      }

-- | Top level lambda expression
gen (Bind n (Lam t ref body)) = do
    -- [TODO] - Localize this computation
    -- Make a new block for this function and add to `GenState`
    modify $ \s -> s {blocks = blocks s ++ [blockState n]}

    result <- step body
    term' <- terminator result

    instructions <- stack <$> current

    let fn = functionDefaults {
          name          = lname $ rname n
        , parameters  = ([Parameter ty nm [] | (ty, nm) <- params], False)
        , returnType  = native $ retT t
        , basicBlocks = [basicBlock instructions term']
      }

    define fn
    return $ local t $ rname ref
  where
    params :: [(Type, Name)]
    params = case t of
      (TArrow ta _) -> [(native ta, lname $ rname ref)]
      _             -> []

gen (Bind r App{}) = err $ "Top level function call " <> rname r

-- | Generate code for an expression not at the top level
--
-- Step should return an operand, which is the LHS of the operand it just dealt
-- with. Step is free to push instructions to the current block.
step :: Expr -> Codegen Operand

-- | Convert a reference into a local operand.
step (Var (Ref n t Local)) = return $ local t n

-- | Use a global variable
step (Var (Ref n t Global)) = load t $ externf t n

-- | Make a constant operand out of the constant
step (Lit (LNumber n)) = return $ ConstantOperand $ Int 64 (toInteger n)
step (Lit (LBool True)) = return $ ConstantOperand $ Int 1 1
step (Lit (LBool False)) = return $ ConstantOperand $ Int 1 0

-- Apply the function
step (App _t (Lam t (Ref n _ _) _body) val) = do
    arg <- step val
    call (retT t) (externf t n) arg

-- | Apply function by name
step (App _t (Var (Ref n t Global)) val) = do
    arg <- step val
    call (retT t) (externf t n) arg

-- | Apply something that is not a function
step (App _t a _) = err $ "Applied non function " <> show a

-- \ Higher order function - throw an error
step (Lam _t ref _) = err $ "Higher order function" <> show ref

-- * Code generation

-- | Make an LLVM module from a `Progn`
compile :: Progn -> Either Error Module
compile prog = execM (run prog) genState >>= return . mod
  where
    -- | Step through the AST and _throw_ away the results
    run :: Progn -> Codegen ()
    run (Progn ps e) = mapM_ gen t
      where
        -- [TODO] - This is a terrible approximation
        t = ps ++ [Bind (Ref "main" (TArrow TInt TInt) Global)
                    (Lam (TArrow TInt TInt) (Ref "_" TInt Local) e)]

-- | Tweak passes of LLVM compiler
--
-- More info on opt passes:
--   - http://www.stephendiehl.com/llvm/#optimization-passes
--   - https://www.stackage.org/haddock/nightly-2017-06-28/llvm-hs-4.2.0/LLVM-PassManager.html
passes :: PassSetSpec
passes = defaultCuratedPassSetSpec {optLevel = Just 0}

-- | Generate native code with C++ FFI
toLLVM :: Module -> IO Text
toLLVM modl =
  withContext $ \context ->
      withModuleFromAST context modl $ \m -> do
           verify m
           withPassManager passes $ \pm -> do
                _ <- runPassManager pm m
                toS <$> moduleLLVMAssembly m

-- | Return compiled LLVM IR
llvm :: Progn -> IO (Either Error Text)
llvm ast = case compile ast of
  Left e     -> return $ Left e
  Right mod' -> toLLVM mod' >>= return . Right

err :: Text -> Codegen a
err = throwError . GenError
