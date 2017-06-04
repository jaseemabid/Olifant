{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Olifant where

import Prelude hiding (mod)

import Control.Monad.Except
import Control.Monad.State
import Data.Default
import Data.Text.Lazy.IO as TIO
import LLVM.AST
import LLVM.AST.AddrSpace (AddrSpace(..))
import LLVM.AST.Constant (Constant(..))
import LLVM.AST.Global (Global(..))
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
import LLVM.Pretty (ppllvm)

---
--- Language definition
---

-- | Simple integer language with just numbers, + and -
data Calc = Number Integer
          | Plus Calc Calc
          | Binding String
          | Assignment String Calc

-- | 64 bit integer
number :: Type
number = IntegerType 64

-- | Pointer to the `number` type
pointer :: Type
pointer = PointerType number $ AddrSpace 0

---
--- State
---

-- | Symbol tables maps a string to a LLVM operand
type SymbolTable = [(String, Operand)]

-- | Stack of instructions
data CodegenState =
    CodegenState
    { stack  :: [Named Instruction]  -- List of operations
    , symtab :: SymbolTable -- Symbol table
    , counter :: Int
    } deriving Show

instance Default CodegenState where
    def = CodegenState
      { stack = []
      , symtab = []
      , counter = 0
      }

-- | Codegen state monad
newtype Codegen a = Codegen {
    runCodegen :: State CodegenState a
  } deriving (Functor, Applicative, Monad, MonadState CodegenState)

-- | A specialized state monad holding a Module
--
-- The state monad upon evaluation will emit a Module containing the AST.
newtype LLVM a = LLVM (State Module a)
  deriving (Functor, Applicative, Monad, MonadState Module)

-- | Runs the operation with an initial state
runLLVM :: Module -> LLVM a -> Module
runLLVM modl (LLVM m) = execState m modl

---
--- Primitive wrappers
---

-- | Variable assignment
--
-- Converts expression of the form `%1 = 2` to `* %1 = 2`
--
-- XXX: Having to use `error` here is shitty
store :: Operand -> Operand -> Codegen ()
store (LocalReference _type n) val =
    push $ Do $ Store False ptr val Nothing 0 []
  where
    ptr = LocalReference pointer n
store _ _ = error "Cannot store anything other than local reference"

-- | Fetch a variable from memory
--
-- Enforce operand is a pointer type. This is shitty types :/
load :: Operand -> Codegen Operand
load (LocalReference _type n) = unnamed $ Load False ptr Nothing 0 []
  where
    ptr = LocalReference pointer n
load _ = error "Cannot load anything other than local pointer reference"

alloca :: Type -> Maybe String -> Codegen Operand
alloca ty Nothing = unnamed $ Alloca ty Nothing 0 []
alloca ty (Just ref)= named ref $ Alloca ty Nothing 0 []

assign :: String -> Operand -> Codegen ()
assign var x = modify $ \s -> s { symtab = (var, x) : symtab s }

add :: Operand -> Operand -> Instruction
add lhs rhs = LLVM.AST.Add False False lhs rhs []

---
--- AST Manipulation
---

-- | Name an instruction and add to stack.
--
--  - Takes an expression of the form `Add 1 2`
--  - Gets a fresh name for it, `%2`
--  - Adds `%2 = Add 1 2` to the stack
--  - Returns `%2`
--
unnamed :: Instruction -> Codegen Operand
unnamed ins = do
    new <- fresh
    push $ new := ins
    return $ LocalReference number new
  where
    -- | Make a fresh unnamed variable; %4 or %5
    fresh :: Codegen Name
    fresh =  do
        n <- gets counter
        modify $ \s -> s {counter = n + 1}
        return $ UnName . fromIntegral $ n

-- | Add the instruction to the stack with a specific name.
--
--  - Takes an expression of the form `Add 1 2` and a name `%foo`
--  - Adds `%foo = Add 1 2` to the stack
--  - Returns `%foo`
--
named :: String -> Instruction -> Codegen Operand
named str ins =
    push (op := ins) >> return (LocalReference number op)
  where
    op :: Name
    op = Name str

-- | Push a named instruction to stack
push :: Named Instruction -> Codegen ()
push ins = modify $ \s -> s {stack = stack s ++ [ins]}

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

step (Assignment bind ref) = do
    result <- step ref
    ptr <- alloca number (Just bind) :: Codegen Operand
    store ptr result
    assign bind ptr
    return ptr

-- | Find the operand for the variable from the symbol table and return it.
step (Binding binding) = do
    symbols <- gets symtab
    case lookup binding symbols of
        Just op -> return op
        Nothing -> error $ "> Undefined variable " ++ binding

---
--- Module level elements
---

-- | Create a simple block from a list of instructions and a terminator
mkBasicBlock :: [Named Instruction] -> Named Terminator -> BasicBlock
mkBasicBlock = BasicBlock (Name "entry")

-- | Return the last expression from a block
mkTerminator :: Operand -> Codegen (Named Terminator)
mkTerminator result = do
    r <- load result
    return $ Do $ Ret (Just r) []

---
--- Default values
---

progn :: Calc
progn = Assignment "a" $ Plus (Number 1) (Plus (Number 2) (Number 3))

---
--- Code generation
---

run :: Calc -> Codegen Module
run calc = do
    result <- step calc
    terminator <- mkTerminator result

    ins <- gets stack

    let block = mkBasicBlock ins terminator
    let main'' = GlobalDefinition $ main' [block]

    return $ defaultModule {
          moduleName = "calc"
        , moduleDefinitions = [main'']
        }

  where
      main' :: [BasicBlock] -> Global
      main' blocks = functionDefaults {
          name = Name "main"
          , returnType = number
          , basicBlocks = blocks
        }

-- | Generate native code with C++ FFI
toLLVM :: Module -> IO ()
toLLVM mod = withContext $ \context -> do
    errOrLLVM <- runExceptT $ withModuleFromAST context mod moduleLLVMAssembly
    case errOrLLVM of
      Left err -> Prelude.putStrLn $ "error: " ++ err
      Right llvm -> Prelude.putStrLn llvm

compile :: Calc -> Module
compile prog = evalState (runCodegen (run prog)) def

-- | Generate native code with C++ FFI
pretty :: Calc -> IO ()
pretty = TIO.putStrLn . ppllvm . compile

-- | Print compiled LLVM IR to stdout
main :: IO ()
main = toLLVM $ compile progn
