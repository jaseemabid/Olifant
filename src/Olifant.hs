{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Olifant where

import Prelude hiding (mod)
import Control.Monad.Except
import Control.Monad.State
import Data.Text.Lazy.IO as TIO
import LLVM.AST
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

-- | 64 bit integer - the only supported type as of now
number :: Type
number = IntegerType 64

---
--- State
---

-- | Stack of instructions
data CodegenState = CodegenState {
    stack :: [Named Instruction]
  , counter :: Int
  } deriving Show

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
--- Codegen handlers
---

-- | Make a fresh unnamed variable; %4 or %5
unnamed :: Codegen Name
unnamed =  do
    n <- gets counter
    modify $ \s -> s {counter = n + 1}
    return $ UnName . fromIntegral $ n

-- | Step through the AST
--
-- This is wrong, needs a rewrite
step :: Calc -> Codegen Operand

step (Number n) = return $ ConstantOperand $ Int 64 n

step (Plus a b) = do
    lhs <- step a :: Codegen Operand
    rhs <- step b :: Codegen Operand

    -- %result = fadd 1 2a
    instr $ LLVM.AST.FAdd NoFastMathFlags lhs rhs []


-- | References
local ::  Name -> Operand
local = LocalReference number

-- | Return the last expression from a block
--
-- This is wrong. Should return strictly an unnamed variable
terminator :: Codegen (Named Terminator)
terminator = do
  n <- gets counter
  return $ Do $ Ret (Just $ local $ UnName $ fromIntegral n - 1) []

---
--- LLVM type handlers
---

-- | Create a module from name
--
-- Note: Add source file as well for AOT compilation
mkModule :: String -> Module
mkModule label = defaultModule { moduleName = label }

-- | Create a simple block with fixed terminator
mkBasicBlock :: Codegen BasicBlock
mkBasicBlock = do
    instructions <- gets stack
    term <- terminator
    new <- unnamed
    return $ BasicBlock (Name "entry") instructions term

-- | Name an instruction and add to stack
--
-- Converts a `Add 1 2` to `%6 = Add 1 2`
instr :: Instruction -> Codegen Operand
instr ins = do
  new <- unnamed
  let bound = new := ins
  modify $ \s -> s {stack = stack s ++ [bound]}
  return $ LocalReference number new

---
--- LLVM handlers
---

-- | Add a definition to the module
addDefn :: Definition -> LLVM ()
addDefn d = do
  definitions <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = definitions ++ [d] }

---
--- Everything below this line is crap
---

run :: Calc -> LLVM Module
run calc = do
    module' <- get
    let instructions = execState (runCodegen (step calc)) codegen
    let block = evalState (runCodegen mkBasicBlock) instructions

    -- add block to main
    let main'' = GlobalDefinition $ main' [block]

    addDefn main''
    return module'

  where
      main' :: [BasicBlock] -> Global
      main' blocks = functionDefaults {
          name = Name "main"
          , returnType = number
          , basicBlocks = blocks
        }

      -- | Default CodegenState
      codegen :: CodegenState
      codegen = CodegenState [] 0

---
--- A simple sample program
---

progn :: Calc
progn = Plus (Number 4)  (Plus (Number 1121) (Number 2))

---
--- Top level data
---

-- | Module to encapsulate everything
modn :: Module
modn = mkModule "calc"

-- | Generate native code with C++ FFI
toLLVM :: Module -> IO ()
toLLVM modl = withContext $ \context -> do
    errOrLLVM <- runExceptT $ withModuleFromAST context modl moduleLLVMAssembly
    case errOrLLVM of
      Left err -> Prelude.putStrLn $ "error: " ++ err
      Right llvm -> Prelude.putStrLn llvm

-- | Generate native code with C++ FFI
pretty :: Calc -> IO ()
pretty prog = TIO.putStrLn $ ppllvm $ runLLVM modn (run prog)

-- | Print compiled LLVM IR to stdout
main :: IO ()
main = toLLVM $ runLLVM modn (run progn)
