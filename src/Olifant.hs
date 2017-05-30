{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Olifant where

import           Control.Monad.Except
import           Control.Monad.State
import           LLVM.AST
import qualified LLVM.AST as AST
import           LLVM.AST.Constant
import           LLVM.AST.Global
import           LLVM.Context
import           LLVM.Module

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
newtype CodegenState = CodegenState {
    stack :: [Named Instruction]
  } deriving Show

-- | Codegen state monad
newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

-- | Specialized state monad holding a module
newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

-- | Runs the operation with an initial state
runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM modl (LLVM m) = execState m modl

---
--- State manipulators
---

-- | Add a definition to the module
addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

-- Count
count :: Codegen Int
count = length <$> gets stack

---
--- Manipulate LLVM types
---

-- | Make a fresh unnamed variable; %4 or %5
unnamed :: Codegen Name
unnamed = UnName . fromIntegral <$> count

-- | References
local ::  Name -> Operand
local = LocalReference number

-- | Create a module from name
--
-- Note: Add source file as well for AOT compilation
mkModule :: String -> AST.Module
mkModule label = defaultModule { moduleName = label }

-- | Create a simple block with fixed terminator
mkBasicBlock :: Codegen BasicBlock
mkBasicBlock = do
    instructions <- gets stack
    term <- terminator
    new <- unnamed
    return $ BasicBlock (Name "entry") instructions (new := term)

-- | Name an instruction and add to stack
--
-- Converts a `Add 1 2` to `%6 = Add 1 2`
instr :: Instruction -> Codegen Operand
instr ins = do
  n <- count
  -- XXX: This is potentially wrong
  let ref = Name $ show n
  let bound = ref := ins
  -- XXX: Abstract this away
  modify $ \(CodegenState s) -> CodegenState $ s ++ [bound]
  return $ local ref

-- | Step through the AST
--
-- This is wrong, needs a rewrite
step :: Calc -> Codegen Operand
step (Number n) = return $ ConstantOperand $ Int 64 n
step (Plus a b) = do
    -- l <- unnamed
    lhs <- step a
    -- let ls = l := lhs

    -- r <- unnamed
    rhs <- step b

    -- let rs = r := rhs

    instr $ LLVM.AST.FAdd NoFastMathFlags lhs rhs []

-- | Return the last expression from a block
--
-- This is wrong. Should return strictly an unnamed variable
terminator :: Codegen Terminator
terminator = do
  n <- count
  -- %5 -> return %5
  let ret ref = Ret $ Just $ local $ Name $ show $ ref - 1
  return $ ret n []

---
--- Everything below this line is crap
---

run :: Calc -> LLVM AST.Module
run calc = do
    module' <- get
    let x = execState (runCodegen (step calc)) (CodegenState []) :: CodegenState
    let block = evalState (runCodegen mkBasicBlock) x

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

seven :: Calc
seven = Plus (Number 4)  (Plus (Number 1) (Number 2))

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

main :: IO ()
main = do
    let modn = mkModule "calc" :: AST.Module

    withContext $ \context -> do
        let newast = runLLVM modn (run seven)
        liftError $ withModuleFromAST context newast $ \m -> do
        llstr <- moduleLLVMAssembly m
        putStrLn llstr
