{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Olifant where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import           LLVM.AST
import qualified LLVM.AST as AST
import           LLVM.AST.Constant
import           LLVM.AST.Global
import           LLVM.Context
import           LLVM.Module

---
--- AST
---

-- Simple integer language with just numbers, + and -
data Calc = Number Integer
          | Plus Calc Calc

---
--- LLVM Types
---

-- The only supported type as of now
number :: Type
number = IntegerType 64

---
--- Codegen
---

-- type SymbolTable = [(String, Operand)]
-- type Names = Map.Map String Int


-- %foo = add 1 2
newtype CodegenState = CodegenState [Named Instruction] -- Stack of instructions
    deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

-- Specialized state monad holding a module
newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

-- Runs the operation with the specific state
runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM modl (LLVM m) = execState m modl

-- Add source file as well for AOT
emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

-- define :: --
-- external :: --

--  1 + 3

-- Add stuff into main block
mkBasicBlock :: Codegen BasicBlock
mkBasicBlock = do
    ins <- stack
    term <- terminator
    new <- fresh
    return $ BasicBlock (Name "entry") ins (new := term)

count :: Codegen Int
count = do
  CodegenState st <- get
  return $ length st

fresh :: Codegen Name
fresh = UnName . fromIntegral <$> count

stack :: Codegen [Named Instruction]
stack = do
  CodegenState st <- get
  return st

-- | Add an instruction to state; name
--
-- Converts a `Add 1 2` to `%6 = Add 1 2`
instr :: Instruction -> Codegen Operand
instr ins = do
  n <- count
  let ref = Name $ show $ fromIntegral n

  let bound = ref := ins
  modify $ \(CodegenState s) -> CodegenState $ s ++ [bound]
  return $ local ref

-- a + b ; operand for this

-- References
local ::  Name -> Operand
local = LocalReference number

step :: Calc -> Codegen Operand
step (Number n) = return $ ConstantOperand $ Int 64 n
step (Plus a b) = do
    -- l <- fresh
    lhs <- step a
    -- let ls = l := lhs

    -- r <- fresh
    rhs <- step b

    -- let rs = r := rhs

    instr $ LLVM.AST.FAdd NoFastMathFlags lhs rhs []

terminator :: Codegen Terminator
terminator = do
  n <- count
  -- %5 -> return %5
  let ret ref = Ret $ Just $ local $ Name $ show $ fromIntegral $ ref-1
  return $ ret n []

dummy ::  Codegen ()
dummy = return ()

run :: Calc -> LLVM AST.Module
run calc = do
    mod <- get
    let x = execState (runCodegen (step calc)) (CodegenState []) :: CodegenState
    let block = evalState (runCodegen mkBasicBlock) x

    -- add block to main
    let main'' = GlobalDefinition $ main' [block]

    addDefn main''
    return mod

  where
      main' :: [BasicBlock] -> Global
      main' blocks = functionDefaults {
          name = Name "main"
          , returnType = number
          , basicBlocks = blocks
        }

zero :: Calc
zero = Number 0

seven :: Calc
seven = Plus (Number 4)  (Plus (Number 1) (Number 2))

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

main :: IO ()
main = do
    let modn = (emptyModule "calc") :: AST.Module

    withContext $ \context -> do
        let newast = runLLVM modn (run seven)
        liftError $ withModuleFromAST context newast $ \m -> do
        llstr <- moduleLLVMAssembly m
        putStrLn llstr

    -- print $ moduleLLVMAssembly $
    -- return ()
