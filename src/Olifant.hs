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
data CodegenState = CodegenState {
    stack :: [Named Instruction]
  , counter :: Int
  } deriving Show

-- | Codegen state monad
newtype Codegen a = Codegen {
    runCodegen :: State CodegenState a
  } deriving (Functor, Applicative, Monad, MonadState CodegenState)

-- | Specialized state monad holding a module
newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

-- | Runs the operation with an initial state
runLLVM :: AST.Module -> LLVM a -> AST.Module
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
terminator :: Codegen Terminator
terminator = do
  n <- gets counter
  return $ Ret (Just $ local $ UnName $ fromIntegral n - 1) []

---
--- LLVM type handlers
---

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

run :: Calc -> LLVM AST.Module
run calc = do
    module' <- get
    let x = execState (runCodegen (step calc)) (CodegenState [] 0)
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
seven = Plus (Number 4)  (Plus (Number 1121) (Number 2))

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

main :: IO ()
main = do
    let modn = mkModule "calc" :: AST.Module

    withContext $ \context -> do
        let newast = runLLVM modn (run seven)

        -- print $ (basicBlocks moduleDefinitions $  newast)

        liftError $ withModuleFromAST context newast $ \m -> do
        llstr <- moduleLLVMAssembly m
        putStrLn llstr
