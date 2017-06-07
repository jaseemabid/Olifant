module Main where

import Protolude

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.CallingConvention
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.AST.Instruction
import LLVM.AST.Linkage
import LLVM.AST.Visibility

import Olifant

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [simple]

simple :: TestTree
simple = testCase "Compile simple arithmentic" $ compile progn @?= m

-- | Sample program `a = 1 + (2 + 3)`
progn :: [Calc]
progn = [Assignment "a" $ Plus (Number 1) (Plus (Number 2) (Number 3))]

-- | Module for `a = 1 + (2 + 3)`
m :: Module
m =
    Module
    { moduleName = "calc"
    , moduleSourceFileName = "<string>"
    , moduleDataLayout = Nothing
    , moduleTargetTriple = Nothing
    , moduleDefinitions =
          [ GlobalDefinition
                Function
                { linkage = External
                , visibility = Default
                , dllStorageClass = Nothing
                , callingConvention = C
                , returnAttributes = []
                , returnType = IntegerType {typeBits = 64}
                , name = Name "main"
                , parameters = ([], False)
                , functionAttributes = []
                , section = Nothing
                , comdat = Nothing
                , alignment = 0
                , garbageCollectorName = Nothing
                , prefix = Nothing
                , basicBlocks =
                      [ BasicBlock
                            (Name "entry")
                            [ UnName 0 :=
                              LLVM.AST.Instruction.Add
                              { nsw = False
                              , nuw = False
                              , operand0 =
                                    ConstantOperand
                                        Int {integerBits = 64, integerValue = 2}
                              , operand1 =
                                    ConstantOperand
                                        Int {integerBits = 64, integerValue = 3}
                              , metadata = []
                              }
                            , UnName 1 :=
                              LLVM.AST.Instruction.Add
                              { nsw = False
                              , nuw = False
                              , operand0 =
                                    ConstantOperand
                                        Int {integerBits = 64, integerValue = 1}
                              , operand1 =
                                    LocalReference
                                        IntegerType {typeBits = 64}
                                        (UnName 0)
                              , metadata = []
                              }
                            , Name "a" :=
                              Alloca
                              { allocatedType = IntegerType {typeBits = 64}
                              , numElements = Nothing
                              , alignment = 0
                              , metadata = []
                              }
                            , Do
                                  Store
                                  { volatile = False
                                  , address =
                                        LocalReference
                                            PointerType
                                            { pointerReferent =
                                                  IntegerType {typeBits = 64}
                                            , pointerAddrSpace = AddrSpace 0
                                            }
                                            (Name "a")
                                  , value =
                                        LocalReference
                                            IntegerType {typeBits = 64}
                                            (UnName 1)
                                  , maybeAtomicity = Nothing
                                  , alignment = 0
                                  , metadata = []
                                  }
                            , UnName 2 :=
                              Load
                              { volatile = False
                              , address =
                                    LocalReference
                                        PointerType
                                        { pointerReferent =
                                              IntegerType {typeBits = 64}
                                        , pointerAddrSpace = AddrSpace 0
                                        }
                                        (Name "a")
                              , maybeAtomicity = Nothing
                              , alignment = 0
                              , metadata = []
                              }
                            ]
                            (Do
                                 Ret
                                 { returnOperand =
                                       Just
                                           (LocalReference
                                                IntegerType {typeBits = 64}
                                                (UnName 2))
                                 , metadata' = []
                                 })
                      ]
                , personalityFunction = Nothing
                }
          ]
    }
