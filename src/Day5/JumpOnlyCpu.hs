module Day5.JumpOnlyCpu (
  mkJmpInterpreter
 ,incrementOnlyInterpreter
 ,threshold3Interpreter
 ,JmpInterpreter
 ,Instruction
 ,Operation
) where

import qualified Data.Sequence as S
import Data.Sequence (Seq, index, adjust)

{-
 Can easily be modified to record a history of operations if program execution time is expected to be small
 Originall Worked that way but it was very slow.
-}

type Instruction = Int
type InstructionSet = Seq Instruction
type Operation = Instruction -> Instruction

type OpProcessor = Operation -> InstructionSet -> ProgCounter -> (InstructionSet, ProgCounter)

type JmpInterpreter = [Instruction] -> Int
type ProgCounter = Int

{-# INLINE instIncrement #-}
instIncrement :: Operation
instIncrement = (+ 1)

{-# INLINE instMaintainThreshold #-}
instMaintainThreshold :: Int -> Operation
instMaintainThreshold threshold = let op v = if v >= threshold then v - 1 else v + 1 in op

{-# INLINE instThreshold3 #-}
instThreshold3 :: Operation
instThreshold3 = instMaintainThreshold 3

{-# INLINE processOperation #-}
processOperation :: OpProcessor
processOperation op iset pc =
  (newISet, newPC)
  where newISet = adjust op pc iset
        newPC = pc + index iset pc

executeProgram :: Operation -> ProgCounter -> InstructionSet -> Int -> Int
executeProgram op pc iset cycles
  | 0 <= pc && pc < S.length iset = executeProgram op newPC newISet (cycles + 1)
  | otherwise = cycles
    where (newISet, newPC) = processOperation op iset pc

mkProgramExecutor :: Operation -> (ProgCounter -> InstructionSet -> Int -> Int)
mkProgramExecutor = executeProgram

mkJmpInterpreter :: Operation -> JmpInterpreter
mkJmpInterpreter op = let execute = mkProgramExecutor op
                          interp insts = execute 0 (S.fromList insts) 0
                      in interp

incrementOnlyInterpreter :: JmpInterpreter
incrementOnlyInterpreter = mkJmpInterpreter instIncrement

threshold3Interpreter :: JmpInterpreter
threshold3Interpreter = mkJmpInterpreter instThreshold3
