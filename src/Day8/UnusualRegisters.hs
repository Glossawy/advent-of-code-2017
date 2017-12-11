{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
module Day8.UnusualRegisters (findGreatestAlltimeRegister, findGreatestValuedRegister) where

import Data.Map ((!))
import qualified Data.Map as Map

type RegisterMap = Map.Map String Int
type Comparator = Int -> Int -> Bool
data Operation = Inc | Dec
type Program = [Instruction]
data Instruction = Instruction { operandReg :: String
                                ,operation :: Operation
                                ,operandAmt :: Int
                                ,compareReg :: String
                                ,comparators :: [Comparator]
                                ,compareAmt :: Int }

parseOperation :: String -> Operation
parseOperation x
  | x == "inc" = Inc
  | x == "dec" = Dec
  | otherwise  = error "Operation must be inc or dec"

parseComparators :: String -> [Comparator]
parseComparators x = if x == "!=" then [(/=)] else map fetchCmp x
  where fetchCmp c
          | c == '<' = (<)
          | c == '=' = (==)
          | c == '>' = (>)
          | otherwise = error "Unexpected comparator type"

parseInstruction :: String -> Instruction
parseInstruction (words -> [reg, op, amt, _, cmpReg, cmp, cmpAmt]) =
  Instruction reg (parseOperation op) (read amt) cmpReg (parseComparators cmp) (read cmpAmt)

parseProgram :: String -> Program
parseProgram = map parseInstruction . filter (/= "") . lines

allocateRegisters :: Program -> RegisterMap
allocateRegisters prog = Map.fromList $ map (,0) $ (compareReg <$> prog) ++ (operandReg <$> prog)

process :: RegisterMap -> Instruction -> RegisterMap
process rm inst = if not validCondition then rm else case operation inst of
  Inc -> Map.insert (operandReg inst) (operandValue + operandAmt inst) rm
  Dec -> Map.insert (operandReg inst) (operandValue - operandAmt inst) rm
  where operandValue = rm ! operandReg inst
        cmpValue = rm ! compareReg inst
        validCondition = any (\f -> f cmpValue (compareAmt inst)) $ comparators inst

interpret :: RegisterMap -> Program -> [RegisterMap]
interpret rm [] = [rm]
interpret rm (i:is) = let next = process rm i in rm : interpret next is

startProgram :: Program -> [RegisterMap]
startProgram prog = interpret (allocateRegisters prog) prog

findGreatestValuedRegister :: String -> Int
findGreatestValuedRegister = maximum . Map.elems . last . startProgram . parseProgram

findGreatestAlltimeRegister :: String -> Int
findGreatestAlltimeRegister = maximum . map (maximum . Map.elems) . startProgram . parseProgram
