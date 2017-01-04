{-# LANGUAGE BinaryLiterals #-}

module Instruction where

import           Data.ByteString.Char8 (ByteString)
import           Data.Word

data Instruction
  = StoreLit Literal
  | StoreSym ByteString
  | Comp CComp CDest CJump

data Reg = D | A | M deriving Show

-- this is what we call a "lawless typeclass", which is
-- technically bad practice, but to hell with it
class ToChunk a where toChunk :: a -> ByteString

type CComp
  = Zero
  | One
  | NegOne
  | Register Reg
  | NotReg Reg
  | NegReg Reg
  | SuccReg Reg
  | PredReg Reg
  | AddReg Reg Reg
  | SubReg Reg Reg
  | AndReg Reg Reg
  | OrReg Reg Reg
    deriving Show

instance ToChunk a where
  toChunk Zero         = "0101010"
  toChunk One          = "0111111"
  toChunk NegOne       = "0111010"
  toChunk (Register D) = "0001100"
  toChunk (Register A) = "0110000"
  toChunk (Register M) = "1110000"
  toChunk (NotReg D)   = "0001101"
  toChunk (NotReg A)   = "0110001"
  toChunk (NotReg M)   = "1110001"
  toChunk (NegReg D)   = "0001111"
  toChunk (NegReg A)   = "0110011"
  toChunk (NegReg M)   = "1110011"
  toChunk (SuccReg D)  = "0011111"
  toChunk (SuccReg A)  = "0110111"
  toChunk (SuccReg M)  = "1110111"
  toChunk (PredReg D)  = "0001110"
  toChunk (PredReg A)  = "0110010"
  toChunk (PredReg M)  = "1110010"
  toChunk (AddReg D A) = "0000010"
  toChunk (AddReg D M) = "1000010"
  toChunk (SubReg D A) = "0010011"
  toChunk (SubReg D M) = "1010011"
  toChunk (SubReg A D) = "0000111"
  toChunk (SubReg M D) = "1000111"
  toChunk (AndReg D A) = "0000000"
  toChunk (AndReg D M) = "1000000"
  toChunk (OrReg D A)  = "0010101"
  toChunk (OrReg D M)  = "1010101"

data CDest
  = NoDest
  | MemAtA
  | IntoD
  | MemAndD
  | IntoA
  | MemAndA
  | DAndA
  | MemDAndA

instance ToChunk CDest where
  toChunk NoDest   = "000"
  toChunk MemAtA   = "001"
  toChunk IntoD    = "010"
  toChunk MemAndD  = "011"
  toChunk IntoA    = "100"
  toChunk MemAndA  = "101"
  toChunk DAndA    = "110"
  toChunk MemDAndA = "111"

data CJump
  = JNull
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP

instance ToChunk CJump where
  toChunk JNull = "000"
  toChunk JGT   = "001"
  toChunk JEQ   = "010"
  toChunk JGE   = "011"
  toChunk JLT   = "100"
  toChunk JNE   = "101"
  toChunk JLE   = "110"
  toChunk JMP   = "111"
