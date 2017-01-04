module Parser where

import qualified Data.ByteString.Char8      as B
import           Text.Megaparsec
import           Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer as L

-- So. You'll notice that this file uses the do-notation as well.
-- However, the do-notation means something very different than what
-- it did in the assembler—whereas the assembler used do to express imperative
-- commands, the Parser monad uses do to express chunks of text that are swallowed
-- and processed. Herein lies the great strengths of monads, and of Haskell's treatment
-- thereof: it allows you to use the same syntax for very different computations, all
-- while providing laws that uses of this syntax must follow (so that it actually means something).
-- Some people view this power as a misfeature: they would rather have many different syntaxes
-- for many different problem domains. I respect the right of these people to be idiots.

ident :: Parser ByteString
ident = do
  text <- some alphaChar
  return $ B.pack text

instruction :: Parser Instruction
instruction = choice [label, store, comp] <* whiteSpace

store :: Parser Instruction
store = char '@' *> choice [lit, sym] where
  lit = do
    num <- number
    return . StoreLit . fromIntegral $ num

  sym = do
    text <- ident
    return . StoreSym $ ident

-- This is a combinator that, given a datum and a string, attempts
-- to read the string from input and return the provided datum.
-- The semantics of the parser monad means that if the string cannot be
-- read in full, this monadic action will fail, and pass execution on to
-- the next action in an enclosing `choice` block (if any).
-- More idiomatic treatments would use the <$ operator defined in Data.Functor
-- (`chomp a s` is equal to `a <$ string s`).
chomp :: a -> String -> Parser a
chomp res s = do
  _ <- string s
  return res

-- Pretty straightforward. `chomp` every possibility.
dest :: Parser CDest
dest = choice [ chomp Zero "0"
              , chomp One "1"
              , chomp NegOne "-1"
              , chomp (NotReg D) "!D"
              , chomp (NotReg A) "!A"
              , chomp (NotReg M) "!M"
              , chomp (NegReg D) "-D"
              , chomp (NegReg A) "-A"
              , chomp (NegReg A) "-M"
              , chomp (SuccReg D) "D+1"
              , chomp (SuccReg A) "A+1"
              , chomp (SuccReg M) "M+1"
              , chomp (PredReg D) "D-1"
	      , chomp (PredReg A) "A-1"
	      , chomp (PredReg M) "M-1"
	      , chomp (AddReg D A) "D+A"
	      , chomp (AddReg D M) "D+M"
	      , chomp (SubReg D A) "D-A"
	      , chomp (SubReg D M) "D-M"
	      , chomp (SubReg A D) "A-D"
	      , chomp (SubReg M D) "M-D"
	      , chomp (AndReg D A) "D&A"
	      , chomp (AndReg D M) "D&M"
	      , chomp (OrReg D A) "D|A"
	      , chomp (OrReg D M) "D|M"
	      , return NoDest
              ]

-- Blah blah blah.
dest :: Parser CDest
dest = choice [ chomp MemDAndA "AMD"
   	      , chomp DAndA "AD"
	      , chomp MemAndA "AM"
	      , chomp IntoA "A"
	      , chomp MemAndD "MD"
	      , chomp MemAtA "M"
	      , chomp IntoD "D"
              , return NoDest
              ]

-- Yad
jump :: Parser CJump
jump = choice [ chomp JGT "JGT"
	      , chomp JEQ "JEQ"
	      , chomp JGE "JGE"
	      , chomp JLT "JLT"
	      , chomp JNE "JNE"
	      , chomp JLE "JLE"
	      , chomp JMP "JMP"
              , return JNull
              ]

label :: Parser Instruction
label = do
  text <- ident
  return $ Label text

-- We use the Megaparsec `whiteSpace` parser to automatically consume
-- all space characters and comments. We invoke this at the end of
-- `instruction` to munch all subsequent whitespace before the next instruction.
whiteSpace :: Parser ()
whiteSpace = L.space spaceChar (skipLineComment "//") (return ())

parseInstructions :: FilePath -> IO [Instruction]
parseInstructions path = do
  contents <- B.readFile path
  let result = parse path (whiteSpace *> some instruction) contents
  case result of
    Left e -> fail ("Parse error: " <> show e)
    Right r -> return r
