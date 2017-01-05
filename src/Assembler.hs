{-# LANGUAGE OverloadedStrings #-}

module Assembler where

import           Control.Monad.RWS.Strict
import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8    as B
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Word

import           Instruction

data Assembler = Assembler
  { romCounter :: Word16
  , ramCounter :: Word16
  , symbols    :: Map ByteString Word16
  }

emptyAssembler :: Assembler
emptyAssembler = Assembler 0 0 mempty

-- Don't panic. This is a moderately complicated monad, but all will become clear.
-- The RWS (reader-writer-state) monad provides an ordered, sensible way to combine
-- three effectful operations:
-- 1) reading from an immutable environment. Our immutable environment is a list of Instructions.
-- 2) Accumulating a final result. Our result is a list of ByteStrings.
-- 3) reading/modifying some mutable state. Our state is the Assembler datum.
-- An alternative way to do this would be a function of two arguments ([Instruction] and Assembler)
-- and returning
type AssemblyM a = RWS [Instruction] [ByteString] Assembler a

-- this is the entry point for our monadic computation. note how its
-- signature is pure but it dispatches to impure functions with the execRWS function
-- (execRWS returns a tuple of the final state (an Assembler) and the accumulated value,
-- but we're only interested in the accumulated value, so we hit it with snd)
runAssembly :: [Instruction] -> [ByteString]
runAssembly instrs = snd $ execRWS (buildSymbolTable >> assemble) instrs emptyAssembler

recordWordPadded :: Word16 -> AssemblyM ()
recordWordPadded w = tell [zeroes <> given] -- tell records the given datum in the accumulated writer-component
  where given = bshow w
        needed = 16 - B.length given
        zeroes = B.replicate needed '0'

setSymbol :: ByteString -> Word16 -> AssemblyM ()
setSymbol s w = do
  -- given a function, `gets` applies it to the current state (our Assembler)
  -- and returns the value.
  oldSyms <- gets symbols
  -- similarly, given a function, `modify`
  modify (\a -> a { symbols = M.insert s w oldSyms })

buildSymbolTable :: AssemblyM ()
buildSymbolTable = do
  instrs <- ask

  forM_ instrs $ \i -> do
    rom <- gets romCounter
    case i of
      Label l -> setSymbol l rom
      _       -> modify (\s -> s { romCounter = rom + 1 })


  forM_ [0..15] $ \r -> do
    let reg = "R" <> (bshow r)
    setSymbol reg r

  setSymbol "SP" 0
  setSymbol "LCL" 1
  setSymbol "ARG" 2
  setSymbol "THIS" 3
  setSymbol "THAT" 4
  setSymbol "SCREEN" 16384
  setSymbol "KBD" 24576
  modify' (\s -> s { ramCounter = 16 })

assemble :: AssemblyM ()
assemble = do
  -- fetch the first the first instruction in the current state
  -- asks takes a function and applies it to the reader-component
  instr <- asks head

  case instr of
    Label _ -> return ()
    -- computation: dispatch to recordComp
    -- the c@CComp syntax allows us to save the matched variable under a name.
    Comp comp dst jump -> tell ["111" <> acc] where acc = B.concat [toChunk comp, toChunk dst, toChunk jump]
    -- literal store: just return the base-2 immediate value
    StoreLit addr -> recordWordPadded addr
    -- symbolic store: look up the symbol, and dispatch appropriately
    StoreSym sym -> do
      -- get the symbols slot and pass it to the map-lookup function.
      -- this returns a Maybe Word16 value
      mLookedUp <- gets (M.lookup sym . symbols)
      case mLookedUp of
        -- cache hit!
        Just result -> recordWordPadded result
        -- cache miss
        Nothing -> do
          ram <- gets ramCounter
          setSymbol sym ram
          recordWordPadded ram
          modify' (\s -> s { ramCounter = ram + 1 })

  -- Ask our reader environment: do you have any values left in your tail?
  done <- asks (null . tail)
  if done
     then return () -- bail out, we're done
     else local tail assemble -- recurse, but drop the first instruction

bshow :: Show a => a -> ByteString
bshow = B.pack . show
