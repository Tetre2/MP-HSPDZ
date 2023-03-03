module VMops (addc, adds, addm, ldi, ldsi, inputmixed, printregplain, open, startfile, endfile) where

import Data.Binary as B
import Data.Binary.Put
import Data.ByteString.Lazy

startfile :: ByteString
startfile = runPut $ putWord32be 0x0

endfile :: ByteString
endfile = runPut $ do
  putWord32be 0x3
  putWord32be 0x0
  putWord32be 0x0
  putWord32be 0x1fff
  putWord32be 0x0
  putWord32be 0x103
  putWord32be 0x0
  putWord32be 0x0
  putWord32be 0x1fff
  putWord32be 0x0
  putWord32be 0xca
  putWord32be 0x0
  putWord32be 0x0
  putWord32be 0x1fff
  putWord32be 0x0
  putWord32be 0x4
  putWord32be 0x0
  putWord32be 0x0
  putWord32be 0x1fff
  putWord32be 0x0
  putWord32be 0x104
  putWord32be 0x0
  putWord32be 0x0
  putWord32be 0x1ff

endop :: Word32
endop = 0

-- Addition of clear integers
addc :: Word32 -> Word32 -> Word32 -> ByteString
addc res v1 v2 = runPut $ add 0x20 res v1 v2

-- Addition of secret integers
adds :: Word32 -> Word32 -> Word32 -> ByteString
adds res v1 v2 = runPut $ add 0x21 res v1 v2

-- Addition of mixed integers
addm :: Word32 -> Word32 -> Word32 -> ByteString
addm res v1 v2 = runPut $ add 0x22 res v1 v2

add :: Word32 -> Word32 -> Word32 -> Word32 -> Put
add addop res v1 v2 = do
  putWord32be addop
  putWord32be res
  putWord32be v1
  putWord32be v2
  putWord32be endop

-- Assign (constant) immediate value to clear register (vector)
ldi :: Word32 -> Word32 -> ByteString
ldi dest val = runPut $ assign 0x1 dest val

-- Assign (constant) immediate value to secret register (vector)
ldsi :: Word32 -> Word32 -> ByteString
ldsi dest val = runPut $ assign 0x2 dest val

assign :: Word32 -> Word32 -> Word32 -> Put
assign assop dest val = do
  putWord32be assop
  putWord32be dest
  putWord32be val
  putWord32be endop

-- Store private input in secret registers (vectors).
-- The input is read as integer or floating-point number and the latter is then converted to the internal representation using the given precision.
-- This instruction uses compile-time player numbers.
inputmixed :: Word32 -> Word32 -> ByteString
inputmixed dest player = runPut $ getInput dest player

getInput :: Word32 -> Word32 -> Put
getInput dest player = do
  putWord32be 0xF2
  putWord32be 0x3
  putWord32be 0x0
  putWord32be dest
  putWord32be player
  putWord32be endop

printregplain :: Word32 -> ByteString
printregplain source = runPut $ do
  printOutput source
  printCharacter 0x0a

printOutput :: Word32 -> Put
printOutput source = do
  putWord32be 0xb3
  putWord32be source
  putWord32be endop

printCharacter :: Word32 -> Put
printCharacter char = do
  putWord32be 0xb4
  putWord32be char
  putWord32be endop

open :: Word32 -> Word32 -> ByteString
open dest source = runPut $ reveal dest source

reveal :: Word32 -> Word32 -> Put
reveal dest source = do
  putWord32be 0xa5
  putWord32be 0x3
  putWord32be 0x1
  putWord32be dest
  putWord32be source
  putWord32be endop

--Clear subtraction
subc :: Word32 -> Word32 -> Word32 -> ByteString
subc res v1 v2 = runPut $ subtract' 0x25 res v1 v2

--Secret subtraction
subs :: Word32 -> Word32 -> Word32 -> ByteString
subs res v1 v2 = runPut $ subtract' 0x26 res v1 v2

--Subtract clear from secret value (v1 secret, v2 clear)
subml :: Word32 -> Word32 -> Word32 -> ByteString
subml res v1 v2 = runPut $ subtract' 0x27 res v1 v2

--Subtract secret from clear value (v1 clear, v2 secret)
submr :: Word32 -> Word32 -> Word32 -> ByteString
submr res v1 v2 = runPut $ subtract' 0x28 res v1 v2

subtract' :: Word32 -> Word32 -> Word32 -> Word32 -> Put
subtract' op res v1 v2 = do
  putWord32be op
  putWord32be res
  putWord32be v1
  putWord32be v2
  putWord32be endop