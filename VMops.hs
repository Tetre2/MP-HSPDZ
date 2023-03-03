module VMops (Public (..), Secret (..), 
  addc, adds, addml, addmr,
  ldi, ldsi,
  inputmixed, 
  printregplain, 
  open, 
  startfile, endfile,
  subc, subml, submr, subs,
  muls, mulc) where

import Data.Binary as B
import Data.Binary.Put
import Data.ByteString.Lazy


newtype Public = Public Int deriving (Show)

newtype Secret = Secret Int deriving (Show)


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
addc :: Public -> Public -> Public -> ByteString
addc (Public res) (Public v1) (Public v2) = runPut $ add 0x20 (fromIntegral res) (fromIntegral v1) (fromIntegral v2)

-- Addition of secret integers
adds :: Secret -> Secret -> Secret -> ByteString
adds (Secret res) (Secret v1) (Secret v2) = runPut $ add 0x21 (fromIntegral res) (fromIntegral v1) (fromIntegral v2)

-- Addition of mixed integers
addmr :: Secret -> Public -> Secret -> ByteString
addmr (Secret res) (Public v1) (Secret v2) = runPut $ add 0x22 (fromIntegral res) (fromIntegral v1) (fromIntegral v2)

-- Addition of mixed integers
addml :: Secret -> Secret -> Public -> ByteString
addml (Secret res) (Secret v1) (Public v2) = runPut $ add 0x22 (fromIntegral res) (fromIntegral v1) (fromIntegral v2)

add :: Word32 -> Word32 -> Word32 -> Word32 -> Put
add addop res v1 v2 = do
  putWord32be addop
  putWord32be res
  putWord32be v1
  putWord32be v2
  putWord32be endop


-- Assign (constant) immediate value to clear register (vector)
ldi :: Public -> Int -> ByteString
ldi (Public dest) val = runPut $ assign 0x1 (fromIntegral dest) (fromIntegral val)

-- Assign (constant) immediate value to secret register (vector)
ldsi :: Secret -> Int -> ByteString
ldsi (Secret dest) val = runPut $ assign 0x2 (fromIntegral dest) (fromIntegral val)

assign :: Word32 -> Word32 -> Word32 -> Put
assign assop dest val = do
  putWord32be assop
  putWord32be dest
  putWord32be val
  putWord32be endop

-- Store private input in secret registers (vectors).
-- The input is read as integer or floating-point number and the latter is then converted to the internal representation using the given precision.
-- This instruction uses compile-time player numbers.
inputmixed :: Secret -> Int -> ByteString
inputmixed (Secret dest) player = runPut $ getInput (fromIntegral dest) (fromIntegral player)

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

--Reveal secret registers (vectors) to clear registers (vectors)
open :: Public -> Secret -> ByteString
open (Public dest) (Secret source) = runPut $ reveal (fromIntegral dest) (fromIntegral source)

reveal :: Word32 -> Word32 -> Put
reveal dest source = do
  putWord32be 0xa5
  putWord32be 0x3
  putWord32be 0x1
  putWord32be dest
  putWord32be source
  putWord32be endop

--Clear subtraction
subc :: Public -> Public -> Public -> ByteString
subc (Public res) (Public v1) (Public v2) = runPut $ subtract' 0x25 (fromIntegral res) (fromIntegral v1) (fromIntegral v2)

--Secret subtraction
subs :: Secret -> Secret -> Secret -> ByteString
subs (Secret res) (Secret v1) (Secret v2) = runPut $ subtract' 0x26 (fromIntegral res) (fromIntegral v1) (fromIntegral v2)

--Subtract clear from secret value (v1 secret, v2 clear)
subml :: Secret -> Secret -> Public -> ByteString
subml (Secret res) (Secret v1) (Public v2) = runPut $ subtract' 0x27 (fromIntegral res) (fromIntegral v1) (fromIntegral v2)

--Subtract secret from clear value (v1 clear, v2 secret)
submr :: Secret -> Public -> Secret -> ByteString
submr (Secret res) (Public v1) (Secret v2) = runPut $ subtract' 0x28 (fromIntegral res) (fromIntegral v1) (fromIntegral v2)

subtract' :: Word32 -> Word32 -> Word32 -> Word32 -> Put
subtract' op res v1 v2 = do
  putWord32be op
  putWord32be res
  putWord32be v1
  putWord32be v2
  putWord32be endop


-- Offline data usage. Necessary to avoid reusage while using preprocessing from files. 
-- Also used to multithreading for expensive preprocessing.
use :: ByteString
use = runPut $ do
  putWord32be 0x17
  putWord32be 0x0
  putWord32be 0x0
  putWord32be 0x1
  putWord32be endop


-- (Element-wise) multiplication of secret registers (vectors).
muls :: Secret -> Secret -> Secret -> ByteString
muls (Secret res) (Secret v1) (Secret v2) = Data.ByteString.Lazy.concat [m, use] -- Unkonwn why use op is needed
  where m = runPut $ do{ 
    putWord32be 0xa6;
    putWord32be 0x3;
    putWord32be (fromIntegral res);
    putWord32be (fromIntegral v1);
    putWord32be (fromIntegral v2);
    putWord32be endop}

-- Clear multiplication.
mulc :: Public -> Public -> Public -> ByteString
mulc (Public res) (Public v1) (Public v2) = runPut $ do 
  putWord32be 0x30
  putWord32be (fromIntegral res)
  putWord32be (fromIntegral v1)
  putWord32be (fromIntegral v2)
  putWord32be endop
  