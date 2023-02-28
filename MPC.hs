module CppHaskell where

import Control.Exception (throw)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Binary.Put (putWord32be, runPut)
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Internal
import VMops

newtype Public = Public Int deriving (Show)

newtype Secret = Secret Int deriving (Show)

data Expression
  = AddSS Secret Secret
  | AddSP Secret Public
  | AddPS Public Secret
  | AddPP Public Public
  | MPCInput Int
  | Reveal Secret
  | Constant Int
  deriving (Show)

data Line = SAssignment Secret Expression | PAssignment Public Expression | Output Public deriving (Show) -- byta ut Secret och Public till Expression?

type Circuit = ([Line], Int)

mpcPlusPP :: Public -> Public -> State Circuit Public
mpcPlusPP p1 p2 = do
  id <- getNewId
  (a, b) <- get
  put (a ++ [PAssignment (Public id) (AddPP p1 p2)], b)
  return (Public b)

mpcPlusPS :: Public -> Secret -> State Circuit Secret
mpcPlusPS p1 p2 = do
  id <- getNewId
  (a, b) <- get
  put (a ++ [SAssignment (Secret id) (AddPS p1 p2)], b)
  return (Secret b)

mpcPlusSP :: Secret -> Public -> State Circuit Secret
mpcPlusSP p1 p2 = do
  id <- getNewId
  (a, b) <- get
  put (a ++ [SAssignment (Secret id) (AddSP p1 p2)], b)
  return (Secret b)

mpcPlusSS :: Secret -> Secret -> State Circuit Secret
mpcPlusSS p1 p2 = do
  id <- getNewId
  (a, b) <- get
  put (a ++ [SAssignment (Secret id) (AddSS p1 p2)], b)
  return (Secret b)

--------- assign var to var currently not used and not a priority
-- mpcAssign :: VarId -> State Circuit VarId
-- -- mpcAssign (Public _) = error "cannot assign private value to public variable"
-- mpcAssign (Public v1) = do
--   id <- getNewId
--   (a, b) <- get
--   put (a ++ [Assignment (Public id) (Var (Public v1))], b)
--   return (Public v1)
-- mpcAssign (Private v1) = do
--   id <- getNewId
--   (a, b) <- get
--   put (a ++ [Assignment (Private id) (Var (Private v1))], b)
--   return (Private v1)

mpcCreateSecretConstant :: Int -> State Circuit Secret
mpcCreateSecretConstant v = do
  id <- getNewId
  (a, b) <- get
  put (a ++ [SAssignment (Secret id) (Constant v)], b)
  return (Secret id)

mpcCreatePublicConstant :: Int -> State Circuit Public
mpcCreatePublicConstant v = do
  id <- getNewId
  (a, b) <- get
  put (a ++ [PAssignment (Public id) (Constant v)], b)
  return (Public id)

mpcInput :: Int -> State Circuit Secret
mpcInput p = do
  id <- getNewId
  (a, b) <- get
  put (a ++ [SAssignment (Secret id) (MPCInput p)], b)
  return (Secret b)

mpcOutput :: Public -> State Circuit Public
mpcOutput (Public v) = do
  (a, b) <- get
  put (a ++ [Output (Public v)], b)
  return (Public b)

getNewId :: State Circuit Int
getNewId = do
  (a, b) <- get
  put (a, b + 1)
  return (b + 1)

mpcReveal :: Secret -> State Circuit Public
mpcReveal p = do
  id <- getNewId
  (a, b) <- get
  put (a ++ [PAssignment (Public id) (Reveal p)], b)
  return (Public id)

-- toStr :: Circuit -> String
-- toStr (c, i) = unwords $ Prelude.map line2string c

-- line2string :: Line -> String
-- line2string (Assignment a (Add b c)) = show a ++ " = " ++ show b ++ " + " ++ show c ++ "\n"
-- line2string (Assignment a (MPCInput p)) = show a ++ " = input\n"
-- line2string (Output a) = "The output is " ++ show a ++ "\n"
-- line2string (Assignment a (Var p)) = show a ++ " = " ++ show p ++ "\n"
-- line2string (Assignment a (Reveal p)) = show a ++ " = " ++ show p ++ "\n"

tobytestring :: Circuit -> ByteString
tobytestring (c, i) = BL.concat $ Prelude.map line2bytestring c

line2bytestring :: Line -> ByteString
line2bytestring (SAssignment (Secret a) (AddPS (Public b) (Secret c))) = addm (fromIntegral a) (fromIntegral b) (fromIntegral c)
line2bytestring (SAssignment (Secret a) (AddSP (Secret b) (Public c))) = addm (fromIntegral a) (fromIntegral b) (fromIntegral c)
line2bytestring (SAssignment (Secret a) (AddSS (Secret b) (Secret c))) = adds (fromIntegral a) (fromIntegral b) (fromIntegral c)
line2bytestring (PAssignment (Public a) (AddPP (Public b) (Public c))) = addc (fromIntegral a) (fromIntegral b) (fromIntegral c)
line2bytestring (SAssignment (Secret a) (MPCInput p)) = inputmixed (fromIntegral a) (fromIntegral p)
line2bytestring (Output (Public a)) = printregplain (fromIntegral a)
-- line2bytestring (Assignment a (Var p)) = runPut $ putWord32be 77
line2bytestring (PAssignment (Public a) (Reveal (Secret s))) = open (fromIntegral a) (fromIntegral s)
line2bytestring (PAssignment (Public a) (Constant b)) = ldi (fromIntegral a) (fromIntegral b)
line2bytestring (SAssignment (Secret a) (Constant b)) = ldsi (fromIntegral a) (fromIntegral b)

tmp :: State Circuit Circuit
tmp = do
  a <- mpcInput 1
  b <- mpcInput 2
  c <- mpcReveal b
  e <- mpcCreateSecretConstant 4
  get

aa :: State Circuit Circuit
aa = do
  a <- mpcCreateSecretConstant 2
  b <- mpcCreateSecretConstant 7
  c <- mpcCreateSecretConstant 10
  d <- mpcPlusSS a b
  e <- mpcPlusSS d c
  f <- mpcReveal e
  mpcOutput f
  get

-- main = putStrLn $ toStr $ execState tmp ([], 0)
-- main = BL.writeFile "example.txt" $ tobytestring $ execState tmp ([], 0)

main = BL.writeFile "example.bc" $ compileMPC aa ([], 0)

compileMPC :: State Circuit Circuit -> Circuit -> ByteString
compileMPC circut base = BL.append (BL.append startfile (tobytestring $ execState circut base)) endfile