
module MPC where

import Control.Exception (throw)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Binary.Put (putWord32be, runPut)
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Internal
import VMops

data Expression
  = AddSS Secret Secret --redo Add, Sub, Mul with a better structure in the future
  | AddSP Secret Public
  | AddPS Public Secret
  | AddPP Public Public
  | SubSS Secret Secret
  | SubSP Secret Public
  | SubPS Public Secret
  | SubPP Public Public
  | MulPP Public Public
  | MulSS Secret Secret
  | MPCInput Int
  | Reveal Secret
  | Constant Int
  deriving (Show)

data Line = SAssignment Secret Expression | PAssignment Public Expression | Output Public deriving (Show)

type Circuit = ([Line], Int)

class Monad m => MPCMonad m where 
  mpcPlusPP :: Public -> Public -> m Public
  mpcPlusPS :: Public -> Secret -> m Secret
  mpcPlusSP :: Secret -> Public -> m Secret
  mpcPlusSS :: Secret -> Secret -> m Secret
  mpcMinusPP :: Public -> Public -> m Public
  mpcMinusPS :: Public -> Secret -> m Secret
  mpcMinusSP :: Secret -> Public -> m Secret
  mpcMinusSS :: Secret -> Secret -> m Secret
  mpcMultiplicationPP :: Public -> Public -> m Public
  mpcMultiplicationSS :: Secret -> Secret -> m Secret
  mpcCreateSecretConstant :: Int -> m Secret
  mpcCreatePublicConstant :: Int -> m Public
  mpcInput :: Int -> m Secret
  mpcOutput :: Public -> m Public
  mpcReveal :: Secret -> m Public

instance MPCMonad (State Circuit) where
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



  mpcMinusPP :: Public -> Public -> State Circuit Public
  mpcMinusPP p1 p2 = do
    id <- getNewId
    (a, b) <- get
    put (a ++ [PAssignment (Public id) (SubPP p1 p2)], b)
    return (Public b)

  mpcMinusPS :: Public -> Secret -> State Circuit Secret
  mpcMinusPS p1 p2 = do
    id <- getNewId
    (a, b) <- get
    put (a ++ [SAssignment (Secret id) (SubPS p1 p2)], b)
    return (Secret b)

  mpcMinusSP :: Secret -> Public -> State Circuit Secret
  mpcMinusSP p1 p2 = do
    id <- getNewId
    (a, b) <- get
    put (a ++ [SAssignment (Secret id) (SubSP p1 p2)], b)
    return (Secret b)

  mpcMinusSS :: Secret -> Secret -> State Circuit Secret
  mpcMinusSS p1 p2 = do
    id <- getNewId
    (a, b) <- get
    put (a ++ [SAssignment (Secret id) (SubSS p1 p2)], b)
    return (Secret b)


  mpcMultiplicationPP :: Public -> Public -> State Circuit Public
  mpcMultiplicationPP p1 p2 = do
    id <- getNewId
    (a, b) <- get
    put (a ++ [PAssignment (Public id) (MulPP p1 p2)], b)
    return (Public b)

  mpcMultiplicationSS :: Secret -> Secret -> State Circuit Secret
  mpcMultiplicationSS p1 p2 = do
    id <- getNewId
    (a, b) <- get
    put (a ++ [SAssignment (Secret id) (MulSS p1 p2)], b)
    return (Secret b)


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
  mpcOutput p = do
    (a, b) <- get
    put (a ++ [Output p], b)
    return (Public b)



  mpcReveal :: Secret -> State Circuit Public
  mpcReveal p = do
    id <- getNewId
    (a, b) <- get
    put (a ++ [PAssignment (Public id) (Reveal p)], b)
    return (Public id)





getNewId :: State Circuit Int
getNewId = do
  (a, b) <- get
  put (a, b + 1)
  return (b + 1)

makeByteCode :: (forall m.MPCMonad m => m ()) -> ByteString 
makeByteCode s = addByteCodePadding $ execState s ([], 0) 

addByteCodePadding :: Circuit -> ByteString
addByteCodePadding circut = BL.append (BL.append startfile (tobytestring circut)) endfile


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



toStr :: Circuit -> String
toStr (c, i) = unwords $ Prelude.map line2string c

line2string :: Line -> String
line2string (SAssignment a (AddPS b c)) = show a ++ " = " ++ show b ++ " + " ++ show c ++ "\n"
line2string (SAssignment a (AddSP b c)) = show a ++ " = " ++ show b ++ " + " ++ show c ++ "\n"
line2string (SAssignment a (AddSS b c)) = show a ++ " = " ++ show b ++ " + " ++ show c ++ "\n"
line2string (PAssignment a (AddPP b c)) = show a ++ " = " ++ show b ++ " + " ++ show c ++ "\n"
line2string (SAssignment a (SubPS b c)) = show a ++ " = " ++ show b ++ " - " ++ show c ++ "\n"
line2string (SAssignment a (SubSP b c)) = show a ++ " = " ++ show b ++ " - " ++ show c ++ "\n"
line2string (SAssignment a (SubSS b c)) = show a ++ " = " ++ show b ++ " - " ++ show c ++ "\n"
line2string (PAssignment a (SubPP b c)) = show a ++ " = " ++ show b ++ " - " ++ show c ++ "\n"
line2string (PAssignment a (MulPP b c)) = show a ++ " = " ++ show b ++ " * " ++ show c ++ "\n"
line2string (SAssignment a (MulSS b c)) = show a ++ " = " ++ show b ++ " * " ++ show c ++ "\n"
line2string (SAssignment a (MPCInput p)) = show a ++ " = input(" ++ show p ++ ")\n"
line2string (Output a) = "Output is in " ++ show a ++ "\n"
line2string (PAssignment a (Reveal s)) = show a ++ " = " ++ show s ++ "\n"
line2string (PAssignment a b) = show a ++ " = " ++ show b ++ "\n"
line2string (SAssignment a b) = show a ++ " = " ++ show b ++ "\n"

tobytestring :: Circuit -> ByteString
tobytestring (c, i) = BL.concat $ Prelude.map line2bytestring c

line2bytestring :: Line -> ByteString
line2bytestring (PAssignment p (AddPP p1 p2)) = addc p p1 p2
line2bytestring (PAssignment p (SubPP p1 p2)) = subc p p1 p2
line2bytestring (PAssignment p (MulPP p1 p2)) = mulc p p1 p2
line2bytestring (PAssignment p (Reveal s)) = open p s
line2bytestring (PAssignment p (Constant b)) = ldi p b

line2bytestring (SAssignment s (AddPS p s1)) = addmr s p s1
line2bytestring (SAssignment s (AddSP s1 p)) = addml s s1 p
line2bytestring (SAssignment s (AddSS s1 s2)) = adds s s1 s2
line2bytestring (SAssignment s (SubPS p s1)) = submr s p s1
line2bytestring (SAssignment s (SubSP s1 p)) = subml s s1 p
line2bytestring (SAssignment s (SubSS s1 s2)) = subs s s1 s2
line2bytestring (SAssignment s (MulSS s1 s2)) = muls s s1 s2
line2bytestring (SAssignment s (MPCInput i)) = inputmixed s i
line2bytestring (SAssignment s (Constant i)) = ldsi s i

line2bytestring (Output (Public a)) = printregplain (fromIntegral a)

tmp::forall m.MPCMonad m => m ()
tmp = do
  a <- mpcCreateSecretConstant 1
  b <- mpcCreateSecretConstant 2
  c <- mpcReveal a
  d <- mpcReveal b
  e <- mpcMultiplicationPP c d
  return ()

aa::forall m.MPCMonad m => m ()
aa = do
  a <- mpcCreateSecretConstant 2
  b <- mpcCreateSecretConstant 7
  c <- mpcCreateSecretConstant 10
  d <- mpcPlusSS a b
  e <- mpcMinusSS d c
  f <- mpcReveal e
  mpcOutput f
  return () 
  

-- main = putStrLn $ toStr $ execState tmp ([], 0)
-- main = BL.writeFile "example.txt" $ tobytestring $ execState tmp ([], 0)

--main = BL.writeFile "example.bc" $ compileMPC aa ([], 0)

main = BL.writeFile "example.bc" $ makeByteCode tmp

