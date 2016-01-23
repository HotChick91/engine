{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module FileLoader where

import Prelude as P

import Data.List
import Data.Foldable
import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import Data.ByteString.Lazy as L
import qualified Data.Vector as V

import Data.HashMap.Strict as HM

import Foreign()
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array()

{-foreign import ccall "add_elem" add_elem_c :: Int -> Int -> IO ()-}
{-foreign import ccall "set_size" set_size_c :: Int -> IO ()-}
foreign import ccall "push_oct_tree_partial" push_partial_c :: CInt ->CInt ->CInt ->CInt ->CInt ->CInt ->CInt ->CInt -> IO ()
foreign import ccall "push_oct_tree_solid" push_solid_c :: CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "push_oct_tree_empty" push_empty_c :: IO ()
foreign export ccall load_file :: CString -> IO ()

data Node = Empty | Solid (Float, Float, Float) | Partial Int Int Int Int Int Int Int Int

{-push_partial_c :: Int ->Int ->Int ->Int ->Int ->Int ->Int ->Int -> IO ()-}
{-push_partial_c a b c d e f g h = P.putStrLn "pushed partial"-}

{-push_empty_c :: IO ()-}
{-push_empty_c = P.putStrLn "pushed empty"-}

{-push_solid_c :: Float -> Float -> Float -> IO ()-}
{-push_solid_c r g b = P.putStrLn "pushed solid"-}

{-add_elem_c :: Int -> Int -> IO ()-}
{-add_elem_c a b = P.putStrLn $ show a ++ " " ++ show b-}

processNode :: Value -> IO ()
processNode o = do
  node <- case parseMaybe parseNode o of
            Just x -> return x
            Nothing -> fail "Could not find node"
  case node of 
    Empty -> push_empty_c
    Solid (r,g,b) -> push_solid_c (CFloat r) (CFloat g) (CFloat b)
    Partial c0 c1 c2 c3 c4 c5 c6 c7 -> push_partial_c (ci c0) (ci c1) (ci c2) (ci c3) (ci c4) (ci c5) (ci c6) (ci c7)
  where ci = CInt . fromIntegral

parseNode :: Value -> Parser Node
parseNode (Object o) = do
  t <- case HM.lookup "type" o of
      Just (String t) -> return t
      Just _ -> fail "Wrong type of type"
      Nothing -> fail "No type field"
  case t of
    "Empty" -> return Empty
    "Solid" -> 
      case parseMaybe parseColors (Object o) of
        Just (r, g, b) -> return $  Solid (r, g, b)
        Nothing -> fail "Wrong colors"
    "Partial" ->
      case parseMaybe parseChildren (Object o) of
        Just chi -> return $ Partial (chi !! 0) (chi!!1) (chi!!2) (chi!!3) (chi!!4) (chi!!5) (chi!!6) (chi!!7)
        Nothing -> fail "Not enough children"
    _ -> fail "Wrong node type"
parseNode _ = fail "Expected Node object"

parseColors :: Value -> Parser (Float, Float, Float)
parseColors (Object o) = do
  col <- case HM.lookup "color" o of
    Just (Object x) -> return x
    Just _ -> fail "Wrong type of color field"
    Nothing -> fail "No color field found"
  r <- case HM.lookup "r" col of
    Just (Number n) -> return $ n
    Just _ -> fail "Wrong type of color r field"
    Nothing -> fail "No color r field found"
  g <- case HM.lookup "g" col of
    Just (Number n) -> return $ n
    Just _ -> fail "Wrong type of color g field"
    Nothing -> fail "No color g field found"
  b <- case HM.lookup "b" col of
    Just (Number n) -> return $ n
    Just _ -> fail "Wrong type of color b field"
    Nothing -> fail "No color b field found"
  return (toRealFloat r,toRealFloat g,toRealFloat b)
parseColors _ = fail "Expected colors object"

parseChildren :: Value -> Parser [Int]
parseChildren (Object o) = do
  chi <- case HM.lookup "children" o of
    Just (Array a) -> return a
    Just _ -> fail "Wrong type of color field"
    Nothing -> fail "No color field found"
  mapM remNum $ V.toList chi
  where
  remNum :: Value -> Parser Int
  remNum (Number a) = case toBoundedInteger a of Just x ->return x; Nothing -> fail "Wrong number"
  remNum _ = fail "Expected a number"
parseChildren _ = fail "Expected children object"

load_file :: CString -> IO ()
load_file a = do
  str <- peekCString a
  P.putStrLn $ "Loading file: " ++ show str
  if not $ Data.List.isSuffixOf ".json" str then fail "Wrong filetype." else return ()
  P.putStrLn $ "Going good 0"
  {-fileContent <- L.readFile str-}
  P.putStrLn $ "Going good 1"
  {-P.putStrLn $ show $  unpack fileContent-}
  {-case decode fileContent of-}
    {-Nothing -> P.putStrLn "Nothing"-}
    {-Just (Object o) -> P.putStrLn $ show o-}
    {-Just (Array ar) -> (P.putStrLn $ "Arr: " ++ show ar) >> forM_ ar processNode-}
    {-Just _ -> P.putStrLn "something else"-}


  -- add_elem id type


