{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module FileLoader where

import Prelude as P

import Control.Monad (unless)

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

foreign import ccall "push_oct_tree_partial" push_partial_c :: CInt ->CInt ->CInt ->CInt ->CInt ->CInt ->CInt ->CInt -> IO ()
foreign import ccall "push_oct_tree_solid" push_solid_c :: CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "push_oct_tree_empty" push_empty_c :: IO ()
foreign export ccall load_file :: CString -> IO ()

data Node = Empty | Solid (Float, Float, Float) | Partial Int Int Int Int Int Int Int Int

{-push_partial_c :: CInt ->CInt ->CInt ->CInt ->CInt ->CInt ->CInt ->CInt -> IO ()-}
{-push_partial_c a b c d e f g h = P.putStrLn "pushed partial"-}

{-push_empty_c :: IO ()-}
{-push_empty_c = P.putStrLn "pushed empty"-}

{-push_solid_c :: CFloat -> CFloat -> CFloat -> IO ()-}
{-push_solid_c r g b = P.putStrLn "pushed solid"-}

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
  unless (Data.List.isSuffixOf ".json" str) $ fail "Wrong filetype."
  P.putStrLn $ "Going good 0"
  fileContent <- L.readFile str
  {-let fileContent = L8.pack ("[" ++-}
                      {-"  {\"id\" : 0, \"type\": \"Partial\", \"children\": [1,2,3,4,5,6,7,8]}" ++-}
                      {-", {\"id\" : 1, \"type\": \"Solid\", \"color\" : {\"r\": 0.0, \"g\": 0.0, \"b\": 1.0}}" ++-}
                      {-", {\"id\" : 2, \"type\": \"Solid\", \"color\" : {\"r\": 1.0, \"g\": 0.0, \"b\": 0.0}}" ++-}
                      {-", {\"id\" : 3, \"type\": \"Solid\", \"color\" : {\"r\": 1.0, \"g\": 0.0, \"b\": 1.0}}" ++-}
                      {-", {\"id\" : 4, \"type\": \"Empty\"}" ++-}
                      {-", {\"id\" : 5, \"type\": \"Solid\", \"color\" : {\"r\": 0.0, \"g\": 1.0, \"b\": 1.0}}" ++-}
                      {-", {\"id\" : 6, \"type\": \"Empty\"}" ++-}
                      {-", {\"id\" : 7, \"type\": \"Empty\"}" ++-}
                      {-", {\"id\" : 8, \"type\": \"Empty\"}" ++-}
                      {-"]")-}
  fileContent `seq` P.putStrLn $ "Going good 1"
  P.putStrLn $ show $  L.unpack fileContent
  P.putStrLn $ "Going good 2"
  case decode fileContent of
    Nothing -> P.putStrLn "Nothing"
    Just (Array ar) -> forM_ ar processNode
    Just _ -> P.putStrLn "something else"
  P.putStrLn $ "Going good 3"

