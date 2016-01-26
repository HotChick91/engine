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
  t <- o .: "type" :: Parser String
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
  col <- o .: "color"
  r <- col .: "r" :: Parser Scientific
  g <- col .: "g" :: Parser Scientific
  b <- col .: "b" :: Parser Scientific
  return (toRealFloat r,toRealFloat g,toRealFloat b)
parseColors _ = fail "Expected colors object"

parseChildren :: Value -> Parser [Int]
parseChildren (Object o) = do
  chi <- o .: "children" :: Parser Array
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
  fileContent <- L.readFile str
  fileContent `seq` P.putStrLn $ "File loaded, parsing..."
  P.putStrLn $ show $  L.unpack fileContent
  case decode fileContent of
    Nothing -> P.putStrLn "Incorrect file syntax (check for missing commas and brackets)"
    Just (Array ar) -> forM_ ar processNode
    Just _ -> P.putStrLn "Wrong file content (should be array)."
  P.putStrLn $ "File parsing complete."

