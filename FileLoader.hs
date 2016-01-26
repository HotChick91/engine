{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module FileLoader where

import Prelude as P hiding (readFile)

import Control.Monad (unless)

import Data.List
import Data.Foldable
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy as L hiding (putStrLn)

import Foreign.C.Types
import Foreign.C.String

foreign import ccall "push_oct_tree_partial" push_partial_c :: CInt ->CInt ->CInt ->CInt ->CInt ->CInt ->CInt ->CInt -> IO ()
foreign import ccall "push_oct_tree_solid" push_solid_c :: CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "push_oct_tree_empty" push_empty_c :: IO ()
foreign export ccall load_file :: CString -> IO ()

data Node = Empty | Solid (Float, Float, Float) | Partial Int Int Int Int Int Int Int Int

{-push_partial_c :: CInt ->CInt ->CInt ->CInt ->CInt ->CInt ->CInt ->CInt -> IO ()-}
{-push_partial_c a b c d e f g h = putStrLn "pushed partial"-}

{-push_empty_c :: IO ()-}
{-push_empty_c = putStrLn "pushed empty"-}

{-push_solid_c :: CFloat -> CFloat -> CFloat -> IO ()-}
{-push_solid_c r g b = putStrLn "pushed solid"-}

processNode :: Value -> IO ()
processNode val = do
  node <- maybe (fail "Error processing node") return $ parseMaybe parseNode val
  case node of 
    Empty -> push_empty_c
    Solid (r,g,b) -> push_solid_c (CFloat r) (CFloat g) (CFloat b)
    Partial c0 c1 c2 c3 c4 c5 c6 c7 -> push_partial_c (ci c0) (ci c1) (ci c2) (ci c3) (ci c4) (ci c5) (ci c6) (ci c7)
  where
    ci = CInt . fromIntegral

parseNode :: Value -> Parser Node
parseNode = withObject "NodeObject" $ \o -> do
  t <- o .: "type" :: Parser String
  case t of
    "Empty" -> return Empty
    "Solid" -> do
      col <- o .: "color"
      r <- col .: "r" :: Parser Float
      g <- col .: "g" :: Parser Float
      b <- col .: "b" :: Parser Float
      return $ Solid (r, g, b)
    "Partial" -> do
      chi <- o .: "children" :: Parser [Int]
      return $ Partial (chi!!0) (chi!!1) (chi!!2) (chi!!3) (chi!!4) (chi!!5) (chi!!6) (chi!!7)
    _ -> fail ("Wrong node type! (unknown type: " ++ t ++ ")")


load_file :: CString -> IO ()
load_file a = do
  str <- peekCString a
  putStrLn $ "Loading file: " ++ show str
  unless (Data.List.isSuffixOf ".json" str) $ fail "Wrong filetype."
  fileContent <- readFile str
  fileContent `seq` putStrLn $ "File loaded, parsing..."
  putStrLn $ show $ unpack fileContent
  case decode fileContent of
    Nothing -> putStrLn "Incorrect file syntax (check for missing commas and brackets)"
    Just (Array ar) -> forM_ ar processNode
    Just _ -> putStrLn "Wrong file content (should be array)."
  putStrLn $ "File parsing complete."

