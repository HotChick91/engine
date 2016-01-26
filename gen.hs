{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Array.IO
import System.IO.Unsafe
import Data.IORef
import System.IO
import Control.Monad
import Data.Aeson.Types

import qualified Data.ByteString.Lazy.Char8 as B

data Node
    = Empty
    | Solid !Float !Float !Float
    deriving (Eq, Show)

type Pixmap = IOArray (Int, Int) Node
type Voxmap = IOArray (Int, Int, Int) Node
data Octree = Node Node | Partial !Octree !Octree !Octree !Octree !Octree !Octree !Octree !Octree
type Octarr = IOArray Int ByteString
type Pairs = [Pair]

-- numer of nodes in `octarrRef`
{-# NOINLINE iRef #-}
iRef :: IORef Int
iRef = unsafePerformIO $ newIORef 0

-- max number of nodes in output json array 
kMax :: Int
kMax = 1024 * 1024

-- array of json bytestrings, last intermediate structure before dumping
{-# NOINLINE octarrRef #-}
octarrRef :: Octarr
octarrRef = unsafePerformIO $ newArray_ (0, kMax-1)

-- append json bytestring to `octarrRef`
oaAppend :: ByteString -> IO Int
oaAppend js = do
    i <- readIORef iRef
    -- 0 is reserved for root
    let i' = i + 1
    when (i' >= kMax) $ error "indices exhausted"
    writeIORef iRef $ i'
    writeArray octarrRef i' js
    return i'
    
-- build json bytestring for empty node
jEmpty :: Int -> Int -> Int -> ByteString
jEmpty x y z = encode $ object ps
    where
    ps :: Pairs
    ps = [("x", Number $ fromIntegral x), ("y", Number $ fromIntegral y), ("z", Number $ fromIntegral z), ("type", String "Empty")]
    
-- build json bytestring for solid node
jSolid :: Float -> Float -> Float -> Int -> Int -> Int -> ByteString
jSolid r g b x y z = encode $ object ps
    where
    ps :: Pairs
    ps = [("r", Number $ realToFrac r), ("g", Number $ realToFrac g), ("b", Number $ realToFrac b), ("x", Number $ fromIntegral x), ("y", Number $ fromIntegral y), ("z", Number $ fromIntegral z), ("type", String "Solid")]
    
-- build json bytestring for partial node
jPartial :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ByteString
jPartial i0 i1 i2 i3 i4 i5 i6 i7 x y z = encode $ object ps
    where
    ps :: Pairs
    ps = [("x", Number $ fromIntegral x), ("y", Number $ fromIntegral y), ("z", Number $ fromIntegral z), ("type", String "Partial"), ("nodes", nodes)]
    nodes :: Value
    nodes = toJSON $ (map fromIntegral [i0, i1, i2, i3, i4, i5, i6, i7] :: [Int])

-- convert octree nodes to json bytestrings, store them in `octarrReg`, and return # of nodes (= root id)
writeOctree :: Octree -> Int -> Int -> Int -> IO Int
writeOctree (Node Empty) x y z = oaAppend $ jEmpty x y z
writeOctree (Node (Solid r g b)) x y z = oaAppend $ jSolid r g b x y z
writeOctree (Partial n0 n1 n2 n3 n4 n5 n6 n7) x y z = do
    i0 <- writeOctree n0 0 0 0
    i1 <- writeOctree n1 0 0 1
    i2 <- writeOctree n2 0 1 0
    i3 <- writeOctree n3 0 1 1
    i4 <- writeOctree n4 1 0 0
    i5 <- writeOctree n5 1 0 1
    i6 <- writeOctree n6 1 1 0
    i7 <- writeOctree n7 1 1 1
    oaAppend $ jPartial i0 i1 i2 i3 i4 i5 i6 i7 x y z

-- write `n` json bytestrings from `octarrReg` to `handle` as a json array
dumpOa :: Int -> Handle -> IO ()
dumpOa n handle = do
    dump "["
    forM_ [0..(n-1)] $ \i -> do
        dump =<< readArray octarrRef i
        when (i /= n-1) $ dump ",\n"
    dump "]\n"
    where
    dump :: ByteString -> IO ()
    dump = B.hPut handle

width :: Int
width = 128

dims :: ((Int,Int), (Int,Int))
dims = ((0,0), (width-1,width-1))

wall :: IO Pixmap
wall = newArray dims (Solid 0.6 0.6 0.9)

table :: IO Pixmap
table = do
    pm <- space
    -- assembleTable
    forM_ [(width `div` 4)..(width `div` 2)] $ \i -> writeArray pm (i, width `div` 4) (Solid 0.6 0.3 0.0)
    forM_ [0..(width `div` 4)] $ \i -> writeArray pm (3 * width `div` 8, i) (Solid 0.6 0.3 0.0)
    return pm

forWidth :: (Int -> IO ()) -> IO ()
forWidth = forM_ [0..(width-1)]

space :: IO Pixmap
space = do
    pm <- newArray dims Empty
    -- paint ceiling
    forWidth $ \i -> writeArray pm (i, width-1) (Solid 0.9 0.9 0.9)
    -- lay flooring
    forWidth $ \i -> writeArray pm (i, 0) (Solid 0.7 0.6 0.4)
    -- paint walls
    forWidth $ \i -> writeArray pm (0, i) (Solid 0.9 0.9 0.9)
    forWidth $ \i -> writeArray pm (width-1, i) (Solid 0.9 0.9 0.9)
    return pm

makeVoxmap :: [Pixmap] -> IO Voxmap
makeVoxmap pms = do
    vm <- newArray_ ((0,0,0),(width-1,width-1,width-1))
    let indexed = zip [0..] pms
    forM_ indexed $ \(x,pm) -> forM_ (range dims) $ \(y,z) -> writeArray vm (x,y,z) =<< readArray pm (y, z)
    return vm

sameNode :: Octree -> Octree -> Bool
sameNode (Node n0) (Node n1) = n0 == n1
sameNode _ _ = False

makeOctree :: Voxmap -> IO Octree
makeOctree vm = makeOctree' width (0, 0, 0)
    where
    makeOctree' :: Int -> (Int, Int, Int) -> IO Octree
    makeOctree' 1 p = Node <$> readArray vm p
    makeOctree' n (x,y,z) = do
        ots <- mapM (makeOctree' n_2) ps
        return $ if all (sameNode $ ots !! 0) ots
            then ots !! 0
            else Partial (ots !! 0) (ots !! 1) (ots !! 2) (ots !! 3) (ots !! 4) (ots !! 5) (ots !! 6) (ots !! 7)
        where
        ps :: [(Int, Int, Int)]
        ps = 
            [ (x,     y, z), (x,     y, z+n_2), (x,     y+n_2, z), (x,     y+n_2, z+n_2)
            , (x+n_2, y, z), (x+n_2, y, z+n_2), (x+n_2, y+n_2, z), (x+n_2, y+n_2, z+n_2)
            ]
        n_2 :: Int
        n_2 = n `div` 2

main :: IO ()
main = do
    w <- wall
    t <- table
    s <- space
    let ss0 = replicate (width `div` 2 - 1) s
    let ts = replicate (width `div` 4) t
    let ss1 = replicate (width `div` 4 - 1) s
    let pms = w : ss0 ++ ts ++ ss1 ++ [w]
    vm <- makeVoxmap pms
    ot <- makeOctree vm
    n <- writeOctree ot {-(Node (Solid 1 0 0))-} 0 0 0
    writeArray octarrRef 0 =<< readArray octarrRef n
    dumpOa n stdout
