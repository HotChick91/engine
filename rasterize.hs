{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}

module Main where

import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import Data.Array.IO
import System.IO.Unsafe
import Data.IORef
import System.IO
import Control.Monad
import Data.Aeson.Types hiding ( parse )

import qualified Data.ByteString.Lazy.Char8 as B

import Debug.Trace

import Control.Monad
import Data.Maybe
import qualified Data.Array as Ar
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

-- {{{1

data Obj = Vertex Float Float Float | Face Int Int Int deriving ( Show )
data Model = Model (Ar.Array Int (Float, Float, Float)) [(Int, Int, Int)] deriving ( Show )

fillModel :: [Obj] -> Model
fillModel l = let (v,f) = fillModel' l ([],[]) in Model (Ar.listArray (1,length v) $ reverse v) (reverse f)
  where
  fillModel' [] a = a
  fillModel' (l:ls) (v, f) = fillModel' ls newModel
    where
    newModel = case l of
      Vertex x y z -> ( (x,y,z) : v, f)
      Face x y z -> (v, (x,y,z) : f)

-- Parsing OBJ files
objFile :: GenParser Char st [Obj]
objFile = do
  result <- many line
  eof
  return $ catMaybes result

line :: GenParser Char st ( Maybe Obj )
line = do
  result <- v_start  <|> face <|> comment <|> objName <|> none
  newline
  return result

v_start = do
  char 'v'
  verticle <|> normal

verticle :: GenParser Char st ( Maybe Obj )
verticle = do
  spaces
  xf <- sign
  x <- floating -- TODO change into floating <|> int
  spaces
  yf <- sign
  y <- floating
  spaces
  zf <- sign
  z <- floating
  return $ Just $ Vertex (xf x)(yf y)(zf z) 

normal :: GenParser Char st ( Maybe Obj )
normal = do
  char 'n'
  spaces
  xf <- sign
  x <- floating
  spaces
  yf <- sign
  y <- floating
  spaces
  zf <- sign
  z <- floating
  return $ Nothing

face :: GenParser Char st (Maybe Obj)
face = do
  char 'f'
  spaces
  x <- int
  spaces
  y <- int
  spaces
  z <- int
  return $ Just $ Face x y z

objName :: GenParser Char st (Maybe Obj)
objName = do
  char 'g'
  many $ noneOf ['\n']
  return Nothing

comment :: GenParser Char st (Maybe Obj)
comment = do
  char '#'
  many $ noneOf ['\n']
  return Nothing

none :: GenParser Char st (Maybe Obj)
none = return Nothing

-- Collisions
-- {{{1
type Triple = (Float, Float, Float)
type Triangle = (Triple, Triple, Triple)
data Cube = Cube {startPoint :: Triple, endPoint :: Triple } deriving ( Show )

normals :: Cube -> (Triple, Triple, Triple)
normals box = splitTriple $ mapTriple (\x -> x / (abs x)) $ minus (startPoint box) (endPoint box)
  where
  mapTriple :: (a -> b) -> (a,a,a) -> (b,b,b)
  mapTriple f (a,b,c) = (f a, f b, f c)
  splitTriple (a,b,c) = ( (a,0,0), (0,b,0), (0,0,c) )

radius :: Cube -> Float
radius box = let (r1, _, _) = startPoint box ; (r2, _, _) = endPoint box in abs (r2 - r1)

-- Separating axis test
isIntersecting :: Cube -> Triangle -> Bool
isIntersecting box triangle = triangleInside || not (boxNormalSeparates || triangleNormalSeparates || edgesProductSeparates)
  where
  boxNormalSeparates = let (n1, n2, n3) = normals box in
    or $ map
      (existsSeparationPlane trianglePoints cubePoints)
      [n1, n2, n3]
  triangleNormalSeparates =
      existsSeparationPlane cubePoints trianglePoints (triangleNormal triangle)
  edgesProductSeparates = or $ map
      (existsSeparationPlane trianglePoints cubePoints)
      [crossProd te ce | te <- triangleEdges, ce <- cubeEdges]
  cubeEdges = let (a,b,c) = normals box in [a,b,c]
  triangleEdges = let (t1, t2, t3) = triangle in [t2 `minus` t1, t3 `minus` t2, t1 `minus` t3]
  cubePoints = [(x,y,z) | x <- [x1, x2], y <- [y1, y2], z <- [z1, z2]]
  (x1, y1, z1) = startPoint box
  (x2, y2, z2) = endPoint box
  trianglePoints = let (t1, t2, t3) = triangle in [t1, t2, t3]
  triangleInside = or $ map (insideCube box) trianglePoints

pointDist (a,b,c) (x,y,z) = sqrt ((x-a)**2 + (y-b)**2 + (z-c)**2)

midPoint ( (a,b,c), (d,e,f), (g,h,i) ) = ( (a+d+g) / 3, (b+e+h) / 3, (c+f+i) / 3 )

insideCube :: Cube -> Triple -> Bool
insideCube box point = and $ forTriples (\a b c -> inRange (a,b) c) (startPoint box) (endPoint box) point
  where
  inRange (a,b) c = if a < b then c >= a && c <= b else c >= b && c <= a
  forTriples x (a,b,c) (d,e,f) (g,h,i) = [x a d g, x b e h, x c f i]

existsSeparationPlane :: [Triple] -> [Triple] -> Triple -> Bool
existsSeparationPlane testPoints constraintPoints normalVect =
  minT > maxC || maxT < minC
  where
    (minC, maxC) = project constraintPoints normalVect
    (minT, maxT) = project testPoints normalVect
    inRange (a,b) c = c >= a && c <= b

project :: [Triple] -> Triple -> (Float, Float)
project points axis = (minVal, maxVal)
  where
  minVal = minimum newP
  maxVal = maximum newP
  newP = map (dotProd axis) points

triangleNormal :: Triangle -> Triple
triangleNormal (p1, p2, p3) = normalize $ crossProd (minus p2 p1) (minus p3 p1)

normalize :: Triple -> Triple
normalize (x, y, z) = (x / len, y / len, z / len)
  where
  len = sqrt (x**2 + y**2 + z**2)

minus :: Triple -> Triple -> Triple
minus (a1, a2, a3) (b1, b2, b3) = (a1 - b1, a2 - b2, a3 - b3)

dotProd :: Triple -> Triple -> Float
dotProd (a1, a2, a3) (b1, b2, b3) = a1 * b1 + a2 * b2 + a3 * b3

crossProd :: Triple -> Triple -> Triple
crossProd (a1, a2, a3) (b1, b2, b3) = (c1, c2, c3)
  where
  c1 = a2 * b3 - a3 * b2
  c2 = a3 * b1 - a1 * b3
  c3 = a1 * b2 - a2 * b1

-- Building octree
-- {{{1
data Node = Empty | Solid (Float, Float, Float) | Partial (Node,Node,Node,Node,Node,Node,Node,Node) deriving ( Show )

initialRadius = 128.0

targetDepth = 9

tmpColor = (0.5, 0.5, 0.5)

buildOctree :: [Triangle] -> Node
buildOctree list = buildOctree' list (Cube sP eP) 0
  where
  sP = (-initialRadius, -initialRadius, -initialRadius)
  eP = (initialRadius, initialRadius, initialRadius)
  buildOctree' :: [Triangle] -> Cube -> Int -> Node
  buildOctree' [] _ _ = Empty -- or Solid, but thats TODO
  buildOctree' list box depth = {-trace debugMsg $-} if depth >= targetDepth then Solid tmpColor else
    Partial $ fromListToEight newNodes
    where
    debugMsg = show (list) ++ " " ++ (show box) ++ " " ++ (show depth)
    fromListToEight [n0, n1, n2, n3, n4, n5, n6, n7] = (n0, n1, n2, n3, n4, n5, n6, n7)
    directions = [(-1, -1, -1) , (-1, -1,  1),
                  (-1,  1, -1) , (-1,  1,  1),
                  ( 1, -1, -1) , ( 1, -1,  1),
                  ( 1,  1, -1) , ( 1,  1,  1)]
    newNodes = map createSubNode directions
    center = let (a,b,c) = startPoint box ; (d,e,f) = endPoint box in ( (a+d) / 2, (b+e) / 2, (c+f) / 2)
    r = radius box
    createSubNode (dX, dY, dZ) = id --myList `seq` trace ("My list is: " ++ (show myList) ++ "\n") 
      $ buildOctree' myList myCube (depth + 1)
      where
      myList = id -- trace ("Checking " ++ (show myCube) ++ "===========\n") 
        $ filter (isIntersecting myCube) list
      myCube = Cube {startPoint = center, endPoint = (\(x,y,z) -> (x + r * dX / 2, y + r * dY / 2, z + r * dZ / 2))(center)}

-- {{{1
-- Writing JSON

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
jEmpty x y z = A.encode $ object ps
    where
    ps :: Pairs
    ps = [("x", Number $ fromIntegral x), ("y", Number $ fromIntegral y), ("z", Number $ fromIntegral z), ("type", String "Empty")]
    
-- build json bytestring for solid node
jSolid :: Float -> Float -> Float -> Int -> Int -> Int -> ByteString
jSolid r g b x y z = A.encode $ object ps
    where
    ps :: Pairs
    ps = [("r", Number $ realToFrac r), ("g", Number $ realToFrac g), ("b", Number $ realToFrac b), ("x", Number $ fromIntegral x), ("y", Number $ fromIntegral y), ("z", Number $ fromIntegral z), ("type", String "Solid")]
    
-- build json bytestring for partial node
jPartial :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ByteString
jPartial i0 i1 i2 i3 i4 i5 i6 i7 x y z = A.encode $ object ps
    where
    ps :: Pairs
    ps = [("x", Number $ fromIntegral x), ("y", Number $ fromIntegral y), ("z", Number $ fromIntegral z), ("type", String "Partial"), ("nodes", nodes)]
    nodes :: Value
    nodes = toJSON $ (map fromIntegral [i0, i1, i2, i3, i4, i5, i6, i7] :: [Int])

-- convert octree nodes to json bytestrings, store them in `octarrReg`, and return # of nodes (= root id)
writeOctree :: Node -> Int -> Int -> Int -> IO Int
writeOctree Empty x y z = oaAppend $ jEmpty x y z
writeOctree (Solid (r,g,b)) x y z = oaAppend $ jSolid r g b x y z
writeOctree (Partial (n0,n1,n2,n3,n4,n5,n6,n7)) x y z = do
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

-- }}}1

main :: IO ()
main = do
  input <- readFile "model.obj"
  obj <- case (parse objFile "" input) of
    Left err -> error $ show err
    Right o -> return o
  let model = fillModel obj
  let Model vertices faces = model
  let triangles = map (\(a,b,c) -> (vertices Ar.! a, vertices Ar.! b, vertices Ar.! c) ) faces
  {-print triangles-}
  {-print "-------------"-}
  -- use triangles for "buildOctree"
  let tree = buildOctree triangles
  {-print tree-}
  {-print "-------------"-}

  -- write JSON
  n <- writeOctree tree {-(Node (Solid 1 0 0))-} 0 0 0
  writeArray octarrRef 0 =<< readArray octarrRef n
  dumpOa n stdout

-- vim: set foldmethod=marker:
