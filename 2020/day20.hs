
{-# LANGUAGE BangPatterns #-}

import Data.Maybe
import Data.Array
import Data.List
import Data.List.Split
import Control.Monad

--------------------------------------------------------------------------------

type Arr = Array (Int,Int) Char

data Tile = Tile !Int !Arr deriving (Eq,Ord,Show)

data Dir = N | S | E | W deriving (Eq,Ord,Show)

tileNo :: Tile -> Int
tileNo (Tile no _) = no

mbTileNo :: Maybe Tile -> Int
mbTileNo mb = case mb of { Just t -> tileNo t ; Nothing -> 0 }

--------------------------------------------------------------------------------

seaMonsterTemplate :: [[Char]]
seaMonsterTemplate = 
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "
  ]

seaMonsterCoordsRef :: [(Int,Int)]
seaMonsterCoordsRef = [ (i,j) | i<-[0..n-1] , j<-[0..m-1] , (seaMonsterTemplate !! i ) !! j == '#' ]  where
  n = length       seaMonsterTemplate      -- 3
  m = length (head seaMonsterTemplate)     -- 20

seaMonsterN = 3  :: Int
seaMonsterM = 20 :: Int

seaMonsterCoords :: [(Int,Int)]
seaMonsterCoords = [(0,18),(1,0),(1,5),(1,6),(1,11),(1,12),(1,17),(1,18),(1,19),(2,1),(2,4),(2,7),(2,10),(2,13),(2,16)]

checkSeaMonsterAtLocation :: BigArr -> (Int,Int) -> Bool
checkSeaMonsterAtLocation arr (loc_i,loc_j) = and list where
  list = [ arr ! (loc_i + y , loc_j + x) == '#' | (y,x) <- seaMonsterCoords ]
  ((1,1),(n,m)) = bounds arr

removeSeaMonsterAt :: (Int,Int) -> [((Int,Int),Char)]
removeSeaMonsterAt (loc_i,loc_j) = 
  [ ( (loc_i + y , loc_j + x) , 'O' ) | (y,x) <- seaMonsterCoords ]

findAllSeaMonsters :: BigArr -> [(Int,Int)]
findAllSeaMonsters arr = 
  [ (i,j) 
  | i<-[1..n-seaMonsterN+1] 
  , j<-[1..m-seaMonsterM+1] 
  , checkSeaMonsterAtLocation arr (i,j)
  ]
  where 
    ((1,1),(n,m)) = bounds arr

type Roughness = Int

roughness :: Arr -> Int
roughness arr = length $ filter (=='#') $ elems arr

checkAndRemoveMonsters :: BigArr -> IO Roughness
checkAndRemoveMonsters arr = do
  let monsters = findAllSeaMonsters arr
  let update = concat $ map removeSeaMonsterAt monsters
  let final = arr // update
  -- printArr final
  -- putStrLn $ "\nroughness = " ++ show (roughness final)
  return (roughness final)

--------------------------------------------------------------------------------

type BigArr = Arr

makeBigArr :: MetaArr -> BigArr
makeBigArr meta0 = bigarr where

  bigarr = accumArray (flip const) '-' ((1,1),(big_n,big_m)) list

  list = 
    [ ( (i,j) , tile!(small_i,small_j) )
    | meta_i  <- [1..meta_n]
    , meta_j  <- [1..meta_m]
    , small_i <- [1..small_n]
    , small_j <- [1..small_m]
    , let i = (meta_i-1) * small_n + small_i
    , let j = (meta_j-1) * small_m + small_j
    , let tile = meta ! (meta_i, meta_j)
    ]

  big_n = small_n * meta_n
  big_m = small_m * meta_m

  meta = fmap (\(Just (Tile no arr)) -> removeBorders arr) meta0

  ((1,1),(small_n,small_m)) = bounds (meta!(1,1))
  ((1,1),(meta_n ,meta_m )) = bounds meta

--------------------------------------------------------------------------------

type MetaArr = Array (Int,Int) (Maybe Tile)

select1 :: [a] -> [(a,[a])]
select1 [] = []
select1 (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select1 xs ]

backtrack :: Int -> [Tile] -> MetaArr
backtrack metasize tiles = head $ go [] tiles where
  go :: [Tile] -> [Tile] -> [MetaArr]
  go acc []    = [createMetaArr metasize acc]
  go acc tiles = concat 
    [ go acc' rest | (tile0,rest) <- select1 tiles 
    , this <- possibleTiles tile0
    , let acc' = acc ++ [this] 
    , let meta = createMetaArr metasize acc' 
    , checkMetaArr meta 
    ]

printMetaArr :: MetaArr -> IO ()
printMetaArr meta = do
  putStrLn "==================="
  forM_ [1..n] $ \i -> putStrLn $ intercalate " , " $ map show [ mbTileNo (meta!(i,j)) | j<-[1..m] ]

  let corners = map mbTileNo [ meta!(1,1) , meta!(n,1) , meta!(1,m) , meta!(n,m) ]
  putStrLn $ "\ncorners = " ++ show corners
  putStrLn $ "product of corners = " ++ show (product corners)
  where
    ((1,1),(n,m)) = bounds meta

--------------------------------------------------------------------------------

side :: Dir -> Tile -> Array Int Char
side dir (Tile no arr) = arrSide dir arr

arrSide :: Dir -> Arr -> Array Int Char
arrSide dir arr = 
  case dir of
    N -> array (1,m) [ (j, arr!(1,j)) | j<-[1..m] ]
    S -> array (1,m) [ (j, arr!(n,j)) | j<-[1..m] ]
    W -> array (1,n) [ (i, arr!(i,1)) | i<-[1..n] ]
    E -> array (1,n) [ (i, arr!(i,m)) | i<-[1..n] ]
  where
    ((1,1),(n,m)) = bounds arr

checkMetaArr :: MetaArr -> Bool
checkMetaArr meta = and horizs && and verts where
  horizs = [ check (side S <$> (lkp i j)) (side N <$> lkp (i+1) j) | i<-[1..n-1], j<-[1..m  ] ]
  verts  = [ check (side E <$> (lkp i j)) (side W <$> lkp i (j+1)) | i<-[1..n]  , j<-[1..m-1] ]
  lkp !i !j = meta ! (i,j)
  bnds@((1,1),(n,m)) = bounds meta

check :: Eq a => Maybe a -> Maybe a -> Bool
check (Just x) (Just y) = x == y
check _        _        = True

createMetaArr :: Int -> [Tile] -> MetaArr
createMetaArr metasize tiles = accumArray (flip const) Nothing ((1,1),(metasize,metasize)) (zip coords $ map Just tiles) where
  coords = [ (i,j) | i<-[1..metasize], j<-[1..metasize] ]

--------------------------------------------------------------------------------

iter :: Int -> (a -> a) -> (a -> a)
iter 0 f = id
iter n f = iter (n-1) f . f

flipArr :: Arr -> Arr
flipArr arr = array bnds [ ((n+1-i,j),c) | ((i,j),c) <- assocs arr ] where
  bnds@((1,1),(n,m)) = bounds arr

-- | CW
rotArr :: Arr -> Arr
rotArr arr = array bnds [ ((j,n+1-i),c) | ((i,j),c) <- assocs arr ] where
  bnds@((1,1),(n,m)) = bounds arr

removeBorders :: Arr -> Arr
removeBorders arr = array ((1,1),(n-2,m-2)) [ ( (i,j) , arr!(i+1,j+1) ) | i<-[1..n-2], j<-[1..m-2] ] where
  bnds@((1,1),(n,m)) = bounds arr

-- | D4 symmetry
possibleTiles :: Tile -> [Tile]
possibleTiles (Tile id arr) = [ Tile id arr' | arr' <- possibleArrs arr ] 

-- | D4 symmetry
possibleArrs :: Arr -> [Arr]
possibleArrs what = 
  [ iter k rotArr          what  | k <- [0..3] ] ++ 
  [ iter k rotArr (flipArr what) | k <- [0..3] ] 

printArr :: Arr -> IO ()
printArr arr = do
  let bnds@((1,1),(n,m)) = bounds arr
  forM_ [1..n] $ \i -> putStrLn [ arr!(i,j) | j<-[1..m] ]
  putStrLn ""

printTile :: Tile -> IO ()
printTile (Tile no arr) = do
  putStrLn "-------------------"
  putStrLn $ "Tile #" ++ show no
  printArr arr

--------------------------------------------------------------------------------

parseTile :: [String] -> Tile
parseTile (header:rest) = Tile no arr where
  no = if isPrefixOf "Tile " header then read (init (drop 4 header)) else error "header"
  n = length rest
  m = length (head rest)
  arr = array ((1,1),(n,m)) [ ((i,j),c) | (i,line) <- zip [1..] rest, (j,c) <- zip [1..] line ]

load :: FilePath -> IO [Tile]
load fn = do
  ls <- lines <$> readFile fn
  let tiles = splitWhen null ls
  return $ map parseTile $ filter (not . null) tiles

--------------------------------------------------------------------------------

metaArrCornersMb :: MetaArr -> [Maybe Tile]
metaArrCornersMb arr = list where
  ((i1,j1),(i2,j2)) = bounds arr
  list = [ arr ! (i1,j1) 
         , arr ! (i1,j2) 
         , arr ! (i2,j1) 
         , arr ! (i2,j2)
         ] 

metaArrCorners :: MetaArr -> [Integer]
metaArrCorners = map (fromIntegral . tileNo . fromJust) . metaArrCornersMb

main :: IO ()
main = do
  tiles <- load "input20"     -- "test20"

  let ntiles = length tiles
  let metasize = round $ sqrt (fromIntegral ntiles :: Double)
  putStrLn $ "numer of tiles = " ++ show ntiles
  putStrLn $ "meta N         = " ++ show metasize

  ---- solve ----

  let meta = backtrack metasize tiles

  -- forM_ (possibleTiles (head tiles)) printTile
  -- printMetaArr meta

  ---- part 1 ----

  putStrLn "\npart 1 (be patient, takes about 45 seconds on my machine)"
  let corners = metaArrCorners meta
  putStrLn $ "corners          = " ++ show corners
  putStrLn $ "answer to part 1 = " ++ show (product corners)

  ---- part 2 ----

  putStrLn "\npart 2"

  let bigarr = makeBigArr meta
  -- printArr bigarr

  roughness_list <- forM (possibleArrs bigarr) checkAndRemoveMonsters
  putStrLn $ "roughness with different orientations = " ++ show roughness_list
  putStrLn $ "answer to part 2 = " ++ show (minimum roughness_list)

--------------------------------------------------------------------------------
