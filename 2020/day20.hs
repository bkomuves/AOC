
{-# LANGUAGE BangPatterns #-}

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

seaMonsterTemplate = 
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "
  ]

seaMonsterCoordsRef = [ (i,j) | i<-[0..n-1] , j<-[0..m-1] , (seaMonsterTemplate !! i ) !! j == '#' ]  where
  n = length       seaMonsterTemplate      -- 3
  m = length (head seaMonsterTemplate)     -- 20

seaMonsterN = 3
seaMonsterM = 20

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

roughness :: Arr -> Int
roughness arr = length $ filter (=='#') $ elems arr

checkAndRemoveMonsters :: BigArr -> IO ()
checkAndRemoveMonsters arr = do
  let monsters = findAllSeaMonsters arr
  let update = concat $ map removeSeaMonsterAt monsters
  let final = arr // update
  -- printArr final
  putStrLn $ "\nroughness = " ++ show (roughness final)

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

type MetaArr = Array (Int,Int) (Maybe Tile)

checkMetaArr :: MetaArr -> Bool
checkMetaArr meta = and horizs && and verts where
  horizs = [ check (side S <$> (lkp i j)) (side N <$> lkp (i+1) j) | i<-[1..n-1], j<-[1..m  ] ]
  verts  = [ check (side E <$> (lkp i j)) (side W <$> lkp i (j+1)) | i<-[1..n]  , j<-[1..m-1] ]
  lkp !i !j = meta ! (i,j)
  bnds@((1,1),(n,m)) = bounds meta

check :: Eq a => Maybe a -> Maybe a -> Bool
check (Just x) (Just y) = x == y
check _ _ = True

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

load fn = do
  ls <- lines <$> readFile fn
  let tiles = splitWhen null ls
  return $ map parseTile tiles

main = do
  -- tiles <- load "test20"
  tiles <- load "input20"
  let ntiles = length tiles
  let metasize = round $ sqrt (fromIntegral ntiles :: Double)
  putStrLn $ "numer of tiles = " ++ show (ntiles)
  putStrLn $ "meta N = " ++ show (metasize)
  putStrLn ""
  -- forM_ (possibleTiles (head tiles)) printTile
  let meta = backtrack metasize tiles
  printMetaArr meta
  let bigarr = makeBigArr meta
  printArr bigarr

  putStrLn "====================================================="
  forM_ (possibleArrs bigarr) checkAndRemoveMonsters
  -- checkAndRemoveMonsters bigarr

--------------------------------------------------------------------------------
-- solution to part1

{-

===================
3433 , 1973 , 3319 , 2749 , 2801 , 3083 , 1451 , 2843 , 1867 , 3767 , 3491 , 2011
2837 , 3307 , 1087 , 3821 , 2833 , 1091 , 1901 , 1447 , 1753 , 2731 , 1583 , 3793
3613 , 1667 , 2273 , 2579 , 1873 , 3607 , 1289 , 2693 , 1847 , 1789 , 1567 , 2287
2411 , 3853 , 1423 , 1949 , 1471 , 1009 , 2207 , 1381 , 1723 , 3701 , 2957 , 2423
3643 , 3559 , 1889 , 1999 , 3517 , 3691 , 1709 , 2371 , 1223 , 2857 , 2557 , 1997
1811 , 1559 , 3617 , 2081 , 2683 , 3359 , 2161 , 2137 , 1511 , 2239 , 1409 , 2003
2671 , 3769 , 3257 , 2477 , 1097 , 3079 , 1979 , 1181 , 2909 , 1579 , 2269 , 2399
3301 , 3169 , 3529 , 3217 , 2939 , 2351 , 3019 , 2053 , 2027 , 2089 , 1291 , 2099
3881 , 2707 , 2447 , 2963 , 2531 , 2339 , 3583 , 2221 , 3547 , 1039 , 2417 , 3109
1213 , 2281 , 1109 , 2521 , 2659 , 1103 , 2633 , 3947 , 1231 , 2143 , 2549 , 2111
1013 , 1307 , 2393 , 1151 , 2719 , 3119 , 2063 , 3877 , 1163 , 2677 , 1063 , 1279
3833 , 3943 , 2917 , 2791 , 1693 , 1051 , 1367 , 2851 , 2543 , 3413 , 1429 , 3001

corners = [3433,3833,2011,3001]
product of corners = 79412832860579

-}

--------------------------------------------------------------------------------
