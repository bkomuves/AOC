
import Data.Nat
import Data.Maybe
import Data.List1

import Decidable.Equality

import Common
import Parser

%hide Prelude.Left
%hide Prelude.Right

--------------------------------------------------------------------------------
-- tree shapes

data Shape : Type where
  Lf : Shape
  Nd : Shape -> Shape -> Shape

DecEq Shape where
  decEq Lf Lf = Yes Refl
  decEq (Nd l1 r1) (Nd l2 r2) = case (decEq l1 l2 , decEq r1 r2) of
    (Yes p1, Yes p2) => Yes (cong2 Nd p1 p2)
    _                => No  believe_me
  decEq _ _ = No believe_me

--------------------------------------------------------------------------------
-- paths, indexed by shapes

namespace Path

  -- paths ending at a leaf
  public export
  data Path : Shape -> Type where
    Nil    : Path Lf
    Left   : Path l -> Path (Nd l r)
    Right  : Path r -> Path (Nd l r)

  public export
  length : Path s -> Nat
  length Nil       = 0
  length (Left  p) = S (length p)
  length (Right p) = S (length p)

  export
  Show (Path s) where
    show Nil       = "."
    show (Left  p) = "L" ++ show p
    show (Right p) = "R" ++ show p

  public export
  leftMost : (s : Shape) -> Path s
  leftMost (Lf    ) = Nil
  leftMost (Nd l _) = Left (leftMost l)

  public export
  rightMost : (s : Shape) -> Path s
  rightMost (Lf    ) = Nil
  rightMost (Nd _ r) = Right (rightMost r)

  public export
  moveLeft : {s : Shape} -> Path s -> Maybe (Path s)
  moveLeft  Nil      = Nothing
  moveLeft (Left  p) = Left <$> moveLeft p
  moveLeft (Right p) = Just $ maybe (Left $ rightMost _) Right (moveLeft p)

  public export
  moveRight : {s : Shape} -> Path s -> Maybe (Path s)
  moveRight  Nil      = Nothing
  moveRight (Right p) = Right <$> moveRight p
  moveRight (Left  p) = Just $ maybe (Right $ leftMost _) Left (moveRight p)

--------------------------------------------------------------------------------
-- grafting

  -- replace the given leaf with a new shape
  public export
  graft : Shape -> {s : Shape} -> Path s -> Shape
  graft new {s=Lf    } Nil       = new
  graft new {s=Nd l r} (Left  p) = Nd   (graft new p) r
  graft new {s=Nd l r} (Right p) = Nd l (graft new p)

--------------------------------------------------------------------------------
-- trees indexed by shape

data Tree : Shape -> Type where
  Leaf : Nat -> Tree Lf
  Node : Tree s -> Tree t -> Tree (Nd s t)

Eq (Tree s) where
  Leaf x     == Leaf y     = x == y
  Node l1 r1 == Node l2 r2 = l1==l2 && r1==r2

namespace Path

  public export
  valueAt : Path s -> Tree s -> Nat
  valueAt Nil       (Leaf x  ) = x
  valueAt (Left  p) (Node l _) = valueAt p l
  valueAt (Right p) (Node _ r) = valueAt p r

  public export
  modifyAt : (Nat -> Nat) -> Path s -> Tree s -> Tree s
  modifyAt f Nil       (Leaf x  ) = Leaf (f x)
  modifyAt f (Left  p) (Node l r) = Node   (modifyAt f p l) r
  modifyAt f (Right p) (Node l r) = Node l (modifyAt f p r)

  public export
  maybeModifyAt : (Nat -> Nat) -> Maybe (Path s) -> Tree s -> Tree s
  maybeModifyAt _ Nothing  t = t
  maybeModifyAt f (Just p) t = modifyAt f p t

  public export
  replaceLeafBy : {s, s' : Shape} -> Tree s' -> (p : Path s) -> Tree s -> Tree (graft s' p)
  replaceLeafBy new = go where
    go : forall s. (p : Path s) -> Tree s -> Tree (graft s' p)
    go Nil       _          = new
    go (Left  p) (Node l r) = Node   (go p l) r
    go (Right p) (Node l r) = Node l (go p r)

  -- used for testing
  public export
  flatten : {s : Shape} -> (t : Tree s) -> List (Nat, Path s)
  flatten t = go (leftMost s) where
    go : Path s -> List (Nat, Path s)
    go p = (valueAt p t, p) :: fromMaybe [] (go <$> moveRight p)

--------------------------------------------------------------------------------
-- paths ending at a cherry

namespace Cherry

  public export
  data Path2 : Shape -> Type where
    Cherry : Path2 (Nd Lf Lf)
    Left   : Path2 l -> Path2 (Nd l r)
    Right  : Path2 r -> Path2 (Nd l r)

  export
  Show (Path2 t) where
    show Cherry    = ":"
    show (Left  p) = "L" ++ show p
    show (Right p) = "R" ++ show p

  public export
  cherryAt : Path2 s -> Tree s -> (Nat,Nat)
  cherryAt Cherry    (Node (Leaf x) (Leaf y)) = (x,y)
  cherryAt (Left  p) (Node l _)               = cherryAt p l
  cherryAt (Right p) (Node _ r)               = cherryAt p r

  -- replace the given cherry with a new shape
  -- NB. Apparently the order of clauses matter here!
  public export
  cherryGraft : Shape -> {s : Shape} -> Path2 s -> Shape
  cherryGraft new {s=Nd l r} (Left  p) = Nd   (cherryGraft new p) r
  cherryGraft new {s=Nd l r} (Right p) = Nd l (cherryGraft new p)
  cherryGraft new Cherry               = new

  public export
  replaceCherryBy : {s, s' : Shape} -> Tree s' -> (p : Path2 s) -> Tree s -> Tree (cherryGraft s' p)
  replaceCherryBy new = go where
    go : forall s. (p : Path2 s) -> Tree s -> Tree (cherryGraft s' p)
    go Cherry    _          = new
    go (Left  p) (Node l r) = Node   (go p l) r
    go (Right p) (Node l r) = Node l (go p r)

--------------------------------------------------------------------------------

-- left leaf of a cherry
cherryLeft : Path2 s -> Path s
cherryLeft Cherry    = Left  Nil
cherryLeft (Left  p) = Left  (cherryLeft p)
cherryLeft (Right p) = Right (cherryLeft p)

-- right leaf of a cherry
cherryRight : Path2 s -> Path s
cherryRight Cherry    = Right Nil
cherryRight (Left  p) = Left  (cherryRight p)
cherryRight (Right p) = Right (cherryRight p)

-- check if it's the left or right leaf of a cherry
isCherry : {s : Shape} -> Path s -> Maybe (Path2 s)
isCherry {s=Nd Lf Lf} (Left  Nil) = Just Cherry
isCherry {s=Nd Lf Lf} (Right Nil) = Just Cherry
isCherry              (Left  p  ) = Left  <$> isCherry p
isCherry              (Right p  ) = Right <$> isCherry p
isCherry              Nil         = Nothing

-- same but with a condition on length too
isLongCherry : Nat -> {s : Shape} -> Path s -> Maybe (Path2 s)
isLongCherry minlen p = if length p >= minlen
  then isCherry p
  else Nothing

--------------------------------------------------------------------------------
-- explosion and split

explodeAt : {s : Shape} -> (p : Path2 s) -> (t : Tree s) -> Tree (cherryGraft Lf p)
explodeAt p t = case cherryAt p t of
  (x,y) => replaceCherryBy (Leaf 0) p
         $ maybeModifyAt (+x) (moveLeft  $ cherryLeft  p)
         $ maybeModifyAt (+y) (moveRight $ cherryRight p)
         $ t

splitAt :  {s : Shape} -> (p : Path s) -> (t : Tree s) -> Tree (graft (Nd Lf Lf) p)
splitAt p t =
  let k = valueAt p t
      a = halve k
      b = minus k a
      cherry = Node (Leaf a) (Leaf b)
  in  replaceLeafBy cherry p t

findLeaf : {s : Shape} -> (Path s -> Bool) -> Maybe (Path s)
findLeaf cond = go (leftMost s) where
  go : Path s -> Maybe (Path s)
  go p = if cond p
    then Just p
    else case moveRight p of
      Nothing => Nothing
      Just q  => go q

findLeafMb : {s : Shape} -> (Path s -> Maybe a) -> Maybe (Path s, a)
findLeafMb cond = go (leftMost s) where
  go : Path s -> Maybe (Path s, a)
  go p = case cond p of
    Just y  => Just (p,y)
    Nothing => case moveRight p of
      Nothing => Nothing
      Just q  => go q

--------------------------------------------------------------------------------

record Snailfish where
  constructor MkFish
  shape : Shape
  fish  : Tree shape

Eq Snailfish where
  (==) (MkFish s1 t1) (MkFish s2 t2) = case decEq s1 s2 of
    Yes Refl => t1 == t2
    No  _    => False

showTree : Tree s -> String
showTree (Leaf n  ) = show n
showTree (Node l r) = "[" ++ showTree l ++ "," ++ showTree r ++ "]"

Show Snailfish where
  show (MkFish _ tree) = showTree tree

reduceStep1 : Snailfish -> Maybe Snailfish
reduceStep1 (MkFish s t) = case findLeafMb (\p => isLongCherry 5 p) of
  Nothing    => Nothing
  Just (p,q) => Just $ MkFish _ (explodeAt q t)

reduceStep2 : Snailfish -> Maybe Snailfish
reduceStep2 (MkFish s t) = case findLeaf (\p => valueAt p t >= 10) of
  Nothing => Nothing
  Just p  => Just $ MkFish _ (splitAt p t)

reduce : Snailfish -> Snailfish
reduce sf =
  case reduceStep1 sf of
    Just res => reduce res
    Nothing  => case reduceStep2 sf of
      Just res => reduce res
      Nothing  => sf

addFish : Snailfish -> Snailfish -> Snailfish
addFish x y = reduce $ MkFish _ (Node x.fish y.fish)

sumFish1 : List1 Snailfish -> Snailfish
sumFish1 (first:::rest) = foldl addFish first rest

sumFish : List Snailfish -> Snailfish
sumFish []      = fatal "summing empty list"
sumFish (x::xs) = sumFish1 (x:::xs)

treeMagnitude : Tree s -> Nat
treeMagnitude (Leaf x  ) = x
treeMagnitude (Node l r) = 3 * treeMagnitude l + 2 * treeMagnitude r

magnitude : Snailfish -> Nat
magnitude f = treeMagnitude f.fish

--------------------------------------------------------------------------------
-- parsing

Parser : Type -> Type
Parser = GenParser Char

snailfishP : Parser Snailfish
snailfishP = fishP where

  mutual
    fishP, litP, pairP : Parser Snailfish

    fishP = pairP <|> litP

    litP = do
      d <- satisfy $ \c => c >= '0' && c <= '9'
      pure $ MkFish Lf (Leaf $ cast (ord d - 48))

    pairP = do
      _ <- satisfy (=='[')
      l <- fishP
      _ <- satisfy (==',')
      r <- fishP
      _ <- satisfy (==']')
      pure (MkFish (Nd l.shape r.shape) (Node l.fish r.fish))

parseFish : String -> Snailfish
parseFish str = case runParser snailfishP (unpack str) of
  Just (sf,Nil) => sf
  _             => fatal $ "cannot parse snailfish `" ++ str ++ "`"

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- filterNonEmpty <$> readLines "input18"
  let input : List Snailfish
      input = map parseFish ls
  let s = sumFish input
  -- mapM_ printLn input
  -- putStrLn "======================="
  -- printLn  $ s
  putStrLn $ "part 1 magnitude = " ++ show (magnitude s)
  let ms : List Nat
      ms = [ magnitude (addFish x y) | x <- input , y <- input , x /= y ]
  putStrLn $ "part 2 magnitude = " ++ show (foldl max 0 ms)

