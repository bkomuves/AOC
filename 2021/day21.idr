
import Data.Maybe
import Data.SortedMap
import Control.Monad.State

import Common

--------------------------------------------------------------------------------

data Player 
  = Player1
  | Player2

Eq Player where
  (==) Player1 Player1 = True
  (==) Player1 Player2 = False
  (==) Player2 Player1 = False
  (==) Player2 Player2 = True

Ord Player where
  compare Player1 Player1 = EQ
  compare Player1 Player2 = LT
  compare Player2 Player1 = GT
  compare Player2 Player2 = EQ

Show Player where
  show Player1 = "player #1" 
  show Player2 = "player #2" 

otherPlayer : Player -> Player
otherPlayer Player1 = Player2
otherPlayer Player2 = Player1

--------------------------------------------------------------------------------

Dice : Type
Dice = Int

Score : Type
Score = Int

Pos : Type
Pos = Int

fwdBy : Int -> Pos -> Pos
fwdBy n p = let m = mod n 10 in 1 + mod ( p - 1 + m ) 10 

--------------------------------------------------------------------------------

{-
input:
Player 1 starting position: 10
Player 2 starting position: 1
-}

-- (player1, player2)
Input : Type
Input = (Pos,Pos)

example, input : Input
example = (4 ,8)
input   = (10,1)

--------------------------------------------------------------------------------

record S where
  constructor MkS 
  diceNext : Dice
  counter  : Nat
  pos1     : Pos
  pos2     : Pos
  score1   : Score
  score2   : Score

initialS : Input -> S
initialS (pos1,pos2) = MkS 1 0 pos1 pos2 0 0 

Game : Type -> Type
Game a = StateT S IO a

deterministicDice : Game Dice
deterministicDice = do
  state <- get 
  let k  = diceNext state
  let k' = if k == 100 then 1 else k+1
  put $ { diceNext := k' , counter $= S } state 
  pure k

roll3 : Game (Dice,Dice,Dice)
roll3 = do
  a <- deterministicDice  
  b <- deterministicDice
  c <- deterministicDice
  pure (a,b,c)

roll : Game Int
roll = do
  (a,b,c) <- roll3
  pure (a+b+c)

move : Pos -> Game Pos
move pos = do
  n <- roll
  let pos1 = fwdBy n pos
  pure pos1

play : Player -> Game Player
play Player1 = do
  state <- get
  pos' <- move (pos1 state)
  let score' = score1 state + pos'
  modify { pos1 := pos' , score1 := score' }
  if score' >= 1000 then pure Player1 else play Player2
play Player2 = do
  state <- get
  pos' <- move (pos2 state)
  let score' = score2 state + pos'
  modify { pos2 := pos' , score2 := score' }
  if score' >= 1000 then pure Player2 else play Player1

--------------------------------------------------------------------------------
-- part 1

scoreOf : Player -> S -> Score
scoreOf player s = case player of { Player1 => score1 s ; Player2 => score2 s } 

part1 : Input -> IO ()
part1 input = do
  putStrLn "\npart 1"
  let s0 = initialS input
  (s1,winner) <- runStateT s0 (play Player1) 
  let ls  = scoreOf (otherPlayer winner) s1
  let cnt = counter s1
  putStrLn $ "loser player's score = " ++ show ls
  putStrLn $ "dice has been rolled = " ++ show cnt
  putStrLn $ "answer to part 1     = " ++ show (ls * cast cnt)

--------------------------------------------------------------------------------
-- part 2

winScore : Score
winScore = 21

record PlayerState where
  constructor PS 
  playerPos   : Pos 
  playerScore : Score

nextState : Dice -> PlayerState -> PlayerState
nextState dice (PS pos score) = 
  let pos1 = fwdBy dice pos in PS pos1 (score + pos1)

listPlayerStates : List PlayerState
listPlayerStates = [ PS pos score | pos<-[1..10] , score<-[0..winScore-1] ]

record GameState where
  constructor GS
  turn    : Player
  player1 : PlayerState
  player2 : PlayerState

listGameStates : List GameState
listGameStates = 
  [ GS p s t 
  | p <- [Player1,Player2] 
  , s <- listPlayerStates
  , t <- listPlayerStates
  ]

Eq PlayerState where
  (==) (PS q1 s1) (PS q2 s2) = (q1 == q2 && s1 == s2)

Ord PlayerState where
  compare (PS q1 s1) (PS q2 s2) = case compare q1 q2 of
    LT => LT
    GT => GT
    EQ => compare s1 s2

Show PlayerState where
  show (PS q sc) = "{ pos = " ++ show q ++ " ; score = " ++ show sc ++ " }"

Eq GameState where
  (==) (GS t1 p1 q1) (GS t2 p2 q2) = (t1 == t2 && p1 == p2 && q1 == q2)

Ord GameState where
  compare (GS t1 p1 q1) (GS t2 p2 q2) = case compare p1 p2 of
    LT => LT
    GT => GT
    EQ => case compare q1 q2 of
      LT => LT
      GT => GT
      EQ => compare t1 t2 

Show GameState where
  show (GS t pl1 pl2) = show t ++ " | #1 = " ++ show pl1 ++ " | #2 = " ++ show pl2 

WinCount : Type
WinCount = (Nat,Nat)

Universe : Type
Universe = SortedMap GameState WinCount

(+) : WinCount -> WinCount -> WinCount
(+) (a,b) (c,d) = (a+c, b+d)

zero : WinCount
zero = (0,0)

sum : List WinCount -> WinCount
sum = foldl (+) zero

diracRoll1 : List Dice
diracRoll1 = [1,2,3]

diracRoll3 : List Dice
diracRoll3 = [ a+b+c | a<-diracRoll1, b<-diracRoll1, c<-diracRoll1 ]

dirac : StateT Universe IO ()
dirac = mapM_ (ignore . worker) listGameStates where

  worker : GameState -> StateT Universe IO WinCount
  worker gs@(GS turn ps1@(PS _ sc1) ps2@(PS _ sc2)) = do
    if sc1 >= winScore 
      then pure (1,0)
      else if sc2 >= winScore
        then pure (0,1)
        else do
          univ <- get
          case SortedMap.lookup gs univ of
            Just ab => pure ab
            Nothing => do
              let next = otherPlayer turn
              this <- case turn of
                Player1 => do
                  xs <- forM diracRoll3 $ \d => worker (GS next (nextState d ps1) ps2)
                  pure $ sum xs
                Player2 => do
                  xs <- forM diracRoll3 $ \d => worker (GS next ps1 (nextState d ps2))
                  pure $ sum xs
              modify (SortedMap.insert gs this)
              pure this

mkGameState : Input -> GameState
mkGameState (q1,q2) = GS Player1 (PS q1 0) (PS q2 0)

answer : GameState -> Universe -> String
answer gs universe = case SortedMap.lookup gs universe of
  Nothing    => "fatal: game state not found"
  Just (a,b) => show (max a b) ++ " " ++ show (a,b)

part2 : IO ()
part2 = do
  putStrLn "\npart2"
  let ex  = mkGameState example
  let inp = mkGameState input
  universe <- execStateT SortedMap.empty dirac
  putStrLn $ "answer for example = " ++ answer ex  universe 
  putStrLn $ "answer for input   = " ++ answer inp universe

--------------------------------------------------------------------------------

main : IO ()
main = do
  part1 input
  part2
