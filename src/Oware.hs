-- Welcome to Haskell
-- you must use at least one language pragma
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Oware
    where

import           Data.Array (Array)
import qualified Data.Array as Array
import           Data.Char (toLower)
import           Data.List (intercalate, intersperse)
import           Data.Maybe (fromMaybe)
import           System.Random (getStdRandom, randomR)

-- | There are two players, and two sides of the board.
data Side = One | Two deriving (Show, Eq, Ord, Enum, Array.Ix)

-- | Each side of the board has 6 holes labelled A-F.
data Location = A | B | C | D | E | F deriving (Show, Eq, Ord, Enum, Array.Ix)

-- | We can identify a particular hole by
--   the side and location of it.
type Hole = (Side, Location)

-- | A board is then a map from the hole to
--   the number of counters in that hole.
newtype Board = Board (Array Hole Count)
type Count = Int

(!) :: Board -> Hole -> Count
(!) (Board b) hole = b Array.! hole

-- | The initial board setup has 4 counters in every hole.
initialBoard :: Board
initialBoard = Board $ Array.listArray ((One, A), (Two, F)) (repeat 4)

-- | How to print a board.
instance Show Board where
    show (Board b) =
            intercalate "\n" $
            [topLegend] ++ boardPart ++ [bottomLegend]
        where
        boardPart =
            surrounded breaker $
            [Two, One] <$$> \side ->
                surround "|" $
                [A .. F] <$$> \location ->
                    cell (b Array.! (side, location))
        topLegend = map toLower bottomLegend
        bottomLegend = surround " " $ cell <$> [A .. F]
        surrounded x xs = x : intersperse x xs ++ [x]
        surround x xs = x ++ intercalate x xs ++ x
        breaker = '+' : concatMap fst (zip (repeat "----+") [A .. F])
        (<$$>) = flip (<$>)
        cell it = (if length s == 1 then "  " else " ") ++ s ++ " "
            where s = show it

data Player = Player
    {
        -- given a board, a player picks a move
        -- the player is restricted to the operations in
        -- PlayerMonad below
        runPlayer :: forall m. PlayerMonad m => State -> m Location
    }

class Monad m => PlayerMonad m where
    input :: m String
    output :: String -> m ()
    getRandom :: (Int, Int) -> m Int

instance PlayerMonad IO where
    input = getLine
    output = putStrLn
    getRandom = getStdRandom . randomR

-- | Go from one hole to the next hole (an anti-clockwise path around the board).
next :: Hole -> Hole
next (One, F) = (Two, F)
next (Two, A) = (One, A)
next (One, l) = (One, succ l)
next (Two, l) = (Two, pred l)

-- | Go from one hole to the previous hole.
prev :: Hole -> Hole
prev (Two, F) = (One, F)
prev (One, A) = (Two, A)
prev (One, l) = (One, pred l)
prev (Two, l) = (Two, succ l)

-- | Switch from one side to the other.
other :: Side -> Side
other One = Two
other Two = One

-- | Shows what holes would be seeded by the chosen hole.
holesToSeed :: Hole -> Board -> [Hole]
holesToSeed chosenHole board =
    take count (filter (/= chosenHole) (iterate next chosenHole))
    where
    count = board ! chosenHole

-- | Plays a move, dispersing only. Returns the
--   new board and the last hole filled.
seed :: Hole -> Board -> (Board, Hole)
seed chosenHole b@(Board board) =
    (seededBoard, lastHole)
    where
    holesToSeed' = holesToSeed chosenHole b
    initBoard = board Array.// [(chosenHole, 0)]
    lastHole = last holesToSeed'
    seededBoard = Board (Array.accum (+) initBoard (zip holesToSeed' (repeat 1)))

-- | Performs captures by going backwards from the hole
--   and emptying any holes that have 2 or 3 counters.
performCaptures :: Side -> Board -> Hole -> (Board, Count)
-- if last hole is on player's side, they do not capture
performCaptures side board (holeSide, _) | side == holeSide = (board, 0)
performCaptures side (Board b) hole =
    (newBoard, captured)
    where
    newBoard = Board (b Array.// map (, 0) capturedLocations)

    (capturedLocations, captured) =
        (map snd countAndLocation, sum (map fst countAndLocation))
        where
        countAndLocation =
            takeWhile (\(c, loc) -> c == 2 || c == 3) $
            map (\loc -> (b Array.! loc, loc)) locationsToCheck

    locationsToCheck =
        takeWhile (\loc -> fst loc == fst hole) (iterate prev hole)

-- | Plays a move and returns the new board and
--   the number of counters captured by that move.
playMove :: Hole -> Board -> (Board, Count)
playMove hole@(side, _) board = uncurry (performCaptures side) (seed hole board)

type Score = (Int, Int)

addScore :: Side -> Int -> Score -> Score
addScore One c (one, two) = (one + c, two)
addScore Two c (one, two) = (one, two + c)

-- | The state of play
data State = State
    { board :: Board -- ^ the state of the board
    , score :: Score -- ^ the scores (1, 2)
    , turn :: Side -- ^ whose turn it is
    }

-- | The initial state of the game.
startingState :: State
startingState = State initialBoard (0, 0) One

-- | Does the player have any pieces?
hasPieces :: Side -> Board -> Bool
hasPieces side board = any (> 0) (map (board !) (playerHoles side))

-- | The holes on the select player's side.
playerHoles :: Side -> [Hole]
playerHoles side = map (side,) [A .. F]

validMoves :: Side -> Board -> [Location]
validMoves side board =
    map snd holesThatGiveOpponentValidMoves

    where
    -- if the other player has no pieces, then
    -- you must give them some
    holesThatGiveOpponentValidMoves =
        filter (\hole -> hasPieces (other side) $ fst $ playMove hole board)
        movableHoles

    movableHoles =
        filter (\hole -> board ! hole > 0) $
        playerHoles side

-- | A 'Nothing' result means the player has no valid moves.
playTurn :: PlayerMonad m => Player -> Player -> State -> m (Maybe State)
playTurn p1 p2 state@State{turn, board} =
    if null validMoves'
    then pure Nothing
    else do
        move <- untilResult getPlayerMove
        let (newBoard, captured) = playMove (turn, move) board
        let newState = state {
            board = newBoard,
            turn = other turn,
            score = addScore turn captured (score state)
        }
        pure (Just newState)

    where
        validMoves' = validMoves turn board

        player = if turn == One then p1 else p2

        getPlayerMove :: PlayerMonad m => m (Maybe Location)
        getPlayerMove = do
            move <- runPlayer player state
            pure $
                if move `elem` validMoves'
                then Just move
                else Nothing

-- | Plays a game and returns the scores.
playGame :: PlayerMonad m => Player -> Player -> m Score
playGame p1 p2 = go startingState
    where
    go state =
        playTurn p1 p2 state >>=
        maybe (pure (score (endGame state))) go

    -- when the game ends any remaining pieces
    -- go to the player whose turn it was
    endGame s@State{turn, score, board} =
        s { score = addScore turn remainingPieces score }
        where remainingPieces = case board of Board b -> sum (Array.elems b) 

-- | Runs the monadic action until it returns something that isn't `Nothing`.
untilResult :: Monad m => m (Maybe a) -> m a
untilResult f = f >>= maybe (untilResult f) pure

humanPlayer :: Player
humanPlayer = Player go
    where
    go :: PlayerMonad m => State -> m Location
    go State{board, turn, score} = do
        output ""
        output (show board)
        output ""
        let (score1, score2) = score
        output ("        Scores: " ++ show score1 ++ " - " ++ show score2)
        output ""
        untilResult getMove

        where
            playerName = "Player " ++ show turn

            getMove = do
                output (playerName ++ ", choose a move " ++ show (validMoves turn board) ++  ": ")
                parseMove <$> input

            parseMove = \case
                [c] ->
                    case toLower c of
                        'a' -> Just A
                        'b' -> Just B
                        'c' -> Just C
                        'd' -> Just D
                        'e' -> Just E
                        'f' -> Just F
                        _ -> Nothing
                _ -> Nothing

randomPlayer :: Player
randomPlayer = Player go
    where
    go :: PlayerMonad m => State -> m Location
    go State{turn, board} =
        let moves = validMoves turn board in
        (moves !!) <$> getRandom (0, length moves - 1)
