
module Day1.Solution
(
    runSolveFirst
    ,runSolveSecond
)
    where

import Text.Parsec (ParseError, digit, (<?>), parse, sepEndBy, (<|>))
import Text.Parsec.Char (char, spaces)
import Text.Parsec.Combinator (many1)
import Prelude hiding (Left, Right)
import Text.Parsec.String (Parser)
import Control.Monad.State.Lazy (execState, State, get, put)
import qualified Data.Either as E
import Debug.Trace (trace)

data Move = LeftMove Int | RightMove Int deriving (Show)

-- First part
runSolveFirst :: String -> Int
runSolveFirst input = do
    let moves = parseInput input
    case moves of
        E.Left err -> error $ show err
        E.Right ms -> snd $ execState (mapM_ processMove ms) (50, 0)

processMove :: Move -> State (Int, Int) ()
processMove move = do
    (current, count) <- get
    let newPos = rotate move current
    if newPos == 0
        then put (newPos, count + 1)
        else put (newPos,  count)

rotate :: Move -> Int -> Int
rotate (LeftMove n) pos = (pos - n) `mod` 100
rotate (RightMove n) pos = (pos + n) `mod` 100

-- Second part
runSolveSecond :: String -> Int
runSolveSecond input = do
    let moves = parseInput input
    case moves of
        E.Left err -> error $ show err
        E.Right ms -> snd (execState (mapM_ processMoveWithTracking ms) (50, 0))

processMoveWithTracking :: Move -> State (Int, Int) ()
processMoveWithTracking move = do
    (current, count) <- get
    let newPosition = trace (show move ++ " new position " ++ show (rotate move current) ++ " old position " ++ show current) $ rotate move current
    if newPosition == 0
        then put (newPosition, count + 1 + (getMoveValue move `div` 100))
        else put $ countPasses move current count


getMoveValue :: Move -> Int
getMoveValue (LeftMove m) = m
getMoveValue (RightMove m) = m

countPasses :: Move -> Int -> Int -> (Int, Int)
countPasses (LeftMove m) currentPosition currentCount =
    let (multiplier, remainder) = divMod m 100
        newPosition = (currentPosition - remainder) `mod` 100 in
    if newPosition > currentPosition && currentPosition /= 0
        then (newPosition, currentCount + 1 + abs multiplier)
        else (newPosition, currentCount + abs multiplier)

countPasses (RightMove m) currentPosition currentCount =
    let (multiplier, remainder) = divMod m 100
        newPosition = (currentPosition + remainder) `mod` 100 in
    if newPosition < currentPosition && currentPosition /= 0
        then (newPosition, currentCount + 1 + abs multiplier)
        else (newPosition, currentCount + abs multiplier)

-- Parsing
parseInput :: String -> Either ParseError [Move]
parseInput = parse parseMoves ""

parseMoves :: Parser [Move]
parseMoves = runParse `sepEndBy` spaces

runParse :: Parser Move
runParse = do
    direction <- parseRight <|> parseLeft <?> "Expected a direction (R or L)"
    number <- parseNumber <?> "Expected a number after direction"
    case direction of
        'L' -> return $ LeftMove number
        'R' -> return $ RightMove number
        _ -> fail "Unrecognized direction"

parseRight :: Parser Char
parseRight = char 'R' <?> "Expected 'R' for Right direction"

parseLeft :: Parser Char
parseLeft = char 'L' <?> "Expected 'L' for Left direction"

parseNumber :: Parser Int
parseNumber = read <$> many1 digit <?> "Expected a number"
