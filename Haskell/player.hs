import Data.List (mapAccumL)
import GHC.Word (Word64)
import Data.Bits --many things shall hail from here
import System.Environment (getArgs)

data OpponentsMove = OpponentsMove Int Int
data MyMove        = MyMove Int Int
data Board         = Board { filled :: !Word64, mine :: !Word64 }
                   deriving (Eq, Show, Ord)
data Time          = Time Int

main :: IO ()
main = do
  [color] <- getArgs
  let (board, initialOutput)
        | color == "Black" = (boardWFirstMove, formatMyMove firstMove ++ "\n")
        | otherwise        = (initialBoard, "")
  interact ((initialOutput ++) . play board)

-- applies opponents' and my moves
oneMove :: Board -> OpponentsMove -> (Board, MyMove)
oneMove b m = (Board (setBit (filled newb) (processMyMove mym)) (setBit (mine newb) (processMyMove mym)), mym)
  where newb = Board (setBit (filled newb) (processOpponentsMove m)) (mine b)
        mym = intToMyMove (head (validMoves newb))

parseOneMove :: String -> (OpponentsMove, Time)
parseOneMove = (\[l1, l2, l3] -> ((OpponentsMove l1 l2), Time l3)) .
                 ((map read) . words)

intToMyMove :: Int -> MyMove
intToMyMove n = MyMove (n `mod` 8) (n `div` 8)

formatMyMove :: MyMove -> String
formatMyMove (MyMove x y) = (show x) ++ " " ++ (show y)

play :: Board -> String -> String
play initBoard rawInput = unlines $ map formatMyMove myMoves
 where opponentsMoves         = map (fst . parseOneMove) $ lines $ rawInput
       (_finalBoard, myMoves) = mapAccumL oneMove initBoard opponentsMoves

initialBoard :: Board -- Board with starting configuration
initialBoard = Board
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b) [(3, 3), (4, 3), (3, 4), (4, 4)]))
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b) [(3, 3), (4, 4)]))

boardWFirstMove :: Board -- Board with first move (3, 2)
boardWFirstMove = Board
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b) [(3, 2), (3, 3), (4, 3), (3, 4), (4, 4)]))
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b) [(3, 2), (3, 4)]))
  
firstMove :: MyMove
firstMove = MyMove 3 2

processOpponentsMove :: OpponentsMove -> Int
processOpponentsMove (OpponentsMove x y) = 8*y + x

processMyMove :: MyMove -> Int
processMyMove (MyMove x y) = 8*y + x

notDone :: a
notDone = undefined

validMoves :: Board -> [Int]
validMoves b = filter (\k -> not (testBit (filled b) k)) $ concat $ map
  (\j -> map (head . ((dropWhile (\i -> not (testBit (mine b) i) &&
                                        (testBit (filled b) i))))) $
             filter (\xs -> (testBit (filled b) (head xs))) $
                    map ((takeWhile (\x -> (0 <= x) && (x <= 63))) . tail .
                         (\f -> iterate f j))
                        [(+ (-9)), (+ (-8)), (+ (-7)), (+ (-1)), (+ 1), (+ 7),
                         (+ 8), (+ 9)])
  myPieceIndexes
  where
    myPieceIndexes = filter (testBit (mine b)) [0..63]

-- adjacentSquares :: Int -> [Int]
-- adjacentSquares n = [n - 9, n - 8, n - 7, n - 1, n + 1, n + 7, n + 8, n + 9]
