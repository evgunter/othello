import Data.List (mapAccumL, maximumBy)
import GHC.Word (Word64)
import Data.Bits --many things shall hail from here
import System.Environment (getArgs, withArgs)
import System.IO (hSetBuffering, BufferMode (LineBuffering), stdout)
import Data.Maybe (catMaybes)

data Move = Move Int Int
          deriving (Show)
type OpponentsMove = Move
type MyMove        = Move
type BoardPosIndex = Int
data Board         = Board { filled :: !Word64, mine :: !Word64 }
                   deriving (Eq, Show, Ord)
data Time          = Time Int

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [color] <- getArgs
  let (board, initialOutput)
        | color == "Black" = (boardWFirstMove, formatMyMove firstMove ++ "\n")
        | otherwise        = (initialBoard, "")
  interact $ (("Sam is initialized.\n" ++ initialOutput) ++) .
    playString board
  
-- applies opponents' and my moves
oneMove :: Board -> OpponentsMove -> (Board, MyMove)
oneMove b m = (Board (setBit (filled newb) (processMove mym))
                     (foldr (flip setBit)
                            (setBit (mine newb) (processMove mym)) toFlip),
               mym)
  where
    newb = Board (setBit (filled b) (processMove m))
                 (foldr (flip clearBit) (mine b) (flipped b m))
    (mym, toFlip) = pickMove currentStrategy (validMoves newb)

pickMove :: ([(BoardPosIndex, [BoardPosIndex])] ->
              (BoardPosIndex, [BoardPosIndex])) ->
             [(BoardPosIndex, [BoardPosIndex])] -> (MyMove, [BoardPosIndex])
pickMove _ [] = (Move (-1) (-1), []) -- pass
pickMove f xs = (\(i, is) -> (boardPosToMyMove i, is)) $ f xs

currentStrategy :: [(BoardPosIndex, [BoardPosIndex])] ->
                    (BoardPosIndex, [BoardPosIndex])
currentStrategy = pickMoveGreedy

pickMoveSimple :: [(BoardPosIndex, [BoardPosIndex])] ->
                   (BoardPosIndex, [BoardPosIndex])
pickMoveSimple = head

pickMoveGreedy :: [(BoardPosIndex, [BoardPosIndex])] ->
                   (BoardPosIndex, [BoardPosIndex])
pickMoveGreedy = maximumBy greedyBetterMove

greedyBetterMove (a, as) (b, bs) | length as >  length bs = GT
                                 | length as == length bs = EQ
                                 | otherwise              = LT

parseOneMove :: String -> (OpponentsMove, Time)
parseOneMove = (\[l1, l2, l3] -> ((Move l1 l2), Time l3)) .
                 ((map read) . words)

boardPosToMyMove :: BoardPosIndex -> MyMove
boardPosToMyMove n = Move (n `mod` 8) (n `div` 8)

formatMyMove :: MyMove -> String
formatMyMove (Move x y) = (show x) ++ " " ++ (show y)

playString :: Board -> String -> String
playString initBoard rawInput = unlines $ map formatMyMove myMoves
 where opponentsMoves         = map (fst . parseOneMove) $ lines $ rawInput
       (_finalBoard, myMoves) = play initBoard opponentsMoves

play :: Board -> [OpponentsMove] -> (Board, [MyMove])
play initBoard opponentsMoves = mapAccumL oneMove initBoard opponentsMoves

initialBoard :: Board -- Board with starting configuration
initialBoard = Board
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b)
                                          [(3, 3), (4, 3), (3, 4), (4, 4)]))
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b)
                                          [(3, 3), (4, 4)]))

boardWFirstMove :: Board -- Board with first move (3, 2)
boardWFirstMove = Board
  (foldr (flip setBit) (0 :: Word64)
         (map (\(a, b) -> a + 8*b) [(3, 2), (3, 3), (4, 3), (3, 4), (4, 4)]))
  (foldr (flip setBit) (0 :: Word64)
         (map (\(a, b) -> a + 8*b) [(3, 2), (3, 4)]))
  
firstMove :: MyMove
firstMove = Move 3 2

processMove :: Move -> BoardPosIndex
processMove (Move x y) = 8*y + x

-- notDone = error . ("NotDone: " ++)

validMoves :: Board -> [(BoardPosIndex, [BoardPosIndex])]
validMoves b = filter (\(k, _) -> not (testBit (filled b) k)) $ concat $ map
  (\j -> catMaybes $ map (takeWhileAndAfter (\i -> not (testBit (mine b) i) &&
                                                   (testBit (filled b) i))) $
                         filter (\xs -> (xs /= []) &&
                                        (testBit (filled b) (head xs))) $
                                map (map processMove)
                                    (directions (boardPosToMyMove j))) $
  filter (testBit (mine b)) [0..63]

--list of spaces to be flipped from mine to opponents'
flipped :: Board -> OpponentsMove -> [BoardPosIndex]
flipped b mov = concat $ map (\(_, xs) -> xs) $ filter
  (\(k, _) -> ((testBit (filled b) k) && (not (testBit (mine b) k)))) $
  catMaybes $ map (takeWhileAndAfter (\i -> (testBit (mine b) i))) $
                  filter (\xs -> (xs /= []) &&
                                 (testBit (filled b) (head xs))) $
                         map (map processMove) (directions mov)

directions :: Move -> [[Move]]
directions m = map
  ((takeWhile (\(Move a b) -> (0 <= a) && (a <= 7) && (0 <= b) && (b <= 7))) .
   tail . (\f -> iterate f m))
  [(\(Move a b) -> Move (a-1) (b-1)), (\(Move a b) -> Move (a-1)  b),
   (\(Move a b) -> Move (a-1) (b+1)), (\(Move a b) -> Move  a    (b-1)),
   (\(Move a b) -> Move  a    (b+1)), (\(Move a b) -> Move (a+1) (b-1)),
   (\(Move a b) -> Move (a+1)  b   ), (\(Move a b) -> Move (a+1) (b+1))]

takeWhileAndAfter :: Eq a => (a -> Bool) -> [a] -> Maybe (a, [a])
takeWhileAndAfter f xs | b == []   = Nothing
                       | otherwise = Just (head b, a)
  where (a, b) = span f xs

test :: (Board, [MyMove])
test = play boardWFirstMove [Move 2 2, Move 2 4,
                             Move 5 4, Move 4 2,
                             Move 7 6, Move 4 5]



-- adjacentSquares :: Int -> [Int]
-- adjacentSquares n = [n - 9, n - 8, n - 7, n - 1, n + 1, n + 7, n + 8, n + 9]

{-
game with BetterPlayer
3 2
2 2 2
1 2
2 4 4
5 5
5 4 4
6 5
4 2 2
3 5
0 2 2
1 1
0 0 0
1 4
0 4 4
2 1
3 0 0
1 0
2 0 0
5 2
7 6 6
1 5
6 2 2
3 1
0 6 6
0 1
4 0 0
0 3
4 5 5
2 3
4 1 1
6 6
7 7 7
2 5
2 6 6
5 1
6 0 0
2 7
1 6 6
3 6
3 7 7
4 6
1 7 7
6 1
7 0 0
6 3
7 2 2
7 1
5 3 3
7 3
7 4 4
7 5
1 3 3
0 5
5 7 7
0 7
6 4 4
5 6
6 7 7
4 7
5 0 0
-1 -1
-}
