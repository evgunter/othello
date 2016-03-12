import Data.List (mapAccumL)
import GHC.Word (Word64)
import Data.Bits --many things shall hail from here
import System.Environment (getArgs, withArgs)
import System.IO (hSetBuffering, BufferMode (LineBuffering), stdout)

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
  interact ((initialOutput ++) . playString board)

-- applies opponents' and my moves
oneMove :: Board -> OpponentsMove -> (Board, MyMove)
oneMove b m = (Board (setBit (filled newb) (processMove mym))
                     (foldr (flip setBit) (setBit (mine newb) (processMove mym))
                          toFlip),
               mym)
  where
    newb = Board (setBit (filled b) (processMove m))
                 (foldr (flip clearBit) (mine b) (flipped b m))
    (mym, toFlip) = head' (validMoves newb)
    head' [] = (Move (-1) (-1), [])
    head' xs = (\(a, b) -> (boardPosToMyMove a, b))  (head xs)

--oneMove = pass where pass b  _ = (b, Move (-1) (-1))

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
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b) [(3, 3), (4, 3), (3, 4), (4, 4)]))
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b) [(3, 3), (4, 4)]))

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

notDone = error . ("NotDone: " ++)

validMoves :: Board -> [(BoardPosIndex, [BoardPosIndex])]
validMoves b = filter (\(k, _) -> not (testBit (filled b) k)) $ concat $ map
  (\j -> map (takeWhileAndAfter (\i -> not (testBit (mine b) i) &&
                                        (testBit (filled b) i))) $
             filter (\xs -> (xs /= []) && (testBit (filled b) (head xs))) $
                    map (map processMove) (directions (boardPosToMyMove j))) $
  filter (testBit (mine b)) [0..63]

--list of spaces to be flipped from mine to opponents'
flipped :: Board -> OpponentsMove -> [BoardPosIndex]
flipped b mov = concat $ map (\(_, xs) -> xs) $ filter
  (\(k, _) -> ((testBit (filled b) k) && (not (testBit (mine b) k)))) $
  map (takeWhileAndAfter (\i -> (testBit (mine b) i))) $
        filter (\xs -> (xs /= []) && (testBit (filled b) (head xs))) $
               map (map processMove) (directions mov)

directions :: Move -> [[Move]]
directions m = map
  ((takeWhile (\(Move a b) -> (0 <= a) && (a <= 7) && (0 <= b) && (b <= 7))) .
   tail . (\f -> iterate f m))
  [(\(Move a b) -> Move (a-1) (b-1)), (\(Move a b) -> Move (a-1)  b),
   (\(Move a b) -> Move (a-1) (b+1)), (\(Move a b) -> Move  a    (b-1)),
   (\(Move a b) -> Move  a    (b+1)), (\(Move a b) -> Move (a+1) (b-1)),
   (\(Move a b) -> Move (a+1)  b   ), (\(Move a b) -> Move (a+1) (b+1))]

takeWhileAndAfter :: (a -> Bool) -> [a] -> (a, [a])
takeWhileAndAfter f xs = (head b, a)
  where (a, b) = span f xs

test :: (Board, [MyMove])
test = play boardWFirstMove [Move 2 2, Move 2 4,
                             Move 5 4, Move 4 2,
                             Move 7 6, Move 4 5]



-- adjacentSquares :: Int -> [Int]
-- adjacentSquares n = [n - 9, n - 8, n - 7, n - 1, n + 1, n + 7, n + 8, n + 9]

{-
3 2
2 2 2
1 2
2 4 4
5 5
5 4 4
6 5
4 2 2
5 2
7 6 6
3 5
4 5 5
-}
