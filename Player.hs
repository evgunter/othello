import Debug.Trace (trace)
import Data.List (mapAccumL, maximumBy, sort, groupBy)
import GHC.Word (Word64)
import Data.Bits (Bits, testBit, setBit, bitSize, clearBit, complement, (.&.))
import System.Environment (getArgs, withArgs)
import System.IO (hSetBuffering, BufferMode (LineBuffering), stdout)
import Data.Maybe (catMaybes)

data Move = Move Int Int
          deriving (Eq, Show)
type OpponentsMove = Move
type MyMove        = Move
type BoardPosIndex = Int
data Board         = Board { filled :: !Word64, mine :: !Word64 }
                   deriving (Eq)
type Time          = Int

instance Show Board where
 show (Board fill my) = unlines $ groupsOf 8
   $ map onePos [0..bitSize fill - 1]
   where onePos i = case (testBit fill i, testBit my i) of
                     (False, _)     -> '.'
                     (True,  False) -> 'O'
                     (True,  True)  -> 'M'

groupsOf :: Int -> [a] -> [[a]]
groupsOf i _ | i < 1 = error $ "groupOf: " ++ show i
groupsOf i xs = case splitAt i xs of
                (as, []) -> [as]
                (as, bs) -> as : groupsOf i bs

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [color] <- getArgs
  let initBoard | color == "Black" = initialBBoard
                | otherwise        = initialWBoard
  interact $ ("Sam is initialized.\n" ++ ) . playString initBoard

-- applies opponents' and my moves
oneMove :: Board -> OpponentsMove -> (Board, MyMove)
oneMove b m = trace ("oneMove: opponent: " ++ show m)
                    (newerBoard, trace traceString mym)
  where
    newb          = doOpponentsMove    b (m,  (flipped b m))
    (mym, toFlip) = pickMove (flip currentStrategy newb) $ emptyToPass $
                      map (\(a, as) -> (boardPosToMyMove a, as))
                          (validMoves newb)
    newerBoard    = doMyMove newb (mym, toFlip)
    traceString   = "oneMove: mine: " ++ show mym
                  ++ "\npost-my-move board:" ++ show [filled newerBoard, mine newerBoard] ++ "\n" ++ show newerBoard

doMyMove :: Board -> (MyMove, [BoardPosIndex]) -> Board
doMyMove b (m, toFlip) = Board (setMoveBit (filled b) m)
                               (foldr (flip setBit)
                                      (setMoveBit (mine b) m) toFlip)
doOpponentsMove :: Board -> (OpponentsMove, [BoardPosIndex]) -> Board
doOpponentsMove b (_, [])     = b
doOpponentsMove b (Move (-1) (-1), _) = b -- looks redundant but i'm getting
                                          -- a weird error and it can't hurt
doOpponentsMove b (m, toFlip) = Board (setMoveBit (filled b)  m)
                                      (foldr (flip clearBit) (mine b) toFlip)

pickMove :: ([(MyMove, [BoardPosIndex])] ->
              (MyMove, [BoardPosIndex])) ->
             [(MyMove, [BoardPosIndex])] -> (MyMove, [BoardPosIndex])
pickMove _ [] = (Move (-1) (-1), []) -- pass
pickMove f xs = f xs

currentStrategy :: [(MyMove, [BoardPosIndex])] -> Board ->
                    (MyMove, [BoardPosIndex])
currentStrategy = minimaxWrap

minimaxWrap :: [(MyMove, [BoardPosIndex])] -> Board ->
                  (MyMove, [BoardPosIndex])
minimaxWrap _ b = fst $ minimax b depth

minimax :: Board -> Int -> ((Move, [BoardPosIndex]), Double)
minimax b i
  | i == 0    = maximumBy rank $ map
                  (\m -> (m, eval (doMyMove b m))) $ emptyToPass $
                    map (\(a, as) -> (boardPosToMyMove a, as)) $ validMoves b
  | otherwise = maximumBy rank $ map
     ((\(firstLayer, (_, value)) -> (firstLayer, (- value))) .
      (\firstLayer -> (firstLayer, minimax (flipBoard ((doMyMove b)
                                                       firstLayer)) (i - 1))))
     $ emptyToPass $ map (\(a, as) -> (boardPosToMyMove a, as)) $ validMoves b
  where rank (_, z) (_, w) | z > w     = GT
                           | otherwise = LT

emptyToPass :: [(Move, [BoardPosIndex])] -> [(Move, [BoardPosIndex])]
emptyToPass [] = [(Move (-1) (-1), [])]
emptyToPass x  = x

-- this heuristic function is based on https://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello/
-- in particular, the relative weightings are copied directly.
eval :: Board -> Double
eval b@(Board f m) = 10 * totalTiles + 801.724 * cornersOcc +
                     382.026 * cornerNext + 78.922 * mobility
  where
  totalTiles = 100 * (fromIntegral (2*(popCount' m) - (popCount' f)))
                   / (fromIntegral (popCount' f))
  cornersOcc =  fromIntegral $
      25 * (2 *  sum (map true1False0
                          [(testBit m  0       ), (testBit m  7      ),
                           (testBit m (8*7)    ), (testBit m (8*7 + 7))])
              -  sum (map true1False0
                          [(testBit f  0       ), (testBit f  7      ),
                           (testBit f (8*7)    ), (testBit f (8*7 + 7))]))
  cornerNext = 
    (-12.5) * (fromIntegral
               ((true1False0 (not (testBit f  0       ))) *
           (2 * (sum (map true1False0
                          [(testBit m  1       ), (testBit m  8      ),
                           (testBit m  9       )]))
              - (sum (map true1False0
                          [(testBit f  1       ), (testBit f  8      ),
                           (testBit m  9       )])))
            +  (true1False0 (not (testBit f  7       ))) *
           (2 * (sum (map true1False0
                          [(testBit m  6       ), (testBit m (8*1 + 6)),
                           (testBit m (8*1 + 1))]))
              - (sum (map true1False0
                          [(testBit f  6       ), (testBit f (8*1 + 6)),
                           (testBit f (8*1 + 1))])))
            +  (true1False0 (not (testBit f (8*7    )))) *
           (2 * (sum (map true1False0
                          [(testBit m (8*6)    ), (testBit m (8*6 + 1)),
                           (testBit m (8*7 + 1))]))
              - (sum (map true1False0
                          [(testBit f (8*6)    ), (testBit f (8*6 + 1)),
                           (testBit f (8*7 + 1))])))
            +  (true1False0 (not (testBit f (8*7 + 7)))) *
           (2 * (sum (map true1False0
                          [(testBit m (8*6 + 7)), (testBit m (8*6 + 6)),
                           (testBit m (8*7 + 6))]))
              - (sum (map true1False0
                          [(testBit f (8*6 + 7)), (testBit f (8*6 + 6)),
                           (testBit f (8*7 + 6))])))))
  mobility = if (lm + lo) == 0 then 0 else 100 * (lm - lo) / (lm + lo) where
    lm = fromIntegral $ length $ validMoves $           b
    lo = fromIntegral $ length $ validMoves $ flipBoard b
  true1False0 bool = if bool then 1 else 0

depth :: Int
depth = 4

parseOneMove :: String -> (OpponentsMove, Time)
parseOneMove = (\[l1, l2, l3] -> ((Move l1 l2), l3)) .
                ((map read) . words) . (\x -> trace ("Sam got: " ++ show x) x)

boardPosToMyMove :: BoardPosIndex -> MyMove
boardPosToMyMove n = Move (n `mod` 8) (n `div` 8)

formatMyMove :: MyMove -> String
formatMyMove (Move x y) = (show x) ++ " " ++ (show y)

playString :: Board -> String -> String
playString initBoard rawInput = unlines $ map formatMyMove myMoves
 where opponentsMoves         = map (fst . parseOneMove) $ lines $ rawInput
       (_finalBoard, myMoves) = play initBoard opponentsMoves

play :: Board -> [OpponentsMove] -> (Board, [MyMove])
play = mapAccumL oneMove

initialWBoard :: Board -- Board with starting configuration for white
initialWBoard = Board
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b)
                                          [(3, 3), (4, 3), (3, 4), (4, 4)]))
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b)
                                          [(3, 3), (4, 4)]))

initialBBoard :: Board -- Board with starting configuration for black
initialBBoard = Board
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b)
                                          [(3, 3), (4, 3), (3, 4), (4, 4)]))
  (foldr (flip setBit) (0 :: Word64) (map (\(a, b) -> a + 8*b)
                                          [(3, 4), (4, 3)]))

flipBoard :: Board -> Board
flipBoard b = Board fill ((complement (mine b)) .&. fill) where
  fill = filled b

checkValid :: Move -> Move
checkValid m@(Move x y) | 0 <= x && x < 8 && 0 <= y && y < 8 = m
                        | otherwise = error $ "bad move: " ++ show m

setMoveBit :: Word64 -> Move -> Word64
setMoveBit w (Move (-1) (-1)) = w
setMoveBit w m = setBit w $ moveIndex $ checkValid m

moveIndex :: Move -> BoardPosIndex
moveIndex (Move x y) = (8*y + x)

validMoves :: Board -> [(BoardPosIndex, [BoardPosIndex])]
validMoves b = map (foldr (\(a, as) (_, bs) -> (a, as ++ bs)) (-1, [])) $
 groupBy (\x y -> fst x == fst y) . sort $
 filter (\(k, _) -> not (testBit (filled b) k)) $ concat $ map
  (\j -> catMaybes $ map (takeWhileAndAfter (\i -> not (testBit (mine b) i) &&
                                                   (testBit (filled b) i))) $
                         filter (\xs -> (xs /= []) &&
                                        (testBit (filled b) (head xs))) $
                                map (map moveIndex)
                                    (directions (boardPosToMyMove j))) $
  filter (testBit (mine b)) [0..63]

--list of spaces to be flipped from mine to opponents'
flipped :: Board -> OpponentsMove -> [BoardPosIndex]
flipped b mov = concat $ map (\(_, xs) -> xs) $ filter
  (\(k, _) -> ((testBit (filled b) k) && (not (testBit (mine b) k)))) $
  catMaybes $ map (takeWhileAndAfter (\i -> (testBit (mine b) i))) $
                  filter (\xs -> (xs /= []) &&
                                 (testBit (filled b) (head xs))) $
                         map (map moveIndex) (directions mov)

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

popCount' :: Bits a => a -> Int
popCount' x = length $ filter id $ map (testBit x) [0 .. (bitSize x) - 1]

testPassError'' :: Board
testPassError'' = Board 296248274759515775 34494545471

{-
testPassError' :: Board
testPassError' = Board 5417840316194575 284872480853775

testPassError :: Board --was actually zero division error
testPassError = Board 18427885250269937407 18410874641327652607

testdiagonal :: Board
testdiagonal = Board (35978936582144 + 2^(4+5*8)) 35390664998912

tupleToBoardIndex :: (Int, Int) -> BoardPosIndex
tupleToBoardIndex (x, y) = 8*y + x

simple :: [(BoardPosIndex, [BoardPosIndex])] -> Board ->
           (BoardPosIndex, [BoardPosIndex])
simple xs _ = head xs

greedy :: [(BoardPosIndex, [BoardPosIndex])] -> Board ->
           (BoardPosIndex, [BoardPosIndex])
greedy xs _ = maximumBy best xs
  where best (a, as) (b, bs) | length as >  length bs = GT
                             | length as == length bs = EQ
                             | otherwise              = LT

greedyCorners :: [(BoardPosIndex, [BoardPosIndex])] -> Board ->
                  (BoardPosIndex, [BoardPosIndex])
greedyCorners xs _ = maximumBy best xs
  where best (a, as) (b, bs) | a `elem` corners       = GT
                             | b `elem` corners       = LT
                             | a `elem` cornerDiag    = LT
                             | b `elem` cornerDiag    = GT
                             | a `elem` cornerEdge    = LT
                             | b `elem` cornerEdge    = GT
                             | length as >  length bs = GT
                             | otherwise              = LT
                             --no EQ because unnecessary

corners :: [BoardPosIndex]
corners = map tupleToBoardIndex [(0, 0), (0, 7), (7, 0), (7, 7)]

cornerDiag :: [BoardPosIndex]
cornerDiag = map tupleToBoardIndex [(1, 1), (1, 6), (6, 1), (6, 6)]

cornerEdge :: [BoardPosIndex]
cornerEdge = map tupleToBoardIndex [(0, 1), (1, 0), (0, 6), (6, 0), (1, 7),
                                    (7, 1), (6, 7), (7, 6)]

edges :: [BoardPosIndex]
edges = map tupleToBoardIndex $ concat $ map
   (\n -> [(0, n), (7, n), (n, 0), (n, 7)]) [0..7]
-}
