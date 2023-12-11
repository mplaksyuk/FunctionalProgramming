-- Імпорт необхідних модулів
import Data.List
import Data.Maybe
import qualified Data.Map as Map

-- Тип даних для представлення позиції на шаховій дошці
type Position = (Int, Int)

-- Функція, яка генерує всі можливі ходи коня з заданої позиції
knightMoves :: Position -> [Position]
knightMoves (x, y) = filter (\(a, b) -> a `elem` [1..8] && b `elem` [1..8]) 
    [(x+i, y+j) | (i, j) <- [(1,2), (1,-2), (-1,2), (-1,-2), (2,1), (2,-1), (-2,1), (-2,-1)]]

-- Функція, яка створює граф для заданої шахової дошки
createGraph :: [Position] -> Map.Map Position [Position]
createGraph positions = Map.fromList [(pos, knightMoves pos) | pos <- positions]

-- Функція, яка знаходить найкоротший шлях коня на шаховій дошці
knightPath :: Position -> Position -> [Position]
knightPath start end = fromJust $ bfs start end (createGraph [(x, y) | x <- [1..8], y <- [1..8]])

-- Функція, яка застосовує алгоритм обхіду в ширину для пошуку найкоротшого шляху на графі
bfs :: Position -> Position -> Map.Map Position [Position] -> Maybe [Position]
bfs start end graph = bfs' [(start, [])] Map.empty
  where
    bfs' [] _ = Nothing
    bfs' ((pos, path):queue) visited
      | pos == end = Just (reverse (pos:path))
      | pos `Map.member` visited = bfs' queue visited
      | otherwise = bfs' (queue ++ [(nextPos, pos:path) | nextPos <- graph Map.! pos]) (Map.insert pos True visited)

main :: IO ()
main = print (knightPath (1, 1) (8, 8))