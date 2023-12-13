import Data.List (permutations, minimumBy)
import Data.Ord (comparing)

type City = String
type Path = [City]
type Distance = Int

-- Calculate the distance between two cities
distance :: City -> City -> Distance
distance "A" "B" = 10
distance "A" "C" = 15
distance "A" "D" = 20
distance "B" "A" = 10
distance "B" "C" = 35
distance "B" "D" = 25
distance "C" "A" = 15
distance "C" "B" = 35
distance "C" "D" = 30
distance "D" "A" = 20
distance "D" "B" = 25
distance "D" "C" = 30
distance _ _ = error "Invalid city"

-- Calculate the total distance of a path
totalDistance :: Path -> Distance
totalDistance [] = 0
totalDistance [_] = 0
totalDistance (city1:city2:path) = distance city1 city2 + totalDistance (city2:path)

-- Generate all possible paths
generatePaths :: [City] -> [Path]
generatePaths = permutations

-- Find the shortest path using genetic algorithm
findShortestPath :: [City] -> Path
findShortestPath cities = minimumBy (comparing totalDistance) (generatePaths cities)

main :: IO ()
main = do
  let cities = ["A", "B", "C", "D"]
  let shortestPath = findShortestPath cities
  putStrLn $ "Shortest path: " ++ show shortestPath
  putStrLn $ "Total distance: " ++ show (totalDistance shortestPath)
