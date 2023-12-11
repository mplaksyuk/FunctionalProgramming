type Graph a = [(a, [a])]  -- Представлення графу, де кожна вершина має список сусідніх вершин

paths :: Eq a => a -> a -> Graph a -> [[a]]
paths start end graph = pathsHelper start end graph []

pathsHelper :: Eq a => a -> a -> Graph a -> [a] -> [[a]]
pathsHelper current end graph visited
  | current == end = [[end]]  -- Зупинка: досягнуто кінцеву вершину
  | otherwise = do
      neighbor <- neighbors
      path <- pathsHelper neighbor end graph (current : visited)
      return (current : path)
  where
    neighbors = filter (`notElem` visited) $ maybe [] id (lookup current graph)

sampleGraph :: Graph Char
sampleGraph = [('A', ['B', 'C']),
                ('B', ['A', 'D', 'E']),
                ('C', ['A', 'F', 'G']),
                ('D', ['B']),
                ('E', ['B', 'H']),
                ('F', ['C']),
                ('G', ['C', 'I']),
                ('H', ['E']),
                ('I', ['G'])]

main :: IO ()
main = print $ paths 'A' 'I' sampleGraph
    