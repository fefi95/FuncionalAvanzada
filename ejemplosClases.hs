-- Ejemplos de las Clases de Funcional Avanzada.

import qualified Data.Maybe as Myb
import Data.List ((\\), nub)
-------------------------------------------------------------------------------
-- Clase 12-04-16
-------------------------------------------------------------------------------

-- DFS y BFS del parcial del trimestre Enero-Marzo 2016

search [] _ _ _ _ = Myb.Nothing
search (r : rs) s op p n =
    if p r then Myb.Just r
    else search rs' s' op p n
    where s' = r : s
          ms = (n r) \\ s'
          rs' = nub (op rs ms)

bfs :: Eq a => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
bfs p n r = search [r] [] (++) p n

dfs :: Eq a => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
dfs p n r = search [r] [] (flip (++)) p n

--Función que genera los hijos
child 1 = [2, 3]
child 2 = []
child 3 = [4, 5, 6]
child 4 = [4]
child 5 = [6]
child 6 = [1]
-- child _ = []
