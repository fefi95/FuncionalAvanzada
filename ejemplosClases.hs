-- Ejemplos de las Clases de Funcional Avanzada.

import qualified Data.Maybe as Myb
import Data.List ((\\), nub)
import qualified Data.Monoid as Mnid
-------------------------------------------------------------------------------
-- Clase 12-04-16
-------------------------------------------------------------------------------

-- DFS y BFS del parcial del trimestre Enero-Marzo 2016

search [] _ _ _ _ = Myb.Nothing
search (r : rs) s op p n =
    if p r then Myb.Just r
    else search rs' s' op p n
    where s' = r : s -- Visitados
          ms = (n r) \\ s'
          rs' = nub (op rs ms) -- cola o pila de los que voy a recorrer

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

-- Eliminando la recursión explícita del código anterior tenemos:

-------------------------------------------------------------------------------
-- Clase 14-04-16 (Data.Monoids, Data.Functor, Data.Applicative). PRUEBAS
-------------------------------------------------------------------------------
mybConcatF = Mnid.getFirst (Mnid.mconcat . map  Mnid.First $ [Myb.Nothing, Myb.Just 2, Myb.Just 1])
-- => Just 2
mybConcatL = Mnid.getLast (Mnid.mconcat . map  Mnid.Last $ [Myb.Nothing, Myb.Just 2, Myb.Just 1])
-- => Just 1
mybConcatN = Mnid.getLast (Mnid.mconcat . map  Mnid.Last $ [Myb.Nothing, Myb.Nothing, Myb.Nothing])
-- => Nothing

-- Implementar instancia de Either e.
data Ider e a = Lef e | Rai a

instance Functor (Ider a) where
    fmap _ (Lef e) = Lef e
    fmap f (Rai a) = Rai (f a)

-- Implementar instancia de Pair a.
data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

-- "The composition of two Functors is also a Functor."
-- Es cierto puesto que Si tenemos Functor f y Functor f' tenemos que
-- fmap :: (a -> b) -> f a -> f b y fmap :: (a -> b) -> f' a -> f' b
-- y sabemos que fmap debe cumplir con fmap (g . h) = (fmap g) . (fmap h)
-- por lo tanto el nuevo fmap seria de un functor dentro de otro.

-- APPLICATIVE
-- Explicación/Demostración u <*> (v <*> w) = pure(.) <*> u <*> v <*> w

-- Determinamos la firma de los argumentos involucrados en la expresión izq
-- <*> :: f (a -> b) -> f a -> f b podemos inferir que
-- u :: f (a -> b) y v <*> w :: f a usando el tipo de <*> podemos determinar que
-- v :: f (a -> c) y w :: f c. Por el otro lado de la igualdad tenemos que

-- Determinamos la firma de los argumentos involucrados en la expresión izq
-- pure (.) :: f ((a -> b) (b -> c) -> a -> c) por lo tanto,
-- u :: f (a -> b) y v :: f (b -> c) luego,
-- pure(.) <*> u <*> v :: f (a -> c) lo que implica que w :: f c.
-- Los tipos de ambas expresiones coinciden!.

-- esta propiedad de puede entender como la composición de funciones en donde
-- f(g x) = (f . g) x

-------------------------------------------------------------------------------
-- Clase 03-05-16 (Monads: Writer)
-------------------------------------------------------------------------------
runWriter $ (return 42 :: Writer [String] Int)
=> (42, [])

runWriter $ (return 42 >> tell ["dfsdf"])
=> ((), ["dfsdf"])

runState (get >>= put (+1)) 41
=> ((), 42)
