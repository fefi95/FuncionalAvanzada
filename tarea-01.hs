import Data.List
import Data.Functor
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Tree
-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Cairo

data Sample a = Sample { x :: [a], y :: a }
    deriving (Show)

data Hypothesis a = Hypothesis { c :: [a] }
    deriving (Show)

alpha :: Double
alpha = 0.03

epsilon :: Double
epsilon = 0.0000001

guess :: Hypothesis Double
guess = Hypothesis { c = [0.0, 0.0, 0.0] }

training :: [Sample Double]
training = [
  Sample { x = [  0.1300098690745405, -0.2236751871685913 ], y = 399900 },
  Sample { x = [ -0.5041898382231769, -0.2236751871685913 ], y = 329900 },
  Sample { x = [  0.502476363836692, -0.2236751871685913 ], y = 369000 },
  Sample { x = [ -0.7357230646969468, -1.537766911784067 ], y = 232000 },
  Sample { x = [  1.257476015381594, 1.090416537446884 ], y = 539900 },
  Sample { x = [ -0.01973172848186497, 1.090416537446884 ], y = 299900 },
  Sample { x = [ -0.5872397998931161, -0.2236751871685913 ], y = 314900 },
  Sample { x = [ -0.7218814044186236, -0.2236751871685913 ], y = 198999 },
  Sample { x = [ -0.7810230437896409, -0.2236751871685913 ], y = 212000 },
  Sample { x = [ -0.6375731099961096, -0.2236751871685913 ], y = 242500 },
  Sample { x = [ -0.07635670234773261, 1.090416537446884 ], y = 239999 },
  Sample { x = [ -0.0008567371932424295, -0.2236751871685913 ], y = 347000 },
  Sample { x = [ -0.1392733399764744, -0.2236751871685913 ], y = 329999 },
  Sample { x = [ -0.9219563120780225, -0.2236751871685913 ], y = 259900 },
  Sample { x = [  0.3766430885792084,  1.090416537446884 ], y = 449900 },
  Sample { x = [ -0.856523008944131,  -1.537766911784067 ], y = 299900 },
  Sample { x = [ -0.9622229601604173, -0.2236751871685913 ], y = 199900 },
  Sample { x = [  0.7654679091248329,  1.090416537446884 ], y = 499998 },
  Sample { x = [  1.296484330711414,   1.090416537446884 ], y = 599000 },
  Sample { x = [ -0.2940482685431793, -0.2236751871685913 ], y = 252900 },
  Sample { x = [  3.117291823687202,   2.40450826206236 ], y = 699900 },
  Sample { x = [ -0.1417900054816241, -1.537766911784067 ], y = 255000 },
  Sample { x = [ -0.4991565072128776, -0.2236751871685913 ], y = 242900 },
  Sample { x = [ -0.04867338179108621, 1.090416537446884 ], y = 259900 },
  Sample { x = [  2.377392165173198,  -0.2236751871685913 ], y = 573900 },
  Sample { x = [ -1.133356214510595,  -0.2236751871685913 ], y = 249900 },
  Sample { x = [ -0.6828730890888036, -0.2236751871685913 ], y = 464500 },
  Sample { x = [  0.6610262906611214, -0.2236751871685913 ], y = 469000 },
  Sample { x = [  0.2508098133217248, -0.2236751871685913 ], y = 475000 },
  Sample { x = [  0.8007012261969283, -0.2236751871685913 ], y = 299900 },
  Sample { x = [ -0.2034483103577911, -1.537766911784067 ], y = 349900 },
  Sample { x = [ -1.259189489768079,  -2.851858636399542 ], y = 169900 },
  Sample { x = [  0.04947657290975102, 1.090416537446884 ], y = 314900 },
  Sample { x = [  1.429867602484346,  -0.2236751871685913 ], y = 579900 },
  Sample { x = [ -0.2386816274298865,  1.090416537446884 ], y = 285900 },
  Sample { x = [ -0.7092980768928753, -0.2236751871685913 ], y = 249900 },
  Sample { x = [ -0.9584479619026928, -0.2236751871685913 ], y = 229900 },
  Sample { x = [  0.1652431861466359,  1.090416537446884 ], y = 345000 },
  Sample { x = [  2.78635030976002,    1.090416537446884 ], y = 549000 },
  Sample { x = [  0.202993168723881,   1.090416537446884 ], y = 287000 },
  Sample { x = [ -0.4236565420583874, -1.537766911784067 ], y = 368500 },
  Sample { x = [  0.2986264579195686, -0.2236751871685913 ], y = 329900 },
  Sample { x = [  0.7126179335166897,  1.090416537446884 ], y = 314000 },
  Sample { x = [ -1.007522939253111,  -0.2236751871685913 ], y = 299000 },
  Sample { x = [ -1.445422737149154,  -1.537766911784067 ], y = 179900 },
  Sample { x = [ -0.1870899845743182,  1.090416537446884 ], y = 299900 },
  Sample { x = [ -1.003747940995387,  -0.2236751871685913 ], y = 239500 } ]

-- Funcion que compara dos valores
veryClose :: Double -> Double -> Bool
veryClose v0 v1 = (abs (v0 - v1)) < epsilon


addOnes :: [Sample Double] -> [Sample Double]
addOnes = map (\s -> Sample { x = 1.0 : (x s), y = y s })

theta :: Hypothesis Double -> Sample Double -> Double
theta h s = sum (zipWith (*) (c h) (x s))

cost :: Hypothesis Double -> [Sample Double] -> Double
cost h ss = su / 2 * n
            where auxSum (n, su) s = (n + 1, ((theta h s) - (y s)) ** 2 + su)
                  (n, su) = foldl' auxSum (0, 0) ss

descend :: Double -> Hypothesis Double -> [Sample Double]
            -> Hypothesis Double
descend alpha h ss = undefined

gd :: Double -> Hypothesis Double -> [Sample Double]
    -> [(Integer, Hypothesis Double, Double)]
-- gd alpha h ss = undefined
gd alpha h ss = unfoldr newH (h, 0)
                where newH (h1, i) = if veryClose (cost h1 ss') (cost h2 ss')
                                     then Nothing
                                     else Just ((i, h1, cost h1 ss'),(h2, i + 1))
                                     where h2 = descend alpha h1 ss'
                      ss' = addOnes ss

-- Monoid

newtype Max a = Max { getMax :: a }
    deriving (Show)

instance (Ord a, Monoid a) => Monoid (Max a) where
    mempty = Max mempty -- Corregir!!
    mappend (Max a) (Max b)
        |a > b = (Max a)
        |otherwise = (Max b)

-- Zippers
data Filesystem a = File a | Directory a [Filesystem a]

data FsCrumb a = Crumb a [Filesystem a] [Filesystem a]

data Breadcrumbs a = C [FsCrumb a]

type Zipper a = (Filesystem a, Breadcrumbs a)

-- goDown :: Zipper a -> Maybe (Zipper a)
goRight :: Zipper a -> Maybe (Zipper a)
goRight (f, C ((Crumb b (fs : fss) hs) : zs)) = Just (fs, C (Crumb b fss (f : hs)): zs)

goLeft :: Zipper a -> Maybe (Zipper a)

goBack :: Zipper a -> Maybe (Zipper a)
goBack (f, C ((Crumb a fss hs) : zs)) = Just (Directory a (fss ++ [f] ++ hs), C zs)
goBack _ = Nothing

-- tothetop ::
-- modify   ::

focus :: Filesystem a -> Zipper a
focus fs = (fs, C [])

defocus  :: Zipper a -> Filesystem a
defocus (fs, z) = fs
