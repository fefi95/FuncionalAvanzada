import Data.List
import Data.Functor
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Tree
import Data.Maybe (fromJust)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

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
cost h ss = su / (2 * n)
            where auxSum (n, su) s = (n + 1, ((theta h s) - (y s)) ** 2 + su)
                  (n, su) = foldl' auxSum (0, 0) ss

descend :: Double -> Hypothesis Double -> [Sample Double]
            -> Hypothesis Double
descend alpha h ss = Hypothesis $ (reverse . fst) (foldl' iterJ ([], 0) (c h))
                     where iterJ (th', j) th = (th - (alpha/m)* s : th', j + 1)
                                where (s, m) = iterI j ss
                           iterI j = foldl' sumI (0, 0)
                                where sumI (su, l) s = (su + ((theta h s) - (y s)) * ((x s)!! j) , l + 1 )

gd :: Double -> Hypothesis Double -> [Sample Double]
    -> [(Integer, Hypothesis Double, Double)]
gd alpha h ss = unfoldr newH (h, 0)
                where newH (h1, i) = if veryClose (cost h1 ss') (cost h2 ss')
                                     then Nothing
                                     else Just ((i, h1, cost h1 ss'),(h2, i + 1))
                                     where h2 = descend alpha h1 ss'
                      ss' = addOnes ss

-- Monoid

newtype Max a = Max { getMax :: Maybe a }
    deriving (Show)

instance Ord a => Monoid (Max a) where
    mempty = Max Nothing
    mappend (Max (Just a)) (Max (Just b))
        |a > b = Max (Just a)
        |otherwise = Max (Just b)
    mappend (Max (Just a)) mempty = (Max (Just a))
    mappend mempty (Max (Just b)) = (Max (Just b))

-- Zippers
data Filesystem a = File a | Directory a [Filesystem a] deriving (Show)

data FsCrumb a = Crumb a [Filesystem a] [Filesystem a] deriving (Show)
data Breadcrumbs a = C [FsCrumb a] deriving (Show)

type Zipper a = (Filesystem a, Breadcrumbs a)

goDown :: Zipper a -> Maybe (Zipper a)
goDown (Directory a (f : hs), C bs) = Just (f, C (Crumb a [] hs : bs))
goDown _                            = Nothing

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (f, C ((Crumb a (fs : fss) hs) : bs)) = Just (fs, C (Crumb a fss (f : hs) : bs))
goLeft _                                     = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (f, C ((Crumb a fss (fs : hs)) : bs)) = Just (fs, C (Crumb a (f : fss) hs : bs))
goRight _                                     = Nothing

goBack :: Zipper a -> Maybe (Zipper a)
goBack (f, C ((Crumb a fss hs) : bs)) = Just (Directory a ((reverse fss) ++ [f] ++ hs), C bs)
goBack _                              = Nothing

-- goBack (f, C ((Crumb a [] hs) : bs)) = Just (Directory a ([f] ++ hs), C bs)
-- goBack z @ (f, C ((Crumb a fss hs) : bs)) = (goBack . fromJust) $ goLeft z
-- goBack _                              = Nothing


tothetop :: Zipper a -> Zipper a
tothetop (f, C []) = (f, C [])
tothetop z = tothetop . fromJust $ goBack z

modify :: (a -> a) -> Zipper a -> Zipper a
modify g (Directory a fs, bs) = (Directory (g a) fs, bs)
modify g (File a, bs)         = (File (g a), bs)

focus :: Filesystem a -> Zipper a
focus fs = (fs, C [])

defocus  :: Zipper a -> Filesystem a
defocus (fs, _) = fs

myDisk :: Filesystem String
myDisk =
    Directory "root"
        [ File "goat_yelling_like_man.wmv"
        , File "pope_time.avi"
        , Directory "pics"
            [ File "watermelon_smash.gif"
            , File "skull_man(scary).bmp"
            ]
        , File "dijon_poupon.doc"
        , Directory "programs"
            [ File "fartwizard.exe"
            , Directory "source code"
                [ File "best_hs_prog.hs"
                , File "random.hs"
                ]
            ]
        ]
