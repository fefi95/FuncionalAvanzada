\documentclass[11pt,fleqn]{article}

\usepackage{color}
\usepackage{tikz}
\usetikzlibrary{positioning,shapes}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}
\definecolor{lightorange}{rgb}{1,0.94,0.9}


\usepackage[spanish]{babel}
\usepackage{lmodern}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{listings}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}

\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey},
   literate={á}{{\'a}}1
            {é}{{\'e}}1
            {í}{{\'i}}1
            {ó}{{\'o}}1
            {ú}{{\'u}}1
            {ñ}{{\~n}}1
}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tareas 4}

\author{Stefani Castellanos\\
11-11394\\
\href{mailto:scct95@gmail.com}{<scct95@gmail.com>}}

\date{Junio 11, 2016}

\maketitle

\pagebreak

\begin{lstlisting}

> import System.Environment (getArgs)
> import System.Random (randomRIO)
> import Control.Concurrent --(putMVar, takeMVar,
>                           -- tryTakeMVar, readChan, writeChan,
>                           -- threadDelay)
> import Data.Sequence
> import System.IO.Error (catchIOError)

\end{lstlisting}

\section{El baño unisex (15 puntos)}

\noindent
Considere una empresa que provee un baño compartido por hombres y
mujeres. Suponga que el baño tiene un suministro infinito de
consumibles, agua, electricidad, fósforos y revistas -- obviamente
no es en Venezuela. Su problema es coordinar el ingreso de hombres,
mujeres y personal de limpieza al baño, de manera que se cumpla
el reglamento de la empresa:

\begin{itemize}
\item
  Si hay hombres en el baño, no pueden ingresar mujeres, y viceversa.
\item
  No puede haber más de tres personas en el baño simultáneamente.
\item
  Aquel que no pueda ingresar, debe hacer una fila para esperar su
  oportunidad de ingresar al baño. La fila es servida por orden de llegada.
\item
  El personal de limpieza agradece la colaboración de los usuarios y
  usuarias al permitirles el acceso para cumplir con el aseo
  del baño. Cuando el personal de limpieza llega, intenta ingresar
  al baño y si lo encuentra ocupado se ``colea'' ubicándose de
  primero en la fila. Cuando el personal de limpieza ingresa al
  baño, nadie más puede ingresar hasta que termine con lo que sea
  que esa gente hace en el baño.
\end{itemize}

Modelaremos el ``tráfico'' en el baño con hilos. Esto es, habrá un
flujo \emph{continuo} de hilos generados al azar -- algunos serán
hilos hombre, otros mujer y otros ``personal de limpieza''.
La distribución de los hilos es uniforme con 49\% de probabilidad
de que sea masculino, 49\% de probabilidad que sea femenino y 2\% de
probabilidad que sea ``personal de limpieza''.

Los hilos ingresan al baño respetando las reglas establecidas y se
toman cierto tiempo variable para desperdiciar el tiempo de la
compañía en sus procesos biológicos o de gestión del comfort higiénico.

Proponga una solución empleando sincronización clásica (MVar y Chan) y
otra solución empleando STM (TVar y TChan) para este particular
escenario de servicio sanitario. El programa debe permitir seleccionar
el tipo de simulación con un argumento de línea de comandos, y una vez
iniciada la corrida mostrar, permanentemente:

\begin{itemize}
\item
  La ocupación del baño -- indicando cantidad y género de los
  presentes.
\item
  La ocupación de la cola de espera -- indicando orden y género de los
  que aguardan para liberar sus demonios.
\item
  La presencia del personal de limpieza -- cuando estén prevenidos
  para ingresar y mientras torturan a los que aguardan.
\end{itemize}


\begin{lstlisting}

> data Genre = Women
>            | Men
>            | Cleaning
>            deriving (Show, Eq)
>
> newtype GenreInfo = G (MVar Bool, Genre)
>
> instance Show GenreInfo where
>   show (G (mvar, genre)) = show genre
>
> bathroomHandler :: MVar (Seq GenreInfo) -> Chan String -> MVar Bool
>                    -> MVar Bool -> IO ()
> bathroomHandler pqueC brC cS cfS = do
>   c <- tryTakeMVar cS
>   case c of
>       Nothing -> do (mv, g) <- nextInLine pqueC
>                     putMVar mv True
>                     putStrLn ("Hay 1 " ++ show g
>                               ++ " en el baño*")
>                     bathroom g 1 pqueC brC cS cfS
>       Just c' -> do putMVar cfS True
>                     putStrLn "Entra limpieza"
>                     waitFinishCleaning pqueC brC cS cfS
>                     bathroomHandler pqueC brC cS cfS
>
> bathroom :: Genre -> Int -> MVar (Seq GenreInfo) -> Chan String
>             -> MVar Bool -> MVar Bool -> IO ()
> bathroom genre 3 pqueC brC cS cfS = do
>   (mv, g) <- nextInLine pqueC
>   if g == genre
>   then do br <- readChan brC
>           putStrLn ("Hay 2 " ++ show g
>                     ++ " en el baño!")
>           putMVar mv True
>           putStrLn ("Hay 3 " ++ show g
>                     ++ " en el baño")
>           bathroom g 3 pqueC brC cS cfS
>   else do waitTillEveryoneLeaves genre brC 3
>           putMVar mv True
>           putStrLn ("Hay 1 "
>                     ++ show g ++ " en el baño***")
>           bathroom g 1 pqueC brC cS cfS
>
> bathroom genre n pqueC brC cS cfS = do
>   c <- tryTakeMVar cS
>   case c of
>       Nothing -> do (mv, g) <- nextInLine pqueC
>                     if g == genre
>                     then do putMVar mv True
>                             putStrLn ("Hay " ++ show (n+1) ++ " "
>                                      ++ show g ++ " en el baño")
>                             bathroom g (n+1) pqueC brC cS cfS
>                     else do waitTillEveryoneLeaves genre brC n
>                             putMVar mv True
>                             putStrLn ("Hay 1 "
>                                      ++ show g ++ " en el baño**")
>                             bathroom g 1 pqueC brC cS cfS
>       Just c' -> do waitTillEveryoneLeaves genre brC n
>                     putStrLn "Entra limpieza"
>                     putMVar cfS True
>                     waitFinishCleaning pqueC brC cS cfS
>                     bathroomHandler pqueC brC cS cfS
>
> takeSeq :: Seq GenreInfo ->
>            IO (Seq GenreInfo, Maybe (GenreInfo, Seq GenreInfo))
> takeSeq ss = do
>   case viewl ss of
>        EmptyL  -> return (ss, Nothing)
>        x :< xs -> do putStrLn ("La cola de espera es:"
>                               ++ show ss)
>                      return (xs, Just (x, xs))
>
> nextInLine pqueC = do
>   m <- modifyMVar pqueC takeSeq
>   case m of
>       Nothing -> nextInLine pqueC
>       Just (G (mv, g), rest) -> do return (mv, g)
>
> waitTillEveryoneLeaves :: Genre -> Chan String -> Int -> IO ()
> waitTillEveryoneLeaves g brC 0 = return ()
> waitTillEveryoneLeaves g brC n = do
>   br <- readChan brC
>   putStrLn ("Hay " ++ show (n-1) ++ " "
>             ++ show g ++ " en el baño!!")
>   waitTillEveryoneLeaves g brC (n-1)
>
> waitFinishCleaning pqueC brC cS cfS = do
>   cf <- takeMVar cfS
>   case cf of
>       True -> do putMVar cfS True
>                  waitFinishCleaning pqueC brC cS cfS
>       False -> bathroomHandler pqueC brC cS cfS
>
> cleaningThread cS cfS = do
>   putStrLn "Limpieza quiere entrar.."
>   putMVar cS True
>   cf <- takeMVar cfS
>   clean
>   putMVar cfS False
>
> genreThread genre pqueC brC = do
>   mv <- newEmptyMVar :: IO (MVar Bool)
>   modifyMVar_ pqueC (\ss -> return (ss |> G (mv, genre)))
>   mv' <- takeMVar mv
>   useBathroom
>   writeChan brC "done"
>
> clean = do r <- randomRIO (100000, 500000)
>            threadDelay r
>
> useBathroom = do r <- randomRIO (100000, 500000)
>                  threadDelay r
>
> peopleInLine pqueC brC cS cfS = do
>   r <- randomRIO (0, 1.0) :: IO Double
>   let g = people r
>   if g == Cleaning
>   then forkIO (cleaningThread cS cfS)
>   else forkIO (genreThread g pqueC brC)
>   useBathroom
>   peopleInLine pqueC brC cS cfS
>
> people r
>   |r > 0.0 && r < 0.49  = Women
>   |r > 0.49 && r < 0.98 = Men
>   |r > 0.98 && r < 1.0  = Cleaning
>   |otherwise = Cleaning
>
> main = do
>   pqueC <- newMVar $ empty
>   brC <- newChan
>   cS <- newEmptyMVar :: IO (MVar Bool)
>   cfS <- newEmptyMVar :: IO (MVar Bool)
>   forkIO (peopleInLine pqueC brC cS cfS)
>   bathroomHandler pqueC brC cS cfS

\end{lstlisting}

\end{document}
