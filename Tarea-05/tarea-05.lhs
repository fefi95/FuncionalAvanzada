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

\usepackage{mathrsfs}
\usepackage{amsmath}

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

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 5}

\author{Stefani Castellanos\\
11-11394\\
\href{mailto:scct95@gmail.com}{<scct95@gmail.com>}}

\date{Junio 28, 2016}

\maketitle

\pagebreak

\section*{Un problema de programación dinámica\ldots}

\noindent
Usted cuenta con $n$ globos de agua \textbf{idénticos} en todo
aspecto, y se encuentra en un edificio de $k$ pisos, con
la misma altura entre piso y piso.
\\

\noindent
Por razones que no vienen al caso, y quizás porque Ud. es
computista, le causa particular interés saber desde cuáles
pisos Ud. puede dejar caer el globo de agua sin que
explote al aterrizar. Para contestar esa pregunta, Ud.
puede suponer:

\begin{itemize}
\item
  Si un globo sobrevive a la caída, puede usarlo de nuevo.
\item
  Si un globo sobrevive a la caída desde el piso $p$, entonces
  sobreviviría a la caída desde cualquier piso por debajo
  de $p$.
\item
  Si un globo explota, no puede usarse de nuevo.
\item
  Si un globo explota al caer desde el piso $p$, entonces
  explotaría al caer desde cualquier piso por encima
  de $p$.
\end{itemize}

\noindent
Suponga que $n = 1$. Independientemente del $k$, la única forma
de encontrar la respuesta es dejar caer el globo desde el primer
piso. Si explota, tenemos la respuesta; si sobrevive, repetimos
desde el segundo piso. En el peor caso, hay que probar los $k$
pisos para saber si sobrevive o explota.
\\

\noindent
Pero si $n > 1$, Ud. tiene más alternativas. Sólo como ejemplo,
Ud. podría lanzar el globo desde el piso $k \div 2$. Si explota,
usar el globo que queda para ir desde el primer piso, pero nunca
pasaría de $k \div 2$; si no explota, puede volver a usarlo
desde el piso $k \div 2 + (k \div 2) \div 2$. Mientras más
globos tenga, más intentos «audaces» puede hacer.
\\

\noindent
Entonces, la pregunta final resulta: ¿cuál es la cantidad
\textbf{mínima} de globos que deben lanzarse para determinar
cuál es el piso «crítico» a partir del cuál se rompen al caer?
\\

\noindent
Ud. debe implantar en Haskell dos soluciones a este problema:

\begin{itemize}
\item
  Una solución utilizando recursión directa y sincera -- esta
  es la solución «de control».

  \begin{lstlisting}

> {-# LANGUAGE BangPatterns #-}
>
> import Data.Array.ST (newArray, readArray, writeArray, runSTArray)
> import Data.Array
> import Control.Monad (forM_)
> import Criterion
> import Criterion.Main

  \end{lstlisting}
  
  \noindent
  \colorbox{lightorange}{
  \parbox{\linewidth}{
   Una solución inocente a este problema es escribir la recursión
   directa; utilizar soluciones anteriores para encontrar el menor
   de los "peores casos" para el n y el k proporcionados. En general,
   se lanza el globo desde un piso, si explota se tiene un globo menos
   y un piso menos que revisar. Si no explota, se tiene la misma cantidad
   de globos revisando los restantes k - x, para x entre 1 y k. En
   cualquier caso se agrega un lanzamiento.\\

   Note que esta solución es altamente ineficiente puesto que se
   comporta como fibonacci recursivo inocente, es decir, para
   cada entrada se requieren recalcular varias veces problemas
   más pequeños.

  }
  }
  \\

  \begin{lstlisting}

> droppingsR :: Int -> Int -> Int
> droppingsR 1 !k = k
> droppingsR !n !k
>   |k > 0 = 1 + minimum [max (droppingsR (n - 1) (x - 1)) (droppingsR n (k - x)) | x <- [1 .. k]]
>   |otherwise = 0
>
> droppingsA 1 !k = k
> droppingsA !n !k
>   |k > 1 = 1 + foldr (aux n k) k [1..k]
>   |otherwise = k
> aux n k !x !m = min m (max (droppingsA (n - 1) (x - 1)) (droppingsA n (k - x)))
>
> --droppingsR 1 !k |k > 0 = k
> --droppingsR !n !k
> --      |k == 0    = 1
> --      |k == 1    = 1
> --      |odd k     = (min (droppingsR (n - 1) kd2) (droppingsR n (kd2 + 1))) + 1
> --      |otherwise = (min (droppingsR (n - 1) kd2) (droppingsR n kd2)) + 1
> --                  where kd2 = k `div` 2

  \end{lstlisting}
\item
  Una solución utilizando técnicas de programación dinámica
  apoyadas en arreglos Haskell -- esta será la solución
  eficiente.

  \begin{lstlisting}

> droppingsD :: Int -> Int -> Int
> droppingsD n k = (auxD n k) ! (n, k)
>
> auxD n k = runSTArray $ do
>   a <- newArray ((0, 0),(n, k)) 5000
>   forM_ [0..k] $ \j -> do writeArray a (0,j) 0
>                           writeArray a (1,j) j
>   forM_ [0..n] $ \i -> do writeArray a (i,0) 0
>                           writeArray a (i,1) 1
>   forM_ [2..n] $ \i -> do
>           forM_ [2..k] $ \j -> do
>               forM_ [1..j] $ \x -> do
>                   aij <- readArray a (i,j)
>                   anx <- readArray a (i - 1, x - 1)
>                   akx <- readArray a (i , j - x)
>                   writeArray a (i,j) (min aij (1 + (max anx akx)))
>   return a

  \end{lstlisting}

\end{itemize}

\noindent
La solución para este algoritmo es directa y emplea técnicas de
programación dinámica sobre arreglos \emph{mutables}. Ud. puede
presentar una solución utilizando arreglos mutables sobre el
monad \texttt{ST}, pero sepa que recibirá cinco (5) puntos extra,
si presenta una solución con arreglos \emph{inmutables} llenos
de \emph{thunks} escritos de manera astuta.
\\

\begin{lstlisting}

> main = --defaultMain [
>        --             bench "Recursive" $ nf (droppingsR 2) 20,
>        --             bench "Dynamic" $ nf (droppingsD 2) 20
>        --            ]
>        forM_ [1..30] $ \i -> do putStr $ "Recursive " ++ show i ++ " "
>                                 print $  droppingsA 2 i
>                                 putStr $ "Dynamic " ++ show i ++ " "
>                                 print $  droppingsD 2 i

\end{lstlisting}

\end{document}
