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

\pagebreak
  \begin{lstlisting}

> {-# LANGUAGE BangPatterns #-}
>
> import Data.Array.ST (newArray, readArray, writeArray,
>                       runSTArray)
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
   de los "peores casos" para el $n$ y el $k$ proporcionados. En general,
   se lanza el globo desde un piso, si explota se tiene un globo menos
   y un piso menos que revisar. Si no explota, se tiene la misma cantidad
   de globos revisando los restantes $k - x$, para $x$ entre 1 y k. En
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
>   |k > 1 = 1 + foldr (aux n k) k [1..k]
>   |otherwise = k
> aux n k !x !m = min m (max (droppingsR (n - 1) (x - 1)) (droppingsR n (k - x)))

  \end{lstlisting}
\item
  Una solución utilizando técnicas de programación dinámica
  apoyadas en arreglos Haskell -- esta será la solución
  eficiente.

  \noindent
  \colorbox{lightorange}{
  \parbox{\linewidth}{
  En el caso recursivo se utilizaron soluciones más "pequeñas"
  para dar con el resultado, para la solución usando
  programación dinámica realizaremos el mismo truco, pero
  que no será necesario recalcular casos más pequeños
  puesto que cada solución a un problema se guardará
  en la posición $(i, j)$ de la matriz, la $i$
  representan la cantidad de globos para esa solución y la $j$
  la cantidad de pisos. Luego el resultado final se encuentra
  en la posición $(n, k)$ del arreglo. Se cambiará tiempo por
  espacio. El código es directo :
  }
  }
  \\

  \begin{lstlisting}

> droppingsD :: Int -> Int -> Int
> droppingsD n k = (auxD n k) ! (n, k)
>
> auxD n k = runSTArray $ do
>   a <- newArray ((0, 0),(n, k)) k
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

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
\textbf{Extra:} La solución anterior funciona bien,
sin embargo se puede notar que una vez calculada una
posición $(i, j)$ de la matriz, no hay manera que cálculos
subsecuentes afecten el resultado que se encuentra en dicha
posición. Por lo tanto la solución se puede hallar con una
matriz inmutable que dependa de los cálculos anteriores para
calcular la posición indicada. De nuevo el resultado se
encontrará en la posición $(n, k)$.\\

La función f es la que realmente realiza los cálculos,
se definen los casos bases y finalmente se obtienen las
posiciones a partir de sub-problemas.
}
}
\\

\begin{lstlisting}

> droppingsDI :: Int -> Int -> Int
> droppingsDI n k = (auxDI n k) ! (n, k)
>
> auxDI n k = table
>        where table = array ((0,0), (n, k)) [(i, f i) | i <- range ((0,0), (n, k))]
>              f (i, 0) = 0
>              f (i, 1) = 1
>              f (0, j) = 0
>              f (1, j) = j
>              f (i, j) = 1 + foldr (aux i j) j [1..j]
>              aux i j x m = min m (max (table ! (i - 1, x - 1)) (table ! (i, j - x)))

\end{lstlisting}

\begin{lstlisting}

> main = --defaultMain [
>        --             bench "Recursive" $ nf (droppingsR 2) 21,
>        --             bench "Dynamic M." $ nf (droppingsD 2) 21
>        --             bench "Dynamic I." $ nf (droppingsDI 2) 21,
>        --            ]
>        forM_ [1..30] $ \i -> do putStr $ "Recursive " ++ show i ++ " "
>                                 print $  droppingsR 3 i
>                                 putStr $ "Dynamic " ++ show i ++ " "
>                                 print $  droppingsD 3 i
>                                 putStr $ "Dynamic I " ++ show i ++ " "
>                                 print $  droppingsDI 3 i

\end{lstlisting}

\end{document}
