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

> import Control.Parallel.Strategies (using, parList, rseq, r0, parListChunk, parTuple2, rpar, rparWith)
> import Control.Parallel (pseq, par)
> import System.Environment (getArgs)
> import qualified Control.Monad.Par as P

\end{lstlisting}

\section{Ordenamiento en Paralelo (10 puntos)}

\noindent
Considere el algoritmo de ordenamiento \emph{Mergesort}, cuya
implantación tradicional en Haskell fue discutida en clase.

\noindent
Proponga una implantación usando el monad \texttt{Par} con la
técnica de \emph{dataflow parallelism}, y otra implantación usando
estrategias de paralelismo. En ambos casos, asegúrese de encontrar
una solución con un balance de trabajo razonablemente equilibrado
para dos (2) núcleos de procesamiento.

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Para tener una base de comparación se agrega el merge sort secuencial
explicado en clase. De esta manera se puede analizar cual ha sido la mejora.

}
}
\\

\begin{lstlisting}

> msort []  = []
> msort [x] = [x]
> msort xs  = merge (msort miti) (msort mita)
>             where (miti, mita) = halve xs
>
> merge xs [] = xs
> merge [] ys = ys
> merge xs@(x:t) ys@(y:u)
>       | x <= y = x : merge t ys
>       | otherwise = y : merge xs u
>
> halve xs = (ping xs, pong xs)
>
> ping []       = []
> ping [x]      = [x]
> ping (x:_:xs) = x : ping xs
>
> pong []       = []
> pong [x]      = []
> pong (_:x:xs) = x : pong xs
>

\end{lstlisting}

\begin{lstlisting}

> msortS 0 []  = []
> msortS 0 [x] = [x]
> msortS 0 xs = merge (msortS 0 miti) (msortS 0 mita)
>              where (miti, mita) = halveS xs
> msortS n xs = mitiSort `par` mitaSort `pseq` merge mitiSort mitaSort
>              where (miti, mita) = halveS xs
>                    mitiSort = (msortS (n-1) miti)
>                    mitaSort = (msortS (n-1) mita)
>
> mergeS xs [] = xs
> mergeS [] ys = ys
> mergeS xs@(x:t) ys@(y:u)
>       | x <= y = x : mergeS t ys
>       | otherwise = y : mergeS xs u
>
> halveS = go ([], [])
>    where go (eac, oac) []       = (eac, oac)
>          go (eac, oac) [x]      = (x:eac, oac)
>          go (eac, oac) (x:y:xs) = go (x:eac ,y:oac) xs

\end{lstlisting}

\begin{lstlisting}

> msortP []  = []
> msortP [x] = [x]
> msortP xs  = merge (msortP miti) (msortP mita)
>              where (miti, mita) = halveP xs
>   --runPar $ do
>   --[miti, mita, sortMiti, sortMita] <- sequence $ replicate 2 P.new
>   --P.fork $ do P.put miti miti'
>   --            P.put mita mita'
>   --            where (miti', mita') = halveP xs
>   --P.fork $ P.put sortMiti (msortP miti)
>   --P.fork $ P.put sortMita (msortP mita)
>   --get $ merge sortMiti sortMita
>
> mergeP xs [] = xs
> mergeP [] ys = ys
> mergeP xs@(x:t) ys@(y:u)
>       | x <= y = x : mergeP t ys
>       | otherwise = y : mergeP xs u
>
> halveP xs = (pingP xs, pongP xs)
>
> pingP []       = []
> pingP [x]      = [x]
> pingP (x:_:xs) = x : ping xs
>
> pongP []       = []
> pongP [x]      = []
> pongP (_:x:xs) = x : pong xs

\end{lstlisting}

\begin{lstlisting}

> main = do
>   (x:_) <- getArgs
>   case x of
>       "Sec" -> print $ msort [1..100000]
>       "Str" -> print $ msortS 8 [1..100000]
>       "Par" -> print $ msortP [1..100000]
>       _     -> putStrLn "Not a valid option, ja ja!"

\end{lstlisting}

\end{document}
