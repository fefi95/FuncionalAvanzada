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

\end{lstlisting}

\end{document}
