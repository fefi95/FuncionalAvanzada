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

\author{Ernesto Hernández-Novich\\
86-17791\\
\href{mailto:emhn@usb.ve}{<emhn@usb.ve>}}

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

  \noindent
  \colorbox{lightorange}{
  \parbox{\linewidth}{

  }
  }
  \\

  \begin{lstlisting}
> droppingsR :: Int -> Int -> Int
> droppingsR n k = undefined
  \end{lstlisting}
\item
  Una solución utilizando técnicas de programación dinámica
  apoyadas en arreglos Haskell -- esta será la solución
  eficiente.
  \begin{lstlisting}
> droppingsD :: Int -> Int -> Int
> droppingsD n k = undefined
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

\end{document}
