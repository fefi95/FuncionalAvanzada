\documentclass[11pt,fleqn]{article}

\usepackage{tikz}
\usepackage{multicol}
\usepackage{latexsym}
\usepackage{array}
\usepackage[english,spanish]{babel}
\usepackage{lmodern}
\usepackage{listings}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}
\usepackage{xcolor}

\usepackage{algorithmic}
\usepackage{algorithm}

\usetikzlibrary{positioning,shapes,folding,positioning,shapes,trees}

\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=blue
}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}
\definecolor{lightorange}{rgb}{1,0.9,0.8}



\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey}
}

\long\def\ignore#1{}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 2}

\author{Stefani Castellanos\\
11-11394\\
\href{mailto:scct95@gmail.com}{<scct95@gmail.com>}}

\date{Mayo 12, 2016}

\maketitle

\pagebreak

\section*{Yo dawg! I heard you liked functors\ldots}

\begin{lstlisting}

> import Control.Applicative

\end{lstlisting}

\noindent
Considere el siguiente tipo de datos, en el cual \texttt{f}
puede ser \emph{cualquier} \texttt{Functor}.

\begin{lstlisting}

> data Bonus f a = Malus a
>                | Bonus (f (Bonus f a))

\end{lstlisting}

\noindent
Escriba las instancias \texttt{Functor}, \texttt{Applicative}
y \texttt{Monad} para el tipo \texttt{Functor~f}.

\begin{lstlisting}

> instance Functor f => Functor (Bonus f) where
>    fmap g (Malus a)  = Malus (g a)
>    fmap g (Bonus fa) = Bonus $ fmap (fmap g) fa

\end{lstlisting}


\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
\textbf{Explicación:} para escribir la instancia de Functor
es necesario proporcionar una implementación para fmap.\\

Primero notemos que la firma de fmap para nuestro tipo de
dato es la siguiente: fmap :: (a -> b) -> Bonus f a -> Bonus f b
por lo tanto tenemos 2 casos, uno para el constructo Malus y
otro para el constructor bonus.\\

Otro elemento a considerar es que de la firma de la instancia de
functor Functor f => Functor (Bonus f) podemos deducir que f es
un functor, es decir, tiene fmap.\\

El primer caso (Malus) es bastante directo, se aplica la función g
sobre el elemento en a. El segundo caso es más complicado,sabemos
que dentro de Bonus hay un functor que guarda Bonus f a y esos
también deben ser modificados por fmap. El fmap más externo aplica
la función (fmap g) a los elementos que guarda que son del tipo Bonus f a
que tambíen tiene fmap, a estos le aplicamos g.
}
}
\\

\begin{lstlisting}

> instance Functor f => Applicative (Bonus f) where
>     pure = Malus
>
>     (Malus g) <*> b  = fmap g b
>     (Bonus fg) <*> b = Bonus $ fmap (flip (<*>) b) fg

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
\textbf{Explicación:} Compila!!!! REVISAR!!
}
}
\\

\begin{lstlisting}

> instance Functor f => Monad (Bonus f) where
>    return = pure
>


\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
\textbf{Explicación:}
}
}
\\

\section*{Práctica obligada}

\noindent
Reescriba la librería \texttt{MiniLogo} construyendo un
monad combinado que aproveche \texttt{RWS}, \texttt{Exception}
(estilo nuevo -- \textbf{no} use \texttt{Error}) e \texttt{IO}.
Al reescribirla, incorpore las siguientes restricciones:

\begin{itemize}
\item
  La tabla de colores debe estar en el ambiente «sólo lectura».
\item
  El diagrama en curso debe estar en el ambiente «acumulador».
\item
  Es un error fatal usar el color rojo después del verde (o
  viceversa).
\item
  Es un error fatal ir hacia la izquierda dibujando con rojo.
  Note que «izquierda» es cualquier dirección con tendencia
  a la izquierda (más de 90 y menos de 270 grados), siendo
  necesario saber cuán «izquierdosa» fue la tortuga. No se
  complique: limpie la ventana y redibuje.
\item
  Una vez presentado el dibujo, el programa espera por una
  tecla. Si se oprime la 'a', debe «borrarse» el último
  paso del dibujo; si se oprime la 's' debe «repetirse» el
  siguiente paso del dibujo; y si se oprime cualquier otra
  tecla debe terminar el programa.
\item
  Si ocurrió un error, no interesa el estado de la simulación.
\end{itemize}

\section*{Can I has pizza?}

\noindent
Tengo más hambre que el país. Tengo tanta hambre que lloro.
Haría lo que fuera por pizza. Mentira: nunca programaría en Python,
Java, ni (¡asco!) PHP. Tampoco en JavaScript. Pero nos desviamos del
tema y es que \textbf{necesito} pizza\ldots

\begin{lstlisting}

> data Want a = Want ((a -> Pizza) -> Pizza)

\end{lstlisting}

\noindent
Una cosa es comer la pizza, tanto mejor si está en un combo 2x1,
pero de todas todas, al final uno queda feliz.

\begin{lstlisting}

> data Pizza = Eat (IO Pizza)
>            | Combo Pizza Pizza
>            | Happy

\end{lstlisting}

\noindent
Y la pizza es un espectáculo digno de ver, y aunque el proceso de
comerla (\texttt{IO}) oculte lo que tiene, es posible hacerse
una imagen aproximada -- sobre todo para hacer «pizza debugging».

\begin{lstlisting}

> instance Show Pizza where
>    show (Eat x)     = " :-P "
>    show (Combo x y) = " combo(" ++ show x
>                                 ++ ","
>                                 ++ show y ++ ") "
>    show Happy       = " :-D "

\end{lstlisting}

\noindent
Como dije, quiero pizza, y haría cualquier cosa por obtenerla
hasta ser feliz.
\begin{lstlisting}

> want :: Want a -> Pizza
> want = undefined

\end{lstlisting}

\noindent
Eso no quiere decir que sin pizza no se pueda ser feliz -- aunque
suene difícil o inconcebible, a veces uno simplemente es feliz.

\begin{lstlisting}

> happy :: Want a
> happy = undefined

\end{lstlisting}

\noindent
Imagínate la pizza. Es tuya. La devoras. Y con cada bocado es
inevitable querer más\ldots

\begin{lstlisting}

> nomnom :: IO a -> Want a
> nomnom = undefined

\end{lstlisting}

\noindent
Y mientras masticas la pizza piensas que hoy es jueves.
¡Jueves de 2x1! Hay que aprovechar el combo, hasta que no
quede nada.

\begin{lstlisting}

> combo :: Want a -> Want ()
> combo = undefined

\end{lstlisting}

\noindent
«Es demasiada pizza» -- obviamente una falacia: nunca es
demasiada pizza, porque \emph{siempre} hay un pana con el
cual compartir las ganas de pizza (sobre todo si pagan
por partes iguales).

\begin{lstlisting}

> pana :: Want a -> Want a -> Want a
> pana = undefined

\end{lstlisting}

\noindent
Es momento de ir a por ella. Toda la que haya. Toda la que se
pueda. Suficiente para todos. Allí se pide el combo. Todos comen.
Y cada uno es feliz.

\begin{lstlisting}

> pizzeria :: [Pizza] -> IO ()
> pizzeria = undefined

\end{lstlisting}

\noindent
Porque las ganas son sólo el contexto. Porque cada topping que
se agrega no hace sino anticipar la secuencia de bocados, que
uno, sólo o acompañado, quiere satisfacer. La pizza no será un
\texttt{Monad}, pero las ganas de comerla si.

\begin{lstlisting}

> instance Monad Want where
>   return x       = undefined
>   (Want f) >>= g = undefined

\end{lstlisting}

\noindent
\textbf{Nota:} el propósito de este ejercicio es que noten que
son los \emph{tipos} los que deben describir el comportamiento.
Hay sólo una manera correcta de escribir todas las funciones aquí
solicitades, para lo cual lo único necesario es considerar el
tipo a producir y los tipos de los argumentos. Así mismo, para
escribir la instancia \texttt{Monad}, sólo es necesario respetar
las firmas.

\noindent
Para saber si su código funciona, primero debe compilar todo
sin errores. Luego, ejecute

\begin{verbatim}
ghci> tengo hambre
\end{verbatim}

\noindent
y el contenido le hará entender si lo logró.

\ignore{
\begin{lstlisting}

> hambre :: Want ()
> hambre = pana (ponle (topping 42))
>               (pana (ponle (topping 69))
>                     (pana (ponle (topping 17))
>                           (ponle (topping 23) >> nomnom (putStrLn ""))))
>
> tengo :: Want a -> IO ()
> tengo x = pizzeria [want x]
>
> topping :: Int -> String
> topping 17 = "/nlmce"
> topping 23 = "/y./p6"
> topping 42 = "htptuc2"
> topping 69 = "t:irofr"
>
> ponle :: String -> Want ()
> ponle xs = mapM_ (nomnom . putChar) xs

\end{lstlisting}
}

\end{document}
