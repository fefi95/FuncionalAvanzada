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
\definecolor{lightorange}{rgb}{1,0.94,0.9}


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

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 3}

\author{Stefani Castellanos\\
11-11394\\
\href{mailto:scct95@gmail.com}{<scct95@gmail.com>}}

\date{Junio 1, 2016}

\maketitle

\pagebreak

\begin{lstlisting}

> import Test.QuickCheck
> import Data.Maybe (fromJust)
> import Text.ParserCombinators.Parsec

\end{lstlisting}


\section{\emph{Buffer} de un editor}

\noindent
Durante la programación de un editor de texto es necesario modelar
el \emph{buffer} que contiene una línea particular mientras es
modificada por el usuario. Las modificaciones se realizan según la
posición del cursor en la línea.

\noindent
Un modelo muy eficiente para el \emph{buffer} de un editor sería

\begin{lstlisting}

> type Buffer = (String,String)

\end{lstlisting}

\noindent
donde el primer \verb=String= contiene los caracteres antes del cursor
pero en orden \textbf{inverso}, y el segundo \verb=String= contiene los
caracteres después del cursor en el orden normal, e.g. suponiendo que
el cursor sobre una letra es modelado con $\underline{X}$, la línea

\begin{center}
Esto es un ejemplo, $\underline{d}$e como opera el buffer
\end{center}

\noindent
se modelaría como

\begin{lstlisting}

> ejemplo = (" ,olpmeje nu se otsE","de como opera el buffer")

\end{lstlisting}

\noindent
Defina las operaciones

\begin{lstlisting}

> empty      :: Buffer                -- Buffer nuevo
> empty = ([],[])
>
> cursor     :: Buffer -> Maybe Char  -- Leer bajo el cursor
> cursor (_, []) = Nothing
> cursor (_, rs) = Just $ head rs
>
> insert :: Char -> Buffer -> Buffer -- ...antes del cursor
> insert c (ls, rs) = (c : ls, rs)
>
> delete :: Buffer -> Buffer         -- ...anterior al cursor
> delete b@([], rs)   = b
> delete (l : ls, rs) = (ls, rs)
>
> remove :: Buffer -> Buffer         -- ...bajo al cursor
> remove b@(ls, [])   = b
> remove (ls, r : rs) = (ls, rs)
>
> left :: Buffer -> Buffer           -- Cursor a la izquierda
> left b@([], rs)   = b
> left (l : ls, rs) = (ls, l : rs)
>
> right :: Buffer -> Buffer          -- Cursor a la derecha
> right b@(ls, [])   = b
> right (ls, r : rs) = (r : ls, rs)
>
> atLeft :: Buffer -> Bool           -- Extremo izquierdo?
> atLeft ([],_) = True
> atLeft _      = False
>
> atRight :: Buffer -> Bool          -- Extremo derecho?
> atRight (_, []) = True
> atRight _       = False

\end{lstlisting}

\noindent
Implante estas operaciones y ofrezca suficientes propiedades
QuickCheck para comprobar la correctitud de la implantación.
Por supuesto que «suficientes» es un eufemismo por «use técnicas
de análisis de cobertura para garantizar que sus casos de prueba
ejercitan \textbf{toda} la librería».
\\

\noindent
Note que \emph{ninguna} de las operaciones produce errores;
esto quiere decir que, por ejemplo, \texttt{left} sobre un
\emph{buffer} vacío debe producir el mismo \emph{buffer} vacío,
así como \texttt{delete} al principio de la línea, no borra nada.
\\

\noindent
Finalmente, queremos mantener la implantación eficiente así que
debe \emph{evitar} el uso de la concatenación de listas (\verb=++=).
\\

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Para ayudar a crear pruebas más generalizadas es útil crear una
función que permita realizar repetir una acción sobre el buffer n veces.
Se define:
}
}
\\

\begin{lstlisting}

> repN :: Int -> (Buffer -> Buffer) -> Buffer -> Buffer
> repN 0 _ b = b
> repN n f b |n > 0 = repN (n - 1) f (f b)

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Entonces, crear pruebas se hace más interesante. Cada una de las
pruebas está clasificadas en tres categorias: trivial, mejor y seria.
Las triviales son aquellas que representan un buffer vacío. Las
mejores son las que no representan un buffer vacío. Y por último,
las serias son aquellas en donde el buffer tiene al menos 42
caracteres. Evidentemente todas las serias son mejores.\\

Se realiza esta clasificación para tener un idea de la calidad
de pruebas que se están realizando, no es muy útil probar 100
veces el buffer vacío.\\

La primera prueba consiste en insertar un n elementos a un buffer
y luego borrarlos, esto debe producir el mismo buffer.
}
}
\\

\begin{lstlisting}

> prop_insert_delete :: Buffer -> Char -> Property
> prop_insert_delete b@(ls, rs) c =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   repN 100 (delete . insert c) b == b

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Al insertar un elemento a un buffer y moverse a la izquierda
leer el elemento sobre el que está posicionado (cursor) debe
ser el mismo que fue insertado.
}
}
\\

\begin{lstlisting}

> prop_insert_cursor :: Buffer -> Char -> Property
> prop_insert_cursor b@(ls, rs) c =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   c == fromJust (cursor ((left . insert c) b))

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Moverse a la izquierda la cantidad de elementos que están
a la izquierda del cursor debe resultar en quedar "a la izquierda"
del buffer
}
}
\\
\begin{lstlisting}

> prop_left_atleft b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   atLeft $ repN (length ls) left b

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Moverse a la izquierda la cantidad de elementos a
la izquierda del buffer debe resultar en que a
la derecha tiene toda la información en el orden natural.
}
}
\\

\begin{lstlisting}

> prop_left_to_original b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   (snd . repN (length ls) left) b == (reverse ls) ++ rs

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Análogo a \texttt{left\_atleft}.

}
}
\\

\begin{lstlisting}

> prop_right_atright b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   atRight $ repN (length rs) right b

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Análogo a \texttt{letf\_to\_original}
}
}
\\

\begin{lstlisting}

> prop_right_to_original b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   (reverse . fst . repN (length rs) right) b == (reverse ls) ++ rs

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Eliminar todo lo que está a la izquierda y todo lo que
está a la derecha del cursor debe resultar en un buffer
vacío.
}
}
\\

\begin{lstlisting}

> prop_clear_all b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   (repN (length ls) delete . repN (length rs) remove) b == empty

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Moverse a la izquierda tanto elementos y borrar dicho elemento
es igual a moverse a la izquierda sin borrar nada. Es decir,
borrar a la izquierda cuando se está allí no produce cambios.
}
}
\\

\begin{lstlisting}

> prop_clear_all_left b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   (delete . repN (length ls) left) b == repN (length ls) left b

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Análogo a \texttt{clear\_all\_right}
}
}
\\

\begin{lstlisting}

> prop_clear_all_right b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   (remove . repN (length rs) right) b == repN (length rs) right b

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Moverse la misma cantidad a la derecha y a la izquierda no produce
cambios en el buffer, siempre y cuando no se muevan más caracteres
hacia un lado de los que están.
}
}
\\

\begin{lstlisting}

> prop_left_to_right b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   repN (length ls) (right . left) b == b

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Moverse a la izquierda al menos una caracter más que
la cantidad de elementos a la izquierda es igual
a moverse exactamente esa cantidad. En otras palabras,
si ya se está en la izquierda moverse más hacia este
sentido no produce cambios.
}
}
\\
\begin{lstlisting}

> prop_left_overflow b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   repN (length ls + 1) left b == repN (length ls) left b

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Análogo al anterior.
}
}
\\
\begin{lstlisting}

> prop_right_overflow b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   repN (length rs + 1) right b == repN (length rs) right b

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Moverse completamente a la derecha y leer el último caracter
con \texttt{cursor} debe resultar en \texttt{Nothing}.
}
}
\\

\begin{lstlisting}

> prop_right_cursor b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   cursor (repN (length rs) right b) == Nothing

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Moverse un paso a la derecha sin importar el estado del buffer
implica que ya no se está ``a la izquierda", siempre y cuando el buffer
no está vacío ya que en este caso el cursor no puede moverse a la derecha.

Para ese caso lo ideal es realizar una prueba unitaria.
}
}
\\

\begin{lstlisting}

> prop_right_atleft b@(ls, rs) =
>   classify (null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   if null ls && null rs
>   then True --obviar el caso en que ambos son vacios
>   else not $ atLeft $ right b

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Análogo al anterior.
}
}
\\
\begin{lstlisting}

> prop_left_atright b@(ls, rs) =
>   classify (null ls ) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 20 && length rs > 20) "seria" $
>   if null ls && null rs
>   then True --obviar el caso en que ambos son vacios
>   else not $ atRight $ left b

\end{lstlisting}

\pagebreak

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Por último tenemos el programa principal que correo todas las
pruebas.
}
}
\\

\begin{lstlisting}

> main = do
>   quickCheck prop_insert_delete
>   quickCheck prop_insert_cursor
>   quickCheck prop_left_atleft
>   quickCheck prop_left_to_original
>   quickCheck prop_right_atright
>   quickCheck prop_right_to_original
>   quickCheck prop_clear_all
>   quickCheck prop_clear_all_left
>   quickCheck prop_clear_all_right
>   quickCheck prop_left_to_right
>   quickCheck prop_left_overflow
>   quickCheck prop_right_overflow
>   quickCheck prop_right_cursor
>   quickCheck prop_right_atleft
>   quickCheck prop_left_atright

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Al correr el main obtenemos:
99\% expressions used (672/676)\\
 66\% boolean coverage (2/3)\\
       0\% guards (0/1), 1 always True\\
     100\% 'if' conditions (2/2)\\
     100\% qualifiers (0/0)\\
100\% alternatives used (20/20)\\
100\% local declarations used (0/0)\\
96\% top-level declarations used (26/27)\\

Según el reporte de \texttt{Haskell} la expresión que no ha sido
probada es \texttt{ejemplo}, sin embargo, dado que es una
simple definición de ejemplo se ignora. La guardia que "siempre
es cierta" es el \texttt{n > 0} del código auxiliar que se utilizó
para repetir código; da un error para números negativos pero
no interesa para probar las funciones del buffer.\\

Resulta evidente que se pueden incluir algunas pruebas
unitarias para complementar el código, pero el análisis
de cobertura de \texttt{Haskell} indica que se logró cubrir
el código que interesaba.
}
}
\\

\end{document}
