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
Insertar un elemento a un buffer y luego borrarlo debe
producir el mismo buffer.
}
}
\\

\begin{lstlisting}

> prop_insert_delete :: Buffer -> Char -> Property
> prop_insert_delete b@(ls, rs) c =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 19 && length rs > 19) "seria" $
>   ((repN 100 delete).(repN 100 (insert c))) b == b

\end{lstlisting}

\begin{lstlisting}

> prop_insert_cursor :: Buffer -> Char -> Property
> prop_insert_cursor b@(ls, rs) c =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 19 && length rs > 19) "seria" $
>   c == fromJust (cursor ((left . insert c) b))

\end{lstlisting}

\begin{lstlisting}

> prop_left_atleft b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 19 && length rs > 19) "seria" $
>   atLeft $ repN (length ls) left b

\end{lstlisting}

\begin{lstlisting}

> prop_left_to_original b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 19 && length rs > 19) "seria" $
>   (snd . repN (length ls) left) b == (reverse ls) ++ rs

\end{lstlisting}

\begin{lstlisting}

> prop_right_atright b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 19 && length rs > 19) "seria" $
>   atRight $ repN (length rs) right b

\end{lstlisting}

\begin{lstlisting}

> prop_right_to_original b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 19 && length rs > 19) "seria" $
>   (reverse . fst . repN (length rs) right) b == (reverse ls) ++ rs

\end{lstlisting}

\begin{lstlisting}

> prop_clear_all b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 19 && length rs > 19) "seria" $
>   (repN (length ls) delete . repN (length rs) remove) b == empty

\end{lstlisting}

\begin{lstlisting}

> prop_clear_all_left b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 19 && length rs > 19) "seria" $
>   (delete . repN (length ls) left) b == repN (length ls) left b

\end{lstlisting}

\begin{lstlisting}

> prop_clear_all_right b@(ls, rs) =
>   classify (null ls || null rs) "trivial" $
>   classify (not (null ls || null rs)) "mejor" $
>   classify (length ls > 19 && length rs > 19) "seria" $
>   (remove . repN (length rs) right) b == repN (length rs) right b

\end{lstlisting}

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

\end{lstlisting}
\pagebreak

\section{Uso de \texttt{Parsec}}

\noindent
Un archivo ``Literate Haskell'' (\texttt{.lhs}) incluye código
Haskell combinado con texto arbitrario. A efectos de este ejercicio
supondremos que se trata de texto simple pero se desea convertirlo a
HTML para su publicación en una página Web. Más aún, se adoptan las
siguientes convenciones:

\begin{itemize}
\item
  Si una línea comienza con \texttt{*} se trata de un encabezado
  principal.
\item
  Si una línea comienza con \texttt{\#} se trata de un encabezado
  secundario.
\item
  Un párrafo termina cuando haya una línea en blanco.
\item
  Si una línea comienza \emph{exactamente} con \texttt{>} seguido
  de espacio en blanco, se asume que el resto corresponde a texto
  del programa. Los dos caracteres al principio \textbf{no} deben
  conservarse.
\item
  Los espacios en blanco son irrelevantes salvo el caso anterior.
  En otras palabras, los espacios en blanco entre palabras,
  antes del \texttt{*} al comienzo de una línea, etc. han de
  convertirse en un espacio en blanco sencillo.
\end{itemize}

\noindent
Escriba un programa basado en un reconocedor \texttt{Parsec}
tal que pueda ser utilizado para convertir los archivos \texttt{.lhs}
con el formato antes descrito hacia HTML válido, tomando en cuenta
que:

\begin{itemize}
\item
  Los encabezados principales y secundarios deben envolverse entre
  las marcas HTML \texttt{<h1>} y \texttt{<h2>}.
\item
  Los párrafos sueltos deben envolverse entre las marcas \texttt{<p>}.
\item
  Los segmentos continuos (múltiples líneas seguidas) con código Haskell
  deben envolverse entre las marcas HTML \texttt{<code>}.
\item
  Los símbolos \texttt{<}, \texttt{>} y \texttt{\&} tienen significado
  especial en HTML, por lo que deben ser convertidos a la entidad
  correspondiente.
\item
  El resto del texto debe ser transportado ``tal cual''.
\end{itemize}

\noindent
Su programa debe recibir uno o más nombres de archivo \texttt{.lhs}
desde la línea de comandos y producir sendos archivos \texttt{.html}
con los resultados de la transformación.

\begin{lstlisting}

> lhsParser = h1
>         <|> h2
>         <|> p
>
> p = do
>   ls <- many line
>   string "\n"
>   return $ concat $ ["<p> "] ++ ls ++ [" </p>"]
>
> line :: GenParser Char st String
> line = do
>   ws <- many word
>   eop
>   return $ concat $ ws ++ ["\n"]
>
> word :: GenParser Char st String
> word = do
>   ws <- many1 $ noneOf [' ', '\n']
>   b <- blank
>    <|> string ""
>   return $ ws ++ b
>
> blank :: GenParser Char st String
> blank = many1 (char ' ') >> return " "
>
> h1 = do
>   char '*'
>   cs <- many (noneOf "\n")
>   eop
>   return $ "<h1> " ++ cs ++ " </h1>"
>
> h2 = do
>   char '#'
>   cs <- many (noneOf "\n")
>   eop
>   return $ "<h2> " ++ cs ++ " </h2>"
>
> code = do
>   ls <- many cLine
>   eop
>   return $ concat $ ["<code> "] ++ ls ++ [" </code>"]
>
> cLine = string "> " >> line
>
>
> eop = char '\n'
>
> parseLHS input = parse lhsParser "#dfdsffs" input

\end{lstlisting}

\end{document}
