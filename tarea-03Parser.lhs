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

> import Text.ParserCombinators.Parsec
> import System.IO (readFile, writeFile)
> import Data.Either (either)
> import System.Environment (getArgs)
> import System.IO.Error (catchIOError,
>        isDoesNotExistError, ioeGetFileName)
> import Control.Monad (liftM2, liftM)

\end{lstlisting}

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
con los resultados de la transformación.\\


\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Para realizar el \texttt{Parser} utilizando la librería \texttt{Parsec}
se pueden hacer funciones que analizen pequeñas partes e ir
construyendo \texttt{Parsers} más complicados a partir de estos.\\

Un archivo \texttt{lhs} con la especificaciones del enunciado
solo puede tener cuatro tipo de estructuras: aquellas que
comienzan con \texttt{*},las que comienzan con \texttt{\#},
código que comienza con  \texttt{>} seguido una línea. Por lo que
definimos \texttt{lhsParser} que contiene muchos
\texttt{lhsContents} que se encargan de distinguir cuál
de las estructuras se analizará.\\

El uso de \texttt{try} es indispensable para el correcto
funcionamiento del \texttt{parser}, debido a que si una línea
comienza con espacios en blanco, no se puede determinar a
priori si se trata de una etiqueta \texttt{h1}, \texttt{h2} o
\texttt{p}. Por otro lado, cuando el archivo termina, no es necesario
que el párrafo termine en una líne en blanco lo que
explica el último caso.
}
}
\\

\begin{lstlisting}

> lhsParser =  many lhsContent
>
> lhsContent = eol
>          <|> try h1
>          <|> try h2
>          <|> code
>          <|> liftM2 (\a b -> b) eol (try p)
>          <|> p

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Ahora nos tenemos que preocupar de analizar una etiqueta que
corresponderá a \texttt{h1}. Puede haber una cantidad arbitraria
de espacios en blanco antes de \texttt{*} y después de él. Luego
sigue una línea. \texttt{h2} utiliza la misma estructura
cambiando \texttt{*} por \texttt{\#}.
}
}
\\

\begin{lstlisting}

> h1 = do
>   beginh '*'
>   cs <- line
>   return $ "<h1>\n" ++ cs ++ "</h1>\n"
>
> h2 = do
>   beginh '#'
>   cs <- line
>   return $ "<h2>\n" ++ cs ++ "</h2>\n"
>
> beginh c = blankOrN >> char c >> return ""
>   where blankOrN = blank <|> return ""

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Un párrafo está conformado por muchas lineas que finalizan en un
salto de línea.
}
}
\\

\begin{lstlisting}

> p = liftM (concat . (["<p>\n"] ++) . (++ ["</p>\n"])) (many1 line)

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Analizar un línea es tener muchas palabras seguidas por un
final de línea, potencialmente con espacios en blancos al
principio de la misma.\\

Una palabra son muchos caracteres que no incluyen el espacio
en blanco ni el final de línea. También se excluyen los
símbolos \texttt{<}, \texttt{>} y \texttt{\&} puesto que
estos requieren un tratamiento especial, transformarlo a sus
entidades correspondiente de \texttt{html}.\\

Un espacio en blanco no es más que uno o más espacios en blanco
llevados a un sólo espacio en blanco dado que el ejercicio así
lo requiere. Note que si no hay espacios en blanco no son agregados.
}
}
\\

\begin{lstlisting}

> line :: GenParser Char st String
> line = do
>   ws <- auxParser
>   eol
>   return $ concat $ ws ++ ["\n"]
>   where auxParser = do
>         many1 word
>         <|> liftM2 (++) (blank >>= return . (:[])) (many word)
>
>
> word :: GenParser Char st String
> word = do
>   ws <- auxParser
>   b <- blank <|> return ""
>   return $ ws ++ b
>   where auxParser = do
>           many1 (noneOf [' ', '\n', '>', '<', '&'])
>          <|> (string ">" >> return "&lt;")
>          <|> (string "<" >> return "&gt;")
>          <|> (string "&" >> return "&amp;")
>
> blank :: GenParser Char st String
> blank = many1 (char ' ') >> return " "

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Ahora bien, el código son muchas líneas de código en haskell, y una
línea de código es aquella que empieza exactamente por \texttt{>}
y un espacio en blanco seguida de cualquier caracter que no sea el
final de línea o solo tiene \texttt{>} y el final de línea.\\

Aquí los espacios en blancos se traducen tal y como se presentan.
}
}
\\

\begin{lstlisting}

> code = do
>   ls <- many cLine
>   eol
>   return $ concat $ ["<pre>\n"] ++ ls ++ ["</pre>\n"]
>
> cLine = do
>   string ">"
>   ls <- many (noneOf "\n")
>   eol
>   return $ ls ++ "\n"
>
> eol = try (string "\n\r")
>   <|> try (string "\r\n")
>   <|> string "\n"
>   <|> string "\r"
>   <?> "end of line missing"

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Por último, se realiza el manejo de \texttt{IO}. Para cada archivo,
la función \texttt{lhsFile} recibe una archivo, valida que sea un
\texttt{.lhs} y trata de abrirlo, si falla imprime un error explicando
el fallo, si tiene éxito analiza el contenido y el resultado
lo escribe a un archivo con el mismo nombre pero con extensión
\texttt{.html}.\\

El uso de \texttt{mapM\_} permite realizar esta operación por
cada archivo proporcionado como argumento, sin importar la
cantidad de archivos que le sean proporcionados, además solo interesan
los efecto de borde.
}
}
\\
\begin{lstlisting}

> parseLHS :: String -> Either ParseError [String]
> parseLHS input = parse lhsParser "Couldn't parse your file :(" input
>
> lhsFile lhs = do
>   let (fname, ext) = span ((/=) '.') lhs
>   if (ext /= ".lhs")
>   then putStrLn $ "Expected an lhs file but got " ++ lhs
>   else do
>       putStrLn $ "Converting " ++ lhs ++ " to html..."
>       tryReadFile lhs fname `catchIOError` hdlReadFile
>
> tryReadFile file fname = do
>       lhscontent <- readFile file
>       putStrLn lhscontent
>       let html = fname ++ ".html"
>       either print (writeFile html . concat) (parseLHS lhscontent)
>
> hdlReadFile e
>   | isDoesNotExistError e =
>       case ioeGetFileName e of
>           Just path -> putStrLn $ "File does not exist: " ++ path
>           Nothing -> putStrLn "File does not exist at unknown location!"
>   | otherwise = ioError e
>
> main = do
>   args <- getArgs
>   mapM_ lhsFile args

\end{lstlisting}

\end{document}
