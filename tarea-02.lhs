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

\lstset{literate=
  {á}{{\'a}}1 {é}{{\'e}}1 {í}{{\'i}}1 {ó}{{\'o}}1 {ú}{{\'u}}1
  {Á}{{\'A}}1 {É}{{\'E}}1 {Í}{{\'I}}1 {Ó}{{\'O}}1 {Ú}{{\'U}}1
  {à}{{\`a}}1 {è}{{\`e}}1 {ì}{{\`i}}1 {ò}{{\`o}}1 {ù}{{\`u}}1
  {À}{{\`A}}1 {È}{{\'E}}1 {Ì}{{\`I}}1 {Ò}{{\`O}}1 {Ù}{{\`U}}1
  {ä}{{\"a}}1 {ë}{{\"e}}1 {ï}{{\"i}}1 {ö}{{\"o}}1 {ü}{{\"u}}1
  {Ä}{{\"A}}1 {Ë}{{\"E}}1 {Ï}{{\"I}}1 {Ö}{{\"O}}1 {Ü}{{\"U}}1
  {â}{{\^a}}1 {ê}{{\^e}}1 {î}{{\^i}}1 {ô}{{\^o}}1 {û}{{\^u}}1
  {Â}{{\^A}}1 {Ê}{{\^E}}1 {Î}{{\^I}}1 {Ô}{{\^O}}1 {Û}{{\^U}}1
  {œ}{{\oe}}1 {Œ}{{\OE}}1 {æ}{{\ae}}1 {Æ}{{\AE}}1 {ß}{{\ss}}1
  {ű}{{\H{u}}}1 {Ű}{{\H{U}}}1 {ő}{{\H{o}}}1 {Ő}{{\H{O}}}1
  {ç}{{\c c}}1 {Ç}{{\c C}}1 {ø}{{\o}}1 {å}{{\r a}}1 {Å}{{\r A}}1
  {€}{{\EUR}}1 {£}{{\pounds}}1
}

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

> {-# LANGUAGE DeriveDataTypeable #-}
>
> import Control.Applicative
> import qualified Data.Sequence as DS
> import Control.Monad
> import Control.Monad.Trans
> import Control.Monad.Trans.RWS
> import Control.Monad.Exception
> import Data.Char
> import Data.Sequence as DS
> import Data.Foldable as DF
> import qualified Data.Map as DM
> import qualified Graphics.HGL as G
> import Data.Typeable

\end{lstlisting}

\noindent
Considere el siguiente tipo de datos, en el cual \texttt{f}
puede ser \emph{cualquier} \texttt{Functor}.

\begin{lstlisting}

> data Bonus f a = Malus a
>                | Bonus (f (Bonus f a))
>

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
\textbf{Explicación:} para escribir la instancia de \texttt{Functor}
es necesario proporcionar una implementación para \texttt{fmap}.\\

Primero notemos que la firma de \texttt{fmap} para nuestro tipo de
dato es la siguiente: \texttt{fmap :: (a -> b) -> Bonus f a -> Bonus f b}
por lo tanto tenemos 2 casos, uno para el constructo \texttt{Malus} y
otro para el constructor \texttt{Bonus}.\\

Otro elemento a considerar es que de la firma de la instancia de
Functor \texttt{Functor f => Functor (Bonus f)} podemos deducir que f es
un functor, es decir, tiene \texttt{fmap}.\\

El primer caso (\texttt{Malus}) es bastante directo, se aplica la función \texttt{g}
sobre el elemento en \texttt{a}. El segundo caso es más complicado, sabemos
que dentro de \texttt{Bonus} hay un functor que guarda \texttt{Bonus f a} y esos
también deben ser modificados por \texttt{fmap}. El \texttt{fmap} más externo aplica
la función \texttt{(fmap g)} a los elementos que guarda y a estos les aplicamos \texttt{g}.
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
\textbf{Explicación:} para proporcionar la instancia de \texttt{Applicative} es
necesario implementar las funciones \texttt{pure} y \texttt{(<*>)}. La función \texttt{pure}
es igual a \texttt{Malus}, ya que introduce un elemento en el Functor. Note que la
función está escrita en punto fijo.\\

\texttt{(<*>)} debe tomar una función dentro del \texttt{Applicative} y ``aplicársela''
a un elemento en contexto, su firma es:\\
\texttt{(<*>) :: Bonus f (a -> b) -> Bonus f a -> Bonus f b}
muy parecida a la de fmap, por lo que la escribimos en términos de fmap.\\

El primer caso, si \texttt{g} está dentro de \texttt{Malus} se debe "levantar"
la función y aplicarla al segundo argumento. En el segundo caso la
función debe ser aplicada a cada elemento dentro del \texttt{Functor f},
que es de tipo\\ \texttt{Bonus f (a -> b)}, para eso usamos \texttt{fmap} y
aplicamos las funciones al segundo argumento \texttt{b}.
}
}
\\

\begin{lstlisting}

> instance Functor f => Monad (Bonus f) where
>    return = pure
>
>    (Malus a) >>= f  = f a
>    (Bonus fa) >>= f = Bonus $ fmap (flip (>>=) f) fa

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
\textbf{Explicación:} una vez implementados la instancias de \texttt{Functor}
y \texttt{Applicative}, la de \texttt{Monad} es directa debemos implementar \texttt{return}
y \texttt{(>$>$=)}. \texttt{return} nos más que otro nombre para \texttt{pure}.\\

Para implementar \texttt{(>=)} seguimos el razomiento para \texttt{(<*>)} teniendo en
cuenta que la firma de \texttt{Bind} es:\\
\texttt{(>=) :: Bonus f a -> (a -> Bonus f b) -> Bonus f b}.
}
}
\\

\section*{Práctica obligada}

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

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Explicación: Mi intuición con esta función es correr la
función que guarda \textt{Want} para devolver su resultado.
La función \textt{const :: b -> a -> b} la usamos curryficada, 
por lo tanto recibe una Pizza y devuelve una función de a -> Pizza.
}
}
\\

> want :: Want a -> Pizza
> want (Want f) =  f (const Happy)

\end{lstlisting}

\noindent
Eso no quiere decir que sin pizza no se pueda ser feliz -- aunque
suene difícil o inconcebible, a veces uno simplemente es feliz.

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Explicación: Ser feliz es simplemente ser feliz. Recibe una función y
devuelve Happy.
}
}
\\

\begin{lstlisting}

> happy :: Want a
> happy = Want (\f -> Happy)

\end{lstlisting}

\noindent
Imagínate la pizza. Es tuya. La devoras. Y con cada bocado es
inevitable querer más\ldots

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Explicación: El tipo de \texttt{Eat (IO Pizza)} nos ayudará a
escribir esta función haciendo uso de \\
\texttt{liftM :: Monad m => (a -> r) -> m a -> m r} que recibe
una función (usaremos la que guarda \texttt{Want}) y un cómputo
monádico, en particula \texttt{IO a} lo es. Colocamos este
resultado en una función que devuelve la construcción anterior
y la envolvemos en \texttt{Want} para cumplir con la
firma \texttt{nomnom}.
}
}
\\

\begin{lstlisting}

> nomnom :: IO a -> Want a
> nomnom ioa = Want $ (\f -> Eat (liftM f ioa))

\end{lstlisting}

\noindent
Y mientras masticas la pizza piensas que hoy es jueves.
¡Jueves de 2x1! Hay que aprovechar el combo, hasta que no
quede nada.

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Explicación: el tipo de \texttt{Combo Pizza Pizza} nos permite
combinar dos pizzas en una sola. La pizza que devuelve la función
en \texttt{Want a} la colocamos en un combo. Para cumplir con la firma
llamamos a la función que pone la pizza en combo (h) con argumento ().
}
}
\\

\begin{lstlisting}

> combo :: Want a -> Want ()
> combo (Want f) = Want $ \h -> h ()
>                  where h a = Combo z z
>                        z = f h

\end{lstlisting}

\noindent
«Es demasiada pizza» -- obviamente una falacia: nunca es
demasiada pizza, porque \emph{siempre} hay un pana con el
cual compartir las ganas de pizza (sobre todo si pagan
por partes iguales).

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Explicación: el tipo de \texttt{Combo Pizza Pizza} nos permite
combinar dos pizzas en una sola. Combinaremos ambos resultados
de las pizzas en los argumentos aprovechando la función
\texttt{want}) que "corre" la función dentro del
\texttt{Want a}. Esto lo colocamos dentro de un lambda.
}
}
\\

\begin{lstlisting}

> pana :: Want a -> Want a -> Want a
> pana p1 p2 = Want $ \h -> Combo (want p1) (want p2)

\end{lstlisting}

\noindent
Es momento de ir a por ella. Toda la que haya. Toda la que se
pueda. Suficiente para todos. Allí se pide el combo. Todos comen.
Y cada uno es feliz.

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Explicación: Imprimir cada pizza en la lista es
la función más simple que cumple con la firma.
Para ello usamos la función mapM puesto que solo interesan
los efectos de borde.
}
}
\\

\begin{lstlisting}

> pizzeria :: [Pizza] -> IO ()
> pizzeria = Control.Monad.mapM_ print

\end{lstlisting}

\noindent
Porque las ganas son sólo el contexto. Porque cada topping que
se agrega no hace sino anticipar la secuencia de bocados, que
uno, sólo o acompañado, quiere satisfacer. La pizza no será un
\texttt{Monad}, pero las ganas de comerla si.

\begin{lstlisting}

> instance Monad Want where
>   return x       = Want $ \h -> Happy
>                    where h x = Happy
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
> ponle xs = DF.mapM_ (nomnom . putChar) xs

\end{lstlisting}
}

\end{document}
