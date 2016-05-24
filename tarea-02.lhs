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
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE UndecidableInstances #-}
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
> instance (Show a, Show (f (Bonus f a))) => Show (Bonus f a) where
>     showsPrec d (Malus a) = showParen (d > 10) $
>         showString "Malus " . showsPrec 11 a
>     showsPrec d (Bonus m) = showParen (d > 10) $
>         showString "Bonus " . showsPrec 11 m

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

\begin{lstlisting}

> data LogoProgram =
>     Fd Int                       -- Avanzar N pasos.
>   | Bk Int                       -- Retroceder N pasos.
>   | Rt Int                       -- Girar N grados a derecha.
>   | Lt Int                       -- Girar N grados a izquierda.
>   | Pu                           -- Subir el lápiz.
>   | Pd                           -- Bajar el lápiz.
>   | Pc String                    -- Cambiar el color del lápiz.
>   | Say String                   -- Emitir un mensaje de texto.
>   | Home                         -- Regresar al origen.
>   | Seq (DS.Seq LogoProgram)     -- Secuencia de instrucciones.
>   | Rep Int (DS.Seq LogoProgram) -- Repetir N veces.
>   deriving (Show, Eq)

> {-
>    Catamorfismo (fold) sobre el tipo de datos de las
>    instrucciones de la Máquina Virtual Logo, a ser
>    aprovechado en la fase de conversión de instrucción
>    hacia acciones monádicas.
>  -}
> foldLP a b c d e f g h i j k inst =
>   case inst of
>     (Fd n)    -> a n
>     (Bk n)    -> b n
>     (Rt n)    -> c n
>     (Lt n)    -> d n
>     Pu        -> e
>     Pd        -> f
>     (Pc s)    -> g s
>     (Say s)   -> h s
>     Home      -> i
>     (Seq l)   -> j (fmap (foldLP a b c d e f g h i j k) l)
>     (Rep n l) -> k n (fmap (foldLP a b c d e f g h i j k) l)
>

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Para mayor claridad se introduce el tipo de dato \texttt{MyColors}
que representará el ambiente de lectura del \texttt{Monad RWS}.
}
}
\\

\begin{lstlisting}

> type MyColors = DM.Map String G.Color

\end{lstlisting}

\begin{lstlisting}

> {-
>   @validColors@ es un valor constante que produce una tabla
>   con los colores válidos de la Máquina Virtual Logo,
>   utilizando cadenas alfanuméricas como clave de búsqueda.
> -}
>
> validColors :: MyColors
> validColors = DM.fromList [
>                 ( "black",   G.Black   ),
>                 ( "red",     G.Red     ),
>                 ( "green",   G.Green   ),
>                 ( "blue",    G.Blue    ),
>                 ( "cyan",    G.Cyan    ),
>                 ( "magenta", G.Magenta ),
>                 ( "yellow",  G.Yellow  ),
>                 ( "white",   G.White   )
>               ]
>
> {-
>    @toColor@ es utilizada para convertir una cadena que
>    especifica un color en el tipo de datos Color
>    necesario para dibujar. En caso que la cadena a
>    buscar no corresponda a un color definido por la
>    Máquina Virtual Logo, la ejecución aborta con un error.
>  -}
> toColor :: String -> G.Color
> toColor s =
>   case f of
>       Just c  -> c
>       Nothing -> error $ "'" ++ s ++ "' es un color invalido"
>   where f = DM.lookup (map toLower s) validColors

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
\texttt{LogoState} constituye el tipo de dato que representará
el estado del Monad RWS.
}
}
\\

\begin{lstlisting}

> {-
>    @Figure@ persigue modelar la geometría generada por la
>    interpretación apoyándose solamente en los polígonos y texto.
>    El constructor @Empty@ es utilizado como centinela para
>    detectar cuando debe comenzar y cuando termina un nuevo
>    polígono o primitiva de texto.
>  -}
> data Figure = Poly G.Color [G.Point]
>             | Text G.Color G.Point String
>             | Empty
>             deriving (Show,Eq)
>
> {-
>    Modelo de Estado para la Máquina Virtual Logo, aprovechado
>    en la instanaciación automática del Monad State
> -}
> type Direction   = Int
> data PenStatus   = Up | Down
>                  deriving (Show,Eq)
>
> data LogoState = LogoState {
>    pos :: G.Point,            -- (x,y)
>    dir :: Direction,          -- En grados
>    pns :: PenStatus,
>    pnc :: G.Color,
>    drw :: DS.Seq Figure
> } deriving (Show)
>

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
\texttt{MyException} constituye el tipo de dato que representará
el las excepciones de MiniLogo según lo establecido por el enunciado.
}
}
\\

\begin{lstlisting}

> data MyException = RedAfterGreen   -- Rojo después de verde
>                  | GreenAfterRed   -- Verde después de rojo
>                  | LeftWithRed Int -- Ir a la izquierda con rojo
>                    deriving (Show, Typeable)
>
> instance Exception MyException

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
La primera decisión que se debe tomar es el orden en el que
se colocarán transformadores de \texttt{Monad}s \texttt{RWST},
\texttt{ExceptionT} y \texttt{IO()}, ya que \texttt{ExceptionT} no
es ortogonal. Se escoge como "base" de la estructura \texttt{IO()}
porque la librería gráfica es de este tipo. Sobre él colocamos
\texttt{ExceptionT} y luego \texttt{RWST} ya que "Si ocurrió un error,
no interesa el estado de la simulación'' y si intercambiamos estos,
el estado de la simulación será mostrado al ocurrir una excepción. \\

En cuanto al \texttt{RWST} es evidente que necesitamos un ambiente
de lectura, uno de escritura y un estado (en ese orden). Para el
ambiente de lectura nos interesa tener los colores válidos ya que
estos no pueden cambiar durante la ejecución del programa. Como
acumulador tendremos el diagrama en curso, la secuencia de figuras
que se están dibujando. Y por último, para el estado utilizamos
LogoState.\\

Agregamos un alias para el tipo en cuestión para que la lectura sea más sencilla.
LogoRWSE.
}
}
\\

\begin{lstlisting}

> type LogoRWSE = RWST MyColors (DS.Seq Figure) LogoState
>                 (ExceptionT IO) ()

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
La estuctura general es parecida a la inicial, sin embargo en
cada función que involucraba el campo \texttt{drw} de \texttt{LogoState}
debemos escribir en el \texttt{Writer}, para ello nos valemos de la
función \texttt{censor :: (Monad m, Data.Monoid.Monoid w) =>
(w -> w) -> RWST r w s m a -> RWST r w s m a} que toma una función
que transforma el \texttt{Writer} sin modificar el acumulador. De
esta manera podemos manipular fácilmente el contenido de nuestro
dibujo de manera apropiada.
}
}
\\

\begin{lstlisting}

> {- @noop@ -- Transformación de estado que no hace nada -}
> noop :: LogoRWSE
> noop = return ()
>
> {- @pu@ -- Transformación de estado para subir el lápiz -}
> pu :: LogoRWSE
> pu = do
>   s <- get
>   case pns s of
>     Down -> do censor draw (put $ s { pns = Up })
>             --where drw' = case d of
>             --               (ds :> Empty) -> ds
>             --               _             -> drw s
>             --             where d = DS.viewr $ drw s
>                where draw w = case d of
>                                 (ds :> Empty) -> ds
>                                 _             -> w
>                               where d = DS.viewr w
>
>     Up   -> put $ s
>
> {- @pd@ -- Transformación de estado para bajar el lápiz -}
> pd :: LogoRWSE
> pd = do
>   s <- get
>   case pns s of
>     Down -> put $ s
>     Up   -> do put $ s { pns = Down }
>                tell $ singleton Empty
>
> {- @pd@ -- Transformación de estado para cambiar el color del
>    lápiz -}
> pc :: String -> LogoRWSE
> pc c = do
>   s <- get
>   when (c == "red" && pnc s == G.Green) $ throw $ RedAfterGreen
>   when (c == "green" && pnc s == G.Red) $ throw $ GreenAfterRed
>   put $ s { pnc = toColor c }
>
> {- @say@ -- Transformación de estado para emitir mensaje de
>    texto -}
> say :: String -> LogoRWSE
> say m = do
>   s <- get
>   case pns s of
>     Down -> do censor draw (put s)
>                where draw w = case d of
>                               (ds :> Empty) -> ds |> t
>                               _             -> w |> t
>                               where d = DS.viewr w
>                                     t = Text (pnc s) (pos s) m
> --               case d of
> --              (ds :> Empty) -> do put $ s { drw = ds      |> t }
> --                                  tell $ singleton t --quitar el empty
> --              _             -> do put $ s { drw = (drw s) |> t }
> --                                  tell $ singleton t
> --            where d = DS.viewr $ drw s
> --                  t = Text (pnc s) (pos s) m
>     Up   -> put $ s
>
> {- @fd@ -- Transformación de estado para avanzar @n@ pasos -}
> fd :: Int -> LogoRWSE
> fd n = get >>= moveForward n
>
> {- @bk@ -- Transformación de estado para retroceder @n@ pasos -}
> bk :: Int -> LogoRWSE
> bk n = get >>= moveForward (negate n)
>
> {- @moveForward@ -- Función auxiliar para @fd@ y @bk@ encargada
>    de calcular el desplazamiento en la dirección actual,
>    posiblemente generando la geometría asociada. -}
>
> moveForward :: Int -> LogoState -> LogoRWSE
> moveForward n s | pns s == Up =
>   put $ s { pos = move (pos s) n (dir s) }
> moveForward n s = do
>   censor draw (put $ s { pos = np })
>   where cc = pnc s
>         cp = pos s
>         np = move cp n (dir s)
>         t  = Poly cc [ np, cp ]
>         draw w = case d of
>                    (ds :> Empty)     -> ds |> t
>                    (ds :> Poly pc l) -> if (pc == cc)
>                                         then ds |> Poly cc (np:l)
>                                         else w |> t
>                    _                 -> w |> t
>                    where d  = DS.viewr w
> --  case d of
> --    (ds :> Empty)     -> do put $ s { pos = np, drw = ds |> t}
> --                            tell $ singleton t --el empty
> --    (ds :> Poly pc l) -> if (pc == cc)
> --                         then do put $ s { pos = np, drw = ds |> Poly cc (np:l) }
> --                                 tell $ singleton $ Poly cc (np:l)
> --                         else do put $ s { pos = np, drw = drw s |> t }
> --                                 tell $ singleton t
> --    _                 -> do put $ s { pos = np, drw = drw s |> t }
> --                            tell $ singleton t
> --    where cc = pnc s
> --          cp = pos s
> --          d  = DS.viewr $ drw s
> --          np = move cp n (dir s)
> --          t  = Poly cc [ np, cp ]
>
> move :: G.Point -> Int -> Direction -> G.Point
> move (x,y) n d =
>   let direc  = (pi * (fromIntegral d)) / 180
>       nn     = fromIntegral n
>       nx     = x + (round (nn * (cos direc)))
>       ny     = y + (round (nn * (sin direc)))
>   in (nx,ny)
>
> {- @lt@ -- Transformación de estado para girar @n@ grados a la
>    izquierda -}
> lt :: Int -> LogoRWSE
> lt n = get >>= turnLeft n
>
> {- @rt@ -- Transformación de estado para girar @n@ grados a la
>    derecha -}
> rt :: Int -> LogoRWSE
> rt n = get >>= turnLeft (negate n)
>
> {- @turnLeft@ -- Función auxiliar para @lt@ y @rt@ encargada de
>    calcular la rotación en grados, manteniendo los valores
>    entre 0 y 359. -}
> turnLeft :: Int -> LogoState -> LogoRWSE
> turnLeft n s = do
>   when (pnc s == G.Red && (dir s + n) > 90 && (dir s + n) < 270)
>        (throw $ LeftWithRed (dir s + n))
>   put $ s { dir = (dir s + n) `mod` 360 }
>
> {- @home@ -- Transformación de estado para regresar al estado
>    inicial -}
> home :: LogoRWSE
> home = put $ initial
>
> {- @goHome@ -- Transformación de estado para regrear al origen -}
> goHome :: LogoRWSE
> goHome = do
>   s <- get
>   put $ s { pos = (0,0), dir = 90 }
>
> {- @initial@ -- Estado inicial de la Máquina Virtual Logo -}
> initial :: LogoState
> initial = LogoState { pos = (0,0),
>                       dir = 90,
>                       pns = Up,
>                       pnc = G.White,
>                       drw = DS.empty }
>
> {- @repN@ -- Transformación de estado para repetir @n@ veces
>    una transformación de estado particular. -}
> repN :: Int -> LogoRWSE -> LogoRWSE
> repN 0 p = noop
> repN n p = p >> (repN (pred n) p)
>
> {- @monadicPlot@ -- Aplica un catamorfismo (fold) sobre la
>    estructura de datos que representa un programa para la
>    Máquina Virtual Logo, de manera que lo transforma en la
>    secuencia de transformaciones de estado
>    correspondientes a su interpretación. -}
> monadicPlot = foldLP fd bk rt lt pu pd pc say home seq rep
>   where seq s   = if DS.null s then noop else DF.sequence_ s
>         rep n s = repN n (seq s)
>

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Se agregan la funciones \texttt{deleteLastStep} y \texttt{repeatLastStep}
que se van a encargar de realizar las acciones nuevas del enunciado,
borrar o repetir el último paso según se presione la `a` o la `s`
}
}
\\

\begin{lstlisting}

> deleteLastStep :: LogoProgram -> LogoProgram
> deleteLastStep (Seq p) = case viewr p of
>                               EmptyR -> Seq p
>                               p' :> a -> Seq p'
> deleteLastStep p       = Seq DS.empty
>
> repeatLastStep :: LogoProgram -> LogoProgram
> repeatLastStep (Seq p) = case viewr p of
>                               EmptyR -> Seq p
>                               p' :> a -> Seq $ p |> a
> repeatLastStep p       = Seq $ DS.fromList [p, p]

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Es necesario modificar la función \texttt{runLogoProgram} para
que "corra" el nuevo Monad \texttt{LogoRWSE}. El orden
de ejecución de esas funciones es el inverso al que fue creado
el \texttt{Monad} por lo tanto utilizamos primero \texttt{execRWST}
y luego \texttt{runException} que retorna un
\texttt{Either MyException (LogoState, Seq Figure)}. Utilizamos la
función \texttt{either} para imprimir el error en caso de que haya
ocurrido uno o realizar el diagrama que corresponda al \texttt{LogoProgram}.

Por último se espera por una tecla, si es `a` o `s` se realiza las
\texttt{deleteLastStep} o \texttt{repeatLastStep} respectivamente,
cualquier otra letra ocasiona el cierre de la ventana del dibujo.
}
}
\\

\begin{lstlisting}

> {-|
>   La función @runLogoProgram@ interpreta un programa escrito con
>   las instrucciones de la Máquina Logo, produciendo la salida
>   gráfica asociada.
>
>   El programa abrirá una ventana con las dimensiones y el título
>   suministrados como argumentos, se posicionará la tortuga
>   en el centro de la ventana correspondiente a la coordenada
>   (0,0), apuntando hacia el tope de la pantalla (90 grados),
>   con el lápiz blanco levantado.
>
>   Se convertirá el programa en la secuencia de transformación de
>   estado correspondiente, siendo interpretado hasta obtener el
>   estado final. La geometría calculada como parte de esta
>   transformación será extraída, convertida a las primitivas
>   gráficas correspondientes
>   en HGL y aplicadas sobre la ventana. Una vez representadas
>   las acciones gráficas, el programa espera hasta que se
>   oprima cualquier tecla para terminar la ejecución
>   cerrando la ventana.
>  -}
> runLogoProgram :: Int         -- Anchura en pixels de la ventana.
>                -> Int         -- Altura en pixels de la ventana.
>                -> String      -- Título para la ventana.
>                -> LogoProgram -- Instrucciones de la Máquina Logo.
>                -> IO ()
> runLogoProgram w h t p =
>     G.runGraphics $ do
>       window <- G.openWindow t (w,h)
>       runAux w h t p window
>       where runAux w h t p window = do
>             lgSt <- runExceptionT (execRWST (monadicPlot p) validColors initial)
>             either (\e -> print e >> G.closeWindow window) (draw window) lgSt
>             c <- G.getKey window
>             when (G.keyToChar c == 'a')
>                  (G.clearWindow window >>
>                   runAux w h t (deleteLastStep p) window)
>             when (G.keyToChar c == 's')
>                  (G.clearWindow window >>
>                   runAux w h t (repeatLastStep p) window)
>             G.closeWindow window
>             where draw w lgst = G.drawInWindow w $ G.overGraphics (
>                               DF.toList $ fmap f ((DS.filter (/=Empty) . snd) lgst)
>                               )
>                   f (Poly c p)   = G.withColor c $ G.polyline (map fix p)
>                   f (Text c p s) = G.withColor c $ G.text (fix p) s
>                   (x0,y0)        = origin w h
>                   fix (x,y)      = (x0 + x, y0 - y)
>
>
> origin w h = (half w, half h)
>              where
>                half i = round ((fromIntegral i)/2)

\end{lstlisting}

RECUERDA QUITAR ESTOOOOOOOOOOOOOOOOOOOOOOOOOO
\begin{lstlisting}

> a = Seq $ DS.fromList [
>         Pd, Fd 100,
>         Pc "Red", Rt 90, Fd 100,
>         Pc "Green", Rt 90, Fd 100, Rt 90, Fd 100,
>         Pc "Blue", Rt 135, Fd 142,
>         Pu, Rt 135, Fd 200,
>         Pd, Pc "Green", Fd 10
>     ]
>
> b = Seq $ DS.fromList [
>         Pd, Fd 100,
>         Rt 90, Fd 100, Rt 90, Fd 100, Rt 90, Fd 100,
>         Rt 135, Fd 142, Pu, Rt 135, Fd 200, Pd, Fd 10
>     ]
>
> c = Seq $ DS.fromList [
>         Pd, Pc "Yellow",
>         Rep 4 $ DS.fromList [Fd 100, Rt 90]
>     ]
>
> d = Seq $ DS.fromList [
>        Pd, Pc "Yellow",
>         Rep 36 $ DS.fromList [
>             Rep 4 $ DS.fromList [ Fd 200, Rt 90 ],
>             Rt 10
>         ]
>     ]
>
> e = Seq $ DS.fromList [
>         Pd, Pc "Red", Say "Baz",
>         Pu, Fd 50, Pd, Pc "Blue", Say "Bar",
>         Pu, Fd 50, Pd, Pc "Yellow", Say "Baz"
> 	]
>
> f = Seq $ DS.fromList [
>         Lt 90, Fd 50, Rt 90, Fd 50, Pc "Yellow", Pd, Say "Foo",
>         Pu, Rt 90, Fd 100, Pc "Red", Pd, Say "Bar",
>         Pu, Rt 90, Fd 100, Pc "Blue", Pd, Say "Baz",
>         Pu, Rt 90, Fd 100, Pc "Green", Pd, Say "Qux",
>         Pu, Home, Pd, Pc "White", Pd, Say "Home",
>         Fd 150, Rt 90, Fd 150, Rep 4 $ DS.fromList [ Rt 90, Fd 300 ]
>     ]
>
> main = runLogoProgram 700 700 "Test" a

\end{lstlisting}
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
> happy = Want (\aHappy -> Happy)
>         where aHappy a = Happy

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
> combo (Want f) = Want $ \h -> h ()
>                  where h a = Combo z z
>                        z = f h

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
