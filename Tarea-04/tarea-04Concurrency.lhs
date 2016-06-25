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

\begin{lstlisting}

> import System.Random (randomRIO)
> import Control.Concurrent --(putMVar, takeMVar,
>                           -- tryTakeMVar, readChan, writeChan,
>                           -- threadDelay)
> import Control.Concurrent.STM
> import Data.Sequence
> import System.Environment (getArgs)

\end{lstlisting}

\section{General}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Para resolver el problema del baño unisex se define
un tipo de dato para el género de las personas, si
son mujeres, hombres o personal de limpieza.\\

Se define una manera de mostar las personas en la
cola y funciones que representen el tiempo que las
personas utilizan el baño.\\

\texttt{people} es la función que escoge un género
con las probabilidades indicadas en el enunciado.\\

\texttt{clean} y \texttt{useBathroom} son las actividades
que se puenden realizar en el baño y que toman un tiempo
hacerlas.\\
}
}
\\

\begin{lstlisting}

> data Genre = Women
>            | Men
>            | Cleaning
>            deriving (Eq)
>
> instance Show Genre where
>   show Women = "Mujer"
>   show Men = "Hombre"
>   show Cleaning = "Personal Limpieza"
>
> showPeopleIn genre n = putStrLn ("Hay " ++ show n ++ " "
>                                 ++ show genre ++ " en el baño")
>
> showLine q = putStrLn $ "La cola de espera es:" ++ show q

\end{lstlisting}

\pagebreak

\begin{lstlisting}

> clean = do r <- randomRIO (100000, 500000)
>            threadDelay r
>
> useBathroom = do r <- randomRIO (100000, 500000)
>                  threadDelay r
>
> people r
>   |r > 0.0 && r < 0.49  = Women
>   |r > 0.49 && r < 0.98 = Men
>   |r > 0.98 && r < 1.0  = Cleaning
>   |otherwise = Cleaning
>
> main = do
>   (sim : _) <- getArgs
>   case sim of
>       "Clasica" -> do pqueC <- newMVar $ empty
>                       brC <- newChan
>                       forkIO (peopleInLine pqueC brC)
>                       bathroomHandler pqueC brC
>       "STM"     -> do pqueT <- newTVarIO $ empty
>                       brT <- atomically $ newTChan
>                       forkIO (peopleInLineT pqueT brT)
>                       bathroomHandlerT pqueT brT
>       "T"       -> do pque2 <- newTVarIO $ empty
>                       brT <- atomically $ newTChan
>                       tic <- newTVarIO 1
>                       ser <- newTVarIO 0
>                       forkIO (peopleInLine2 tic ser pque2 brT)
>                       bathroomHandler2 ser pque2 brT
>       _         -> putStrLn ("USO: las simulaciones "
>                             ++ "disponibles son "
>                             ++ "\"Clasica\" y \"STM\"")

\end{lstlisting}

\section{El baño unisex con MVar}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
\textbf{Semántica de la solución:}\\

El baño tiene una señal
que le indica a la siguiente persona en línea que puede
pasar y únicamente ella.\\

Cada persona se coloca en la fila y espera a que la señal
en el baño le indique que puede pasar.\\

Los turnos se logran utilizando los \texttt{MVar} como mutex,
uno por cada persona; el baño sólo escribe en el \texttt{MVar}
de la persona que está al principio de la línea
para dejarla pasar.\\

Esta solución permite preservar el orden de la cola y, al
no tener una variable compartida por cada persona en la cola,
reduce el riesgo de deadlocks, ya que los únicos que pueden
acceder al turno de una persona específica son el baño y
la persona que lo pidió. \\

Se define un tipo de dato para modelar la información
que estará en la cola del baño. El \texttt{MVar} de este tipo
representa el turno de la persona en cola, es decir,
cuando se le permitirá entrar al baño. El \texttt{Genre}
representa el tipo de persona a entrar.\\

\texttt{LineM} representa la cola de espera en el baño
y \texttt{DoneM} es el canal por el cual cada persona
indica que terminó de "liberar sus demonios".
}
}
\\

\begin{lstlisting}

> newtype GenreInfo = G (MVar Bool, Genre)
>
> instance Show GenreInfo where
>   show (G (mvar, genre)) = show genre
>
> type LineM = MVar (Seq GenreInfo)
> type DoneM = Chan String

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Se definen las funciones relacionadas con el manejo de los
turnos en el baño \texttt{bathroomHandler} y \texttt{bathroom}. La primera
de estas deja entrar a la persona al principio de la cola de
espera y llama a la función \texttt{bathroom} con el género
apropiado. \\

\texttt{bathroom} chequea al primero en la cola y actúa según el
género de los que están en el baño, si son el mismo género
y no hay más de tres personas dentro de él, los deja pasar.
De lo contrario espera a que se libere el baño para dejarlo
pasar.\\
}
}
\\

\begin{lstlisting}

> bathroomHandler :: LineM -> DoneM -> IO ()
> bathroomHandler pqueC brC = do
>   (mv, g) <- nextInLine pqueC
>   putMVar mv True
>   case g of
>       Cleaning -> do putStrLn "Entra limpieza"
>                      br <- readChan brC
>                      bathroomHandler pqueC brC
>       _        -> do showPeopleIn g 1
>                      bathroom g 1 pqueC brC
>
> bathroom :: Genre -> Int -> LineM -> DoneM
>             -> IO ()
> bathroom genre 3 pqueC brC = do
>   (mv, g) <- nextInLine pqueC
>   case g of
>       Cleaning -> do waitTillEveryoneLeaves genre brC 3
>                      putMVar mv True
>                      putStrLn "Entra limpieza"
>                      br <- readChan brC
>                      putStrLn br
>                      bathroomHandler pqueC brC
>       _        -> do if g == genre
>                      then do br <- readChan brC
>                              putStrLn br
>                              showPeopleIn g 2
>                              putMVar mv True
>                              showPeopleIn g 3
>                              bathroom g 3 pqueC brC
>                      else do waitTillEveryoneLeaves genre brC 3
>                              putMVar mv True
>                              showPeopleIn g 1
>                              bathroom g 1 pqueC brC
>
> bathroom genre n pqueC brC = do
>   (mv, g) <- nextInLine pqueC
>   case g of
>       Cleaning -> do waitTillEveryoneLeaves genre brC n
>                      putMVar mv True
>                      putStrLn "Entra limpieza"
>                      br <- readChan brC
>                      putStrLn br
>                      bathroomHandler pqueC brC
>       _        -> do if g == genre
>                      then do putMVar mv True
>                              showPeopleIn g (n + 1)
>                              bathroom g (n + 1) pqueC brC
>                      else do waitTillEveryoneLeaves genre brC n
>                              putMVar mv True
>                              showPeopleIn g 1
>                              bathroom g 1 pqueC brC

\end{lstlisting}


\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Para facilitar la escritura de la funciones anteriores
se definen las siguientes funciones auxiliares. \texttt{takeSeq}
toma un elemento de la cola, si hay alguno devuelve la
cabeza y el resto, si está vació devuelve \texttt{Nothing}.\\

\texttt{nextInLine} se vale de la función anterior para otorgar
el turno a la siguiente persona en la cola.\\

\texttt{waitTillEveryoneLeaves} pretende esperar a que salgan
todas las personas en el baño.
}
}
\\

\begin{lstlisting}

> takeSeq :: Seq GenreInfo ->
>            IO (Seq GenreInfo, Maybe (GenreInfo, Seq GenreInfo))
> takeSeq ss = do
>   case viewl ss of
>        EmptyL  -> return (ss, Nothing)
>        x :< xs -> do putStrLn ("La cola de espera es:"
>                               ++ show ss)
>                      return (xs, Just (x, xs))
>
> nextInLine :: LineM -> IO (MVar Bool, Genre)
> nextInLine pqueC = do
>   m <- modifyMVar pqueC takeSeq
>   case m of
>       Nothing -> nextInLine pqueC
>       Just (G (mv, g), rest) -> do return (mv, g)
>
> waitTillEveryoneLeaves :: Genre -> DoneM -> Int -> IO ()
> waitTillEveryoneLeaves genre brC 0 = return ()
> waitTillEveryoneLeaves genre brC n = do
>   br <- readChan brC
>   putStrLn br
>   showPeopleIn genre (n - 1)
>   waitTillEveryoneLeaves genre brC (n-1)
>

\end{lstlisting}


\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
\texttt{cleaningThread} y \texttt{genreThread} son las funciones para
simular en comportamiento de la gente que usa el baño.
Indican que están en la cola y esperan su turno, hacen
lo que deben hacer y notifican que se fueron. En el caso
de la limpieza se colocan al principio de la cola como
exige el enunciado.
}
}
\\

\begin{lstlisting}

> cleaningThread :: LineM -> DoneM -> IO()
> cleaningThread pqueC brC = do
>   mv <- newEmptyMVar :: IO (MVar Bool)
>   modifyMVar_ pqueC (\ss -> return ( G (mv, Cleaning) <| ss))
>   putStrLn "La limpieza quiere entrar.."
>   mv' <- takeMVar mv
>   clean
>   writeChan brC "Listo! Ya Limpie"
>
> genreThread :: Genre -> LineM -> DoneM -> IO()
> genreThread genre pqueC brC = do
>   mv <- newEmptyMVar :: IO (MVar Bool)
>   modifyMVar_ pqueC (\ss -> return (ss |> G (mv, genre)))
>   mv' <- takeMVar mv
>   useBathroom
>   writeChan brC ("Listo! (" ++ show genre ++ ")")

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
\texttt{peopleInLine} serán la función del hilo que
crea a las personas. Tiene un delay para evitar
que la cola de espera crezca demasiado rápido.
}
}
\\

\begin{lstlisting}

> peopleInLine pqueC brC = do
>   r <- randomRIO (0, 1.0) :: IO Double
>   let g = people r
>   if g == Cleaning
>   then forkIO (cleaningThread pqueC brC)
>   else forkIO (genreThread g pqueC brC)
>   r' <- randomRIO (80000, 200000)
>   threadDelay r'
>   peopleInLine pqueC brC

\end{lstlisting}

\pagebreak
\section{El baño unisex con TVar}

\begin{lstlisting}

> newtype GenreInfoT = T (TVar Bool, Genre)
>
> instance Show GenreInfoT where
>   show (T (mvar, genre)) = show genre
>
>
> bathroomHandlerT :: TVar (Seq GenreInfoT) -> TChan String -> IO ()
> bathroomHandlerT pqueC brC = do
>   (pque, (mv, g)) <- atomically $ nextInLineT pqueC
>   putStrLn ("La cola de espera es:" ++ show pque)
>   atomically $ writeTVar mv True
>   case g of
>       Cleaning -> do putStrLn "Entra limpieza"
>                      br <- atomically $ readTChan brC
>                      bathroomHandlerT pqueC brC
>       _        -> do showPeopleIn g 1
>                      bathroomT g 1 pqueC brC
>
> bathroomT :: Genre -> Int -> TVar (Seq GenreInfoT) -> TChan String
>             -> IO ()
> bathroomT genre 3 pqueC brC = do
>   (pque, (mv, g)) <- atomically $ nextInLineT pqueC
>   putStrLn ("La cola de espera es:" ++ show pque)
>   case g of
>       Cleaning -> do waitTillEveryoneLeavesT genre brC 3
>                      atomically $ writeTVar mv True
>                      putStrLn "Entra limpieza"
>                      br <- atomically $ readTChan brC
>                      putStrLn br
>                      bathroomHandlerT pqueC brC
>       _        -> do if g == genre
>                      then do br <- atomically $ readTChan brC
>                              putStrLn br
>                              showPeopleIn g 2
>                              atomically $ writeTVar mv True
>                              showPeopleIn g 3
>                              bathroomT g 3 pqueC brC
>                      else do waitTillEveryoneLeavesT genre brC 3
>                              atomically $ writeTVar mv True
>                              showPeopleIn g 1
>                              bathroomT g 1 pqueC brC
>
> bathroomT genre n pqueC brC = do
>   (pque, (mv, g)) <- atomically $ nextInLineT pqueC
>   putStrLn ("La cola de espera es:" ++ show pque)
>   case g of
>       Cleaning -> do waitTillEveryoneLeavesT genre brC n
>                      atomically $ writeTVar mv True
>                      putStrLn "Entra limpieza"
>                      br <- atomically $ readTChan brC
>                      putStrLn br
>                      bathroomHandlerT pqueC brC
>       _        -> do if g == genre
>                      then do atomically $ writeTVar mv True
>                              showPeopleIn g (n + 1)
>                              bathroomT g (n + 1) pqueC brC
>                      else do waitTillEveryoneLeavesT genre brC n
>                              atomically $ writeTVar mv True
>                              showPeopleIn g 1
>                              bathroomT g 1 pqueC brC

\end{lstlisting}


\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
}
}
\\

\begin{lstlisting}

> takeSeqP ss = case viewl ss of
>                    EmptyL  -> (ss, Nothing)
>                    x :< xs -> do (xs, Just (x, xs))
>
> nextInLineT pqueC = do
>   q <- readTVar pqueC
>   let (q', m) = takeSeqP q
>   writeTVar pqueC q'
>   case m of
>       Nothing -> retry
>       Just (T (mv, g), rest) -> do return (q, (mv, g))
>
> waitTillEveryoneLeavesT :: Genre -> TChan String -> Int -> IO ()
> waitTillEveryoneLeavesT genre brC 0 = return ()
> waitTillEveryoneLeavesT genre brC n = do
>   br <- atomically $ readTChan brC
>   putStrLn br
>   showPeopleIn genre (n - 1)
>   waitTillEveryoneLeavesT genre brC (n-1)
>

\end{lstlisting}


\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
}
}
\\

\begin{lstlisting}

> unlock tv = do b <- readTVar tv
>                if b then return()
>                else retry
>
> cleaningThreadT pqueC brC = do
>   mv <- newTVarIO False
>   atomically $ modifyTVar pqueC (\ss -> ( T (mv, Cleaning) <| ss))
>   putStrLn "La limpieza quiere entrar.."
>   mv' <- atomically $ unlock mv
>   clean
>   atomically $ writeTChan brC "Listo! Ya Limpie"
>
> genreThreadT genre pqueC brC = do
>   mv <- newTVarIO False
>   atomically $ modifyTVar pqueC (\ss -> (ss |> T (mv, genre)))
>   mv' <- atomically $ unlock mv
>   useBathroom
>   atomically $ writeTChan brC ("Listo! (" ++ show genre ++ ")")

\end{lstlisting}


\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
peopleInLine serán la función del hilo que
crea a las personas. Tiene un delay para evitar
que la cola de espera crezca demasiado rápido.
}
}
\\

\begin{lstlisting}

> peopleInLineT pqueC brC = do
>   r <- randomRIO (0, 1.0) :: IO Double
>   let g = people r
>   if g == Cleaning
>   then forkIO (cleaningThreadT pqueC brC)
>   else forkIO (genreThreadT g pqueC brC)
>   r' <- randomRIO (80000, 200000)
>   threadDelay r'
>   peopleInLineT pqueC brC

\end{lstlisting}

\begin{lstlisting}

> ------------------------------------------------------------------------ intento
> newtype GenreInfo2 = I (Int, Genre) deriving Show
>
> type Ticket = TVar Int -- Numero de ticket
>
> takeTicket t = do i <- readTVar t
>                   writeTVar t (i + 1)
>                   return(i)
>
> type Serve = TVar Int -- Numero de ticket de persona a atender
>
> wait2BServe s t = do s' <- readTVar s
>                      if s' >= t then return ()
>                      else retry
>
> type LineT = TVar (Seq GenreInfo2)
> type DoneT = TChan String
>
> bathroomHandler2 ser pqueT brT = do
>   (pque, (t, g)) <- atomically $ nextInLine2 pqueT
>   putStrLn ("La cola de espera es:" ++ show pque)
>   atomically $ writeTVar ser t
>   case g of
>       Cleaning -> do putStrLn "Entra limpieza"
>                      br <- atomically $ readTChan brT
>                      bathroomHandler2 ser pqueT brT
>       _        -> do showPeopleIn g 1
>                      bathroom2 g 1 ser pqueT brT
>
> bathroom2 genre 3 ser pqueT brT = do
>   (pque, (t, g)) <- atomically $ nextInLine2 pqueT
>   putStrLn ("La cola de espera es:" ++ show pque)
>   case g of
>       Cleaning -> do waitTillEveryoneLeavesT genre brT 3
>                      atomically $ writeTVar ser t
>                      putStrLn "Entra limpieza"
>                      br <- atomically $ readTChan brT
>                      putStrLn br
>                      bathroomHandler2 ser pqueT brT
>       _        -> do if g == genre
>                      then do br <- atomically $ readTChan brT
>                              putStrLn br
>                              showPeopleIn g 2
>                              atomically $ writeTVar ser t
>                              showPeopleIn g 3
>                              bathroom2 g 3 ser pqueT brT
>                      else do waitTillEveryoneLeavesT genre brT 3
>                              atomically $ writeTVar ser t
>                              showPeopleIn g 1
>                              bathroom2 g 1 ser pqueT brT
>
> bathroom2 genre n ser pqueT brT = do
>   (pque, (t, g)) <- atomically $ nextInLine2 pqueT
>   putStrLn ("La cola de espera es:" ++ show pque)
>   case g of
>       Cleaning -> do waitTillEveryoneLeavesT genre brT n
>                      atomically $ writeTVar ser t
>                      putStrLn "Entra limpieza"
>                      br <- atomically $ readTChan brT
>                      putStrLn br
>                      bathroomHandler2 ser pqueT brT
>       _        -> do if g == genre
>                      then do atomically $ writeTVar ser t
>                              showPeopleIn g (n + 1)
>                              bathroom2 g (n + 1) ser pqueT brT
>                      else do waitTillEveryoneLeavesT genre brT n
>                              atomically $ writeTVar ser t
>                              showPeopleIn g 1
>                              bathroom2 g 1 ser pqueT brT
>
> nextInLine2 pqueC = do
>   q <- readTVar pqueC
>   let (q', m) = takeSeqP q
>   writeTVar pqueC q'
>   case m of
>       Nothing -> retry
>       Just (I (t, g), rest) -> do return (q, (t, g))
>
> cleaningThread2 ser pqueT brT = do
>   atomically $ modifyTVar pqueT (\ss -> ( I (1, Cleaning) <| ss))
>   putStrLn "La limpieza quiere entrar.."
>   atomically $ wait2BServe ser 1
>   useBathroom
>   atomically $ writeTChan brT "Listo! Ya Limpie"
>
> genreThread2 :: Genre -> Ticket -> Serve -> TVar (Seq GenreInfo2)
>                 -> TChan String -> IO ()
> genreThread2 genre tic ser pqueT brT = do
>   t <- atomically $ takeTicket tic
>   atomically $ modifyTVar pqueT (\ss -> (ss |> I (t, genre)))
>   atomically $ wait2BServe ser t
>   putStrLn $ show genre ++ show t ++" entro al ba;o"
>   useBathroom
>   atomically $ writeTChan brT ("Listo! (" ++ show genre ++ ")")
>
> peopleInLine2 tic ser pqueT brT = do
>   r <- randomRIO (0, 1.0) :: IO Double
>   let g = people r
>   if g == Cleaning
>   then forkIO (cleaningThread2 ser pqueT brT)
>   else forkIO (genreThread2 g tic ser pqueT brT)
>   r' <- randomRIO (80000, 200000)
>   threadDelay r'
>   peopleInLine2 tic ser pqueT brT

\end{lstlisting}


\end{document}
