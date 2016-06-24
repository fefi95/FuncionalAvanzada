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

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Para resolver el problema del baño unisex se define
un tipo de dato para el género de las personas, si
son mujeres, hombres o personal de limpieza.
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

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
También se define un tipo de dato para modelar la información
que estará en la cola del baño. El MVar de este tipo representa
el turno de la persona en cola, es decir, cuando se le permitirá
entrar al baño. El Genre representa el tipo de persona a entrar.
}
}
\\

\section{El baño unisex con MVar}

\begin{lstlisting}

> newtype GenreInfo = G (MVar Bool, Genre)
>
> instance Show GenreInfo where
>   show (G (mvar, genre)) = show genre
>

\end{lstlisting}

\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
Se define las funciones relacionados con el manejo de los
turnos en el baño bathroomHandler y bathroom. La primera
de estas deja entrar a la primera persona de la cola de
espera y llama a la función bathroom con el género
apropiado. \\

bathroom chequea al primero en la cola y actúa según el
género de los que están en el baño, si son el mismo género
y no hay más de tres personas dentro de él, los deja pasar.
De lo contrario espera a que se libere el baño para dejarlo
pasar.\\

La cola en ambas funciones es representada por pqueC y
al terminar de "liberar sus demonios" cada persona
notifica que está abandonando el baño por el canal
brC.

}
}
\\

\begin{lstlisting}

> bathroomHandler :: MVar (Seq GenreInfo) -> Chan String -> IO ()
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
> bathroom :: Genre -> Int -> MVar (Seq GenreInfo) -> Chan String
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
se definen las siguientes funciones auxiliares. takeSeq
toma un elemento de la cola, si hay alguno devuelve la
cabeza y el resto, si está vació devuelve Nothing.

nextInLine se vale de la función anterior para otorgar
el turno a la siguiente persona en la cola.

waitTillEveryoneLeaves pretende esperar a que salgan
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
> nextInLine pqueC = do
>   m <- modifyMVar pqueC takeSeq
>   case m of
>       Nothing -> nextInLine pqueC
>       Just (G (mv, g), rest) -> do return (mv, g)
>
> showPeopleIn genre n = putStrLn ("Hay " ++ show n ++ " "
>                                 ++ show genre ++ " en el baño")
>
> waitTillEveryoneLeaves :: Genre -> Chan String -> Int -> IO ()
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
cleaningThread y genreThread son las funciones para
simular en comportamiento de la gente que usa el baño.
Indican que están en la cola y esperan su turno, hacen
lo que deben hacer y notifican que se fueron. En el caso
de la limpieza se colocan al principio de la cola como
lo exige el enunciado.
}
}
\\

\begin{lstlisting}

> cleaningThread pqueC brC = do
>   mv <- newEmptyMVar :: IO (MVar Bool)
>   modifyMVar_ pqueC (\ss -> return ( G (mv, Cleaning) <| ss))
>   putStrLn "La limpieza quiere entrar.."
>   mv' <- takeMVar mv
>   clean
>   writeChan brC "Listo! Ya Limpie"
>
> genreThread genre pqueC brC = do
>   mv <- newEmptyMVar :: IO (MVar Bool)
>   modifyMVar_ pqueC (\ss -> return (ss |> G (mv, genre)))
>   mv' <- takeMVar mv
>   useBathroom
>   writeChan brC ("Listo! (" ++ show genre ++ ")")
>
> clean = do r <- randomRIO (100000, 500000)
>            threadDelay r
>
> useBathroom = do r <- randomRIO (100000, 500000)
>                  threadDelay r
>

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

> peopleInLine pqueC brC = do
>   r <- randomRIO (0, 1.0) :: IO Double
>   let g = people r
>   if g == Cleaning
>   then forkIO (cleaningThread pqueC brC)
>   else forkIO (genreThread g pqueC brC)
>   r' <- randomRIO (80000, 200000)
>   threadDelay r'
>   peopleInLine pqueC brC
>
> people r
>   |r > 0.0 && r < 0.49  = Women
>   |r > 0.49 && r < 0.98 = Men
>   |r > 0.98 && r < 1.0  = Cleaning
>   |otherwise = Cleaning
>

\end{lstlisting}

\section{El baño unisex con TVar}

\begin{lstlisting}

> type Semaphore = TVar Bool
>
> newSem available = newTVarIO available
>
> lock sem = do b <- readTVar sem
>               if b then writeTVar sem False
>               else retry
>
> unlock sem = writeTVar sem True
>
> waitUnlock sem = do b <- readTVar sem
>                     if b then return()
>                     else retry

\end{lstlisting}

\begin{lstlisting}

> bathroomHandlerT :: Semaphore -> Semaphore -> TVar (Seq Genre) -> TChan String -> IO ()
> bathroomHandlerT wS mS pqueC brC = do
>   (g, pque) <- atomically $ nextInLineT pqueC
>   putStrLn ("La cola de espera es:" ++ show pque)
>   case g of
>       Cleaning -> do putStrLn "Entra limpieza2"
>                      atomically $ lock mS >> lock wS
>                      br <- atomically $ readTChan brC
>                      atomically $ unlock mS >> unlock wS
>                      bathroomHandlerT wS mS pqueC brC
>       Women    -> do atomically $ lock mS
>       Men      -> do atomically $ lock wS
>   showPeopleIn g 1
>   bathroomT g 1 wS mS pqueC brC
>
> bathroomT :: Genre -> Int -> Semaphore -> Semaphore -> TVar (Seq Genre)
>              -> TChan String -> IO ()
> bathroomT genre 3 wS mS pqueC brC = do
>   atomically $ lock wS
>   atomically $ lock wS
>   (g, pque) <- atomically $ nextInLineT pqueC
>   putStrLn ("La cola de espera es:" ++ show pque)
>   case g of
>       Cleaning -> do atomically $ lock mS >> lock wS
>                      waitTillEveryoneLeavesT genre brC 3
>                      putStrLn "Entra limpieza1"
>                      br <- atomically $ readTChan brC
>                      putStrLn br
>                      atomically $ unlock mS >> unlock wS
>                      bathroomHandlerT wS mS pqueC brC
>       _        -> do if g == genre
>                      then do case g of
>                               Women -> do atomically $ lock wS
>                                           br <- atomically $ readTChan brC
>                                           putStrLn br
>                                           atomically $ unlock wS
>                               Men   -> do atomically $ lock mS
>                                           br <- atomically $ readTChan brC
>                                           putStrLn br
>                                           atomically $ unlock mS
>                              showPeopleIn g 2
>                              showPeopleIn g 3
>                              bathroomT g 3 wS mS pqueC brC
>                      else do case genre of
>                               Women -> do atomically $ lock wS
>                                           waitTillEveryoneLeavesT genre brC 3
>                                           atomically $ unlock mS
>                               Men   -> do atomically $ lock mS
>                                           waitTillEveryoneLeavesT genre brC 3
>                                           atomically $ unlock wS
>                              showPeopleIn g 1
>                              bathroomT g 1 wS mS pqueC brC
>
> bathroomT genre n wS mS pqueC brC = do
>   (g, pque) <- atomically $ nextInLineT pqueC
>   putStrLn ("La cola de espera es:" ++ show pque)
>   case g of
>       Cleaning -> do atomically $ lock mS >> lock wS
>                      waitTillEveryoneLeavesT genre brC n
>                      putStrLn "Entra limpieza"
>                      br <- atomically $ readTChan brC
>                      putStrLn br
>                      atomically $ unlock mS >> unlock wS
>                      bathroomHandlerT wS mS pqueC brC
>       _        -> do if g == genre
>                      then do showPeopleIn g (n + 1)
>                              bathroomT g (n + 1) wS mS pqueC brC
>                      else do case genre of
>                               Women -> do atomically $ lock wS
>                                           waitTillEveryoneLeavesT genre brC n
>                                           atomically $ unlock mS
>                               Men   -> do atomically $ lock mS
>                                           waitTillEveryoneLeavesT genre brC n
>                                           atomically $ unlock wS
>                              showPeopleIn g 1
>                              bathroomT g 1 wS mS pqueC brC

\end{lstlisting}


\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
}
}
\\

\begin{lstlisting}

> takeSeqT :: Seq Genre ->
>            IO (Seq Genre, Maybe (Genre, Seq Genre))
> takeSeqT ss = do
>   case viewl ss of
>        EmptyL  -> return (ss, Nothing)
>        x :< xs -> do putStrLn ("La cola de espera es:"
>                               ++ show ss)
>                      return (xs, Just (x, xs))
>
> takeSeqP :: Seq Genre -> Maybe (Genre, Seq Genre)
> takeSeqP ss = case viewl ss of
>                    EmptyL  -> Nothing
>                    x :< xs -> Just (x, xs)
>
> nextInLineT pqueC = do
>   que <- readTVar pqueC
>   let m = takeSeqP que
>   case m of
>       Nothing -> retry
>       Just (g, que') -> do writeTVar pqueC que'
>                            return (g, que)
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

> cleaningThreadT :: TVar (Seq Genre) -> TChan String -> IO ()
> cleaningThreadT pqueC brC = do
>   atomically $ modifyTVar pqueC (\ss -> Cleaning <| ss)
>   putStrLn "La limpieza quiere entrar.."
>   clean
>   atomically $ writeTChan brC "Listo! Ya Limpie"
>
> genreThreadT :: Genre -> Semaphore ->
>                 TVar (Seq Genre) -> TChan String -> IO ()
> genreThreadT genre s pqueC brC = do
>   atomically $ modifyTVar pqueC (\ss -> ss |> genre)
>   atomically $ waitUnlock s
>   putStrLn $ show genre ++ " usa el ba;o"
>   useBathroom
>   atomically $ writeTChan brC ("Listo! (" ++ show genre ++ ")")

\end{lstlisting}


\noindent
\colorbox{lightorange}{
\parbox{\linewidth}{
}
}
\\

\begin{lstlisting}

> peopleInLineT wS mS pqueC brC = do
>   r <- randomRIO (0, 1.0) :: IO Double
>   let g = people r
>   case g of
>       Cleaning -> forkIO (cleaningThreadT pqueC brC)
>       Women    ->  forkIO (genreThreadT g wS pqueC brC)
>       Men      ->  forkIO (genreThreadT g mS pqueC brC)
>   r' <- randomRIO (8000, 200000)
>   threadDelay r'
>   peopleInLineT wS mS pqueC brC

\end{lstlisting}



> main = do
>   (sim : _) <- getArgs
>   case sim of
>       "Clasica" -> do pqueC <- newMVar $ empty
>                       brC <- newChan
>                       forkIO (peopleInLine pqueC brC)
>                       bathroomHandler pqueC brC
>       "STM"     -> do pqueT <- newTVarIO empty
>                       brT <- atomically newTChan
>                       wS <- newSem True
>                       mS <- newSem True
>                       forkIO (peopleInLineT wS mS pqueT brT)
>                       bathroomHandlerT wS mS pqueT brT
>       _         -> putStrLn ("USO: las simulaciones "
>                             ++ "disponibles son "
>                             ++ "\"Clasica\" y \"STM\"")

\end{lstlisting}

\end{document}
