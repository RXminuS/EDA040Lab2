\section{Convenient Functions for Accompaniments}
\label{accomp}

{\small
\begin{verbatim}

> module AutoComp where
> import Haskore hiding (chord, Key)
> import Data.List
> import Data.Maybe
>
> type Key = (PitchClass, Mode)
> data Triad = TriMaj | TriMin
> type Chord = (PitchClass, Triad)
> data BassStyle = Basic | Calypso | Boogie deriving (Eq)
> type ChordProgresion = [(Chord,Dur)]
> --type PitchTriad=(Pich,Pitch,Pitch)
> --type ChordClass = [PitchTriad]
> -- note updaters for mappings
> fd d n = n d v
> vol  n = n   v
> v      = [Volume 80]
>
> -- repeat something n times
> times 1     m = m
> times n m = m :+: (times (n - 1) m)
>
> triadPattern :: Triad -> [Int]
> triadPattern TriMaj = [0,4,7]
> triadPattern TriMin = [0,3,7]
>
> scalePattern :: Key ->[PitchClass]
> scalePattern (pc, Major) = fst.unzip.zipWith trans [0, 2, 4, 5, 7, 9, 11] $ repeat (pc, 3)
> scalePattern (pc, Minor) = fst.unzip.zipWith trans [0, 2, 3, 5, 7, 8, 10] $ repeat (pc, 3)
>
> testScalePattern = scalePattern (A, Major)
> --chord :: Chord -> Dur -> Music
> --chord (pitch, triad) _ = foldr1 (:=:) zipWith trans (triadPattern triad) pitch
>
> autoBass :: BassStyle-> Key -> ChordProgresion -> Music
> --autoBass _ _ _ = Instr "bass" (Note (C, 4) qn [Volume 80])
> autoBass style key [] = Rest 0
> autoBass Basic key (chord:[]) = line $ zipWith fd [hn,hn] [t!!1,t!!5]
> 	where t =  chordMode key (fst $ chord)
>
> autoBass Calypso key (chord:[])
>   | (snd chord == hn) = bar
>   | (snd chord == wn) = times 2 bar
>   | otherwise = Rest (snd chord)
>   where bar = Rest qn :+: (line $ zipWith fd [en,en] [t!!1,t!!3])
>         t   = chordMode key (fst $ chord)
>
> autoBass Boogie key (chord:[]) = times 2 (line $ zipWith fd [en,en,en,en] [t!!1,t!!5,t!!6,t!!5])
> 	where t =  chordMode key (fst $ chord)
> autoBass style key (c:cs) = autoBass style key [c] :+: autoBass style key cs
>
> chordMode :: Key -> Chord -> [Dur->[NoteAttribute]-> Music]
> chordMode key (pc,_) = map Note $ cleanOctave $ zip transformedPitches (repeat 3)
>                        where f (Just a) = shift a (scalePattern key)
>                              f _  = take 12 $ repeat pc
>                              transformedPitches = f (elemIndex pc (scalePattern key))
>                              octaveSplit scale = partition (octaveSplitTest scale) scale
>                              octaveSplitTest scale p = absPitch p >= (head $ map absPitch scale)
>                              cleanOctave scale = (fst.octaveSplit) scale ++ map (trans 12) (snd.octaveSplit $ scale)
> shift :: Int -> [PitchClass] -> [PitchClass]
> shift n l= (iterate f l)!!n
>		where f [] = []
>		      f (x:xs) = xs ++ [x]
>
> testAutoBass = autoBass Boogie (C,Major) [((C,TriMaj), wn), ((F, TriMaj), wn)]
>
>

\end{verbatim} }
