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
> type ChordPitch = (Pitch, Pitch, Pitch)
>
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
>
> autoBass :: BassStyle-> Key -> ChordProgresion -> Music
> autoBass style key [] = Rest 0
> autoBass Basic key (chord:[])
>   | (snd chord == hn) = line $ zipWith fd [hn] [t!!0]
>   | (snd chord == wn) = line $ zipWith fd [hn,hn] [t!!0,t!!4]
>   | otherwise = Rest (snd chord)
>   where t =  chordMode key (fst $ chord)
>
> autoBass Calypso key (chord:[])
>   | (snd chord == hn) = bar
>   | (snd chord == wn) = times 2 bar
>   | otherwise = Rest (snd chord)
>   where bar = Rest qn :+: (line $ zipWith fd [en,en] [t!!0,t!!2])
>         t   = chordMode key (fst $ chord)
>
> autoBass Boogie key (chord:[])
>   | (snd chord == hn) = bar
>   | (snd chord == wn) = times 2 bar
>   | otherwise = Rest (snd chord)
>   where bar = line $ zipWith fd [en,en,en,en] [t!!0,t!!4,t!!5,t!!4]
>         t =  chordMode key (fst $ chord)
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
> testAutoBass = autoBass Boogie (C,Major) [((C,TriMaj), wn), ((F, TriMaj), hn),((C,TriMaj),hn),((G,TriMaj),hn),((C,TriMaj),hn),((G,TriMaj),hn),((C,TriMaj),hn)]
>
>
> ------------------------------------ Chords -------------------------------------
>
> chordPattern :: Key ->[Pitch]
> chordPattern (pc, Major) = zipWith trans [0, 2, 4, 5, 7, 9, 11,12,14,16,17,19] $ repeat (pc, 4)
> chordPattern (pc, Minor) = zipWith trans [0, 2, 3, 5, 7, 8, 10,12,14,15,17,19] $ repeat (pc, 4)
>
> autoChord :: Key -> ChordProgression -> Music
> 
> chordChart :: Key -> [ChordPitch]
> chordChart =  
>
> distance :: ChordPitch -> ChordPitch -> Integer
>
> minimize :: ChordPitch -> [ChordPitch] -> ChordPitch
>
> pick :: Chord -> [ChordPitch]
>
>
>
>
>
>

\end{verbatim} }
