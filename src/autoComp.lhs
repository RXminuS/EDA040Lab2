\section{Convenient Functions for Accompaniments}
\label{accomp}

{\small
\begin{verbatim}

> module AutoComp where
> import Haskore hiding (chord, Key)
> import Data.List
> import Data.Maybe
>
>
> type Key = (PitchClass, Mode)
> data Triad = TriMaj | TriMin deriving (Eq)
> type Chord = (PitchClass, Triad)
> data BassStyle = Basic | Calypso | Boogie deriving (Eq)
> type ChordProgression = [(Chord,Dur)]
> type ChordPitch = (Pitch, Pitch, Pitch)
>
> -- note updaters for mappings
> fd d n = n d v
> vol  n = n   v
> v      = [Volume 60]
>
> -- repeat something n times
> times 1     m = m
> times n m = m :+: (times (n - 1) m)
>
> --triadPattern :: Triad -> [Int]
> --triadPattern TriMaj = [0,4,7]
> --triadPattern TriMin = [0,3,7]
>
> scalePattern :: Key ->[PitchClass]
> scalePattern (pc, Major) = fst.unzip.zipWith trans [0, 2, 4, 5, 7, 9, 11] $ repeat (pc, 3)
> scalePattern (pc, Minor) = fst.unzip.zipWith trans [0, 2, 3, 5, 7, 8, 10] $ repeat (pc, 3)
>
> testScalePattern = scalePattern (A, Major)
>
> autoBass :: BassStyle-> Key -> ChordProgression -> Music
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
> shift :: Eq a => Int -> [a] -> [a]
> shift n l= (iterate f l)!!n
>		where f [] = []
>		      f (x:xs) = xs ++ [x]
>
> ------------------------------------ Chords -------------------------------------
>
> --chordPattern :: Key ->[Pitch]
> --chordPattern (pc, Major) = zipWith trans [0, 2, 4, 5, 7, 9, 11,12,14,16,17,19] $ repeat (pc, 4)
> --chordPattern (pc, Minor) = zipWith trans [0, 2, 3, 5, 7, 8, 10,12,14,15,17,19] $ repeat (pc, 4)
>
> autoChord :: Key -> ChordProgression -> Music
> autoChord _ cp@((c,_):_) = foldl (\acc x -> acc :+: x) (Rest 0) (combine ((permutate c)!!0) cp)
>                          where combine last ((c,d):[]) = toChord d (minimize last (permutate c)):[]
>                                combine last ((c,d):cp) = (toChord d (minimize last (permutate c)) ):(combine (minimize last (permutate c)) cp)
>
>
> --chordChart :: Key -> [ChordPitch]
> --chordChart key =  perm1 ++ perm2 ++ perm3
> --           where  perm1 = take 7 $ zip3 limitedRange (shift 2 limitedRange) (shift 4 limitedRange)
> --                  perm2 = take 7 $ zip3 limitedRange (shift 2 limitedRange) (shift 5 limitedRange)
> --                  perm3 = take 7 $ zip3 limitedRange (shift 3 limitedRange) (shift 5 limitedRange)
> --                  limitedRange = chordPattern key
>
>
> toChord :: Dur -> ChordPitch -> Music
> toChord dur (a,b,c) = foldl (\acc x -> acc :=: (fd dur $ Note x) ) (Rest 0) [a,b,c]
>
> (|-|) :: Pitch -> Pitch -> Int
> (|-|) a b = abs (absPitch a - absPitch b)
>
> distance :: ChordPitch -> ChordPitch -> Int
> distance (a1, b1, c1) (a2, b2, c2) = (a1|-|a2) + (b1|-|b2) + (c1|-|c2)
>
> minimize :: ChordPitch -> [ChordPitch] -> ChordPitch
> minimize prev (x:xs) = foldl (\acc y -> if distance prev y < distance prev acc then y else acc) x xs
> --pick :: Chord -> [ChordPitch] -> [ChordPitch]
> --pick (p, t) _ = filter f
> --           where f _= zipWith elem notes
> --                 notes = fst $ unzip (zipWith trans t (repeat (p, 3)))
>
>
> permutate :: Chord -> [ChordPitch]
> permutate (p, t)
>           | t == TriMin = map toTuple (zipWith (zipWith trans) minPat rootNote)
>           | otherwise = map toTuple (zipWith (zipWith trans) majPat rootNote)
>           where toTuple [a,b,c] = (a,b,c)
>                 minPat = [[0,3,7],[3,7,12],[7,12,15]]
>                 majPat = [[0,4,7],[4,7,12],[7,12,16]]
>                 rootNote = (repeat.repeat) (p,4)
> --TODO: ADD RANGE CONSTRAINTS
> -- >> pick :: Chord -> [ChordPitch] -> [ChordPitch]
> -- > pick (p, t) _ = filter f
> -- >            where f _= zipWith elem notes
> -- >                  notes = fst $ unzip (zipWith trans t!!1 (repeat (p, 4)))
> -- >
> -- >(zipWith trans triadPattern t!!1 (repeat (p, 4)))
> -- >(zipWith trans triadPattern t!!1 (repeat (p, 4)))
> -- >(zipWith trans triadPattern t!!1 (repeat (p, 4)))
> -- >
> -- > map (zipWith trans.flip (repreat (p,4)) ) (triadPattern t)
>
>

\end{verbatim} }
