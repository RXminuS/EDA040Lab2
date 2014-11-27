\section{Convenient Functions for Accompaniments}
\label{accomp}

{\small
\begin{verbatim} 

> module AutoComp where
> import Haskore hiding (chord, Key) 
>  
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
> times  1    m = m
> times n m = m :+: (times (n - 1) m)
>
> triadPattern :: Triad -> [Int]
> triadPattern TriMaj = [0,4,7]    
> triadPattern TriMin = [0,3,7]    
> 
> --chord :: Chord -> Dur -> Music
> --chord (pitch, triad) _ = foldr1 (:=:) zipWith trans (triadPattern triad) pitch  	  
>
> autoBass :: BassStyle-> Key -> ChordProgresion -> Music
> --autoBass _ _ _ = Instr "bass" (Note (C, 4) qn [Volume 80]) 
> autoBass Basic _ chordP = line $ zipWith fd [hn,hn] [c 4, g 4]  
> autoBass Calypso _ chordP = times 2 (Rest qn :+: (line $ zipWith fd [en,en] [c 4, g 4])) 
> autoBass Boogie _ chordP = times 2 (line $ zipWith fd [en,en,en,en] [c 4, g 4,a 4, g 4])
>
> chordMode Key -> Chord -> [(Dur->[NoteAttribute])]
> chordMode =  
>
> shift Int -> [PitchClass] -> [PitchClass]
> shift n l= (iterate f l)!!n
>		where f [] = [] |
>		      f (x:xs) = xs ++ x  
>
> testAutoBass = autoBass Boogie (C,Major) [((C,TriMaj), wn), ((E, TriMin), wn)]
> 
>

\end{verbatim} }
