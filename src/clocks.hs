module Clocks where
import Haskore
import qualified AutoComp as AC

-- note updaters for mappings
fd d n = n d v
vol  n = n   v
v      = [Volume 90]

-- repeat something n times
times  1    m = m
times n m = m :+: (times (n - 1) m)

-- Main Voice:
durationPattern1 = replicate 8 en
durationPattern2 = replicate 6 en ++ [qn]
nPa=[d 6, a 5, fs 5, d 6, a 5, fs 5, d 6, a 5]
nPb=[c 6, a 5, e 5, c 6, a 5, e 5, c 6, a 5]
nPc=[b 5, g 5, e 5, b 5, g 5, e 5, b 5]


fline1 = line.zipWith fd durationPattern1
fline2 = line.zipWith fd durationPattern2

b1 = fline1 nPa;
b2 = fline1 nPb;
b3 = fline2 nPc;

mainVoice  = b1 :+: times 2 b2 :+: b3

-- Putting it all together:
clocksChords =
    [((D,AC.TriMaj),wn), ((A, AC.TriMin), wn), ((A, AC.TriMin), wn), ((E, AC.TriMin), wn)]

melodie = Instr "Bright Acoustic Piano" (Tempo 2 (Phrase [Dyn SF] mainVoice))
melodie2 = Instr "Xylophone" (Tempo 2 (Phrase [Dyn SF] mainVoice))
bass = Instr "Slap Bass 1" (Tempo 2 (Phrase [Dyn SF] (AC.autoBass AC.Boogie (G ,Major) clocksChords)))
chords = Instr "Distortion Guitar" (Tempo 2 (Phrase [Dyn SF] (AC.autoChord (G,Major) clocksChords)))

testClocks = (melodie :=: melodie2 :=: chords ) :+: (melodie :=: melodie2 :=: bass :=: chords)
-- :=: bass :=: chords