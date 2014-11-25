module Twinkle where
import Haskore

-- note updaters for mappings
fd d n = n d v
vol  n = n   v
v      = [Volume 80]

-- repeat something n times
times  1    m = m
times n m = m :+: (times (n - 1) m)

-- Main Voice:
durationPattern=replicate 6 qn++[hn]
nPa=[c 5, c 5, g 5, g 5, a 6, a 6, g 5]
nPb=[f 5, f 5, e 5, e 5, d 5, d 5, c 5]
nPc=[g 5, g 5, f 5, f 5, e 5, e 5, d 5]

v1ab= v1a :+: v1b

fline=line.zipWith fd durationPattern

v1a = fline nPa;  
v1b = fline nPb;  
v1c = fline nPc;  

mainVoice  = v1ab :+: times 2 v1c :+: v1ab
 
-- Putting it all together:
twinkle = Instr "piano" (Tempo 4 (Phrase [Dyn SF] mainVoice))
