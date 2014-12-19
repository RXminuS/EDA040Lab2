\section{AutoComp Documentation}

\subsection{Overview:}

When faced with the challenge of composing accompanying music there are two 'Keys' to take into consideration. The key of the song, the key of a chord/bass. These keys aren't independant of each other and the purpose of this program is to harmonize these keys in such a way that the result is perceived as pleasant.

This documentation doesn't aim to give a full musical-theory background (which is beyond the scope of the writers) but rather to explain the different steps used to create the accompanying track.

To achieve the holy grail of a pleasant result there are two main tools at our disposal. The first is chord voicing, where by picking a 'good' permutation of a chord it will fit within the musical context. The second tool used is different bass-picking styles where starting from a bass root note (defined by the key of the chord) we play notes with a well-defined pitch offset.

\subsection{Imports:}

> module AutoComp where
> import Haskore hiding (chord, Key)
> import Data.List
> import Data.Maybe

\subsection{Data Structures:}
As mentioned before there are two main keys to consider when composing music. Each of these keys accompanied with a scale (Major or Minor) defines a set of notes that can be chosen to fit in the musical context. To represent the key (and Major/Minor scale) the song is played in we use the following data type

> type Key = (PitchClass, Mode)

This key is also used to generate accompanying chords. Each chord (at least in our implementation) consists of tree notes, a root note and two harmonic notes. Depending on what scale the song is played in (Major or Minor) the harmonic notes differ. This is why we introduced the Triad data type so that we can create the correct scale notes, where Triad is defined as:

> data Triad = TriMaj | TriMin deriving (Eq)

By using this Triad class we can define our chords as a combination of a root note and a triad (that fits the scale of the song). By playing each chord for a certain duration and combining several after each other we can define the chord progression of the song. The type classes to represent this are as follows:

> type Chord = (PitchClass, Triad)
> type ChordProgression = [(Chord,Dur)]


Finally, when generating chord voicings we need some way of specifying the individual notes that make up a Chord. Our generic Chord class isn't suited for this so instead we introcduce the ChordPitch class witch is a container that suits this purpose.

> type ChordPitch = [Pitch]

This concludes all the data structures needed to generate the correct chord voicings for the song. However a song with just melodie and chords would still feel empty...because it's all about dat bass! In our implementation we have restricted ourselves to three different bass styles, each with their own unique 'picking pattern'. With a picking pattern we simply mean the notes and timings used to play the particular bass style. But the actual generation of notes for the bass style is discussed later, for now all we have to be concerned about is finding a suitable data class to represent the different bass styles which we chose to be:

> data BassStyle = Basic | Calypso | Boogie deriving (Eq)


\subsection{Chord Voicing Generation}

The process of generating chord voicing for each of the chords in the song has been broken into several iterative steps. First of all we generate all the possible permitation/voicings of a chord after which we select the voicing that lies as close as possible to the previous chord selected. This way we end up with a chord progression for the entire song which in a final step is transformed from the data classes previously presented into 'playable' music classes.

The first step is achieved by the permutate function which returns all the possible chord voicings within a limited range of pitches. Since this function is only used for chord voicing the root note of is fixed to the 4th octave. It enforce that the first rule in the solution requirements is respected. The third rule is respected also because the hardcoded patterns always generate the closest voicing within the triad. We chose these explicit patterns because even though not eastatically pleasing from a programming perspective they make the musical implications clearer.

> permutate :: Chord -> [ChordPitch]
> permutate (p, t) =  zipWith (zipWith trans) (pattern t) rootNote
>    where pattern TriMaj = [[0,3,7],[3,7,12],[7,12,15]]
>          pattern TriMin = [[0,4,7],[4,7,12],[7,12,16]]
>          rootNote = (repeat.repeat) (p,4)


The second iterative step requires us to define a minimal distance between chords which we simply defined as the difference between pitches between corresponding notes between two chords. First we defined a distance operator to give this difference in pitch between Pitch classes. Secondly we defined the distance function which applies this function to each of a chords notes. Since we alway represent chords in a uniform matter (lowest notes to the left) this function returns a correct value. And lastly we defined a mini function that uses the distance function to select the closest lying chord from a list of chords.

> (|-|) :: Pitch -> Pitch -> Int
> (|-|) a b = abs (absPitch a - absPitch b)

> distance :: ChordPitch -> ChordPitch -> Int
> distance cp1 cp2 = sum $ zipWith (|-|) cp1 cp2

> mini :: ChordPitch -> [ChordPitch] -> ChordPitch
> mini prev (x:xs) =
>   foldl (\acc y -> if distance prev y < distance prev acc then y else acc) x xs

Finaly the autoChord function combines the power of 'mini' and permutate by for each chord in the chord progression thereby picking the voicing that lies closest to the previously played chord. The first chord played will always have its root in the lowest note since we select the first item from all available permutations.

> autoChord :: Key -> ChordProgression -> Music
> autoChord _ cp@((c,_):_) =
>   foldl (\acc x -> acc :+: x) (Rest 0) (combine ((permutate c)!!0) cp)
>   where combine last ((c,d):[]) =
>           toChord d (mini last (permutate c)):[]
>         combine last ((c,d):cp) =
>           (toChord d (mini last (permutate c)) ):(combine (mini last (permutate c)) cp)

The combine helper function recusively applies this chord selection algorithm to the end of the song and then the main foldl uses the chosen chords to transform into the final Music. To achieve this it relies on the toChord function which transforms a ChordPitch into Music by applying a specific duration and transforming each individual Pitch into a corresponding Note. Since the ChordPitch is a much more appropriate datatype than Music to use during calculations this function bridges the abstraction layer it imposes.

> toChord :: Dur -> ChordPitch -> Music
> toChord dur cp = foldl (\acc x -> acc :=: (fd dur $ Note x) ) (Rest 0) cp

The fd helper function simply applies a predefined volume to each note thus making our solution more DRY.

> fd d n = n d v
> vol  n = n   v
> v      = [Volume 60]

\subsection{Bass Line Generation}

Again the process has been broken down into several iterative steps. First of all we generate a list of notes that fit with the key of the song and the chord being played. For the musical theory behind this we again refer to the lab-documentation. Special care is taken that this list or scale of notes is an increasing scale and where necessary increases the octave of a note. In a second step we, based on the bass style, pick certain positions in this scale as notes to be played and apply timings appropriate for the bass style. And finally this information is converted into a 'playable' Music class.


To achieve the first step in this process we defined the chordMode function. In the first step of this funciton we shift the scale pattern (of the song key) to the root note of the chord. For instance the scale C3-D3-E3-F3-G3-A3-B3 when applied to a GMaj chord becomes G3-A3-B3-C3-D3-E3-F3. Notice that that the pitches aren't increasing correctly since we simply shift and aren't transposing the notes. Therefore the octaveSplit and cleanOctave functions are used to first identify where this indescrepency happens and then to fix it. So G3-A3-B3-C3-D3-E3-F3 finally becomes G3-A3-B3-C4-D4-E4-F4.

> chordMode :: Key -> Chord -> [Dur->[NoteAttribute]-> Music]
> chordMode key (pc,_) =
>    map Note $ cleanOctave $ zip transformedPitches (repeat 3)
>   where f :: Maybe Int -> [PitchClass]
>         f (Just a) = shift a (scalePattern key)
>         f _  =
>           take 12 $ repeat pc
>         transformedPitches :: [PitchClass]
>         transformedPitches =
>           f (elemIndex pc (scalePattern key))
>         octaveSplit :: [Pitch] -> ([Pitch], [Pitch])
>         octaveSplit scale =
>           partition (octaveSplitTest scale) scale
>         octaveSplitTest :: [Pitch] -> Pitch -> Bool
>         octaveSplitTest scale p =
>           absPitch p >= (head $ map absPitch scale)
>         cleanOctave :: [Pitch] -> [Pitch]
>         cleanOctave scale =
>           (fst.octaveSplit) scale ++ map (trans 12) (snd.octaveSplit $ scale)

to obtain this scale pattern in the key of the song we wrote the scale pattern function which simply transposes the root note of the scale until we have an entire Major or Minor scale.

> scalePattern :: Key ->[PitchClass]
> scalePattern (pc, Major) =
>    fst.unzip.zipWith trans [0, 2, 4, 5, 7, 9, 11] $ repeat (pc, 3)
> scalePattern (pc, Minor) =
>    fst.unzip.zipWith trans [0, 2, 3, 5, 7, 8, 10] $ repeat (pc, 3)

Once again our focus was on trying to keep the musical implications clear thus diminishing the influence of programmatic 'correctness'. For instance, since this function is only used for the genration of the base line we hardcoded the root note to be in the third octave. Also note that we have a special case in the chordMode function where a chord can't be represented in the 'official' scale of the song. In this case the function 'f _' will simply return a scale that only consists of the root note of this chord and thus the bass notes will be monotonous (although appropriate timings will still be applied). This wasn't a problem for the two songs that we chose but it is something that needs to be improved if this function were to be applied on a broader range of music.

In the final step the autoBass function applies a note picking pattern and timing function to the scale obtained in the previous step. The picking pattern consists of the semitone differences between the root note. This picking pattern further depends on the duration of the chord where the only cases handled are where the chord is a 'wn' (Whole Note) or a 'hn' (Half Note). This of course is an extremely crude implementaition but our focus when writing this function was making the requirements stated in the lab-document clear from a musical perspective not necesserally favoring broadly applicable code. In retrospect this was a unfortunate decision but restructuring the code would in essence mean rewriting most of the program. Since it serves the purpose of the lab we decided to maintain our solution but hope that this clarifies our thinking process.

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


\subsection{Final Notes and Thoughts}
There are two helper function that are used throught our solution that we haven't mentioned yet.

> times 1     m = m
> times n m = m :+: (times (n - 1) m)

Which simply repeats something n times. And lastly

> shift :: Eq a => Int -> [a] -> [a]
> shift n l= (iterate f l)!!n
>       where f [] = []
>             f (x:xs) = xs ++ [x]

Which shifts a list n times in a circular fashion, thus moving the last element to the front upon each shift.

Also, we haven't implemented the autoComp function because during the experiments with different volumes and instruments we found that we needed the freedom to choose these individually for the bass and chord lines. Therefore we chose to simply generate and combine these lines in the music files 'twinkle.hs' and 'clocks.hs'.

As a final note we would like to reference this blog post which for us summarises the process of working with literate Haskell:

https://unspecified.wordpress.com/2010/06/04/literate-programming-is-a-terrible-idea/

\end{document}