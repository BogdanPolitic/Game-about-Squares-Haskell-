{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import Text.Printf

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West | Nowhere
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"
    show Nowhere = ""

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Type = Square | Circle | Arrow | NULL deriving (Eq, Ord)

instance Show Type where
    show Square = "SQ"
    show Circle = "CL"
    show Arrow = "AR"
    show NULL = "NL"

data Object = Object {
    t :: Type,
    c :: Color,
    h :: Heading
} deriving (Eq, Ord)

fType :: Type -> Char
fType tp = case tp of
    Square -> 'S'
    Circle -> 'C'
    Arrow -> 'A'
    NULL -> 'N'

fColor :: Color -> Char
fColor cl = case cl of
    Red -> 'R'
    Blue -> 'B'
    Gray -> 'G'

fHeading :: Heading -> Char
fHeading hd = case hd of
    North -> '^'
    South -> 'v'
    East -> '>'
    West -> '<'
    Nowhere -> 'N'

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}

symbol :: Object -> [Char]
symbol o = case (fType $ t o) of
    'S' -> (fColor $ c o) : (fHeading $ h o) : [' ']
    'C' -> ' ' : ' ' : [(toEnum (fromEnum (fColor $ c o) + 32)::Char)]
    'A' -> ' ' : ' ' : [(fHeading $ h o)]
    'N' -> "Nil"
    _ -> "Nil"

instance Show Object where
    show o = symbol o

{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}
data Level = Level {
    levelType :: [[((Object, Object), Position)]]
} deriving (Eq, Ord)

data Bounds = Bounds {
    stLine :: Int,
    stCol :: Int,
    eLine :: Int,
    eCol :: Int
} deriving (Eq, Ord)

showBounds :: Bounds -> [Char]
showBounds b = printf "(%d, %d, %d, %d)" (stLine b) (stCol b) (eLine b) (eCol b)

instance Show Bounds where
    show b = showBounds b

overBound :: [[((Object, Object), Position)]] -> Bounds -> Bounds -> [[((Object, Object), Position)]]
overBound m bounds boundsExtended = extend m extension
    where
        stLineDiff = stLine bounds - stLine boundsExtended
        stColDiff = stCol bounds - stCol boundsExtended
        eLineDiff = eLine boundsExtended - eLine bounds
        eColDiff = eCol boundsExtended - eCol bounds
        extension = (Bounds stLineDiff stColDiff eLineDiff eColDiff)

extend :: [[((Object, Object), Position)]] -> Bounds -> [[((Object, Object), Position)]]
extend m extension = extendUp (extendDown (extendLeft (extendRight m (stCol extension) extension) (eCol extension) extension) (stLine extension) extension) (eLine extension) extension

extendRight :: [[((Object, Object), Position)]] -> Int -> Bounds -> [[((Object, Object), Position)]]
extendRight m nbCells ext
    | nbCells == 0 = m
    | otherwise = extendRight (map (\line -> [(nullObjectPair, ((fst (snd (head line))), (snd (snd (head line))) - 1))] ++ line) m) (nbCells - 1) ext
    where
        nullObjectPair = ((Object NULL Red Nowhere), (Object NULL Red Nowhere))

extendLeft :: [[((Object, Object), Position)]] -> Int -> Bounds -> [[((Object, Object), Position)]]
extendLeft m nbCells ext
    | nbCells == 0 = m
    | otherwise = extendLeft (map (\line -> line ++ [(nullObjectPair, ((fst (snd (head (reverse line)))), (snd (snd (head (reverse line)))) + 1))]) m) (nbCells - 1) ext
    where
        nullObjectPair = ((Object NULL Red Nowhere), (Object NULL Red Nowhere))

extendUp :: [[((Object, Object), Position)]] -> Int -> Bounds -> [[((Object, Object), Position)]]
extendUp m nbCells ext
    | nbCells == 0 = m
    | otherwise = extendUp (m ++ [map (\element -> (nullObjectPair, (fst (snd element) + 1, snd (snd element)))) (head (drop (length m - 1) m))]) (nbCells - 1) ext
    where
        nullObjectPair = ((Object NULL Red Nowhere), (Object NULL Red Nowhere))

extendDown :: [[((Object, Object), Position)]] -> Int -> Bounds -> [[((Object, Object), Position)]]
extendDown m nbCells ext
    | nbCells == 0 = m
    | otherwise = extendDown ([map (\element -> (nullObjectPair, (fst (snd element) - 1, snd (snd element)))) (head m)] ++ m) (nbCells - 1) ext
    where
        nullObjectPair = ((Object NULL Red Nowhere), (Object NULL Red Nowhere))

level :: [(Object, Position)] -> Level -- [[((Object, Object), Position)]]
level [] = Level [[(((Object NULL Red Nowhere), (Object NULL Red Nowhere)), (0, 0))]]
level objects = Level (insertObjects objects (levelRecursive objects initialBounds))
    where
        firstObj = head objects
        initialBounds = (Bounds (fst (snd firstObj)) (snd (snd firstObj)) (fst (snd firstObj)) (snd (snd firstObj)))

finalBounds :: [(Object, Position)] -> Bounds -> Bounds
finalBounds objects bounds = case (length objects) of
    0 -> bounds
    _ -> finalBounds (tail objects) (Bounds newStLine newStCol newELine newECol)
    where
        newStLine = min (stLine bounds) (fst (snd (head objects)))
        newStCol = min (stCol bounds) (snd (snd (head objects)))
        newELine = max (eLine bounds) (fst (snd (head objects)))
        newECol = max (eCol bounds) (snd (snd (head objects)))

levelRecursive :: [(Object, Position)] -> Bounds -> [[((Object, Object), Position)]]
levelRecursive [] _ = [[(((Object NULL Red Nowhere), (Object NULL Red Nowhere)), (0, 0))]]
levelRecursive objects bounds
    | (length objects) == 1 = [[(((Object NULL Red Nowhere), (Object NULL Red Nowhere)), (stLine bounds, stCol bounds))]]
    | otherwise = overBound (levelRecursive (drop (length objects - 1) objects) bounds) bounds boundsExtended
    where
        boundsExtended = finalBounds objects bounds

insertObjects :: [(Object, Position)] -> [[((Object, Object), Position)]] -> [[((Object, Object), Position)]]
insertObjects [] m = m
insertObjects objects m = map (\line -> case (fst (snd (head line))) == (fst (snd (head objects))) of -- daca linia (pozitia i) este egala
                                            True -> map (\obj -> case (snd (snd obj)) == (snd (snd (head objects))) of -- daca si j (poz. coloanei) este egala
                                                                    True -> case t (fst (head objects)) of
                                                                        Square -> ((fst (head objects), (snd (fst obj))), snd (head objects))
                                                                        Circle -> (((fst (fst obj)), fst (head objects)), snd (head objects))
                                                                        Arrow -> (((fst (fst obj)), fst (head objects)), snd (head objects))
                                                                        NULL -> (((fst (fst obj)), fst (head objects)), snd (head objects))
                                                                    False -> obj) line
                                            False -> line) (insertObjects (tail objects) m)

{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}
showLevel :: [[((Object, Object), Position)]] -> [Char]
showLevel [] = ""
showLevel lv = case (length lv) of
                1 -> showLine (head lv)
                _ -> showLine (head lv) ++ "\n" ++ showLevel (tail lv)

showLine :: [((Object, Object), Position)] -> [Char]
showLine [] = "" -- nu ar trebui sa se ajunga niciodata aici oricum
showLine ln = case (length ln) of
                1 -> showObject (head ln)
                _ -> showObject (head ln) ++ "|" ++ showLine (tail ln)

showObject :: ((Object, Object), Position) -> [Char]
showObject ((object1, object2), _) = (showObject1 object1) ++ (showObject2 object2)

showObject1 :: Object -> [Char]
showObject1 (Object NULL _ _) = "  "
showObject1 square = (fColor $ c square) : (fHeading $ h square) : ""

showObject2 :: Object -> [Char]
showObject2 (Object NULL _ _) = " "
showObject2 other = case (t other) of
                        Circle -> (toEnum (fromEnum (fColor $ c other) + 32)::Char) : ""
                        Arrow -> (fHeading $ h other) : ""
                        _ -> " "

instance Show Level where
    show l = showLevel (levelType l)

{-
    *** TODO ***

    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level -- probabil ca va trebui in viitor si o functie equals emptylevel, in care sa nu conteze pozitia ((0, 0))
emptyLevel = (Level [[nullObjectPair]])
    where
        nullObject = (Object NULL Red Nowhere)
        nullObjectPair = ((nullObject, nullObject), (0, 0))

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare color heading position l = case l == emptyLevel of
                                        True -> level [(square, position)]
                                        _ -> Level (insertObjects [(square, position)] (overBound m bounds boundsExtended))
    where
        square = (Object Square color heading)
        m = levelType l
        objects = [((Object Square color heading), position)]
        bounds = (Bounds (fst (snd (head (head m)))) (snd (snd (head (head m)))) (fst (snd (head (reverse (head (reverse m)))))) (snd (snd (head (reverse (head (reverse m)))))))
        boundsExtended = finalBounds objects bounds

{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle color position l = case l == emptyLevel of
                                        True -> level [(circle, position)]
                                        _ -> Level (insertObjects [(circle, position)] (overBound m bounds boundsExtended))
    where
        circle = (Object Circle color North)
        m = levelType l
        objects = [((Object Circle color North), position)]
        bounds = (Bounds (fst (snd (head (head m)))) (snd (snd (head (head m)))) (fst (snd (head (reverse (head (reverse m)))))) (snd (snd (head (reverse (head (reverse m)))))))
        boundsExtended = finalBounds objects bounds

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow heading position l = case l == emptyLevel of
                                        True -> level [(arrow, position)]
                                        _ -> Level (insertObjects [(arrow, position)] (overBound m bounds boundsExtended))
    where
        arrow = (Object Arrow Red heading)
        m = levelType l
        objects = [((Object Arrow Red heading), position)]
        bounds = (Bounds (fst (snd (head (head m)))) (snd (snd (head (head m)))) (fst (snd (head (reverse (head (reverse m)))))) (snd (snd (head (reverse (head (reverse m)))))))
        boundsExtended = finalBounds objects bounds
{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}

inBound :: Position -> Level -> Bool
inBound _ (Level []) = False
inBound pos l = ((fst pos) >= highestLine && (snd pos) >= highestColumn && (fst pos) <= lowestLine && (snd pos) <= lowestColumn)
    where
        m = levelType l
        highestLine = (fst (snd (head (head m))))
        highestColumn = (snd (snd (head (head m))))
        lowestLine = (fst (snd (head (head (drop (length m - 1) m)))))
        lowestColumn = (snd (snd (head (drop (length (head m) - 1) (head (drop (length m - 1) m))))))

findSquare :: Level -> Position -> Object -- Nil daca nu gaseste square la pos
findSquare (Level []) _ = (Object NULL Red Nowhere)
findSquare l pos = case (inBound pos l) of
                    True -> fst $ fst (head $ drop ((snd pos) - (snd (snd (head (head m))))) (head $ drop ((fst pos) - (fst (snd (head (head m))))) m)) --
                    _ -> (Object NULL Red Nowhere)
    where
        m = levelType l

removeSquare :: Level -> Position -> Level -- Practic nuleaza primul obiect din perechea de la acea pozitie (daca e nevoie - adica daca nu e NULL). Daca e NULL deja, nu schimba nimic.
removeSquare (Level []) _ = Level []
removeSquare l pos = Level (map (\line -> case (fst (snd (head line))) == (fst pos) of
                                            True -> map (\element -> case (snd (snd element)) == (snd pos) of
                                                                    True -> ((nullObject, (snd (fst element))), (snd element))
                                                                    _ -> element) line
                                            _ -> line) m)
    where
        nullObject = (Object NULL Red Nowhere)
        m = levelType l

specialHeading :: Position -> Level -> Heading
specialHeading pos inLevel = case t arrow of
                                Arrow -> h arrow
                                _ -> Nowhere
    where
        m = levelType inLevel
        arrow = case (inBound pos inLevel) of
                    True -> (snd $ fst (head $ drop ((snd pos) - (snd (snd (head (head m))))) (head $ drop ((fst pos) - (fst (snd (head (head m))))) m)))
                    _ -> (Object Arrow Red Nowhere)

move1 :: Position -> Heading -> Level -> Level -- a NU se aplica in caz ca acolo nu e un square, ci NULL, deoarece doar va nula obiectul urmator (si nu are niciun sens)
move1 pos heading fromLevel = case heading of
                                North -> addSquare (c square) (case newHeadingCaseNorth /= Nowhere of
                                                                True -> newHeadingCaseNorth
                                                                _ -> (h square)) ((fst pos) - 1, (snd pos)) (removeSquare fromPreviousLevelCaseNorth pos)
                                South -> addSquare (c square) (case newHeadingCaseSouth /= Nowhere of
                                                                True -> newHeadingCaseSouth
                                                                _ -> (h square)) ((fst pos) + 1, (snd pos)) (removeSquare fromPreviousLevelCaseSouth pos)
                                West -> addSquare (c square) (case newHeadingCaseWest /= Nowhere of
                                                                True -> newHeadingCaseWest
                                                                _ -> (h square)) ((fst pos), (snd pos) - 1) (removeSquare fromPreviousLevelCaseWest pos)
                                East -> addSquare (c square) (case newHeadingCaseEast /= Nowhere of
                                                                True -> newHeadingCaseEast
                                                                _ -> (h square)) ((fst pos), (snd pos) + 1) (removeSquare fromPreviousLevelCaseEast pos)
                                Nowhere -> fromLevel
    where
        square = findSquare fromLevel pos
        newHeadingCaseNorth = specialHeading ((fst pos) - 1, (snd pos)) fromLevel
        newHeadingCaseSouth = specialHeading ((fst pos) + 1, (snd pos)) fromLevel
        newHeadingCaseWest = specialHeading ((fst pos), (snd pos) - 1) fromLevel
        newHeadingCaseEast = specialHeading ((fst pos), (snd pos) + 1) fromLevel
        fromPreviousLevelCaseNorth = case (t $ findSquare fromLevel ((fst pos) - 1, (snd pos))) == Square of -- la fel si pentru celelalte (dar deocamdata nu vrea sa iasa !!)
                                        True -> move1 ((fst pos) - 1, (snd pos)) North fromLevel -- sau "heading"
                                        _ -> fromLevel
        fromPreviousLevelCaseSouth = case (t $ findSquare fromLevel ((fst pos) + 1, (snd pos))) == Square of
                                        True -> move1 ((fst pos) + 1, (snd pos)) South fromLevel -- sau "heading"
                                        _ -> fromLevel
        fromPreviousLevelCaseWest = case (t $ findSquare fromLevel ((fst pos), (snd pos) - 1)) == Square of
                                        True -> move1 ((fst pos), (snd pos) - 1) West fromLevel -- sau "heading"
                                        _ -> fromLevel
        fromPreviousLevelCaseEast = case (t $ findSquare fromLevel ((fst pos), (snd pos) + 1)) == Square of
                                        True -> move1 ((fst pos), (snd pos) + 1) East fromLevel -- sau "heading"
                                        _ -> fromLevel

minimize :: Level -> Level
minimize l = minimizeUp $ minimizeDown $ minimizeLeft $ minimizeRight l

minimizeUp :: Level -> Level
minimizeUp l = Level (drop (firstNullLines (levelType l) 0) $ levelType l)

minimizeDown :: Level -> Level
minimizeDown l = Level (take (length (levelType l) - lastNullLines (levelType l)) $ levelType l)

minimizeLeft :: Level -> Level
minimizeLeft l = zipMatrix $ minimizeUp $ zipMatrix l

minimizeRight :: Level -> Level
minimizeRight l = zipMatrix $ minimizeDown $ zipMatrix l

firstNullLines :: [[((Object, Object), Position)]] -> Int -> Int
firstNullLines [] _ = 0
firstNullLines m i = case nullLine (head m) of
                        True -> firstNullLines (tail m) (i + 1)
                        _ -> i

lastNullLines :: [[((Object, Object), Position)]] -> Int
lastNullLines [] = 0
lastNullLines m = firstNullLines (reverse m) 0

nullLine :: [((Object, Object), Position)] -> Bool
nullLine [] = True
nullLine line = case fst (head line) == (nullObject, nullObject) of
                    True -> nullLine (tail line)
                    _ -> False
    where
        nullObject = (Object NULL Red Nowhere)

zipMatrix :: Level -> Level
zipMatrix l = Level (zipMatrixRecursive $ levelType l)

zipMatrixRecursive :: [[a]] -> [[a]]
zipMatrixRecursive [] = []
zipMatrixRecursive m = case length m of
                            1 -> map (\x -> [x]) (last m)
                            _ -> zipWith (\x y -> x : y) (head m) (zipMatrixRecursive $ tail m)

move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
move pos initialLevel = minimize $ move1 pos (h $ findSquare initialLevel pos) initialLevel

allSquares :: Level -> [((Object, Object), Position)]
allSquares (Level []) = []
allSquares l = foldl (\acc1 line -> acc1 ++ (foldl (\acc2 x -> case t (fst (fst x)) == Square of
                                                                True -> [x] ++ acc2
                                                                False -> acc2)) [] line) [] m
    where m = levelType l

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}
instance ProblemState Level Position where
    successors l = foldl (\states pairSqPos -> states ++ [(snd pairSqPos, move (snd pairSqPos) l)]) [] squareCollection
        where squareCollection = allSquares l

    isGoal l = foldl (\squareAtPlace cell -> squareAtPlace && (t (snd (fst cell)) == Circle && c (snd (fst cell)) == c (fst (fst cell)))) True squareCollection
        where
            squareCollection = allSquares l

    -- Doar petru BONUS
    -- heuristic =