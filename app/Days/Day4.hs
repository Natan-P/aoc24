module Days.Day4 (runner) where
import Types
import Utils
import Data.List (isPrefixOf, transpose)

(?:) :: Maybe a -> [a] -> [a]
m ?: xs = maybe xs (:xs) m

occurenceCount :: [String] -> String -> Int
occurenceCount _ [] = 0
occurenceCount subs str = (if any (`isPrefixOf` str) subs then 1 else 0) + occurenceCount subs (tail str)

findWordOccurences :: String -> [String] -> Int
findWordOccurences word tbl = sum $ map (occurenceCount [word, reverse word]) tbl

placeWordAt :: Int -> Int -> Char -> String -> String
placeWordAt l o c s = replicate o c ++ s ++ replicate (l - (o + length s)) c

tblDiagDown tbl = let width = length (head tbl); height = length tbl
    in map (\j -> placeWordAt width (max 0 (width-j-1)) '.' $ foldr (\i acc -> ((tbl !? (j-i)) >>= (!? (width-i-1))) ?: acc) "" [height-1,height-2..0]) [0..(width+height-2)]
tblDiagUp tbl = let width = length (head tbl); height = length tbl
    in map (\j -> placeWordAt width (max 0 (j-width)) '.' $ foldr (\i acc -> ((tbl !? (j-i)) >>= (!? i)) ?: acc) "" [0..height-1]) [0..(width+height-2)]

findCrossMas tbl = let width = length (head tbl); height = length tbl; ti x y = (tbl !! y) !! x
    in sum $ concatMap (\j -> map (\i -> let mses = ti<$>[i,i+2]<*>[j,j+2] in if ti (i+1) (j+1) == 'A' && all (`elem` "MS") mses && mses!!0 /= mses!!3 && mses!!1 /= mses!!2 then 1 else 0) [0..width-3]) [0..height-3]

runner :: ToRun -> String -> (String, String)
runner runs cont =
    let tbl = lines cont
    in (ifFst runs $ show . sum $ map (findWordOccurences "XMAS" . ($ tbl)) [id,transpose,tblDiagUp,tblDiagDown],
        ifSnd runs $ show $ findCrossMas tbl
    )

