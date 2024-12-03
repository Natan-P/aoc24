module Days.Day3 (runner) where
import Types
import Data.Char (isDigit)
import Data.List (minimumBy,maximumBy)
import Data.Ord (comparing)
import Data.Bifunctor (Bifunctor(second), first)

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f (a, b, c) = f a b c
fst3 (x,_,_) = x
snd3 (_,y,_) = y

data MatchType = Str {str :: String} | Enabled {enabled :: Bool} deriving Show
getStr (Str s) = s
getEnabled (Enabled b) = b


match :: [String] -> String -> [[MatchType]]
match initPat x = matcher' [[]] (patsDuper initPat) x
    where
        patsDuper = map (\p -> (p,p))
        matcher' [] pat ent = matcher' [[]] pat ent
        matcher' matches [] s = matcher' ([]:matches) (patsDuper initPat) s
        matcher' (_:matches) _ [] = matches
        matcher' matches@(latestMatch:tailMatches) entirePat entireStr@(c:rest) = if any (null . snd) entirePat then matcher' ([]:matches) (patsDuper initPat) entireStr else let allMatches = map perPatMatcher entirePat
            in (\(m,_,s) -> matcher' m (map snd3 allMatches) s) . maximumBy (\a b -> comparing (length . fst3) a b <> comparing (\(_,(p1,p2),_) -> abs (length p1 - length p2)) a b) $ allMatches
            where
                --perPatMatcher :: String -> ([[MatchType]], String, String)
                perPatMatcher (fstPat,[]) = (,,) matches (fstPat, fstPat) entireStr
                perPatMatcher (fstPat,pat@(matchChar:pat'))
                    | matchChar == 'I' = if isDigit c 
                                             then (,,) ((latestMatch++[Str [c]]):tailMatches) (fstPat,('C':pat')) rest
                                             else (,,) ([]:tailMatches) (fstPat,fstPat) rest
                    | matchChar == 'C' = if isDigit c
                                             then (,,) ((init latestMatch++[Str (getStr (last latestMatch)++[c])]):tailMatches) (fstPat, pat) rest
                                             else (,,) matches (fstPat,pat') entireStr
                    | matchChar == '?' = if (take 3 entireStr) == "n't"
                                             then (,,) ([Enabled False]:tailMatches) (fstPat,pat') (drop 3 entireStr)
                                             else (,,) ([Enabled True]:tailMatches) (fstPat, pat') entireStr
                    | otherwise        = if c == matchChar then (,,) matches (fstPat, pat') rest else (,,) ([]:tailMatches) (fstPat, fstPat) rest

runner :: ToRun -> String -> (String, String)
runner runs cont = 
    let 
    in (ifFst runs $ show . sum . map (product . map (read .str)) $ match ["mul(I,I)"] cont,
        ifSnd runs $ show . fst . foldr (\x acc -> case x of 
                                                       [] -> acc
                                                       [Enabled{enabled=en}] -> second (const en) acc
                                                       sl -> if snd acc then first (+ product (map (read . str) sl)) acc else acc
                                          ) (0, True) $ match ["mul(I,I)", "do?()"] cont
    )

