module Days.Day3 (runner) where
import Types
import Data.Char (isDigit)

match :: String -> String -> [[String]]
match initPat x = matcher' [[]] initPat x
    where
        matcher' [] pat ent = matcher' [[]] pat ent
        matcher' matches [] s = matcher' ([]:matches) initPat s
        matcher' (_:matches) _ [] = matches
        matcher' matches@(latestMatch:tailMatches) pat@(matchChar:pat') entireStr@(c:str)
            | matchChar == 'I' = if isDigit c 
                                     then matcher' ((latestMatch++[[c]]):tailMatches) ('C':pat') str
                                     else matcher' tailMatches initPat str
            | matchChar == 'C' = if isDigit c
                                     then matcher' ((init latestMatch++[(last latestMatch++[c])]):tailMatches) pat str
                                     else matcher' matches pat' entireStr
            | otherwise        = if c == matchChar then matcher' matches pat' str else matcher' ([]:tailMatches) initPat str

runner :: ToRun -> String -> (String, String)
runner runs cont = 
    let 
    in (ifFst runs $ show . sum . map (product . map read) $ match "mul(I,I)" cont,
        ifSnd runs undefined
    )

