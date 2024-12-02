module Days.Day2 (runner) where
import Types

-- could literally just be a tuple but eh
data ScanStep = ScanStep {delta :: Int, curr :: Int, valid :: Bool}

bti x = if x then 1 else 0
isEntryListValid le = case dropWhile valid le of
    [] -> True
    (ScanStep{valid=False}):xs -> False
    _ -> False

allButOne :: [a] -> [[a]]
allButOne = helper []
    where helper _ [] = []
          helper prev (x:xs) = (prev ++ xs) : helper (prev ++ [x]) xs

--runner :: ToRun -> String -> (String, String)
runner runs cont =
    let reports = map (map read . words) . lines $ cont
        reports :: [[Int]]
        scanner rep = scanl (\acc x ->
                let prevNum = curr acc
                    diff = x - prevNum
                    isStepValid = abs diff >= 1 && abs diff <= 3 && signum diff + signum (delta acc) /= 0
                in ScanStep{delta=diff,curr=x,valid=isStepValid}
            ) (ScanStep{delta=0,curr=head rep,valid=True}) (tail rep)

    in (ifFst runs $ show . sum . map (\rep -> bti . isEntryListValid $ scanner rep) $ reports,
        ifSnd runs $ show . sum . map (\rep -> bti $ any (isEntryListValid . scanner) (rep:allButOne rep)) $ reports
    )

