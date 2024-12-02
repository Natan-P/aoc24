module Days.Day2 (runner) where
import Types

-- could literally just be a tuple but eh
data ScanStep = ScanStep {delta :: Int, curr :: Int, valid :: Bool}

bti x = if x then 1 else 0
isEntryListValid le = case dropWhile valid le of
    [] -> True
    (ScanStep{valid=False}):xs -> False
    _ -> False


--runner :: ToRun -> String -> (String, String)
runner runs cont =
    let reports = map (map read . words) . lines $ cont
        reports :: [[Int]]
    in (ifFst runs $ show . sum . map (\rep -> bti . isEntryListValid $
        scanl (\acc x ->
            let prevNum = curr acc
                diff = x - prevNum
                isStepValid = abs diff >= 1 && abs diff <= 3 && signum diff + signum (delta acc) /= 0
            in ScanStep{delta=diff,curr=x,valid=isStepValid}
    ) (ScanStep{delta=0,curr=head rep,valid=True}) (tail rep)) $ reports,
        ifSnd runs $ undefined
    )

