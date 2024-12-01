module Days.Day1 (runner) where
import Types
import Data.List (sort)
import Data.Bifunctor (bimap)

runner :: ToRun -> String -> (String, String)
runner runs cont = 
    let sortedCols = bimap (sort . map read) (sort . map read) . unzip . map ((\[a,b] -> (a,b)) . words) . lines $ cont
    in (ifFst runs $ show . sum . uncurry (zipWith (\x y -> abs $ x-y)) $ sortedCols,
        ifSnd runs $ show . sum . (\(fl,sl) -> map (\f -> sum [f | s <- sl, s==f]) fl) $ sortedCols
    )
