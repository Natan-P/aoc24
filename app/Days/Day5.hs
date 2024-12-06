module Days.Day5 (runner) where
import Types
import Data.Bifunctor (first, second, Bifunctor (bimap))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

split :: Eq a => a -> [a] -> [[a]]
split c str = case break (==c) str of
                  (a, _:b) -> a : split c b
                  (a, _)   -> [a]


runner :: ToRun -> String -> (String, String)
runner runs cont = 
    let (priorityLines,printLines) = second tail . break (== "") $ lines cont
        priorities :: IntMap [Int]
        priorities = IntMap.fromListWith (++) $ map (bimap read ((:[]) . read . tail) . break (== '|')) priorityLines
        prints :: [[Int]]
        prints = map (map read . split ',') printLines
    in (ifFst runs $ show . sum . map (\l -> l !! (length l `div` 2)) $ filter (\p -> all (\(index,el) -> not $ any (`elem` take index p) (IntMap.findWithDefault [] el priorities)) $ zip [0..] p) prints,
        ifSnd runs undefined
    )

