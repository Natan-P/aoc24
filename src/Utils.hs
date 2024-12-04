module Utils where

-- copied from base-4.20.0.1
(!?) :: [a] -> Int -> Maybe a

xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
