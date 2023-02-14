safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)
