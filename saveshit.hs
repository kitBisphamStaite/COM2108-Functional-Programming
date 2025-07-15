highestPointMove = fst $ foldl maxPoints (((0,0),L), -1) couldPlayWithScores

maxPoints :: ((Domino, End),Int) -> ((Domino, End),Int) -> ((Domino, End),Int)
    maxPoints (dom1, score1) (dom2, score2) = if score1 > score2 then (dom1, score1) else (dom2, score2)



highestPointMove = fst $ findHighestPointMove couldPlayWithScores

findHighestPointMove :: [((Domino, End),Int)] -> ((Domino, End),Int)
    findHighestPointMove [x] = x
    findHighestPointMove (x:x' :xs) = findHighestPointMove ((if snd x >= snd x' then x else x'):xs)