main = sequence (map print [1.. 10])
-- [(),(),(),(),(),(),(),(),(),()]

-- main = mapM print [1 .. 10]
-- [(),(),(),(),(),(),(),(),(),()]

-- main = mapM_ print [1 .. 10]
-- results are thrown away