import ITMOPrelude.Primitive

t = Plus (Succ Zero)
t2 = Minus ((Succ Zero))


main = do
	putStrLn (show (t .+. t2))

