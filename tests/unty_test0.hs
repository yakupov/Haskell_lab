import UnTyLambda.Interpreter

t = App (Lam "x" (App (Var "x") (Var "xxx"))) (Var "y")
t2 = App (Lam "xxx" t) (Var "z")


main = do
	putStrLn (show (wh 5 t2))
	putStrLn (show (no 5 t2))
	putStrLn (show (sa 5 t2))
