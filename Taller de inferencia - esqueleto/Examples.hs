module Examples(expr, module Main)
where

import Main

expr :: Int -> String
-- Ejemplos con Var, Zero, Succ y App
expr 1 = "x"
expr 2 = "0"
expr 3 = "succ(x)"
expr 4 = "succ(0)"
expr 5 = "f x"
expr 6 = "f 0"
expr 7 = "succ(f 0)"
expr 8 = "0 f"
expr 9 = "f 0 succ(succ(0))"
expr 10 = "f (0 succ(succ(0)))"
expr 11 = "f x y z"
expr 12 = "f (x y z)"
expr 13 = "f succ(x) y z"
expr 14 = "f (succ(x) y z)"
expr 15 = "x x"
expr 16 = "(\\x -> x)"
expr 17 = "(\\x -> x) y"
expr 18 = "\\s -> \\x -> \\y -> s x y"
expr 19 = "\\s -> \\x -> \\y -> s (x y)"
expr 20 = "(\\s -> \\x -> \\y -> s) (x y)"
expr n = error $ "La expresion " ++ show n ++ " no esta definida"