data AExp = Add AExp AExp | Mult Float Float | Div Float Float | Sub Float Float | Num Float deriving Show

eval :: AExp -> Float
eval (Add arg1 arg2) = (eval arg1) + (eval arg2)
eval (Mult arg1 arg2) = arg1 * arg2
eval (Div arg1 arg2) = arg1 / arg2
eval (Sub arg1 arg2) = arg1 - arg2
eval (Num arg1) = arg1

parsePrefix :: [String] -> Maybe AExp
parsePrefix [] = Nothing
parsePrefix [x] = Num ((reads x :: [(Float, String)]) !! 0)
parsePrefix (x:y:xs)
    | x == "+" = Add(Num ((reads y :: [(Float, String)]) !! 0)) (parsePrefix xs)
    | x == "-" = Sub(Num ((reads y :: [(Float, String)]) !! 0)) (parsePrefix xs)
    | x == "/" = Div(Num ((reads y :: [(Float, String)]) !! 0)) (parsePrefix xs)
    | x == "*" = Mult(Num ((reads y :: [(Float, String)]) !! 0)) (parsePrefix xs)


