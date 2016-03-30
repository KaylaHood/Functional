data Operation = Add | Mult | Div | Sub | Exp | Mod deriving Show
data Input = Number Int | Op Operation deriving Show

readInput :: String -> Input
readInput "+" = Op Add
readInput "-" = Op Sub
readInput "*" = Op Mult
readInput "/" = Op Div
readInput "^" = Op Exp
readInput "%" = Op Mod
readInput x = Number (read x)

data State = LFNum Int Operation | LFOp Int

update :: Input -> State -> Maybe State
update Number LFOp = Nothing
update Op LFNum = Nothing
update Op (LFOp i) = 
update Number (LFNum i o) = let 
