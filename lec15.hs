--Monads and IO()

main :: IO()
--main = putStrLn "Hello, World."
main = do
    x <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "How are you doing, " ++ name ++ "?"
    -- same as
    -- getLine <<= ( \ name =
    -- putStrLn $ "How are you doing, " ++ name ++ "?" )

