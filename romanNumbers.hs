data RomanLetter= Add commentMore actions
    I | V | X | L | C| D |M deriving (Eq, Ord, Show)

type RomanNumber= [RomanLetter]

--a string is parsed into the type RomanNumber
    --check is the string represents a valid roman number
    --if yes, the output shall be a value of type RomanNumber

--syntax
romanMap :: [(Char, RomanLetter)]
romanMap= [('I', I), ('V', V), ('X', X), ('L', L), ('C', C), ('D', D), ('M', M)]

parse:: String-> Maybe RomanNumber
parse [] = Nothing
parse [x] = case lookup x romanMap of
                Nothing-> Nothing
                Just r-> Just [r]
parse (x:xs)= case lookup x romanMap of
                Nothing-> Nothing
                Just r -> case parse xs of
                            Nothing->Nothing
                            Just rs -> Just (r:rs)

--semantics
--each roman letter is associated with an integer value

value:: RomanLetter -> Int
value I = 1
value V = 5
value X = 10
value L = 50
value C = 100
value D = 500
value M = 1000

--when a symb appears after a larger (or equal) symbol it is added
--if a symbol appears before a larger symbol it is subtracted

parse2:: Maybe RomanNumber-> RomanNumber
parse2 Nothing = []
parse2 (Just x)= x

eval:: RomanNumber -> Int
eval []= 0
eval [x]= value(x)
eval (x:y:ys)
    | x>=y = value(x) + eval (y:ys)Add commentMore actions
    |otherwise = eval(y:ys) - value(x)