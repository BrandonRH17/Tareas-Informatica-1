module Main exposing(..)
suma: Int -> Int -> Int 
suma n1 n2 = n1 + n2

multiplicacion: Int -> Int -> Int 
multiplicacion n1 n2 = n1 * n2

type Expresion  =  Suma Expresion Expresion
    | Multiplicacion Expresion Expresion 
    | Valor Int
    

reducir:((Int -> Int -> Int), (Int -> Int -> Int)) -> Expresion -> Int 
reducir (sum, multi) exp = case exp of
    Suma exp1 exp2 -> sum (reducir (sum, multi) exp1)  (reducir (sum, multi) exp2)
    Multiplicacion exp1 exp2 -> multi (reducir (sum, multi) exp1)  (reducir (sum, multi) exp2)
    Valor i -> i










