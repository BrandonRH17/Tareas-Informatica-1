module Main exposing(..)
type Natural = Suc Natural | Cero

eN: Int -> Natural
eN i = if i == 0 
    then Cero 
    else Suc(eN (i-1))

naturalE: Natural -> Int
naturalE n = case n of 
    Cero -> 0
    Suc i -> 1 + naturalE i

sumar n1 n2 = case (n1, n2) of
    (Cero, n2_) -> n2_
    (n1_, Cero) -> n1_
    (Suc n1_, n2_) -> Suc (sumar n1_ n2_)

multiplicacion: Natural -> Natural -> Natural
multiplicacion n1 n2 = case (n1,n2) of 
    (Cero, n2_) -> Cero
    (n1_, Cero) -> Cero
    (Suc n1_, n2_) -> sumar n2_ (multiplicacion n1_ n2_)

resta: Natural -> Natural -> Natural
resta n1 n2 = case (n1, n2) of 
    (Cero, n2_) -> Cero
    (n1_, Cero) -> n1_
    (Suc n1_, Suc n2_) -> (resta n1_ n2_)


division: Natural -> Natural -> (Natural, Natural)
division n1_ n2_ =
    if multiplicacion (div n1_ n2_ Cero) n2_ == n1_ 
    then (div n1_ n2_ Cero, Cero)  
    else (resta (div n1_ n2_ Cero) (Suc Cero), resta (n1_ )(multiplicacion (resta(div n1_ n2_ Cero)(Suc Cero)) n2_ ))

div: Natural -> Natural -> Natural -> Natural 
div n1 n2 n3 = case (n1, n2, n3) of
    (Cero, n2_, n3_) -> n3_
    (n1_, Cero, _) -> Cero 
    (n1_, n2_, n3_) ->  div (resta n1_ n2_) n2_ (sumar n3_(Suc Cero))

type GExpresion a = Valor a 
    | Suma (GExpresion a) (GExpresion a)
    | Mult (GExpresion a) (GExpresion a)

type alias Expresion = GExpresion Int

type Estado = Final Int 
    | Parce (List Char)









