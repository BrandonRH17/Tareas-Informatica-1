module Main exposing (..)
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith n fs zs  =

    case (fs, zs) of

        ([], _) -> []

        (_, []) -> []

        (f::fss, z::zss) -> n f z ::zipWith n fss zss

groupBy : (a -> Bool) -> List a -> (List a, List a) 
groupBy m list = (siFunciona m list, noFunciona m list)

siFunciona j list = case list of 
    [] -> []
    (b::bss) -> if j b 
        then siFunciona j bss 
        else b:: siFunciona j bss

noFunciona j list = case list of 
    [] -> []
    (k::kss) -> if j k
        then k:: noFunciona j kss 
        else noFunciona j kss

bind : Maybe a1 -> (a1 -> Maybe a) -> Maybe a
bind h j = case h of 
    Nothing -> Nothing 
    Just a -> j a 








