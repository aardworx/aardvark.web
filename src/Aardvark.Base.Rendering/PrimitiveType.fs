namespace Aardvark.Base.Rendering


type PrimitiveType =
    | Float of bits : int
    | Int of signed : bool * bits : int
    | Bool

    | Vec of PrimitiveType * int
    | Mat of PrimitiveType * int * int
    | Trafo
     
module PrimitiveType =
    let rec size (t : PrimitiveType) =
        match t with
        | Bool -> 4
        | Float b -> b / 8
        | Int(_,b) -> b / 8
        | Vec(t,d) -> size t * d
        | Mat(t,r,c) -> size t * r * c
        | Trafo -> size (Mat(Float 32, 4, 4))