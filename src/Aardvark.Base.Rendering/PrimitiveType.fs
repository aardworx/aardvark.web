namespace Aardvark.Base.Rendering


type PrimitiveType =
    | Float of bits : int
    | Int of signed : bool * bits : int
    | Bool

    | Vec of PrimitiveType * int
    | Mat of PrimitiveType * int * int
    | Trafo