namespace Example

open Aardvark.Base
open Aardvark.Base.Incremental

[<DomainType>]
type TestModel =
    {
        elements : plist<string>
        box : bool
    }