namespace Aardvark.UI

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph

module Incremental =

    let renderControl (attributes : AttributeMap<'msg>) (sg : ISg) =
        DomNode.render attributes sg
        
    let inline elem (tagName : string) (attrs : AttributeMap<'msg>) (children : alist<DomNode<'msg>>) = 
        DomNode.node tagName attrs children

    let inline voidElem (tagName : string) (attrs : AttributeMap<'msg>) = 
        DomNode.text tagName attrs (Mod.constant "")

    let inline text (content : IMod<string>) =
        DomNode.text "span" AttributeMap.empty content
        
    // Elements - list of elements here: https://developer.mozilla.org/en-US/docs/Web/HTML/Element
    // Void elements
    let inline br x = voidElem "br" x
    let inline area x = voidElem "area" x
    let inline baseHtml x = voidElem "base" x
    let inline col x = voidElem "col" x
    let inline embed x = voidElem "embed" x
    let inline hr x = voidElem "hr" x
    let inline img x = voidElem "img" x
    let inline input x = voidElem "input" x
    let inline link x = voidElem "link" x
    let inline meta x = voidElem "meta" x
    let inline param x = voidElem "param" x
    let inline source x = voidElem "source" x
    let inline track x = voidElem "track" x
    let inline wbr x = voidElem "wbr" x

    // Metadata
    let inline head x = elem "head" x
    let inline style x = elem "style" x
    let inline title x = elem "title" x

    // Content sectioning
    let inline address x = elem "address" x
    let inline article x = elem "article" x
    let inline aside x = elem "aside" x
    let inline footer x = elem "footer" x
    let inline header x = elem "header" x
    let inline h1 x = elem "h1" x
    let inline h2 x = elem "h2" x
    let inline h3 x = elem "h3" x
    let inline h4 x = elem "h4" x
    let inline h5 x = elem "h5" x
    let inline h6 x = elem "h6" x
    let inline hgroup x = elem "hgroup" x
    let inline nav x = elem "nav" x

    // Text content
    let inline dd x = elem "dd" x
    let inline div x = elem "div" x
    let inline div' x children = elem "div" x (AList.ofList children)
    let inline dl x = elem "dl" x
    let inline dt x = elem "dt" x
    let inline figcaption x = elem "figcaption" x
    let inline figure x = elem "figure" x
    let inline li x = elem "li" x
    let inline main x = elem "main" x
    let inline ol x = elem "ol" x
    let inline p x = elem "p" x
    let inline pre a x = DomNode.text "pre" a x
    let inline section x = elem "section" x
    let inline ul x = elem "ul" x

    // Inline text semantics
    let inline a x = elem "a" x
    let inline abbr x = elem "abbr" x
    let inline b x = elem "b" x
    let inline bdi x = elem "bdi" x
    let inline bdo x = elem "bdo" x
    let inline cite x = elem "cite" x
    let inline code x = elem "code" x
    let inline data x = elem "data" x
    let inline dfn x = elem "dfn" x
    let inline em x = elem "em" x
    let inline i x = elem "i" x
    let inline kbd x = elem "kbd" x
    let inline mark x = elem "mark" x
    let inline q x = elem "q" x
    let inline rp x = elem "rp" x
    let inline rt x = elem "rt" x
    let inline rtc x = elem "rtc" x
    let inline ruby x = elem "ruby" x
    let inline s x = elem "s" x
    let inline samp x = elem "samp" x
    let inline small x = elem "small" x
    let inline span x = elem "span" x
    let inline strong x = elem "strong" x
    let inline sub x = elem "sub" x
    let inline sup x = elem "sup" x
    let inline time x = elem "time" x
    let inline u x = elem "u" x
    let inline var x = elem "var" x

    // Image and multimedia
    let inline audio x = elem "audio" x
    let inline map x = elem "map" x
    let inline video x = elem "video" x

    // Embedded content
    let inline objectHtml x = elem "object" x

    // Demarcasting edits
    let inline del x = elem "del" x
    let inline ins x = elem "ins" x

    // Table content
    let inline caption x = elem "caption" x
    let inline colgroup x = elem "colgroup" x
    let inline table x = elem "table" x
    let inline tbody x = elem "tbody" x
    let inline td x = elem "td" x
    let inline tfoot x = elem "tfoot" x
    let inline th x = elem "th" x
    let inline thead x = elem "thead" x
    let inline tr x = elem "tr" x

    // Forms
    let inline button x = elem "button" x
    let inline datalist x = elem "datalist" x
    let inline fieldset x = elem "fieldset" x
    let inline form x = elem "form" x
    let inline label x = elem "label" x
    let inline legend x = elem "legend" x
    let inline meter x = elem "meter" x
    let inline optgroup x = elem "optgroup" x
    let inline option x = elem "option" x
    let inline output x = elem "output" x
    let inline progress x = elem "progress" x
    let inline select x = elem "select" x
    let inline textarea a x = DomNode.text "textarea" a x

    // Interactive elements
    let inline details x = elem "details" x
    let inline dialog x = elem "dialog" x
    let inline menu x = elem "menu" x
    let inline menuitem x = elem "menuitem" x
    let inline summary x = elem "summary" x

[<AutoOpen>]
module Static =

    let renderControl (attributes : list<string * AttributeValue<'msg>>) (sg : ISg) =
        DomNode.render (AttributeMap.ofList attributes) sg

    let inline elem (tagName : string) (attrs : list<string * AttributeValue<'msg>>) (children : list<DomNode<'msg>>) =
        DomNode.node tagName (AttributeMap.ofList attrs) (AList.ofList children)

    let inline voidElem (tagName : string) (attrs : list<string * AttributeValue<'msg>>) = 
        DomNode.text tagName (AttributeMap.ofList attrs) (Mod.constant "")

    let inline text (content : string) =
        DomNode.text "span" AttributeMap.empty (Mod.constant content)

    // Elements - list of elements here: https://developer.mozilla.org/en-US/docs/Web/HTML/Element
    // Void elements
    let inline br x = voidElem "br" x
    let inline area x = voidElem "area" x
    let inline baseHtml x = voidElem "base" x
    let inline col x = voidElem "col" x
    let inline embed x = voidElem "embed" x
    let inline hr x = voidElem "hr" x
    let inline img x = voidElem "img" x
    let inline input x = voidElem "input" x
    let inline link x = voidElem "link" x
    let inline meta x = voidElem "meta" x
    let inline param x = voidElem "param" x
    let inline source x = voidElem "source" x
    let inline track x = voidElem "track" x
    let inline wbr x = voidElem "wbr" x

    // Metadata
    let inline head x = elem "head" x
    let inline title x = elem "title" x

    // Content sectioning
    let inline address x = elem "address" x
    let inline article x = elem "article" x
    let inline aside x = elem "aside" x
    let inline footer x = elem "footer" x
    let inline header x = elem "header" x
    let inline h1 x = elem "h1" x
    let inline h2 x = elem "h2" x
    let inline h3 x = elem "h3" x
    let inline h4 x = elem "h4" x
    let inline h5 x = elem "h5" x
    let inline h6 x = elem "h6" x
    let inline hgroup x = elem "hgroup" x
    let inline nav x = elem "nav" x

    // page content
    let inline body x = elem "body" x

    // Text content
    let inline dd x = elem "dd" x
    let inline div x = elem "div" x
    let inline dl x = elem "dl" x
    let inline dt x = elem "dt" x
    let inline figcaption x = elem "figcaption" x
    let inline figure x = elem "figure" x
    let inline li x = elem "li" x
    let inline main x = elem "main" x
    let inline ol x = elem "ol" x
    let inline p x = elem "p" x
    let inline pre a x = DomNode.text "pre" (AttributeMap.ofList a) (Mod.constant x)
    let inline section x = elem "section" x
    let inline ul x = elem "ul" x

    // Inline text semantics
    let inline a x = elem "a" x
    let inline abbr x = elem "abbr" x
    let inline b x = elem "b" x
    let inline bdi x = elem "bdi" x
    let inline bdo x = elem "bdo" x
    let inline cite x = elem "cite" x
    let inline code x = elem "code" x
    let inline data x = elem "data" x
    let inline dfn x = elem "dfn" x
    let inline em x = elem "em" x
    let inline i x = elem "i" x
    let inline kbd x = elem "kbd" x
    let inline mark x = elem "mark" x
    let inline q x = elem "q" x
    let inline rp x = elem "rp" x
    let inline rt x = elem "rt" x
    let inline rtc x = elem "rtc" x
    let inline ruby x = elem "ruby" x
    let inline s x = elem "s" x
    let inline samp x = elem "samp" x
    let inline small x = elem "small" x
    let inline span x = elem "span" x
    let inline strong x = elem "strong" x
    let inline sub x = elem "sub" x
    let inline sup x = elem "sup" x
    let inline time x = elem "time" x
    let inline u x = elem "u" x
    let inline var x = elem "var" x

    // Image and multimedia
    let inline audio x = elem "audio" x
    let inline map x = elem "map" x
    let inline video x = elem "video" x

    // Embedded content
    let inline objectHtml x = elem "object" x

    // Demarcasting edits
    let inline del x = elem "del" x
    let inline ins x = elem "ins" x

    // Table content
    let inline caption x = elem "caption" x
    let inline colgroup x = elem "colgroup" x
    let inline table x = elem "table" x
    let inline tbody x = elem "tbody" x
    let inline td x = elem "td" x
    let inline tfoot x = elem "tfoot" x
    let inline th x = elem "th" x
    let inline thead x = elem "thead" x
    let inline tr x = elem "tr" x

    // Forms
    let inline button x = elem "button" x
    let inline datalist x = elem "datalist" x
    let inline fieldset x = elem "fieldset" x
    let inline form x = elem "form" x
    let inline label x = elem "label" x
    let inline legend x = elem "legend" x
    let inline meter x = elem "meter" x
    let inline optgroup x = elem "optgroup" x
    let inline option x = elem "option" x
    let inline output x = elem "output" x
    let inline progress x = elem "progress" x
    let inline select x = elem "select" x
    let inline textarea a x = DomNode.text "textarea" (AttributeMap.ofList a) (Mod.constant x)

    // Interactive elements
    let inline details x = elem "details" x
    let inline dialog x = elem "dialog" x
    let inline menu x = elem "menu" x
    let inline menuitem x = elem "menuitem" x
    let inline summary x = elem "summary" x

module Generic =

    type AttributeCreator =
        static member inline AttributeMap(attributes : AttributeMap<'msg>) = attributes
        static member inline AttributeMap(attributes : list<string * AttributeValue<'msg>>) = AttributeMap.ofList attributes
        static member inline AttributeMap(attributes : list<string * IMod<Option<AttributeValue<'msg>>>>) = 
            attributes |> List.map (fun (k, v) -> v |> AMap.bind (function Some v -> AMap.ofList [k, v] | None -> AMap.empty) |> AttributeMap) |> AttributeMap.union
        
    type IDomContent<'msg> =
        abstract member Create : tag : string * attributes : AttributeMap<'msg> -> DomNode<'msg>

    type ContentCreator =
        static member inline Content(l : list<DomNode<'msg>>) = { new IDomContent<'msg> with member x.Create(t,a) = DomNode.node t a (AList.ofList l) }
        static member inline Content(l : IMod<list<DomNode<'msg>>>) = { new IDomContent<'msg> with member x.Create(t,a) = DomNode.node t a (AList.ofMod (Mod.map PList.ofList l)) }
        static member inline Content(l : plist<DomNode<'msg>>) = { new IDomContent<'msg> with member x.Create(t,a) = DomNode.node t a (AList.ofList (PList.toList l)) }
        static member inline Content(l : IMod<plist<DomNode<'msg>>>) = { new IDomContent<'msg> with member x.Create(t,a) = DomNode.node t a (AList.ofMod l) }
        static member inline Content(l : alist<DomNode<'msg>>) = { new IDomContent<'msg> with member x.Create(t,a) = DomNode.node t a l }

        static member inline Content(l : IMod<string>) = { new IDomContent<'msg> with member x.Create(t,a) = DomNode.text t a l }
        static member inline Content(l : string) = { new IDomContent<'msg> with member x.Create(t,a) = DomNode.text t a (Mod.constant l) }

          

    type Creator(tag : string) =
        member x.Tag = tag

        static member Render(c : Creator, attributes : AttributeMap<'msg>, sg : ISg) =
            DomNode.Render(attributes, fun _ -> sg)

        static member Render(c : Creator, attributes : AttributeMap<'msg>, sg : Aardvark.Application.RenderControl -> ISg) =
            DomNode.Render(attributes, sg)

        static member Render(c : Creator, attributes : list<string * AttributeValue<'msg>>, sg : ISg) =
            DomNode.Render(AttributeMap.ofList attributes, fun _ -> sg)

        static member Render(c : Creator, attributes : list<string * AttributeValue<'msg>>, sg : Aardvark.Application.RenderControl -> ISg) =
            DomNode.Render(AttributeMap.ofList attributes, sg)

        // Children and dynamic attributes
        static member Node(tag : Creator, attributes : AttributeMap<'msg>, children : alist<DomNode<'msg>>) =
            DomNode.node tag.Tag attributes children
            
        static member Node(tag : Creator, attributes : AttributeMap<'msg>, children : IMod<list<DomNode<'msg>>>) =
            DomNode.node tag.Tag attributes (children |> Mod.map PList.ofList |> AList.ofMod)
            
        static member Node(tag : Creator, attributes : AttributeMap<'msg>, children : IMod<plist<DomNode<'msg>>>) =
            DomNode.node tag.Tag attributes (children |> AList.ofMod)

        static member Node(tag : Creator, attributes : AttributeMap<'msg>, children : list<DomNode<'msg>>) =
            DomNode.node tag.Tag attributes (AList.ofList children)
 
        static member Node(tag : Creator, attributes : AttributeMap<'msg>, children : plist<DomNode<'msg>>) =
            DomNode.node tag.Tag attributes (AList.ofPList children)
            
            
        // Children and static attributes
        static member Node(tag : Creator, attributes : list<string * AttributeValue<'msg>>, children : alist<DomNode<'msg>>) =
            DomNode.node tag.Tag (AttributeMap.ofList attributes) children
            
        static member Node(tag : Creator, attributes : list<string * AttributeValue<'msg>>, children : IMod<list<DomNode<'msg>>>) =
            DomNode.node tag.Tag (AttributeMap.ofList attributes) (children |> Mod.map PList.ofList |> AList.ofMod)
            
        static member Node(tag : Creator, attributes : list<string * AttributeValue<'msg>>, children : IMod<plist<DomNode<'msg>>>) =
            DomNode.node tag.Tag (AttributeMap.ofList attributes) (children |> AList.ofMod)

        static member Node(tag : Creator, attributes : list<string * AttributeValue<'msg>>, children : list<DomNode<'msg>>) =
            DomNode.node tag.Tag (AttributeMap.ofList attributes) (AList.ofList children)
 
        static member Node(tag : Creator, attributes : list<string * AttributeValue<'msg>>, children : plist<DomNode<'msg>>) =
            DomNode.node tag.Tag (AttributeMap.ofList attributes) (AList.ofPList children)
            

        // Content and dynamic attributes
        static member Node(tag : Creator, attributes : AttributeMap<'msg>, content : IMod<string>) =
            DomNode.text tag.Tag attributes content
            
        static member Node(tag : Creator, attributes : AttributeMap<'msg>, content : string) =
            DomNode.text tag.Tag attributes (Mod.constant content)

        // Content and static attributes
        static member Node(tag : Creator, attributes : list<string * AttributeValue<'msg>>, content : IMod<string>) =
            DomNode.text tag.Tag (AttributeMap.ofList attributes) content
            
        static member Node(tag : Creator, attributes : list<string * AttributeValue<'msg>>, content : string) =
            DomNode.text tag.Tag (AttributeMap.ofList attributes) (Mod.constant content)

        static member Void(tag : Creator, attributes : AttributeMap<'msg>) =
            DomNode.text tag.Tag attributes (Mod.constant "")

        static member Void(tag : Creator, attributes : list<string * AttributeValue<'msg>>) =
            DomNode.text tag.Tag (AttributeMap.ofList attributes) (Mod.constant "")

    let inline attributes (dummy : ^a) (attrs : ^b) =
        ((^a or ^b) : (static member AttributeMap : ^b -> AttributeMap<'msg>) (attrs))
    
    let inline contentInternal (dummy : ^a) (attrs : ^b) =
        ((^a or ^b) : (static member Content : ^b -> IDomContent<'msg>) (attrs))
    

    let inline node (tagName : ^a) (attrs : ^b) (children : ^c) =
        ((^a or ^b or ^c) : (static member Node : ^a * ^b * ^c -> DomNode<'msg>) (tagName, attrs, children))
        
    let inline renderE (tagName : ^a) (attrs : ^b) (children : ^c) =
        ((^a or ^b or ^c) : (static member Render : ^a * ^b * ^c -> DomNode<'msg>) (tagName, attrs, children))
        
    let inline leaf (tagName : ^a) (attrs : ^b)=
        ((^a or ^b) : (static member Void : ^a * ^b -> DomNode<'msg>) (tagName, attrs))
        
    let inline att a = attributes Unchecked.defaultof<AttributeCreator> a
    let inline content a = contentInternal Unchecked.defaultof<ContentCreator> a

    let inline elem tag atts children = node (Creator(tag)) atts children
    let inline render atts children = node (Creator("canvas")) atts children
    let inline voidElem tag atts = leaf (Creator(tag)) atts
    
    //let inline elemNS tag ns atts children = nodeNS Unchecked.defaultof<CreatorNamespace> tag ns atts children
    //let inline voidElemNS tag ns atts = leafNS Unchecked.defaultof<CreatorNamespace> tag ns atts



    let inline text content = elem "span" [] content
    

    // Elements - list of elements here: https://developer.mozilla.org/en-US/docs/Web/HTML/Element
    // Void elements
    let inline br x = voidElem "br" x
    let inline area x = voidElem "area" x
    let inline baseHtml x = voidElem "base" x
    let inline col x = voidElem "col" x
    let inline embed x = voidElem "embed" x
    let inline hr x = voidElem "hr" x
    let inline img x = voidElem "img" x
    let inline input x = voidElem "input" x
    let inline link x = voidElem "link" x
    let inline meta x = voidElem "meta" x
    let inline param x = voidElem "param" x
    let inline source x = voidElem "source" x
    let inline track x = voidElem "track" x
    let inline wbr x = voidElem "wbr" x

    // Metadata
    let inline head a c = elem "head" a c
    let inline title a c = elem "title" a c

    // Content sectioning
    let inline address a c = elem "address" a c
    let inline article a c = elem "article" a c
    let inline aside a c = elem "aside" a c
    let inline footer a c = elem "footer" a c
    let inline header a c = elem "header" a c
    let inline h1 a c = elem "h1" a c
    let inline h2 a c = elem "h2" a c
    let inline h3 a c = elem "h3" a c
    let inline h4 a c = elem "h4" a c
    let inline h5 a c = elem "h5" a c
    let inline h6 a c = elem "h6" a c
    let inline hgroup a c = elem "hgroup" a c
    let inline nav a c = elem "nav" a c

    // page content
    let inline body a c = elem "body" a c

    // Text content
    let inline dd a c = elem "dd" a c
    let inline div a c = elem "div" a c
    let inline dl a c = elem "dl" a c
    let inline dt a c = elem "dt" a c
    let inline figcaption a c = elem "figcaption" a c
    let inline figure a c = elem "figure" a c
    let inline li a c = elem "li" a c
    let inline main a c = elem "main" a c
    let inline ol a c = elem "ol" a c
    let inline p a c = elem "p" a c
    let inline pre a c = elem "pre" a c
    let inline section a c = elem "section" a c
    let inline ul a c = elem "ul" a c

    // Inline text semantics
    let inline a a c = elem "a" a c
    let inline abbr a c = elem "abbr" a c
    let inline b a c = elem "b" a c
    let inline bdi a c = elem "bdi" a c
    let inline bdo a c = elem "bdo" a c
    let inline cite a c = elem "cite" a c
    let inline code a c = elem "code" a c
    let inline data a c = elem "data" a c
    let inline dfn a c = elem "dfn" a c
    let inline em a c = elem "em" a c
    let inline i a c = elem "i" a c
    let inline kbd a c = elem "kbd" a c
    let inline mark a c = elem "mark" a c
    let inline q a c = elem "q" a c
    let inline rp a c = elem "rp" a c
    let inline rt a c = elem "rt" a c
    let inline rtc a c = elem "rtc" a c
    let inline ruby a c = elem "ruby" a c
    let inline s a c = elem "s" a c
    let inline samp a c = elem "samp" a c
    let inline small a c = elem "small" a c
    let inline span a c = elem "span" a c
    let inline strong a c = elem "strong" a c
    let inline sub a c = elem "sub" a c
    let inline sup a c = elem "sup" a c
    let inline time a c = elem "time" a c
    let inline u a c = elem "u" a c
    let inline var a c = elem "var" a c

    // Image and multimedia
    let inline audio a c = elem "audio" a c
    let inline map a c = elem "map" a c
    let inline video a c = elem "video" a c

    // Embedded content
    let inline objectHtml a c = elem "object" a c

    // Demarcasting edits
    let inline del a c = elem "del" a c
    let inline ins a c = elem "ins" a c

    // Table content
    let inline caption a c = elem "caption" a c
    let inline colgroup a c = elem "colgroup" a c
    let inline table a c = elem "table" a c
    let inline tbody a c = elem "tbody" a c
    let inline td a c = elem "td" a c
    let inline tfoot a c = elem "tfoot" a c
    let inline th a c = elem "th" a c
    let inline thead a c = elem "thead" a c
    let inline tr a c = elem "tr" a c

    // Forms
    let inline button a c = elem "button" a c
    let inline datalist a c = elem "datalist" a c
    let inline fieldset a c = elem "fieldset" a c
    let inline form a c = elem "form" a c
    let inline label a c = elem "label" a c
    let inline legend a c = elem "legend" a c
    let inline meter a c = elem "meter" a c
    let inline optgroup a c = elem "optgroup" a c
    let inline option a c = elem "option" a c
    let inline output a c = elem "output" a c
    let inline progress a c = elem "progress" a c
    let inline select a c = elem "select" a c
    let inline textarea a c = elem "textarea" a c

    // Interactive elements
    let inline details a c = elem "details" a c
    let inline dialog a c = elem "dialog" a c
    let inline menu a c = elem "menu" a c
    let inline menuitem a c = elem "menuitem" a c
    let inline summary a c = elem "summary" a c

