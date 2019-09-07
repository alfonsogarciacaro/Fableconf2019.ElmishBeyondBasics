namespace Fable.Elmish.Demetrix.Router

open System.Text.RegularExpressions
open FSharp.Reflection

open Fable.Core

module private RouterHelpers =
    // As Fable erases options, we can cheat here
    // In .NET we would need to build the types properly
    let makeSome (x : obj): obj = x
    let makeNone (_ : System.Type): obj = null
    let isNone (x : obj) = isNull x

    let normalizeName =
        let reg = Regex("[A-Z]")
        fun name ->
            reg.Replace(name, fun (m: Match) ->
                if m.Index = 0 then m.Value.ToLower()
                else "_" + m.Value.ToLower())

    type  FieldType(t : System.Type) =
        let parsePrimitive (t : System.Type): string -> obj =
            let fullname = t.FullName
            if fullname = typeof<string>.FullName then
                box
            elif fullname = typeof<int32>.FullName then
                System.Int32.Parse >> box
            elif fullname = typeof<decimal>.FullName then
                System.Decimal.Parse >> box
            elif FSharpType.IsUnion t then
                let cases = FSharpType.GetUnionCases(t)
                // Check no case has fields
                cases |> Seq.tryPick (fun c ->                    
                    if c.GetFields().Length > 0 then Some (t.FullName + "." + c.Name)
                    else None)
                |> function
                    | None -> ()
                    | Some caseFullName ->
                        failwithf "Nested union types in a route cannot have fields: %s" caseFullName 
                fun (name: string) ->
                    cases |> Array.tryFind (fun c ->
                        normalizeName c.Name = name)
                    |> function
                        | None -> failwithf "Cannot find case in %s corresponding to %s" t.FullName name
                        | Some c -> FSharpValue.MakeUnion(c, [||])
            else
                failwith "Only strings, ints, decimals and unions without fields are supported"
        let parse =
            let fullname =
                if t.IsGenericType then t.GetGenericTypeDefinition().FullName
                else t.FullName
            if fullname = typedefof<obj option>.FullName then
                let parse = parsePrimitive t.GenericTypeArguments.[0]
                fun s ->
                    if System.String.IsNullOrEmpty s then makeNone t
                    else parse s |> makeSome
            else parsePrimitive t
        member __.Parse x = parse x

    and CaseInfo(uci : UnionCaseInfo) =
        let normalizedName = normalizeName uci.Name
        let fieldTypesRaw = uci.GetFields() |> Array.map (fun f -> f.PropertyType)
        let fieldTypes = fieldTypesRaw |> Array.map FieldType
        member __.Source = uci
        member __.NormalizedName = normalizedName
        member __.FieldTypes = fieldTypes

    let fillMissingArgs (fieldTypes: FieldType[]) (args: string[]) =
        // TODO: Throw error if args is longer than field types? Shouldn't happen
        let fieldsLength = fieldTypes.Length
        let args = Array.truncate fieldsLength args
        [|yield! args
          for _i = args.Length to fieldsLength - 1 do yield ""|]

    let argTohash (arg: obj) =
        if isNone arg then None
        else
            if Reflection.isUnion arg then
                Reflection.getCaseName arg |> normalizeName |> Some
            // TODO: .ToString should work for now, but it may not
            // if we add other argument types
            else arg.ToString() |> Some

open RouterHelpers

/// T must be a union type
/// Case names will be transformed to hash like: FooBar -> #foo_bar
/// Use `CompiledName` attribute if you want to use a different name in the actual route
/// An empty hash will match the first case (Home, Index...)
/// Case fields will become hash arguments like: FooBar of string * int -> #foo_bar/baz/15
/// Case fields can only be strings, ints, decimals or unions without fields
/// Optional arguments are accepted but must be at the end
type Router<'T> private (parser : Browser.Types.Location -> 'T, toHash : 'T -> string) =
    member __.Parse x =
        try
            parser x |> Some
        with ex ->
            JS.console.error("Route parser error", ex.Message)
            None
    member __.ToHash x = toHash x
    member __.Href route = Fable.React.Props.Href (toHash route)
    member __.ModifyUrl route = route |> toHash |> Elmish.Navigation.Navigation.modifyUrl
    member __.NewUrl route = route |> toHash |> Elmish.Navigation.Navigation.newUrl
    member __.ModifyLocation route = Browser.Dom.window.location.href <- toHash route            

    static member Create([<Inject>] ?resolver : ITypeResolver<'T>) = 
        let t = resolver.Value.ResolveType()
        if not(FSharpType.IsUnion t) then
            failwithf "%s must be a union type to create the router" t.FullName
        let cases =            
            FSharpType.GetUnionCases(t)
            |> Array.map CaseInfo
        // TODO: Case validation
        let parser (location : Browser.Types.Location) : 'T =
            let parts = (location.hash.Substring 1).Split('/')
            let case, args =
                if parts.[0] = "" then
                    // TODO: Assuming the first case is the default one, is this correct?
                    Array.head cases, [||]
                else
                    match cases |> Array.tryFind (fun c -> c.NormalizedName = parts.[0]) with
                    | None -> failwithf "Cannot parse route %s" location.hash
                    | Some c ->
                        let args =
                            Array.skip 1 parts
                            |> fillMissingArgs c.FieldTypes
                            |> Array.zip c.FieldTypes
                            |> Array.map (fun (f, s) -> f.Parse s)
                        c, args
            FSharpValue.MakeUnion(case.Source, args) :?> 'T
        let toHash (x : 'T) =
            let uci, args = FSharpValue.GetUnionFields(x, t)
            [| yield normalizeName uci.Name
               // TODO: Ensure None fields are at the end
               yield! args |> Array.choose argTohash |]
            |> String.concat "/"
            |> (+) "#"
        Router(parser, toHash)
        
    static member Create(parser : Elmish.UrlParser.Parser<'T -> 'T, 'T>, toHash : 'T -> string) =
        let parse location =
            Elmish.UrlParser.parseHash parser location
            |> Option.defaultWith (fun _ -> failwithf "Cannot parse route %s" location.href)
        Router(parse, toHash)