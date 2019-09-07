[<RequireQualifiedAccess>]
module Fable.Elmish.Demetrix.Validation

open System
open Fable.Core

type DateTime with
    /// yyyy-MM-dd is the format used by HTML <input />
    member dt.ToHTMLInput() =
        if dt = System.DateTime.MinValue then
            "" // Show an empty field
        else
            // TODO: Because of a bug in Fable, the year won't get four digits if less than 1000
            let str = dt.ToString("yyyy-MM-dd").PadLeft(10, '0')
            // TODO: There's an error in Fable formatting where year only gets two digits if its less than 100
            System.Text.RegularExpressions.Regex.Replace(str, "^(\d\d)-", "00$1-")

    static member TryParseDateFromHTMLInput (str: string) =
        try
            let year = int str.[0..3]
            let month = int str.[5..6]
            let day = int str.[8..9]
            let dt = System.DateTime(year, month, day)
            // Because Fable uses JS Date parsing under the hood, sometimes
            // invalid dates (like Feb 31st) are parsed correctly with a different day
            if dt.Year = year && dt.Month = month && dt.Day = day then
                Some dt
            else
                None
        with _ -> None


[<StringEnum; RequireQualifiedAccess>]
type InputType =
    | Email
    | Number
    | Password
    | Tel
    | Text
    | Url
    | Date
    | [<CompiledName("datetime-local")>] DatetimeLocal
    | Time

type Rule<'a> =
    {
        Description : unit -> string
        Validate : string -> Option<'a>
        InputType : InputType
    }

/// ### HOW TO USE (using RequireDate in the samples)
/// - Constructing from string: `RequireDate str`
/// - Accessing value: `x.email.Value // None if value is invalid`
/// - String representation for views: `x.email.StringRepresentation`
[<AbstractClass>]
type Field<'T when 'T : equality>(str: string, rule: Rule<'T>) =
    let v = rule.Validate str
    member __.Value = v
    member __.StringRepresentation = str
    member __.InputType = rule.InputType
    member __.IsValid = Option.isSome v
    member __.Rule = rule

/// Helper to encode validation fields with Thoth.Json
let encode<'V, 'F when 'F :> Field<'V>> (encoder: 'V -> Thoth.Json.JsonValue) (field: 'F) =
    match field.Value with
    | None -> failwithf "INVALID: %O" field.Value
    | Some v -> encoder v

/// Helper to decode validation fields with Thoth.Json
let decode<'V, 'F when 'F :> Field<'V>> (decoder: Thoth.Json.Decoder<'V>) (cons: string->'F): Thoth.Json.Decoder<'F> =
    fun path value ->
        match decoder path value with
        | Ok value -> value.ToString() |> cons |> Ok
        | Error er -> Error er

let isStringOfDigits str =
    str |> Seq.forall Char.IsDigit

let isNumericStringWithMaxDigits maxSize (str:string) =
    str.Length <= maxSize && isStringOfDigits str

let inline isNumberBetween min max i =
    match min, max with
    | Some min, Some max -> min <= i && i <= max
    | Some min, None -> min <= i
    | None, Some max -> i <= max
    | None, None -> true

let numberDescription (min: Option<'a>) (max: Option<'a>) () =
    match min, max with
    | Some min, Some max ->
        String.Format("Number must be between {0} and {1}", min, max)
    | Some min, None ->
        String.Format("Number must be greater than {0}", min)
    | None, Some max ->
        String.Format("Number must be less than {0}", max)
    | None, None ->
        "Field must be a number"

let validString required rule x =
    if System.String.IsNullOrEmpty x then
        if required then None else Some x
    else
        if rule x then Some x else None

let numbersOnly required =
    {
        Description = fun () ->
            if required
            then "Field must be non-empty and contain only numbers"
            else "Field must be empty or contain only numbers"
        Validate = validString required isStringOfDigits
        InputType = InputType.Text
    }

let string (required: bool) (maxSize:int) =
    {
        Description = fun () ->
            if required
            then String.Format("Field must be non-empty and not more than {0} chars", maxSize)
            else String.Format("Field must be empty or not more than {0} chars", maxSize)
        Validate = validString required (fun x -> x.Length <= maxSize)
        InputType = InputType.Text
    }

let requireString<'T> =
    {
        Description = fun () -> "Field must be non-empty"
        Validate = validString true (fun _ -> true)
        InputType = InputType.Text
    }

let date required =
    {
        Description = fun () ->
            if required
            then "Field must be non-empty and a valid date"
            else "Field must be empty or a valid date"
        Validate = fun x ->
            if System.String.IsNullOrEmpty x then
                if required then None else Some DateTime.MinValue
            else
                DateTime.TryParseDateFromHTMLInput x
        InputType = InputType.Date
    }

let integer (minSize: int option) (maxSize: int option): Rule<int> =
    {
        Description = numberDescription minSize maxSize
        Validate = fun str ->
            match Int32.TryParse str with
            | true, x -> Some x
            | false, _ -> None
            |> Option.bind (fun x -> if isNumberBetween minSize maxSize x then Some x else None)
        InputType = InputType.Number
    }

let decimal (minSize: Option<decimal>) (maxSize: Option<decimal>): Rule<decimal> =
    {
        Description = numberDescription minSize maxSize
        Validate = fun str ->
            // TODO: Atm this only works with English numbers
            // because Fable doesn't check current culture
            match Decimal.TryParse str with
            | true, x -> x |> Some
            | false, _ -> None
            |> Option.bind (fun x -> if isNumberBetween minSize maxSize x then Some x else None)
        InputType = InputType.Number // To allow commas we may need to set this to InputType.Text
    }

let numericString required (maxDigits:int) =
    {
        Description = fun () ->
            if required
            then String.Format("Field must be non-empty and not more than {0} digits", maxDigits)
            else String.Format("Field must be empty or not more than {0} digits", maxDigits)
        Validate =
            validString required (isNumericStringWithMaxDigits maxDigits)
        InputType = InputType.Text
    }

type RequireString(v) =
    inherit Field<string>(v, requireString)

type RequireNumbersOnly(v) =
    inherit Field<string>(v, numbersOnly true)

type NumbersOnly(v) =
    inherit Field<string>(v, numbersOnly false)

type RequireDecimal(v) =
    inherit Field<decimal>(v, decimal None None)

type RequireDate(v) =
    inherit Field<DateTime>(v, date true)

type OptionalDate(v) =
    inherit Field<DateTime>(v, date false)
