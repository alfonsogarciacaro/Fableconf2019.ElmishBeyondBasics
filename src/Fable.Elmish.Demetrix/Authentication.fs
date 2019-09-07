[<RequireQualifiedAccess>]
module Fable.Elmish.Demetrix.Authentication

open Fable.Core
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Browser
open Elmish
open Fulma
open Thoth.Json

type Request =
    { Username : string
      Password : string }
    static member Empty =
        { Username = ""
          Password = "" }

type Model<'model> =
    { Model : 'model
      Request : Request
      Error : exn option
      Loading : bool }

type Msg<'user, 'msg> =
    | UpdateUsername of string
    | UpdatePassword of string
    | RequestAuthentication of Request
    | AuthenticationResult of Result<'user, exn>
    | TestUserResult of 'user * Result<bool, exn>
    | AppMsg of 'msg

let private optionOfString str =
    if System.String.IsNullOrWhiteSpace(str) then None else Some(str.Trim())

let private mkInputWithIcon icon inputType value htmlProps dispatch =
    Control.div [
        Control.HasIconLeft
        Control.Props [ Style [ MarginBottom "15px" ] ]
    ] [
        Input.input [
            Input.Value value
            Input.Type inputType
            Input.Props htmlProps
            Input.OnChange (fun ev -> ev.Value |> dispatch)
        ]
        Icon.icon [Icon.IsLeft] [Fa.i [icon] []]
    ]

let private login (model: Model<'model>) dispatch =
    let validRequest =
        match optionOfString model.Request.Username,
              optionOfString model.Request.Password with
        | Some name, Some pw -> Some { Username = name; Password = pw }
        | _ -> None

    Card.card [] [
        Card.header [] [
            Card.Header.title [
                Card.Header.Title.IsCentered
            ] [ Heading.h3 [] [str "Login"] ]
        ]
        Card.content [] [
            model.Error
            |> Option.map (fun err -> Help.help [ Help.Color IsDanger ] [ str err.Message ])
            |> ofOption

            mkInputWithIcon Fa.Solid.User Input.Text model.Request.Username [
                AutoFocus true
                Placeholder "Username"
            ] (UpdateUsername >> dispatch)

            // TODO: Request authentication when hitting enter
            mkInputWithIcon Fa.Solid.Lock Input.Password model.Request.Password [
                Placeholder "Password"
            ] (UpdatePassword >> dispatch)

            Button.button [
                Button.Color IsPrimary
                Button.Size IsMedium
                Button.IsFullWidth
                Button.OnClick(fun _ ->
                    validRequest
                    |> Option.iter (RequestAuthentication >> dispatch))
                Button.Disabled(model.Loading || Option.isNone validRequest)
                Button.IsLoading model.Loading
            ] [ str "Sign in" ]
        ]
    ]

let defaultLoginView (model: Model<'model>) (dispatch: Dispatch<Msg<'user, 'msg>>): ReactElement =
    Hero.hero [ Hero.IsFullHeight; Hero.IsMedium; Hero.IsBold ]
        [ Hero.body []
              [ Container.container [ Container.IsFluid ]
                    [ Columns.columns [ Columns.IsCentered ]
                          [ login model dispatch ] ] ] ]

let [<Literal>] STORAGE_KEY = "__AUTHENTICATION_STORAGE__"

module Program =

    let withAuthenticationCoders (encodeUser: Encoder<'user>)
                                 (decodeUser: Decoder<'user>)
                                 (getUser: 'model->'user option)
                                 (setUser: 'user -> 'msg)
                                 (authenticate: Request -> JS.Promise<'user>)
                                 (testUser: 'user -> JS.Promise<bool>)
                                 (loginView: Model<'model> -> Dispatch<Msg<'user, 'msg>> -> ReactElement)
                                 (program: Program<'arg, 'model, 'msg, ReactElement>) =
            let mapInit init arg =
                let model, appCmd = init arg
                let authCmd =
                    match getUser model with
                    | Some _ -> Cmd.none
                    | None ->
                        let userJson = window.localStorage.getItem STORAGE_KEY
                        match optionOfString userJson with
                        | None -> Cmd.none
                        | Some userJson ->
                            match Decode.fromString decodeUser userJson with
                            | Ok user ->
                                Cmd.OfPromise.either testUser user
                                    (fun x -> TestUserResult(user, Ok x))
                                    (fun er -> TestUserResult(user, Error er))
                            | Error er ->
                                JS.console.error("Cannot deserialized stored user", er, userJson)
                                Cmd.none
                { Model = model; Request = Request.Empty; Error = None; Loading = not(List.isEmpty authCmd) },
                Cmd.batch [Cmd.map AppMsg appCmd; authCmd]

            let mapView view (model: Model<'model>) dispatch =
                match getUser model.Model with
                | Some _ -> view model.Model (AppMsg >> dispatch)
                | None -> loginView model dispatch

            let mapUpdate update msg model =
                match msg with
                | UpdateUsername username ->
                    { model with Request = { model.Request with Username = username } }, Cmd.none
                | UpdatePassword password ->
                    { model with Request = { model.Request with Password = password } }, Cmd.none
                | RequestAuthentication req ->
                    { model with Error = None; Loading = true },
                    Cmd.OfPromise.either authenticate req (Ok >> AuthenticationResult) (Error >> AuthenticationResult)
                | AuthenticationResult(Ok user) ->
                    // Store user in localStorage
                    window.localStorage.setItem(STORAGE_KEY, encodeUser user |> Encode.toString 0)
                    { model with Request = Request.Empty; Error = None; Loading = false },
                    setUser user |> AppMsg |> Cmd.OfFunc.result
                | AuthenticationResult(Error er) ->
                    { model with Error = Some er; Loading = false }, Cmd.none
                | TestUserResult(user, Ok pass) ->
                    let cmd = if pass then setUser user |> AppMsg |> Cmd.OfFunc.result else Cmd.none
                    { model with Error = None; Loading = false }, cmd
                | TestUserResult(_, Error er) ->
                    { model with Error = Some er; Loading = false }, Cmd.none
                | AppMsg msg ->
                    let m, cmd = update msg model.Model
                    { model with Model = m }, Cmd.map AppMsg cmd

            let mapSetState setState model dispatch =
                setState model.Model (AppMsg >> dispatch)

            let mapSubscribe subscribe model =
                subscribe model.Model |> Cmd.map AppMsg

            Program.map mapInit mapUpdate mapView mapSetState mapSubscribe program

    let inline withAuthentication (getUser: 'model->'user option)
                                  (setUser: 'user -> 'msg)
                                  (authenticate: Request -> JS.Promise<'user>)
                                  (testUser: 'user -> JS.Promise<bool>)
                                  (loginView: Model<'model> -> Dispatch<Msg<'user, 'msg>> -> ReactElement)
                                  (program: Program<'arg, 'model, 'msg, ReactElement>) =
        let encoder = Encode.Auto.generateEncoder<'user>()
        let decoder = Decode.Auto.generateDecoder<'user>()
        withAuthenticationCoders encoder decoder getUser setUser authenticate testUser loginView program
