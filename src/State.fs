module App.State

open Fable.Core
open Browser
open Elmish
open Types


let urlUpdate (result: Option<Router.Page>) model =
    match result with
    | None ->

        JS.console.error("Error parsing url: " + window.location.href)
        model, Router.modifyUrl model.CurrentPage

    | Some page ->
        let model = { model with CurrentPage = page }
        match page with
        | Router.Home ->
            let (subModel, subCmd) = Question.Dispatcher.State.init Router.QuestionIndex
            { model with QuestionDispatcher = Some subModel }, Cmd.map QuestionDispatcherMsg subCmd
        | Router.Greet name ->
            { model with CurrentPage = Router.Greet name }, Cmd.none
        | questionPage ->
            let (subModel, subCmd) = Question.Dispatcher.State.init questionPage
            { model with QuestionDispatcher = Some subModel }, Cmd.map QuestionDispatcherMsg subCmd

let init result =
    urlUpdate result Model.Empty

let update msg model =
    match (msg, model) with
    | (QuestionDispatcherMsg msg, { QuestionDispatcher = Some extractedModel }) ->
        let (subModel, subCmd) = Question.Dispatcher.State.update model.Session msg extractedModel
        { model with QuestionDispatcher = Some subModel }, Cmd.map QuestionDispatcherMsg subCmd

    | (QuestionDispatcherMsg capturedMsg, _) ->
        JS.console.log("[App.State] Discarded message")
        printfn "%A" capturedMsg
        model, Cmd.none

    | (ResetDatabase, _) ->
        Database.Restore()
        let redirect =
            match model.CurrentPage with
            | Router.QuestionIndex ->
                Router.Home
            | _ ->
                Router.QuestionIndex
            |> Router.newUrl

        model, redirect

    | (ToggleBurger, _) ->
        { model with IsBurgerOpen = not model.IsBurgerOpen }, Cmd.none

    | SetToken token, _ ->
        { model with Token = Some token }, Cmd.none
