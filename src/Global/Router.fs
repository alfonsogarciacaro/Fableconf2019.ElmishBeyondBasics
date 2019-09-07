module Router

open Browser
open Fable.React.Props
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Elmish.Demetrix

type Page =
    | Home
    | Greet of string option
    | QuestionIndex
    | QuestionShow of int
    | QuestionCreate

let router = Router.Router<Page>.Create()

let private toHash page = router.ToHash page

let href route =
    Href (toHash route)

let modifyUrl route =
    route |> toHash |> Navigation.modifyUrl

let newUrl route =
    route |> toHash |> Navigation.newUrl

let modifyLocation route =
    window.location.href <- toHash route
