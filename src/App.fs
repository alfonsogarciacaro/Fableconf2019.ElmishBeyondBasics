module App.View

open Elmish
open Fable.Core
open Fable.React
open Fable.React.Props
open State
open Types
open Fulma
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.Elmish.Demetrix

let authenticate (req: Authentication.Request) =
    promise {
        if req.Username.StartsWith("bar") then
            failwith "Invalid user"
        do! Promise.sleep 4000
        return req.Username + ":" + System.Guid.NewGuid().ToString()
    }

let testToken (token: Token) =
    promise {
        do! Promise.sleep 3000
        return token.StartsWith("foo")
    }

let private navbarEnd =
    Navbar.End.div [ ]
        [ Navbar.Item.div [ ]
            [ Field.div [ Field.IsGrouped ]
                [ Control.p [ ]
                    [ Button.a [ Button.Props [ Href "https://github.com/MangelMaxime/fulma-demo" ] ]
                        [ Icon.icon [ ]
                            [ Fa.i [ Fa.Brand.Github ] [ ] ]
                          span [ ] [ str "Source" ] ] ] ] ] ]

let private navbarStart dispatch =
    Navbar.Start.div [ ]
        [ Navbar.Item.a [ Navbar.Item.Props [ OnClick (fun _ ->
                                                        Router.QuestionIndex
                                                        |> Router.modifyLocation) ] ]
            [ str "Home" ]
          Navbar.Item.div [ Navbar.Item.HasDropdown
                            Navbar.Item.IsHoverable ]
            [ Navbar.Link.div [ ]
                [ str "Options" ]
              Navbar.Dropdown.div [ ]
                [ Navbar.Item.a [ Navbar.Item.Props [ OnClick (fun _ -> dispatch ResetDatabase)] ]
                    [ str "Reset demo" ] ] ] ]

let private navbarView isBurgerOpen dispatch =
    div [ ClassName "navbar-bg" ]
        [ Container.container [ ]
            [ Navbar.navbar [ Navbar.CustomClass "is-primary" ]
                [ Navbar.Brand.div [ ]
                    [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                        [ Image.image [ Image.Is32x32 ]
                            [ img [ Src "assets/mini_logo.svg" ] ]
                          Heading.p [ Heading.Is4 ]
                            [ str "Fulma-demo" ] ]
                      // Icon display only on mobile
                      Navbar.Item.a [ Navbar.Item.Props [ Href "https://github.com/MangelMaxime/fulma-demo" ]
                                      Navbar.Item.CustomClass "is-hidden-desktop" ]
                                    [ Icon.icon [ ]
                                        [ Fa.i [ Fa.Brand.Github
                                                 Fa.Size Fa.FaLarge ] [ ] ] ]
                      // Make sure to have the navbar burger as the last child of the brand
                      Navbar.burger [ Fulma.Common.CustomClass (if isBurgerOpen then "is-active" else "")
                                      Fulma.Common.Props [
                                        OnClick (fun _ -> dispatch ToggleBurger) ] ]
                        [ span [ ] [ ]
                          span [ ] [ ]
                          span [ ] [ ] ] ]
                  Navbar.menu [ Navbar.Menu.IsActive isBurgerOpen ]
                    [ navbarStart dispatch
                      navbarEnd ] ] ] ]

let private renderPage model dispatch =
    match model with
    | { CurrentPage = Router.Greet name } ->
        let name = defaultArg name "World"
        Heading.h1 [] [JS.decodeURI(name) |> sprintf "Hello %s!" |> str]

    | { CurrentPage = _
        QuestionDispatcher = Some extractedModel } ->
        Question.Dispatcher.View.root model.Session extractedModel (QuestionDispatcherMsg >> dispatch)
    | _ ->
        Render.pageNotFound

let private root model dispatch =
    div [ ]
        [ navbarView model.IsBurgerOpen dispatch
          renderPage model dispatch ]


open Elmish.Debug
open Elmish.Navigation
open Elmish.UrlParser
open Elmish.HMR

// Init the first datas into the database
Database.Init()

Program.mkProgram init update root
|> Program.toNavigable Router.router.Parse urlUpdate
|> Authentication.Program.withAuthentication
    (fun model -> model.Token)
    (SetToken >> Navigable.UserMsg)
    authenticate
    testToken
    Authentication.defaultLoginView
|> Program.withReactSynchronous "elmish-app"
|> Program.run
