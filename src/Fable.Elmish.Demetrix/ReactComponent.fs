module Fable.Elmish.Demetrix.ReactComponent

open Elmish
open Fable.React

// Function is inlined so we can go to the view function source from React dev tools

/// Helper to transform an Elmish view function into a true React component
/// Component will only rerender if the equality function evaluates to false
let inline makeWithEquality (componentName: string) (view: 'Model -> Dispatch<'Msg> -> ReactElement) (equality: 'Model -> 'Model -> bool) =
    let Component = FunctionComponent.Of((fun (p: {| model: _; dispatch: _ |}) ->
        view p.model p.dispatch), displayName = componentName, memoizeWith = fun p1 p2 -> equality p1.model p2.model)
    fun model dispatch ->
        Component {| model = model; dispatch = dispatch |}

/// Helper to transform an Elmish view function into a true React component
/// Component will only rerender if the new model is different to the previous one
let inline make (componentName: string) (view: 'Model -> Dispatch<'Msg> -> ReactElement) =
    makeWithEquality componentName view (=)

/// Components that don't dispatch messages
/// Useful to transform React helper functions into true components with memoization
/// Component will only rerender if the equality function evaluates to false
let makeSimpleWithEquality (componentName: string) (view: 'Model -> ReactElement) (equality: 'Model -> 'Model -> bool) =
    FunctionComponent.Of(view, displayName = componentName, memoizeWith = equality)

/// Components that don't dispatch messages
/// Useful to transform React helper functions into true components with memoization
/// Component will only rerender if the new model is different to the previous one
let inline makeSimple (componentName: string) (view: 'Model -> ReactElement) =
    makeSimpleWithEquality componentName view (=)

/// Components à-la-Elmish without being integrated with the parent component
/// Useful if you need a helper component that shouldn't clutter the app model
/// Component will only rerender if the equality function evaluates to false
let makeStatefulWithEquality (componentName: string)
                             (init: 'Props -> 'State)
                             (update: 'Msg -> 'State -> 'State * Cmd<'Msg>)
                             (view: 'Props -> 'State -> Dispatch<'Msg> -> ReactElement)
                             (equality: 'Props -> 'Props -> bool) =
    let render props =
        let state = Hooks.useStateLazy(fun () -> init props)
        let rec dispatch msg =
            state.update(fun st ->
                let m, cmd = update msg st
                for cmd in cmd do
                    cmd dispatch
                // TODO: Return the old state if the value returned by update
                // is structurally equal to prevent a rerender?
                m)
        view props state.current dispatch
    FunctionComponent.Of(render, displayName = componentName, memoizeWith = equality)

/// Components à-la-Elmish without being integrated with the parent component
/// Useful if you need a helper component that shouldn't clutter the app model
/// Component will only rerender if the new model is different to the previous one
let inline makeStateful componentName
                        (init: 'Props -> 'State)
                        (update: 'Msg -> 'State -> 'State * Cmd<'Msg>)
                        (view: 'Props -> 'State -> Dispatch<'Msg> -> ReactElement) =
    makeStatefulWithEquality componentName init update view (=)
