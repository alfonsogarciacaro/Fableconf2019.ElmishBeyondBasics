module Question.Dispatcher.View

open Types

let root user model dispatch =
    match model with
    | { CurrentPage = Router.QuestionIndex
        IndexModel = Some extractedModel } -> Question.Index.View.root extractedModel (IndexMsg >> dispatch)

    | { CurrentPage = Router.QuestionShow _
        ShowModel = Some extractedModel } -> Question.Show.View.root user extractedModel (ShowMsg >> dispatch)

    | { CurrentPage = Router.QuestionCreate
        CreateModel = Some extractedModel } -> Question.Create.View.root user extractedModel (CreateMsg >> dispatch)

    | _ ->
        Render.pageNotFound
