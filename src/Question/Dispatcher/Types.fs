module Question.Dispatcher.Types

type Model =
    { CurrentPage : Router.Page
      IndexModel : Question.Index.Types.Model option
      ShowModel : Question.Show.Types.Model option
      CreateModel : Question.Create.Types.Model option }

    static member Empty =
        { CurrentPage = Router.QuestionIndex
          IndexModel = None
          ShowModel = None
          CreateModel = None }

type Msg =
    | IndexMsg of Question.Index.Types.Msg
    | ShowMsg of Question.Show.Types.Msg
    | CreateMsg of Question.Create.Types.Msg
