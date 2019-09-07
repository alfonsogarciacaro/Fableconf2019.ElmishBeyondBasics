module App.Types

type Author =
    { Id : int
      Firstname: string
      Surname: string
      Avatar : string }

type Question =
    { Id : int
      Author : Author
      Title : string
      Description : string
      CreatedAt : string }

type Token = string

type Model =
    { CurrentPage : Router.Page
      Session : User
      QuestionDispatcher : Question.Dispatcher.Types.Model option
      IsBurgerOpen : bool
      Token: Token option }

    static member Empty =
        { CurrentPage =
            Router.QuestionIndex
          Session =
            let userId = 3
            match Database.GetUserById userId with
            | Some user -> user
            | None -> failwithf "User#%i not found" userId
          QuestionDispatcher = None
          IsBurgerOpen = false
          Token = None }

type Msg =
    | QuestionDispatcherMsg of Question.Dispatcher.Types.Msg
    | ResetDatabase
    | ToggleBurger
    | SetToken of Token
