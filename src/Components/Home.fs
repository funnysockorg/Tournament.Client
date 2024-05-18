module Tournament.Client.Components.Home
open Elmish
open Feliz

open Tournament.Client
open Tournament.Client.Model
open Tournament.Client.Components.Shared

[<RequireQualifiedAccess>]
type LoginResult =
    | Success
    | Failed of reason:string

module LoginResult =
    let ofQueryParameters = function
        | ("status", status)::rest ->
            match status with
            | "success" ->
                Ok LoginResult.Success
            | "failed" ->
                let reason =
                    match rest with
                    | ("reason", msg)::_ ->
                        msg
                    | _ ->
                        ""
                Ok (LoginResult.Failed reason)
            | _ ->
                sprintf "unknown status: %s with %A" status rest
                |> Error
        | xs ->
            sprintf "unknown query params: %A" xs
            |> Error

[<RequireQualifiedAccess>]
type Msg =
    | Start
    | ToLoginStep
    | Login
    | SetLoginResult of Result<LoginResult, string>
    | SetUser of Result<User, string>

[<RequireQualifiedAccess>]
type Deferred<'a> =
    | HasNotStartedYet
    | InProgress
    | Result of 'a

[<RequireQualifiedAccess>]
type State =
    | Start
    | PreLoginPage
    | LoginResult of Result<LoginResult, string>
    | GetUser of Deferred<Result<User, string>>

module State =
    let empty = State.Start

let init () =
    State.empty, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | Msg.Start ->
        state, Cmd.none
    | Msg.ToLoginStep ->
        let state =
            State.PreLoginPage
        state, Cmd.none
    | Msg.Login ->
        Browser.Dom.window.location.href <- Api.discordSignInUrl
        state, Cmd.none
    | Msg.SetLoginResult parserLoginResult ->
        let state =
            State.LoginResult parserLoginResult
        match parserLoginResult with
        | Ok loginResult ->
            match loginResult with
            | LoginResult.Success ->
                let state =
                    State.GetUser Deferred.InProgress
                let cmd =
                    Cmd.OfPromise.either
                        Api.getCurrentUser
                        ()
                        Ok
                        (sprintf "%A" >> Error)
                    |> Cmd.map Msg.SetUser
                state, cmd
            | LoginResult.Failed(reason) ->
                state, Cmd.none
        | _ ->
            state, Cmd.none
    | Msg.SetUser user ->
        let state =
            State.GetUser (Deferred.Result user)
        state, Cmd.none

let welcomeUnregistered (dispatch: Msg -> unit) =
    content [
        h1 [
            Html.text "Выбор иконки для сервера"
            Html.br []
            Html.div [
                prop.dangerouslySetInnerHTML "«Весёлый&nbsp;носок»"
            ]
        ]
        Html.div [
            prop.id "logo"
            prop.children [
                Html.img [
                    prop.className "w-full aspect-[1.45] max-w-[262px]"
                    prop.src "./images/logo.svg"
                ]
            ]
        ]
        Html.div [
            prop.className "w-[180px] h-[50px]"
            prop.children (
                button "Начать" (fun _ ->
                    dispatch Msg.ToLoginStep
                )
            )
        ]
    ]

let preLogin dispatch =
    content [
        h1 [
            Html.text "Логин"
        ]
        p [
            Html.text "Прежде чем приступить к выбору, нужно войти в приложение через Discord."
        ]
        Html.div [
            prop.className "w-[180px] h-[50px]"
            prop.children (
                button "Войти" (fun _ ->
                    dispatch Msg.Login
                )
            )
        ]
    ]

let preTournament user dispatch =
    content [
        h1 [
            Html.textf "Добро пожаловать, %s!" user.Nickname
        ]
        p [
            Html.text "Теперь Вы готовы к выборам."
        ]
        Html.div [
            prop.className "w-[180px] h-[50px]"
            prop.children (
                button "Приступить!" (fun _ ->
                    dispatch Msg.Start
                )
            )
        ]
    ]

let view (state: State) (dispatch: Msg -> unit) =
    match state with
    | State.Start ->
        welcomeUnregistered dispatch
    | State.PreLoginPage ->
        preLogin dispatch
    | State.LoginResult result ->
        Html.div [
            Html.div [
                prop.text "Что-то очень сильно пошло не так!"
            ]
            Html.pre [
                prop.textf "%A" result
            ]
            button "Попробовать еще раз" (fun _ ->
                dispatch Msg.Login
            )
        ]
    | State.GetUser status ->
        match status with
        | Deferred.InProgress ->
            Html.div [
                prop.text "Загрузочка..."
            ]
        | Deferred.Result user ->
            match user with
            | Ok user ->
                preTournament user dispatch
            | Error err ->
                Html.div [
                    Html.div [
                        prop.text "Что-то очень сильно пошло не так!"
                    ]
                    Html.pre [
                        prop.textf "%A" err
                    ]
                    button "Попробовать еще раз" (fun _ ->
                        dispatch Msg.Login
                    )
                ]
        | Deferred.HasNotStartedYet ->
            failwith "Not Implemented"
