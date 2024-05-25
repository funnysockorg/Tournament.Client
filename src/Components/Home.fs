module Tournament.Client.Components.Home
open Elmish
open Feliz

open Tournament.Client
open Tournament.Client.Model
open Tournament.Client.Components.Shared

[<RequireQualifiedAccess>]
type Deferred<'a> =
    | HasNotStartedYet
    | InProgress
    | Result of 'a

type AuthCallback =
    {
        Code: Api.AuthCallback.DiscordAuthCode
    }

module AuthCallbackParams =
    let ofQueryParameters = function
        | ("code", code)::rest ->
            Ok {
                Code = code
            }
        | xs ->
            sprintf "unknown query params: %A" xs
            |> Error

module AuthCodeExchange =
    open Elmish
    open Feliz

    [<RequireQualifiedAccess>]
    type Msg =
        | Exchange
        | SetResult of Result<Api.ApiAuthToken, string>

    type State =
        {
            Code: AuthCallback
            Result: Deferred<Result<Api.ApiAuthToken, string>>
        }
    module State =
        let create code =
            {
                Code = code
                Result = Deferred.HasNotStartedYet
            }

    let init queryParams =
        let state = State.create queryParams
        state, Cmd.ofMsg Msg.Exchange

    let update (msg: Msg) (state: State) : State * Cmd<Msg> =
        match msg with
        | Msg.Exchange ->
            let state =
                { state with
                    Result = Deferred.InProgress
                }
            let cmd =
                Cmd.OfPromise.either
                    Api.AuthCallback.request
                    state.Code.Code
                    Ok
                    (sprintf "%A" >> Error)
                |> Cmd.map (
                    Result.bind (
                        Result.mapError (fun err ->
                            Fable.Core.JS.JSON.stringify err
                        )
                    )
                    >> Msg.SetResult
                )
            state, cmd
        | Msg.SetResult result ->
            let state =
                { state with
                    Result = Deferred.Result result
                }
            state, Cmd.none

    let view (state: State) (dispatch: Msg -> unit) =
        match state.Result with
        | Deferred.InProgress ->
            spinner
        | Deferred.Result result ->
            match result with
            | Error errMessage ->
                Html.div [
                    Html.div [
                        prop.text "Во время обмена кода что-то пошло не так!"
                    ]
                    Html.pre [
                        prop.textf "%s" errMessage
                    ]
                    button "Попробовать еще раз" (fun _ ->
                        dispatch Msg.Exchange
                    )
                ]
            | Ok(resultValue) ->
                Html.div [
                    prop.text "Успех!"
                ]
        | Deferred.HasNotStartedYet ->
            button "Обменять код" (fun _ ->
                dispatch Msg.Exchange
            )

module GettingUser =
    open Elmish
    open Feliz

    [<RequireQualifiedAccess>]
    type Msg =
        | Start
        | SetResult of Result<User, string>

    type State =
        {
            AuthToken: Api.ApiAuthToken
            Result: Deferred<Result<User, string>>
        }
    module State =
        let create code =
            {
                AuthToken = code
                Result = Deferred.HasNotStartedYet
            }

    let init apiAuthCode =
        let state = State.create apiAuthCode
        state, Cmd.ofMsg Msg.Start

    let update (msg: Msg) (state: State) : State * Cmd<Msg> =
        match msg with
        | Msg.Start ->
            let state =
                { state with
                    Result = Deferred.InProgress
                }
            let cmd =
                Cmd.OfPromise.either
                    Api.getCurrentUser
                    state.AuthToken
                    Ok
                    (sprintf "%A" >> Error)
                |> Cmd.map (
                    Result.bind (
                        Result.mapError (fun err ->
                            Fable.Core.JS.JSON.stringify err
                        )
                    )
                    >> Msg.SetResult
                )
            state, cmd
        | Msg.SetResult result ->
            let state =
                { state with
                    Result = Deferred.Result result
                }
            state, Cmd.none

    let view (state: State) (dispatch: Msg -> unit) =
        match state.Result with
        | Deferred.InProgress ->
            spinner
        | Deferred.Result result ->
            match result with
            | Error errMessage ->
                Html.div [
                    Html.div [
                        prop.text "Во время получения пользовательских данных что-то пошло не так!"
                    ]
                    Html.pre [
                        prop.textf "%s" errMessage
                    ]
                    button "Попробовать еще раз" (fun _ ->
                        dispatch Msg.Start
                    )
                ]
            | Ok(resultValue) ->
                Html.div [
                    prop.text "Успех!"
                ]
        | Deferred.HasNotStartedYet ->
            button "Получить пользовательские данные" (fun _ ->
                dispatch Msg.Start
            )

[<RequireQualifiedAccess>]
type Msg =
    | Start
    | ToLoginStep
    | Login
    | StartAuthCodeExchange of Result<AuthCallback, string>
    | AuthCallback of AuthCodeExchange.Msg
    | GettingUser of GettingUser.Msg
    | SetUser of User

[<RequireQualifiedAccess>]
type State =
    | Start
    | PreLoginPage
    | AuthCodeExchangeQueryParamsParserError of string
    | AuthCodeExchange of AuthCodeExchange.State
    | GettingUser of GettingUser.State
    | GetUser of User

module State =
    let empty = State.Start

let init () =
    State.empty, Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
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
    | Msg.StartAuthCodeExchange queryParamsParserResult ->
        match queryParamsParserResult with
        | Ok x ->
            let codeExchangeState, codeExchangeCmd =
                AuthCodeExchange.init x
            let state =
                State.AuthCodeExchange codeExchangeState
            let cmd =
                codeExchangeCmd
                |> Cmd.map Msg.AuthCallback
            state, cmd
        | Error(errorValue) ->
            let state =
                State.AuthCodeExchangeQueryParamsParserError errorValue
            state, Cmd.none
    | Msg.AuthCallback codeExchangeMsg ->
        match state with
        | State.AuthCodeExchange codeExchangeState ->
            let codeExchangeState, codeExchangeCmd =
                AuthCodeExchange.update codeExchangeMsg codeExchangeState
            match codeExchangeState.Result with
            | Deferred.Result(Ok authToken) ->
                let gettingUserState, gettingUserCmd =
                    GettingUser.init authToken
                let state =
                    State.GettingUser gettingUserState
                let cmd =
                    gettingUserCmd
                    |> Cmd.map Msg.GettingUser
                state, cmd
            | _ ->
                let state =
                    State.AuthCodeExchange codeExchangeState
                let cmd =
                    codeExchangeCmd
                    |> Cmd.map Msg.AuthCallback
                state, cmd
        | _ ->
            state, Cmd.none
    | Msg.GettingUser gettingUserMsg ->
        match state with
        | State.GettingUser gettingUserState ->
            let gettingUserState, gettingUserCmd =
                GettingUser.update gettingUserMsg gettingUserState
            match gettingUserState.Result with
            | Deferred.Result(Ok user) ->
                // todo: move `gettingUserState.AuthToken` to state
                state, Cmd.ofMsg (Msg.SetUser user)
            | _ ->
                let state =
                    State.GettingUser gettingUserState
                let cmd =
                    gettingUserCmd
                    |> Cmd.map Msg.GettingUser
                state, cmd
        | _ ->
            state, Cmd.none
    | Msg.SetUser user ->
        let state =
            State.GetUser user
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
    | State.AuthCodeExchangeQueryParamsParserError errorMessage ->
        Html.div [
            Html.div [
                prop.text "AuthCodeExchangeQueryParamsParserError:"
            ]
            Html.pre [
                prop.textf "%s" errorMessage
            ]
        ]
    | State.AuthCodeExchange authCodeExchange ->
        AuthCodeExchange.view authCodeExchange (Msg.AuthCallback >> dispatch)
    | State.GettingUser gettingUserState ->
        GettingUser.view gettingUserState (Msg.GettingUser >> dispatch)
    | State.GetUser user ->
        preTournament user dispatch
