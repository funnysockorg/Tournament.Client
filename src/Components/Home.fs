module Tournament.Client.Components.Home
open Elmish
open Feliz

open Tournament.Client.Model
open Tournament.Client.Components.Shared

[<RequireQualifiedAccess>]
type Msg =
    | ToLoginStep
    | Login
    | LoginResult of User
    | Start

type State =
    {
        User: User option
        IsPreLoginPage: bool
    }

module State =
    let empty =
        {
            User = None
            IsPreLoginPage = false
        }

let init () =
    State.empty, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | Msg.ToLoginStep ->
        let state =
            { state with
                IsPreLoginPage = true
            }
        state, Cmd.none
    | Msg.Login ->
        let cmd =
            Msg.LoginResult User.mock
            |> Cmd.ofMsg
        state, cmd
    | Msg.LoginResult user ->
        let state =
            { state with
                User = Some user
            }
        state, Cmd.none
    | Msg.Start ->
        state, Cmd.none

let welcomeUnregistered (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [
            "size-full"

            "flex"
            "flex-col"
            "justify-between"
            "items-center"
        ]
        prop.children [
            Html.div []
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
            Html.div []
        ]
    ]

let preLogin dispatch =
    Html.div [
        prop.classes [
            "size-full"

            "flex"
            "flex-col"
            "justify-between"
            "items-center"
        ]
        prop.children [
            Html.div []
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
            Html.div []
        ]
    ]

let preTournament user dispatch =
    Html.div [
        prop.classes [
            "size-full"

            "flex"
            "flex-col"
            "justify-between"
            "items-center"
        ]
        prop.children [
            Html.div []
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
            Html.div []
        ]
    ]

let view (state: State) (dispatch: Msg -> unit) =
    match state.User with
    | None ->
        if not state.IsPreLoginPage then
            welcomeUnregistered dispatch
        else
            preLogin dispatch
    | Some user ->
        preTournament user dispatch
