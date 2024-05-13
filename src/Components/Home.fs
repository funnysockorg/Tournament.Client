module Tournament.Client.Components.Home
open Elmish
open Feliz

open Tournament.Client.Model
open Tournament.Client.Components.Shared

[<RequireQualifiedAccess>]
type Msg =
    | Login
    | LoginResult of User
    | Start

type State =
    {
        User: User option
    }

module State =
    let empty =
        {
            User = None
        }

let init () =
    State.empty, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
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
            Html.h1 [
                prop.classes [
                    "text-3xl"
                    "text-black"
                    "text-opacity-70"
                    "text-center"
                ]
                prop.children [
                    Html.text "Выбор иконки для сервера"
                    Html.br []
                    Html.div [
                        prop.dangerouslySetInnerHTML "«Весёлый&nbsp;носок»"
                    ]
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
                        dispatch Msg.Login
                    )
                )
            ]
            Html.div []
        ]
    ]

let view (state: State) (dispatch: Msg -> unit) =
    match state.User with
    | None ->
        welcomeUnregistered dispatch
    | Some user ->
        Html.div [
            Html.h1 [
                prop.textf "Welcome, %s!" user.Nickname
            ]
            Html.div [
                Html.button [
                    prop.onClick (fun _ -> dispatch Msg.Start)
                    prop.text "Start tournament"
                ]
            ]
        ]
