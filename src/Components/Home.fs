module Tournament.Client.Components.Home
open Elmish
open Feliz

open Tournament.Client.Model

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

let view (state: State) (dispatch: Msg -> unit) =
    match state.User with
    | None ->
        Html.div [
            Html.h1 [
                prop.text "Welcome!"
            ]
            Html.div [
                Html.button [
                    prop.onClick (fun _ -> dispatch Msg.Login)
                    prop.text "Login"
                ]
            ]
        ]
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
