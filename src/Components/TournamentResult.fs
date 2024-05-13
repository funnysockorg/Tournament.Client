module Tournament.Client.Components.TournamentResult
open Elmish
open Feliz

open Tournament.Client.Model
open Tournament.Client.Components.Shared

type State =
    {
        Participants: Participant list
    }

module State =
    let create participants =
        {
            Participants = participants
        }

[<RequireQualifiedAccess>]
type Msg =
    | Submit

let init participants =
    State.create participants, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | Msg.Submit ->
        state, Cmd.none

let view (state: State) (dispatch: Msg -> unit) =
    content [
        h1 [
            Html.text "Итоги Вашего выбора"
        ]
        serverIconItems state.Participants
        Html.div [
            prop.className "w-[180px] h-[50px]"
            prop.children (
                button "Submit" (fun _ ->
                    dispatch Msg.Submit
                )
            )
        ]
    ]
