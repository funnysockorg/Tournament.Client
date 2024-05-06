module Tournament.Client.Components.TournamentResult
open Elmish
open Feliz

open Tournament.Client.Model

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
    Html.div [
        Html.ul (
            state.Participants
            |> List.map (fun partipant ->
                Html.li [
                    prop.text partipant.Name
                ]
            )
        )
        Html.button [
            prop.onClick (fun _ -> dispatch Msg.Submit)
            prop.text "Submit"
        ]
    ]