module Tournament.Client.Components.Tournament
open Elmish
open Feliz

open Tournament.Client.Model

type State =
    {
        Participants: Participant list
        SortState: MergeSort.Start.Main<Participant>
    }

module State =
    let create participants =
        {
            Participants = participants
            SortState = MergeSort.Start.start participants
        }

[<RequireQualifiedAccess>]
type Msg =
    | Choice of MergeSort.Choice
    | Result of Participant list

let init participants =
    State.create participants, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | Msg.Choice choice ->
        let sortState =
            MergeSort.choice choice state.SortState
        let state =
            { state with
                SortState =
                    sortState
            }
        let cmd =
            match MergeSort.getResult sortState with
            | Some result ->
                Cmd.ofMsg (Msg.Result result)
            | None ->
                Cmd.none
        state, cmd
    | Msg.Result result ->
        state, Cmd.none

let view (state: State) (dispatch: Msg -> unit) =
    match MergeSort.getCurrentCandidates state.SortState with
    | Some (x, y) ->
        Html.div [
            Html.h1 [
                prop.text "Choice the best!"
            ]
            Html.div [
                Html.button [
                    prop.onClick (fun _ -> dispatch (Msg.Choice MergeSort.Choice.Left))
                    prop.textf "%s" x.Name
                ]
                Html.button [
                    prop.onClick (fun _ -> dispatch (Msg.Choice MergeSort.Choice.Right))
                    prop.textf "%s" y.Name
                ]
            ]
        ]
    | None ->
        Html.pre [
            prop.textf "%A" state.SortState
        ]
