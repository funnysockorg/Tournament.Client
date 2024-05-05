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
type Choice =
    | Left
    | Right

[<RequireQualifiedAccess>]
type Msg =
    | Choice of Choice
    | Result of Participant list

let init participants =
    State.create participants, Cmd.none

module MergeSortInterp =
    open MergeSort

    let choice choiceArg state =
        match state with
        | Start.Main.JoinSortedLists x ->
            match x with
            | JoinSortedLists.Main.JoinTwoSortedListsReq (joinTwoSortedListsCmd, loop) ->
                match joinTwoSortedListsCmd with
                | JoinTwoSortedLists.Main.IsGreaterThanReq ((x, y), cmd) ->
                    let isGreaterThan =
                        match choiceArg with
                        | Choice.Left -> true
                        | Choice.Right -> false
                    let joinTwoSortedListsCmd =
                        cmd
                        |> JoinTwoSortedLists.IsGreaterThan.exec isGreaterThan
                    JoinSortedLists.Main.JoinTwoSortedListsReq (joinTwoSortedListsCmd, loop)
                    |> Start.Main.JoinSortedLists
                    |> startInterp
                | _ ->
                    startInterp state
            | _ ->
                startInterp state
        | _ ->
            startInterp state

    let getCurrentCandidates = function
        | Start.Main.JoinSortedLists x ->
            match x with
            | JoinSortedLists.Main.JoinTwoSortedListsReq (joinTwoSortedListsCmd, _) ->
                match joinTwoSortedListsCmd with
                | JoinTwoSortedLists.Main.IsGreaterThanReq ((x, y), _) ->
                    Some (x, y)
                | _ ->
                    None
            | _ ->
                None
        | _ ->
            None

    let getResult = function
        | Start.Main.Result xs ->
            Some xs
        | _ ->
            None

let update (msg: Msg) (state: State) =
    match msg with
    | Msg.Choice choice ->
        let sortState =
            MergeSortInterp.choice choice state.SortState
        let state =
            { state with
                SortState =
                    sortState
            }
        let cmd =
            match MergeSortInterp.getResult sortState with
            | Some result ->
                Cmd.ofMsg (Msg.Result result)
            | None ->
                Cmd.none
        state, cmd
    | Msg.Result result ->
        state, Cmd.none

let view (state: State) (dispatch: Msg -> unit) =
    match MergeSortInterp.getCurrentCandidates state.SortState with
    | Some (x, y) ->
        Html.div [
            Html.h1 [
                prop.text "Choice the best!"
            ]
            Html.div [
                Html.button [
                    prop.onClick (fun _ -> dispatch (Msg.Choice Choice.Left))
                    prop.textf "%s" x.Name
                ]
                Html.button [
                    prop.onClick (fun _ -> dispatch (Msg.Choice Choice.Right))
                    prop.textf "%s" y.Name
                ]
            ]
        ]
    | None ->
        Html.pre [
            prop.textf "%A" state.SortState
        ]
