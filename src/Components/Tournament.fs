module Tournament.Client.Components.Tournament
open Elmish
open Feliz

open Tournament.Client.Model
open Tournament.Client.Components.Shared

type State =
    {
        Participants: Participant list
        SortState: MergeSort.Start.Main<Participant>
    }

module State =
    let create participants =
        {
            Participants = participants
            SortState = MergeSort.create participants
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
                Cmd.ofMsg (Msg.Result (List.rev result))
            | None ->
                Cmd.none
        state, cmd
    | Msg.Result result ->
        state, Cmd.none

let view (state: State) (dispatch: Msg -> unit) =
    match MergeSort.getCurrentCandidates state.SortState with
    | Some (x, y) ->
        let serverIconCandidate (participant: Participant) onClick =
            let serverIcon isDarkMode =
                serverIcon isDarkMode [
                    participantIcon participant
                ]
            Html.button [
                prop.classes [
                    "size-[200px]"
                    "bg-fuchsia-100"
                    "rounded-[30px]"
                    "hover:bg-fuchsia-200"

                    "flex"
                    "flex-col"
                    "justify-between"
                    "justify-center"
                    "items-center"
                ]
                prop.children [
                    Html.div []
                    serverIcon true
                    serverIcon false
                    Html.div []
                ]
                prop.onClick onClick
            ]

        content [
            Html.div [
                prop.classes [ "relative" ]
                prop.children [
                    Html.img [
                        prop.classes [
                            "left-[-60px]"
                            "bottom-[10px]"
                            "absolute"
                        ]
                        prop.src "./images/first-arrow.svg"
                    ]
                    serverIconCandidate x (fun _ ->
                        dispatch (Msg.Choice MergeSort.Choice.Left)
                    )
                ]
            ]
            h1 [
                Html.text "Выберите наилучшую!"
            ]
            Html.div [
                prop.classes [ "relative" ]
                prop.children [
                    Html.img [
                        prop.classes [
                            "right-[-50px]"
                            "top-[-10px]"
                            "absolute"
                        ]
                        prop.src "./images/second-arrow.svg"
                    ]
                    serverIconCandidate y (fun _ ->
                        dispatch (Msg.Choice MergeSort.Choice.Right)
                    )
                ]
            ]
        ]
    | None ->
        Html.pre [
            prop.textf "%A" state.SortState
        ]
