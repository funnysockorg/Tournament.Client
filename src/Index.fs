module Index
open Elmish
open Feliz

open Tournament.Client.Model
open Tournament.Client.Components

Fable.Core.JsInterop.import "" "./index.css"

[<RequireQualifiedAccess>]
type Page =
    | Home of Home.State
    | Tournament of Tournament.State
    | TournamentResult of TournamentResult.State

type State =
    {
        Page: Page
    }

module State =
    let empty =
        {
            Page = Page.Home Home.State.empty
        }

[<RequireQualifiedAccess>]
type Msg =
    | HomeHandle of Home.Msg
    | TournamentHandle of Tournament.Msg
    | StartTournament
    | StartTournamentResult of Participant list
    | TournamentResultHandle of TournamentResult.Msg

let init () =
    State.empty, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | Msg.HomeHandle homeMsg ->
        match state.Page with
        | Page.Home homeState ->
            match homeMsg with
            | Home.Msg.Start ->
                state, Cmd.ofMsg Msg.StartTournament
            | homeMsg ->
                let homeState, homeCmd =
                    Home.update homeMsg homeState
                let state =
                    { state with
                        Page =
                            Page.Home homeState
                    }
                state, homeCmd |> Cmd.map Msg.HomeHandle
        | _ ->
            state, Cmd.none
    | Msg.TournamentHandle tournamentMsg ->
        match state.Page with
        | Page.Tournament tournamentState ->
            match tournamentMsg with
            | Tournament.Msg.Result r ->
                state, Cmd.ofMsg (Msg.StartTournamentResult r)
            | tournamentMsg ->
                let tournamentState, tournamentCmd =
                    Tournament.update tournamentMsg tournamentState
                let state =
                    { state with
                        Page =
                            Page.Tournament tournamentState
                    }
                state, tournamentCmd |> Cmd.map Msg.TournamentHandle
        | _ ->
            state, Cmd.none
    | Msg.TournamentResultHandle tournamentResultMsg ->
        match state.Page with
        | Page.TournamentResult tournamentResultState ->
            match tournamentResultMsg with
            | TournamentResult.Msg.Submit ->
                state, Cmd.none
            // | tournamentResultMsg ->
            //     let tournamentResultState, tournamentResultCmd =
            //         TournamentResult.update tournamentResultMsg tournamentResultState
            //     let state =
            //         { state with
            //             Page =
            //                 Page.TournamentResult tournamentResultState
            //         }
            //     state, tournamentResultCmd |> Cmd.map Msg.TournamentResultHandle
        | _ ->
            state, Cmd.none
    | Msg.StartTournament ->
        let tournamentState, tournamentCmd =
            Tournament.init ParticipantsStorage.mocks
        let state =
            { state with
                Page = Page.Tournament tournamentState
            }
        let cmd =
            tournamentCmd |> Cmd.map Msg.TournamentHandle
        state, cmd
    | Msg.StartTournamentResult tournamentState ->
        let tournamentState, tournamentCmd =
            TournamentResult.init tournamentState
        let state =
            { state with
                Page = Page.TournamentResult tournamentState
            }
        let cmd =
            tournamentCmd |> Cmd.map Msg.TournamentResultHandle
        state, cmd

let view (state: State) (dispatch: Msg -> unit) =
    match state.Page with
    | Page.Home homeState ->
        Home.view homeState (Msg.HomeHandle >> dispatch)
    | Page.Tournament tournamentState ->
        Tournament.view tournamentState (Msg.TournamentHandle >> dispatch)
    | Page.TournamentResult tournamentResultState ->
        TournamentResult.view tournamentResultState (Msg.TournamentResultHandle >> dispatch)
