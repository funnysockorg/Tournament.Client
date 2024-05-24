module Index
open Elmish
open Feliz
open Feliz.Router

open Tournament.Client.Model
open Tournament.Client.Components

open Tournament.Client.Components.Shared

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
    | ChangeUrl of string list

let parseUrl segments (state: State) =
    match segments with
    | "auth"::"callback"::query ->
        match query with
        | Route.Query(queryParams)::_ ->
            let homeMsg =
                Home.AuthCallbackParams.ofQueryParameters queryParams
                |> Home.Msg.StartAuthCodeExchange
            let homeState =
                match state.Page with
                | Page.Home home ->
                    home
                | _ ->
                    Home.State.empty
            let state =
                { state with
                    Page =
                        Page.Home homeState
                }
            state, Cmd.ofMsg (Msg.HomeHandle homeMsg)
        | _ ->
            state, Cmd.none
    | _ ->
        state, Cmd.none

let init () =
    let segments = Router.currentUrl()
    parseUrl segments State.empty

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
    | Msg.ChangeUrl segments ->
        parseUrl segments state

let container (children: ReactElement list) =
    Html.div [
        prop.classes [
            "size-full"
            "flex"
            "flex-col"
        ]
        prop.children [
            Html.div [
                prop.className "bg-fuchsia-200"
                prop.children (
                    Html.div [
                        prop.className "h-10 m-1"
                        prop.children [
                            navbar [ navbarToggle ] [ navbarAvatar ]
                        ]
                    ]
                )
            ]
            Html.div [
                prop.classes [
                    "grow"
                    "overflow-auto"
                ]
                prop.children children
            ]
        ]
    ]

let mainContainer (children: ReactElement list) =
    Html.div [
        prop.classes [
            "h-screen"
            "w-screen"
            "flex"
            "justify-center"
        ]
        prop.children (
            Html.div [
                prop.classes [ "max-w-[360px]"; "size-full" ]
                prop.children (
                    container children
                )
            ]
        )
    ]

let view (state: State) (dispatch: Msg -> unit) =
    React.router [
        router.onUrlChanged (Msg.ChangeUrl >> dispatch)
        router.children (
            mainContainer [
                match state.Page with
                | Page.Home homeState ->
                    Home.view homeState (Msg.HomeHandle >> dispatch)
                | Page.Tournament tournamentState ->
                    Tournament.view tournamentState (Msg.TournamentHandle >> dispatch)
                | Page.TournamentResult tournamentResultState ->
                    TournamentResult.view tournamentResultState (Msg.TournamentResultHandle >> dispatch)
            ]
        )
    ]
