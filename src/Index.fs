module Index

open Elmish
open Feliz

type Msg =
    | Incr
    | Decr

type State =
    {
        Counter: int
    }

let init arg =
    let state =
        {
            Counter = 0
        }
    state, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | Incr ->
        let state =
            { state with
                Counter = state.Counter + 1
            }
        state, Cmd.none
    | Decr ->
        let state =
            { state with
                Counter = state.Counter - 1
            }
        state, Cmd.none

let view (state: State) (dispatch: Msg -> unit) =
    Html.div [
        Html.div [
            prop.text (sprintf "Counter is %d" state.Counter)
        ]
        Html.div [
            Html.div [
                Html.button [
                    prop.onClick (fun _ -> dispatch Decr)
                    prop.children [
                        Html.i [
                            prop.text "-"
                        ]
                    ]
                ]
                Html.button [
                    prop.onClick (fun _ -> dispatch Incr)
                    prop.children [
                        Html.i [
                            prop.text "+"
                        ]
                    ]
                ]
            ]
        ]
    ]
