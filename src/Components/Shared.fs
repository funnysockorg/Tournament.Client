module Tournament.Client.Components.Shared
open Elmish
open Feliz

open Tournament.Client.Model

let document = Browser.Dom.document

[<ReactComponent>]
let Resizer () =
    let this = React.useRef null

    Html.div [
        prop.id "resizer"
        prop.classes [
            "size-2"
            "cursor-nw-resize"
            "bg-black"

            "absolute"
            "right-0"
            "bottom-0"
        ]
        prop.ref (fun element ->
            this.current <- element
        )
        prop.onMouseDown (fun e ->
            let resize (e: Browser.Types.Event) =
                let e = e :?> Browser.Types.MouseEvent
                let element = this.current.parentElement
                element.setAttribute (
                    "style",
                    sprintf
                        "width: %dpx; height: %dpx;"
                        (int (e.clientX - element.offsetLeft))
                        (int (e.clientY - element.offsetTop))

                )
            let rec stopResize e =
                document.removeEventListener("mousemove", resize, false);
                document.removeEventListener("mouseup", stopResize, false);
            document.addEventListener("mousemove", resize, false)
            document.addEventListener("mouseup", stopResize, false)
        )
    ]

[<ReactComponent>]
let ResizeBox (children: ReactElement list) =
    Html.div [
        prop.id "resize-box"
        prop.className "relative"
        prop.children [
            yield! children
            yield Resizer()
        ]
    ]

let navbarToggle =
    Html.div [
        prop.id "navbarToggle"
        prop.classes [
            "w-8"
            "h-7"
            "cursor-pointer"

            "flex"
            "flex-col"
            "justify-between"
        ]

        prop.children [
            for _ = 1 to 3 do
                Html.div [ prop.className "bg-black h-[5px]" ]
        ]
    ]

let navbarAvatar =
    let defaultAvatarIcon =
        Html.div [
            prop.id "defaultAvatarIcon"
            prop.children [
                Html.div [
                    prop.classes [
                        "size-3.5"
                        "rounded-full"
                        "bg-white"

                        "absolute"
                        "left-1/2"
                        "top-1/2"
                        "translate-x-[-50%]"
                        "translate-y-[calc(-50%-2px)]"
                    ]
                ]
                Html.div [
                    prop.classes [
                        sprintf "size-6"
                        "rounded-full"
                        "bg-white"

                        "absolute"
                        "left-1/2"
                        "top-1/2"
                        "translate-x-[-50%]"
                        "translate-y-[calc(-50%+15px)]"
                    ]
                ]

            ]
        ]
    Html.div [
        prop.id "navbarAvatar"
        prop.className "size-10 rounded-full bg-rose-500 overflow-hidden relative"
        prop.children [
            defaultAvatarIcon
        ]
    ]

let navbar (leftItems: ReactElement seq) (rightItems: ReactElement seq) =
    Html.div [
        prop.id "navbar"
        prop.classes [
            "size-full"

            "flex"
            "items-center"
        ]
        prop.children [
            Html.div [
                prop.id "navbarLeftItems"
                prop.classes [
                    "flex"
                    "gap-2"

                    "grow"
                ]
                prop.children leftItems
            ]
            Html.div [
                prop.id "navbarRightItems"
                prop.classes [
                    "flex"
                    "gap-2"
                ]
                prop.children rightItems
            ]
        ]
    ]

let button (text: string) onClick =
    Html.button [
        prop.classes [
            "bg-pink-300"
            "rounded-full"
            "size-full"

            "text-2xl"
            "flex"
            "justify-center"
            "items-center"
        ]
        prop.text text
        prop.onClick onClick
    ]

let h1 (children: ReactElement list) =
    Html.h1 [
        prop.classes [
            "text-3xl"
            "text-black"
            "text-opacity-70"
            "text-center"
        ]
        prop.children children
    ]

let p (children: ReactElement list) =
    Html.div [
        prop.classes [
            "text-xl"
            "text-black"
            "text-opacity-70"
            "text-center"
        ]
        prop.children children
    ]

let content (children: ReactElement list) =
    Html.div [
        prop.classes [
            "size-full"

            "flex"
            "flex-col"
            "justify-between"
            "items-center"
            "gap-4"
        ]
        prop.children [
            Html.div []
            yield! children
            Html.div []
        ]
    ]

let serverIcon isDarkMode (children: ReactElement list) =
    Html.div [
        prop.classes [
            "size-[72px]"
            if isDarkMode then
                "bg-neutral-800"
            else
                "bg-zinc-200"

            "flex"
            "gap-2"
            "items-center"
        ]
        prop.children [
            Html.div [
                prop.classes [
                    "w-1"
                    "h-10"
                    if isDarkMode then
                        "bg-white"
                    else
                        "bg-zinc-950"
                    "rounded-r-2xl"
                ]
            ]
            Html.div [
                prop.className "size-12 bg-white rounded-2xl overflow-hidden"
                prop.children (
                    children
                )
            ]
        ]
    ]

let participantIcon (participant: Participant) =
    let nameView (name: string) =
        Html.div [
            prop.classes [
                "size-full"

                "flex"
                "justify-center"
                "items-center"
            ]
            prop.children (
                Html.text name
            )
        ]

    let avatarView (avatar: ParticipantAvatar) =
        Html.img [
            prop.classes [
                "max-w-none"
            ]
            prop.style [
                style.transformOrigin (origin.left, origin.top)
                style.transform [
                    transform.translate(avatar.X, avatar.Y)
                    transform.scale(avatar.Scale)
                ]
            ]
            prop.src avatar.Src
        ]

    match participant.Avatar with
    | Some avatar ->
        avatarView avatar
    | None ->
        nameView participant.Name

let serverIconItems (participants: Participant list) =
    let serverIconItem i (participant: Participant) =
        let serverIcon isDarkMode =
            serverIcon isDarkMode [
                participantIcon participant
            ]
        Html.li [
            prop.classes [
                "hover:bg-fuchsia-100"

                "flex"
                "justify-between"
                "justify-center"
                "items-center"
            ]
            prop.children [
                Html.div []
                Html.div [
                    prop.classes [
                        match i with
                        | 0 ->
                            "bg-yellow-200"
                        | 1 ->
                            "bg-gray-200"
                        | 2 ->
                            "bg-orange-200"
                        | _ ->
                            ""
                        "size-8"
                        "rounded-full"

                        "flex"
                        "justify-center"
                        "items-center"
                    ]
                    prop.textf "%d" (i + 1)
                ]
                serverIcon true
                serverIcon false
                Html.div []
            ]
        ]
    Html.ol [
        prop.classes [
            "overflow-auto"
            "min-h-0"
            "w-full"
        ]
        prop.children (
            participants
            |> List.mapi (fun i partipant ->
                serverIconItem i partipant
            )
        )
    ]

let spinner =
    Html.div [
        prop.text "Загрузка..."
    ]
