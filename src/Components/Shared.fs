module Tournament.Client.Components.Shared
open Elmish
open Feliz

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
    Html.div [
        prop.id "button"
        prop.classes [
            "bg-pink-300"
            "rounded-full"
            "size-full"
            "cursor-pointer"
            "select-none"

            "text-2xl"
            "flex"
            "justify-center"
            "items-center"
        ]
        prop.text text
        prop.onClick onClick
    ]
