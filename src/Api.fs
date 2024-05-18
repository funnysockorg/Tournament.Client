module Tournament.Client.Api
open Fable.Core
open Fable.Core.JS

open Tournament.Client.Model

let clientId = "1237078496866734120"
let apiHost = "http://localhost:4010/api"
let redirectUri = sprintf "%s/discord/callback" apiHost

let discordSignInUrl =
    let query =
        [
            "client_id", clientId
            "response_type", "code"
            "redirect_uri", redirectUri
            "scope", "identify"
        ]
        |> List.map (fun (name, value) ->
            sprintf "%s=%s" name (JS.encodeURIComponent value)
        )
        |> String.concat "&"
    sprintf "https://discord.com/oauth2/authorize?%s" query

let getCurrentUser () : Promise<User> =
    Constructors.Promise.Create(fun result reject ->
        setTimeout
            (fun () ->
                result User.mock
            )
            1500
        |> ignore
    )
