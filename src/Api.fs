module Tournament.Client.Api
open Fable.Core

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

module AuthCallback =
    open Fetch.Types

    type DiscordAuthCode = string
    type ApiAuthToken = string
    type StatusCode = int

    type Response =
        Result<ApiAuthToken, {| StatusCode: StatusCode; Body: string |}>

    let request (code: DiscordAuthCode) : JS.Promise<Response> =
        promise {
            let! result =
                Fetch.fetchUnsafe (sprintf "%s/discord/auth/callback?code=%s" apiHost code) [
                    Method HttpMethod.GET
                ]
            match result.Status with
            | 200 ->
                let! body = result.text()
                return Ok body
            | statusCode ->
                let! body = result.text()
                return Error {|
                    StatusCode = statusCode
                    Body = body
                |}
        }

let getCurrentUser authToken : JS.Promise<User> =
    JS.Constructors.Promise.Create(fun result reject ->
        JS.setTimeout
            (fun () ->
                result User.mock
            )
            1500
        |> ignore
    )
