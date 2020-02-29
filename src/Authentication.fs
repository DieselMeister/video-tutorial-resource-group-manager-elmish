module Authentication

open Fable.Import
open Fable.Core.JsInterop
open Browser.Dom
  

let private msalConfig : Msal.Configuration.Configuration =
    let authSettings: Msal.Configuration.AuthOptions =
        !!{|
            clientId = "58e6d686-7af4-4ee5-b844-109d0b05593e"
            authority = Some "https://login.microsoftonline.com/0749c8ff-47af-4b3e-a245-5520097e8ca9"
            redirectUri = (Some window.location.origin)
            postLogoutRedirectUri = (Some window.location.origin)
        |}

    let cacheSettings: Msal.Configuration.CacheOptions =
        !!{|
            cacheLocation = Some Msal.Configuration.CacheLocation.LocalStorage
            forceRefresh = false
        |}

    !!{|
        auth = authSettings
        cache = Some cacheSettings
    |}


let userAgent =
    Msal.UserAgentApplication.userAgentApplication.Create(msalConfig)


let getAuthenticationParameters (account:Msal.Account.Account option) 
    : Msal.AuthenticationParameters.AuthenticationParameters =
        !!{|
            redirectUri = (Some window.location.origin)
            account = account
            scopes = [| "https://management.azure.com/user_impersonation" |]
        |}


let aquireToken userRequest =
    Async.FromContinuations <| fun (resolve,reject,_) ->
        userAgent.acquireTokenSilent(userRequest)
            .``then``(fun response -> resolve response)
            .catch(fun error ->
                let errorMessage:string = error?errorMessage
                if (errorMessage.Contains("interaction_required")) then
                    userAgent.acquireTokenPopup(userRequest)
                        .``then``(fun response -> resolve response)
                        .catch (fun error ->
                            reject (exn error?errorMessage) 
                        ) |> ignore
                else
                    reject (exn errorMessage)
            ) |> ignore         