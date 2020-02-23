module App

open Elmish
open Elmish.React
open Feliz
open Fable.Import.Msal


(*

    - need login from azure AD
    - we want to display azure resource groups in a table
    - different actions on resource groups in table

    Client: 58e6d686-7af4-4ee5-b844-109d0b05593e
    Tenant-id: 0749c8ff-47af-4b3e-a245-5520097e8ca9
    LoginUri: https://login.microsoftonline.com/0749c8ff-47af-4b3e-a245-5520097e8ca9

     - loggedIn
     - loggedOut


    {
    "id": "/subscriptions/77d9206d-4aa9-4ebb-a46c-6859ab85a638/resourceGroups/my-demo-resource-group-02",
    "name": "my-demo-resource-group-02",
    "type": "Microsoft.Resources/resourceGroups",
    "location": "germanywestcentral",
    "tags": { "fancy-tag": "02" },
    "properties": { "provisioningState": "Succeeded" }
    		


*)

open Thoth.Json

type AzureResult<'a> = 
    {
        Value: 'a list
    }
    static member Decoder innerDecoder : Decoder<AzureResult<'a>> =
        Decode.object(fun get ->
            {
                Value = get.Required.Field "value" (Decode.list innerDecoder)
            }
        )


type ResourceGroup = 
    {
        Id:string
        Name:string
        Location:string
        Tags:Map<string,string>
    }
    static member Decoder : Decoder<ResourceGroup> =
        Decode.object(fun get ->
            {
                Id = get.Required.Field "id" Decode.string
                Name = (get.Optional.Field "name" Decode.string) |> Option.defaultValue ""
                Location = get.Required.Field "location" Decode.string
                Tags = 
                    (get.Optional.Field "tags" (Decode.keyValuePairs Decode.string)) // (string * string) list option
                    |> Option.defaultValue [] 
                    |> Map
            }
        )



type ResoureGroupResult = AzureResult<ResourceGroup>




type Page =
    | LoggedOut
    | LoggedIn


type Model = {
    Page:Page
    UserName:string
    ErrorMessage:string
    InfoMessage:string
    ResourceGroups:ResourceGroup list
    SelectedResourceGroup: ResourceGroup option
    DeleteConfirmationName:string
}


type Msg =
    | Login
    | LoginSuccessful of string
    | Logout

    | GetResourceGroups
    | GotResourceGroups of ResourceGroup list
    
    | OpenDeleteDialog of ResourceGroup
    | CloseDeleteDialog
    | ChangeDeleteConfirmationText of string
    | TriggerResourceGroupDelete of ResourceGroup
    | ResourceGroupDeletionTriggered of string

    | OnError of string

    | DoNothing



module Authentication =

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


module WebAccess =

    open Fable.SimpleHttp

    let getResourceGroups token =
        async {
            let! response =
                Http.request "https://management.azure.com/subscriptions/77d9206d-4aa9-4ebb-a46c-6859ab85a638/resourcegroups?$filter=tagName eq 'fancy-tag'&api-version=2019-10-01"
                |> Http.method GET
                |> Http.header (Headers.authorization ("Bearer " + token))
                |> Http.header (Headers.contentType "application/json")
                |> Http.send

            if response.statusCode = 200 then
                let result = 
                    response.responseText
                    |> Thoth.Json.Decode.fromString (AzureResult.Decoder ResourceGroup.Decoder)
                return result
            else
                let errorMessage = sprintf "error getting resource groups: %i - %s" response.statusCode response.responseText
                return Error errorMessage
        }

        
    let deleteResourceGroup token id =
        async {
            let! response =
                Http.request (sprintf "https://management.azure.com%s?api-version=2019-10-01" id)
                |> Http.method DELETE
                |> Http.header (Headers.authorization ("Bearer " + token))
                |> Http.header (Headers.contentType "application/json")
                |> Http.send

            if response.statusCode = 202 then
                return Ok ()
            else
                let errorMessage = sprintf "error delete resource groups: %i - %s" response.statusCode response.responseText
                return Error errorMessage
        }


module Commands =

    open Browser

    let handleLoginCmd () =
        let msg =
            Authentication.userAgent.handleRedirectCallback(fun error response ->
                console.log(error)
                console.log(response)
            )
            let account = Authentication.userAgent.getAccount()
            if (account <> null) then
                LoginSuccessful <| account.userName
            else
                DoNothing
        Cmd.ofMsg msg


    let loginCmd () =
        Cmd.ofSub (fun _ ->
            let authParams = Authentication.getAuthenticationParameters(None)
            Authentication.userAgent.loginRedirect(authParams)
        )


    let logoutCmd () =
        Cmd.ofSub (fun _ ->
            Authentication.userAgent.logout()
        )


    let getResourceGroupCmd () =
        async {
            try
                let account = Authentication.userAgent.getAccount()
                let authParams = Authentication.getAuthenticationParameters(Some account)
                let! authResponse = Authentication.aquireToken(authParams)
                let! result = WebAccess.getResourceGroups authResponse.accessToken
                match result with
                | Ok result ->
                    return (GotResourceGroups result.Value)
                | Error error ->
                    return (OnError error)
            with
            | _ as ex ->
                return (OnError ex.Message)
        }
        |> Cmd.OfAsync.result


    let deleteResourceGroupCmd resourceGroup =
        async {
            try
                let account = Authentication.userAgent.getAccount()
                let authParams = Authentication.getAuthenticationParameters(Some account)
                let! authResponse = Authentication.aquireToken(authParams)
                let! result = WebAccess.deleteResourceGroup authResponse.accessToken resourceGroup.Id
                match result with
                | Ok result ->
                    return (ResourceGroupDeletionTriggered <| sprintf "resource '%s' group deleted!" resourceGroup.Name)
                | Error error ->
                    return (OnError error)
            with
            | _ as ex ->
                return (OnError ex.Message)
        }
        |> Cmd.OfAsync.result
        


module State =


    let init () =
        {
            Page = LoggedOut
            ErrorMessage = ""
            InfoMessage = ""
            UserName = ""
            ResourceGroups = []
            SelectedResourceGroup = None
            DeleteConfirmationName=""
        }, Commands.handleLoginCmd ()


    let update msg model =
        match msg with
        | Login ->
            model, Commands.loginCmd ()
        | LoginSuccessful username ->
            { model with Page = LoggedIn; UserName = username}, Commands.getResourceGroupCmd ()
        | Logout ->
            { model with Page = LoggedOut}, Commands.logoutCmd ()

        | GetResourceGroups ->
            model, Commands.getResourceGroupCmd ()
        | GotResourceGroups groups ->
            { model with ResourceGroups = groups }, Cmd.none

        | OpenDeleteDialog resourceGroup ->
            { model with 
                SelectedResourceGroup = Some resourceGroup
                DeleteConfirmationName = ""
                ErrorMessage = ""
                InfoMessage = "" }, Cmd.none
        | CloseDeleteDialog ->
            { model with SelectedResourceGroup = None }, Cmd.none
        | ChangeDeleteConfirmationText text ->
            { model with DeleteConfirmationName = text}, Cmd.none
        | TriggerResourceGroupDelete resourceGroup ->
            model, Commands.deleteResourceGroupCmd resourceGroup
        | ResourceGroupDeletionTriggered message ->
            { model with InfoMessage = message }, Cmd.ofMsg CloseDeleteDialog


        | OnError error ->
            { model with ErrorMessage = error}, Cmd.ofMsg CloseDeleteDialog

        | DoNothing ->
            model, Cmd.none



module View =

    open Feliz.Bulma


    let renderLayout content =
        Html.div [
            Bulma.navbar [
                navbar.isDark
                prop.children [
                    Bulma.navbarBrand [
                        prop.children [
                            Bulma.navbarItemA [
                                prop.href "/"
                                prop.text "Start"
                            ]
                        ]
                    ]
                ]
            ]
        
            Bulma.container [
                prop.children [
                    yield! content
                ]
            ]
        ]


    let renderFullColumn content =
        Bulma.columns [
            Bulma.column [
                column.isFull
                prop.children [
                    yield! content
                ]
            ]
        ]


    let renderLoggedOut model dispatch =
        Html.div [
            renderFullColumn [
                Bulma.hero [
                    hero.isInfo
                    prop.children [
                        Bulma.heroBody [
                            Bulma.container [
                                Bulma.title  "Resource Group Manager"
                                Bulma.subtitle "Please Login!"
                            ]
                        ]
                    ]
                ]
            ]

            renderFullColumn [
                Bulma.button [
                    button.isSuccess
                    prop.text "Login"
                    prop.onClick (fun _ -> dispatch Login)
                ]
            ]
    

        
        ]


    let renderDeleteModal model dispatch =
        match model.SelectedResourceGroup with
        | None ->
            Html.none
        | Some resourceGroup ->
            Html.div [
                Bulma.modal [
                    modal.isActive
                    prop.children [
                        Bulma.modalBackground [ ]
                        Bulma.modalContent [
                            Bulma.message [
                                message.isDanger
                                prop.children [
                                    Bulma.messageHeader [
                                        Html.p (sprintf "Delete Resource Group '%s'!" resourceGroup.Name)
                                        Bulma.delete [
                                            prop.onClick (fun _ -> dispatch CloseDeleteDialog)
                                        ]
                                    ]

                                    Bulma.messageBody [
                                        renderFullColumn [
                                            Html.p "to delete the resource group, you have to enter the name."
                                        ]
                                        renderFullColumn [
                                            Bulma.textInput [
                                                prop.onTextChange (fun t -> dispatch <| ChangeDeleteConfirmationText t)
                                                
                                            ]
                                        ]
                                        renderFullColumn [
                                            Bulma.button [
                                                helpers.isPulledRight
                                                button.isDanger
                                                prop.text "Delete"
                                                prop.disabled (resourceGroup.Name <> model.DeleteConfirmationName)
                                                prop.onClick (fun _ -> dispatch <| TriggerResourceGroupDelete resourceGroup)
                                            ]

                                            Bulma.button [
                                                helpers.isPulledLeft
                                                prop.text "Cancel"
                                                prop.onClick (fun _ -> dispatch CloseDeleteDialog)
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
        
            ]

    let renderLoggedIn model dispatch =
        Html.div [

            renderFullColumn [
                Bulma.hero [
                    hero.isInfo
                    prop.children [
                        Bulma.heroBody [
                            Bulma.container [
                                Bulma.title  "Resource Group Manager"
                                Bulma.subtitle (sprintf "Welcome!, %s" model.UserName)
                            ]
                        ]
                    ]
                ]
            ]
    
               
            renderFullColumn [
                Bulma.button [
                    button.isSuccess
                    helpers.isPulledLeft
                    prop.text "Refresh"
                    prop.onClick (fun _ -> dispatch GetResourceGroups)
                ]


                Bulma.button [
                    button.isDanger
                    helpers.isPulledRight
                    prop.text "Logout"
                    prop.onClick (fun _ -> dispatch Logout)
                ]
            ]

            if (model.ErrorMessage <> "") then
                renderFullColumn [
                    Bulma.message [
                        message.isDanger
                        prop.children [
                            Bulma.messageHeader [
                                Html.h1 "Error"
                            ]
                            Bulma.messageBody [
                                Html.p model.ErrorMessage
                            ]
                        ]
                    ]
                ]

            if (model.InfoMessage <> "") then
                renderFullColumn [
                    Bulma.message [
                        message.isInfo
                        prop.children [
                            Bulma.messageHeader [
                                Html.h1 "Information"
                            ]
                            Bulma.messageBody [
                                Html.p model.InfoMessage
                            ]
                        ]
                    ]
                ]
        

            renderFullColumn [
                Bulma.table [
                    table.isFullwidth
                    prop.children [
                        Html.thead [
                            Html.tr [
                                Html.th "Name"
                                Html.th "Location"
                                Html.th "Actions"
                            ]
                        ]
                        Html.tbody [
                            for item in model.ResourceGroups do
                                Html.tr [
                                    Html.td item.Name
                                    Html.td item.Location
                                    Html.td [
                                        Bulma.button [
                                            button.isDanger
                                            prop.text "Delete"
                                            prop.onClick (fun _ -> dispatch <| OpenDeleteDialog item)
                                            prop.style [
                                                style.margin 3
                                            ]
                                        ]

                                        Bulma.buttonLink [
                                            button.isInfo
                                            prop.href (sprintf "https://portal.azure.com/#@hardt-solutions.com/resource/%s/overview" item.Id)
                                            prop.target "_blank"
                                            prop.text "Open Portal"
                                            prop.style [
                                                style.margin 3
                                            ]
                                        ]
                                    ]
                                ]
                        ]
                    ]
                
                ]
            ]


            renderDeleteModal model dispatch

        
            
        
        ]



    let render model dispatch =
        renderLayout [
            match model.Page with
            | LoggedOut ->
                renderLoggedOut model dispatch
            | LoggedIn ->
                renderLoggedIn model dispatch
        ]
    
    






Program.mkProgram State.init State.update View.render
|> Program.withReactSynchronous "elmish-app"
|> Program.run