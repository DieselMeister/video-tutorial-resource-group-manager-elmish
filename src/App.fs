module App

open Elmish
open Elmish.React
open Feliz
open Fable.Import.Msal

open Domain






type Page =
    | LoggedOut
    | LoggedIn


type Model = {
    Page:Page
    UserName:string
    ErrorMessage:string
    InfoMessage:string
    ResourceGroups:ResourceGroup list
    DeleteDialogModel: DeleteDialog.Model option
}


type Msg =
    | Login
    | LoginSuccessful of string
    | Logout

    | GetResourceGroups
    | GotResourceGroups of ResourceGroup list
    
    | OpenDeleteDialog of ResourceGroup
    | CloseDeleteDialog
    | DeleteDialogMsg of DeleteDialog.Msg
    
    | OnError of string

    | DoNothing


    


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



module State =


    let init () =
        {
            Page = LoggedOut
            ErrorMessage = ""
            InfoMessage = ""
            UserName = ""
            ResourceGroups = []
            DeleteDialogModel = None
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


        | DeleteDialogMsg (DeleteDialog.CloseDeleteDialog) ->
            model,Cmd.ofMsg CloseDeleteDialog

        | DeleteDialogMsg (DeleteDialog.OnError error) ->
            model,Cmd.ofMsg <| OnError error

        | DeleteDialogMsg (DeleteDialog.ResourceGroupDeletionTriggered message) ->
            { model with InfoMessage = message}, Cmd.ofMsg CloseDeleteDialog

        | DeleteDialogMsg deleteMsg ->
            match model.DeleteDialogModel with
            | None ->
                model, Cmd.none
            | Some deleteModel ->
                let (newDeleteModel,deleteCmd) = DeleteDialog.update deleteMsg deleteModel
                let cmd = Cmd.map DeleteDialogMsg deleteCmd
                { model with DeleteDialogModel = Some newDeleteModel }, cmd

        | OpenDeleteDialog resourceGroup ->
            let (newDeleteModel,deleteCmd) = DeleteDialog.init resourceGroup
            let cmd = Cmd.map DeleteDialogMsg deleteCmd

            { model with 
                DeleteDialogModel = Some newDeleteModel
                ErrorMessage = ""
                InfoMessage = "" }, cmd

        | CloseDeleteDialog ->
            { model with DeleteDialogModel = None }, Cmd.none

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

            match model.DeleteDialogModel with
            | None ->
                Html.none
            | Some deleteModel ->
                DeleteDialog.render deleteModel (DeleteDialogMsg >> dispatch)
        
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