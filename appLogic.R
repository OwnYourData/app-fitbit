# application specific logic
# last update: 2017-02-13

source('srvDateselect.R', local=TRUE)
source('srvEmail.R', local=TRUE)
source('srvScheduler.R', local=TRUE)

# any record manipulations before storing a record
appData <- function(record){
        record
}

getRepoStruct <- function(repo){
        appStruct[[repo]]
}

repoData <- function(repo){
        data <- data.frame()
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                repo)
                data <- readItems(app, url)
        }
        data
}

# anything that should run only once during startup
appStart <- function(){
        app <- currApp()
        key <- ''
        secret <- ''
        if(length(app) > 0){
                url <- itemsUrl(app[['url']],
                                paste0(app[['app_key']], 
                                       '.token'))
                fa <- readItems(app, url)
                if(nrow(fa) > 1){
                        lapply(fa$id, 
                               function(x) deleteItem(app, url, x))
                        fa <- data.frame()
                }
                if(nrow(fa) == 1){
                        key <- fa$key
                        secret <- fa$secret
                }
        }
        if(nchar(key) > 0){
                updateTextInput(session, 'fitbit_key',
                                value = key)
        } else {
                updateTextInput(session, 'fitbit_key',
                                value = '')
        }
        if(nchar(secret) > 0){
                updateTextInput(session, 'fitbit_secret',
                                value = secret)
        } else {
                updateTextInput(session, 'fitbit_secret',
                                value = '')
        }
        
        # write script to collect Fitbit-Data used by scheduler ------
        app <- currApp()
        scriptRepoUrl <- itemsUrl(app[['url']], scriptRepo)
        scriptItems <- readItems(app, scriptRepoUrl)
        schedulerFitbitScript <- scriptItems[
                scriptItems$name == 'Fitbit Schritte', ]
        if(nrow(schedulerFitbitScript) > 1){
                lapply(schedulerFitbitScript$id,
                       function(x) deleteItem(app, 
                                              scriptRepoUrl,
                                              as.character(x)))
                schedulerFitbitScript <- data.frame()
        }
        scriptData <- list(name           = 'Fitbit Schritte',
                           script         = fitbitStepScript,
                           '_oydRepoName' = 'Fitbit-Skript')
        if(nrow(schedulerFitbitScript) == 0){
                writeItem(app, scriptRepoUrl, scriptData)
        } else {
                updateItem(app, scriptRepoUrl, scriptData,
                           schedulerFitbitScript$id)
        }
}

output$link_fitbit <- renderText({
        fitbit_key <- input$fitbit_key
        fitbit_secret <- input$fitbit_secret
        code <- ''
        access_token <- ''
        refresh_token <- ''
        app <- currApp()
        if((length(app) > 0) & 
           (nchar(fitbit_key) > 0) &
           (nchar(fitbit_secret) > 0)){
                url <- itemsUrl(app[['url']],
                                paste0(app[['app_key']], 
                                       '.token'))
                fa <- readItems(app, url)
                if(nrow(fa) > 1){
                        lapply(fa$id, 
                               function(x) deleteItem(app, url, x))
                        fa <- data.frame()
                }
                if(nrow(fa) == 1){
                        if('access_token' %in% colnames(fa)){
                                access_token <- fa$access_token
                        }
                        if('refresh_token' %in% colnames(fa)){
                                refresh_token <- fa$refresh_token
                        }
                        if((fitbit_key == fa$key) &
                           (fitbit_secret == fa$secret) &
                           (nchar(access_token) > 0) &
                           (nchar(refresh_token) > 0))
                        {
                                insertUI(
                                        selector = '#disonnectFitbitPlaceholder',
                                        ui = actionButton('disonnectFitbit', 'Verbindung zu Fitbit trennen', 
                                                          icon('chain-broken'))
                                )
                                
                                'erfolgreich mit Fitbit verbunden'
                        }
                } else {
                        data <- list(
                                key = fitbit_key,
                                secret = fitbit_secret,
                                access_token = access_token,
                                refresh_token = refresh_token,
                                '_oydRepoName' = 'Fitbit Authorization'
                        )
                        if(nrow(fa) == 0){
                                writeItem(app, url, data)
                        } else {
                                updateItem(app, url, data, fa$id)
                        }
                        
                        # https://dev.fitbit.com/apps/oauthinteractivetutorial
                        fl <- paste0('https://www.fitbit.com/oauth2/authorize?',
                                     'response_type=code&',
                                     'client_id=', as.character(input$fitbit_key), '&',
                                     '&redirect_uri=https%3A%2F%2Ffitbit.datentresor.org&',
                                     'scope=activity&',
                                     'expires_in=86400')
                        paste0("<a href='", fl, 
                               "', class='btn btn-default'>mit Fitbit verbinden</a>")
                }
        } else {
                'derzeit keine Verbindung zu Fitbit'
        }
})

observeEvent(input$disonnectFitbit, {
        app <- currApp()
        if(length(app) > 0){
                # remove token information
                url <- itemsUrl(app[['url']],
                                paste0(app[['app_key']], 
                                       '.token'))
                deleteRepo(app, url)
                
                # remove scheduler entry
                scheduler <- readSchedulerItemsFunction()
                url <- itemsUrl(app[['url']], schedulerKey)
                deleteItem(app, url, scheduler$id)
                
                # update UI
                updateTextInput(session, 'fitbit_key',
                                value = '')
                updateTextInput(session, 'fitbit_secret',
                                value = '')
                removeUI(selector = 'div:has(> #disonnectFitbit)')
                output$link_fitbit <- renderText('derzeit keine Verbindung zu Fitbit')
        }
})