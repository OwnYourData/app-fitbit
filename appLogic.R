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
        if(length(app)>0){
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
}

currDataSelection <- reactive({
        closeAlert(session, 'myDataStatus')
        data <- repoData(paste0(appKey, '.steps'))
        if(nrow(data) == 0) {
                createAlert(session, 'dataStatus', alertId = 'myDataStatus',
                            style = 'warning', append = FALSE,
                            title = 'Keine Daten im gewählten Zeitfenster',
                            content = 'Für das ausgewählte Zeitfenster sind keine Daten vorhanden.')
                data <- data.frame()
        } else {
                dataMin <- min(data$dat, na.rm=TRUE)
                dataMax <- max(data$dat, na.rm=TRUE)
                curMin <- as.Date(input$dateRange[1], '%d.%m.%Y')
                curMax <- as.Date(input$dateRange[2], '%d.%m.%Y')
                daterange <- seq(curMin, curMax, 'days')
                data <- data[as.Date(data$date) %in% daterange, ]
                if(nrow(data) == 0){
                        createAlert(session, 'dataStatus', alertId = 'myDataStatus',
                                    style = 'warning', append = FALSE,
                                    title = 'Keine Daten im gewählten Zeitfenster',
                                    content = 'Für das ausgewählte Zeitfenster sind keine Daten vorhanden.')
                }
        }
        data
})

output$barChart <- renderPlotly({
        data <- currDataSelection()
        pdf(NULL)
        outputPlot <- plotly_empty()
        data$val <- as.numeric(data$value)
        if(nrow(data) > 0){
                outputPlot <- plot_ly(
                        data,
                        x = ~as.Date(data$date),
                        y = ~data$val,
                        type = 'bar'
                ) %>% layout( title = '',
                        showlegend = FALSE,
                        margin = list(l = 80, r = 80),
                        xaxis = list(title = 'Datum'),
                        yaxis = list(title = 'Schritte')
                )
        }
        dev.off()
        outputPlot
})

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
                                        ui = tagList(actionButton('disonnectFitbit', 'Verbindung zu Fitbit trennen', 
                                                          icon('chain-broken')),
                                                     actionButton('importFitBit', 'Daten von Fitbit jetzt importieren',
                                                                  icon('cloud-download')))
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

observeEvent(input$importFitBit, {
        app<-currApp()
        fa_url<-itemsUrl(app[['url']],'eu.ownyourdata.fitbit.token')
        fa<-readItems(app,fa_url)
        if(nrow(fa)==1){
                key<-fa$key
                secret<-fa$secret
                headers<-c('Accept'='*/*','Content-Type'='application/x-www-form-urlencoded','Authorization'=paste('Basic',jsonlite::base64_enc(paste0(key,':',secret))))
                r<-httr::POST('https://api.fitbit.com/oauth2/token',body=list(grant_type='refresh_token',refresh_token=fa$refresh_token),httr::add_headers(.headers=headers),encode='form')
                data<-list(key=key,secret=secret,access_token=httr::content(r)$access_token,refresh_token=httr::content(r)$refresh_token)
                updateItem(app,fa_url,data,fa$id)
                access_token<-httr::content(r)$access_token
                url<-itemsUrl(app[['url']],'eu.ownyourdata.fitbit.steps')
                pia_data<-readItems(app,url)
                pia_data<-as.data.frame(lapply(pia_data,unlist))
                resp<-httr::GET('https://api.fitbit.com/1/user/-/activities/steps/date/today/1m.json',httr::add_headers(.headers=defaultHeaders(access_token)))
                fit_data<-jsonlite::fromJSON(httr::content(resp,as='text'))[[1]]
                if(nrow(fit_data)>0){
                        colnames(fit_data)<-c('date','value')
                        if(nrow(pia_data)>0){
                                df<-merge(pia_data,fit_data,by='date',all=TRUE)
                        } else {
                                df<-fit_data
                                colnames(df)<-c('date','value.y')
                        }
                        df<-df[df$value.y>0,]
                        if(nrow(df)>0){
                                apply(df,1,function(x){
                                        data<-list(date=as.character(x['date']),value=as.integer(x['value.y']),'_oydRepoName'='Schritte')
                                        if(is.na(x['id'])){
                                                writeItem(app,url,data)
                                        } else {
                                                updateItem(app,url,data,x['id'])
                                        }
                                })
                                createAlert(session, 'urlStatus', alertId = 'myFitbitStatus',
                                            style = 'success', append = TRUE,
                                            title = 'Fitbit Import',
                                            content = 'Daten wurden erfolgreich von Fitbit importiert.')
                        } else {
                                createAlert(session, 'urlStatus', alertId = 'myFitbitStatus',
                                            style = 'info', append = TRUE,
                                            title = 'Fitbit Import',
                                            content = 'Es stehen keine neuen Daten zum Import bereit.')
                        }
                }
        }
})