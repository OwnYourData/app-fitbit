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

}

observeEvent(input$fitbit_register, {
        key       <- input$fitbit_key
        secret    <- input$fitbit_secret
        request   <- 'https://api.fitbit.com/oauth2/token'
        authorize <- 'https://www.fitbit.com/oauth2/authorize'
        access    <- 'https://api.fitbit.com/oauth2/token'
        endpoint  <- httr::oauth_endpoint(request, authorize, access)
        myapp     <- httr::oauth_app("oyd_fitbit", key, secret)
        scope     <- c("sleep", "activity", "weight")
        token     <- httr::oauth2.0_token(endpoint, myapp, scope=scope, 
                                          use_basic_auth=TRUE, cache=FALSE)
        token_serial <- rawToChar(serialize(token, NULL, ascii = TRUE))
        app <- currApp()
        if(length(app) > 0){
                url <- itemsUrl(app[['url']], 
                                paste0(app[['app_key']],
                                       '.fitbit_token'))
                recs <- readItems(app, url)
                if(nrow(recs) > 1){
                        lapply(recs$id, 
                               function(x) deleteItem(app, url, x))
                        recs <- data.frame()
                        
                }
                data <- list(
                        value = x,
                        '_oydRepoName' = 'Fitbit Token')
                if(nrow(recs) == 1){
                        updateItem(app, url, data, recs$id)
                } else {
                        writeItem(app, url, data)
                }
        }
})