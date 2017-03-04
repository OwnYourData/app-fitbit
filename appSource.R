# layout for section "Source"
# last update: 2016-10-06

# source("uiSourceItemConfig.R")

appSource <- function(){
       tabsetPanel(
               type='tabs',
               tabPanel('Verbindung zu Fitbit', br(),
                        fluidRow(
                                column(2,
                                       img(src='fitbit.png',
                                           width='70px',
                                           style='margin:20px')),
                                column(10,
                                       textInput('fitbit_key', 'ID'),
                                        textInput('fitbit_secret', 'Secret'),
                                        htmlOutput('link_fitbit'), br(),
                                        tags$div(id = 'disonnectFitbitPlaceholder')
                               )
                       )
                )
        )
}

# constants for configurable Tabs
# defaultSrcTabsName <- c('SrcTab1', 'SrcTab2')
# 
# defaultSrcTabsUI <- c(
#         "
#         tabPanel('SrcTab1',
#                 textInput(inputId=ns('defaultSourceInput1'), 
#                         'Eingabe1:'),
#                 htmlOutput(outputId = ns('defaultSourceItem1'))
#         )
#         ",
#         "
#         tabPanel('SrcTab2',
#                 textInput(inputId=ns('defaultSourceInput2'), 
#                         'Eingabe2:'),
#                 htmlOutput(outputId = ns('defaultSourceItem2'))
#         )
#         "
# )
# 
# defaultSrcTabsLogic <- c(
#         "
#         output$defaultSourceItem1 <- renderUI({
#                 input$defaultSourceInput1
#         })
#         ",
#         "
#         output$defaultSourceItem2 <- renderUI({
#                 input$defaultSourceInput2
#         })
#         "
# )
