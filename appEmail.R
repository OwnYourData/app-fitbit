# application specific email handling
# last update: 2017-02-08

setAllergyEmailStatus <- function(msg){
        output$allergyEmailStatus <- renderUI(msg)
}

observe({
        app <- currApp()
        schedulerEmail <- getPiaSchedulerEmail(app)
        if(length(schedulerEmail) == 0) {
                setAllergyEmailStatus('Status: derzeit sind tägliche Emails nicht eingerichtet')
                updateTextInput(session, 'allergyEmail', value='')
        } else {
                updateTextInput(session, 'allergyEmail', 
                                value=schedulerEmail[['email']])
                setAllergyEmailStatus('Status: tägliche Emails werden an die angegebene Adresse versandt')
        }
})

observeEvent(input$saveAllergyEmail, {
        setAllergyEmailStatus('bearbeite Eingabe...')
        email <- input$allergyEmail
        if(validEmail(email)){
                app <- currApp()
                schedulerEmail <- getPiaSchedulerEmail(app)
                condition_fields <- list(
                        date='Date.now',
                        value='line_1(Integer)'
                )
                condition_structure <- list(
                        repo=appRepos$Befinden,
                        repoName='Befinden',
                        fields=condition_fields
                )
                medintake_fields <- list(
                        date='Date.now',
                        value='line_2[FALSE](Boolean)'
                )
                medintake_structure <- list(
                        repo=appRepos$Medikamenteneinnahme,
                        repoName='Medikamenteneinnahme',
                        fields=medintake_fields
                )
                diary_fields <- list(
                        date='Date.now',
                        value='line_3(String)'
                )
                diary_structure <- list(
                        repo=appRepos$Tagebuch,
                        repoName='Tagebuch',
                        fields=diary_fields
                )
                response_structure <- list(
                        condition_structure,
                        medintake_structure,
                        diary_structure
                )
                if(length(schedulerEmail) == 0) {
                        writeSchedulerEmail(
                                app,
                                appTitle,
                                email,
                                'Dein Befinden für das Allergie-Tagebuch',
                                diaryEmailText,
                                '0 8 * * *',
                                response_structure)
                        setAllergyEmailStatus('der Versand täglicher Emails wurde erfolgreich eingerichtet')
                } else {
                        writeSchedulerEmail(
                                app,
                                appTitle,
                                email,
                                'Dein Befinden für das Allergie-Tagebuch',
                                diaryEmailText,
                                '0 8 * * *',
                                response_structure,
                                id=schedulerEmail[['id']])
                        setAllergyEmailStatus('die Emailadresse wurde erfolgreich aktualisiert')
                }
        } else {
                setAllergyEmailStatus('Fehler: ungültige Emailadresse, die Eingabe wurde nicht gespeichert')
        }
})

observeEvent(input$cancelAllergyEmail, {
        setAllergyEmailStatus('bearbeite Eingabe...')
        app <- currApp()
        schedulerEmail <- getPiaSchedulerEmail(app)
        if(length(schedulerEmail) == 0) {
                setAllergyEmailStatus('derzeit sind tägliche Emails nicht eingerichtet')
                updateTextInput(session, 'allergyEmail', value='')
        } else {
                repo_url <- itemsUrl(app[['url']], schedulerKey)
                deleteItem(app, repo_url, schedulerEmail[['id']])
                updateTextInput(session, 'allergyEmail', value='')
                setAllergyEmailStatus('der Versand täglicher Emails wurde beendet')
        }
})
