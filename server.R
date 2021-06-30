################################################################################
# SERVER
################################################################################
shinyServer(function(input, output, session) {
    
    session$onSessionEnded(function() {
        stopApp()
    })
    
    
    ##-- Acceuil ----
    source("tabs/server/accueil.R", local = TRUE)
    
    ##-- DEFM ----
    source("tabs/server/catA.R", local = TRUE)
    source("tabs/server/catB.R", local = TRUE)
    source("tabs/server/catC.R", local = TRUE)
    source("tabs/server/catBC.R", local = TRUE)
    source("tabs/server/catABC.R", local = TRUE)
    source("tabs/server/Ancien_ABC.R", local = TRUE)
    source("tabs/server/entrees.R", local = TRUE)
    source("tabs/server/sorties.R", local = TRUE)
    source("tabs/server/catD.R", local = TRUE)
    source("tabs/server/catE.R", local = TRUE)
    
})
