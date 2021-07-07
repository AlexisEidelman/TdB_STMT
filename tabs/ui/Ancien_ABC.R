Ancien_ABC <- tabPanel(title = "Ancienneté des inscrits", 
                       value = "catA",
                       dashboardBody(
                         h1(paste0("Part des inscrits en Catégories A, B et C selon l'ancienneté")),
                         hr(),
                         fluidRow(
                           box(title = NULL
                               ,width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE
                               ,plotlyOutput('Anc_defmABC', height = "100%", width = "100%")
                               ,h5(textOutput('Données corrigées des varaitions saisonnières et des jours ouvrables (CVS-CJO)'))
                               ,h5(textOutput('France métropolitaine'))
                               ,h5('Source: Pôle Emploi - Dares, STMT')
                           )
                         ),
                         
                         br(), br(),
                         hr()
                         
                       )
)

