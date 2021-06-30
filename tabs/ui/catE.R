catE <- tabPanel(title = "Catégorie E", 
                 value = "catE",
                 dashboardBody(
                   h1(paste0("Catégorie E")),
                   h2(textOutput('title_E')),
                   hr(),
                   fluidRow(
                     column(3,
                            radioButtons("mcvs_E", label = "Correction des variations saisonnières",
                                         choices = list("OUI" = 1, "NON" = 2),selected = 1)
                     ),
                     column(width = 3,
                            valueBoxOutput("bE_eff", width = NULL)
                     ),
                     column(width = 3,
                            valueBoxOutput("bE_var", width = NULL)
                     ),
                     column(width = 3,
                            valueBoxOutput("bE_evo", width = NULL)
                     )
                   ),
                   
                   fluidRow(
                     column(3
                            ,uiOutput("mgeo_E")
                            ,uiOutput("mcal_E")
                            ,uiOutput("mmot_E")
                            ,uiOutput("msex_E")
                            ,uiOutput("mage_E")
                     ),        
                     column(width = 9,
                            box(title = NULL
                                ,width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE
                                ,plotlyOutput('Graph_defmE', height = "100%", width = "100%")
                                ,h5(textOutput('type_E'))
                                ,h5(textOutput('champ_E'))
                                ,h5('Source: Pôle Emploi - Dares, STMT')
                            )
                     )
                     
                   ),
                   
                   
                   br(), br(),
                   hr()
                   
                 )
)

