catD <- tabPanel(title = "Catégorie D", 
                 value = "catD",
                 dashboardBody(
                   h1(paste0("Catégorie D")),
                   h2(textOutput('title_D')),
                   hr(),
                   fluidRow(
                     column(3,
                            radioButtons("mcvs_D", label = "Correction des variations saisonnières",
                                         choices = list("OUI" = 1, "NON" = 2),selected = 1)
                     ),
                     column(width = 3,
                            valueBoxOutput("bD_eff", width = NULL)
                     ),
                     column(width = 3,
                            valueBoxOutput("bD_var", width = NULL)
                     ),
                     column(width = 3,
                            valueBoxOutput("bD_evo", width = NULL)
                     )
                   ),
                   
                   fluidRow(
                     column(3
                            ,uiOutput("mgeo_D")
                            ,uiOutput("mcal_D")
                            ,uiOutput("mmot_D")
                            ,uiOutput("msex_D")
                            ,uiOutput("mage_D")
                     ),        
                     column(width = 9,
                            box(title = NULL
                                ,width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE
                                ,plotlyOutput('Graph_defmD', height = "100%", width = "100%")
                                ,h5(textOutput('type_D'))
                                ,h5(textOutput('champ_D'))
                                ,h5('Source: Pôle Emploi - Dares, STMT')
                            )
                     )
                     
                   ),
                   
                   
                   br(), br(),
                   hr()
                   
                 )
)

