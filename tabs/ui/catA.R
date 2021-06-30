catA <- tabPanel(title = "Catégorie A", 
                 value = "catA",
                 dashboardBody(
                   h1(paste0("Catégorie A")),
                   h2(textOutput('title_A')),
                   hr(),
                   fluidRow(
                     column(3,
                            radioButtons("mcvs_A", label = "Correction des variations saisonnières",
                                         choices = list("OUI" = 1, "NON" = 2),selected = 1)
                     ),
                     column(width = 3,
                            valueBoxOutput("bA_eff", width = NULL)
                     ),
                     column(width = 3,
                            valueBoxOutput("bA_var", width = NULL)
                     ),
                     column(width = 3,
                            valueBoxOutput("bA_evo", width = NULL)
                     )
                   ),
                   
                   fluidRow(
                     column(3,
                            uiOutput("mgeo_A"),
                            uiOutput("mcal_A"),
                            uiOutput("msex_A"),
                            uiOutput("mage_A")
                     ),        
                     column(width = 9,
                            box(title = NULL
                                ,width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE
                                ,plotlyOutput('Graph_defmA', height = "100%", width = "100%")
                                ,h5(textOutput('type_A'))
                                ,h5(textOutput('champ_A'))
                                ,h5('Source: Pôle Emploi - Dares, STMT')
                            )
                     )
                     
                   ),
                   
                   
                   br(), br(),
                   hr()
                   
                 )
)

