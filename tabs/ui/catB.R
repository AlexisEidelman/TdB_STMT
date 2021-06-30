catB <- tabPanel(title = "Catégorie B", 
                 value = "catB",
                 dashboardBody(
                   h1(paste0("Catégorie B")),
                   h2(textOutput('title_B')),
                   hr(),
                   fluidRow(
                     column(3,
                            radioButtons("mcvs_B", label = "Correction des variations saisonnières",
                                         choices = list("OUI" = 1, "NON" = 2),selected = 1)
                     ),
                     column(width = 3,
                            valueBoxOutput("bB_eff", width = NULL)
                     ),
                     column(width = 3,
                            valueBoxOutput("bB_var", width = NULL)
                     ),
                     column(width = 3,
                            valueBoxOutput("bB_evo", width = NULL)
                     )
                   ),
                   
                   fluidRow(
                     column(3
                            ,uiOutput("mgeo_B")
                            ,uiOutput("mcal_B")
                            ,uiOutput("mmot_B")
                            ,uiOutput("msex_B")
                            ,uiOutput("mage_B")
                     ),        
                     column(width = 9,
                            box(title = NULL
                                ,width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE
                                ,plotlyOutput('Graph_defmB', height = "100%", width = "100%")
                                ,h5(textOutput('type_B'))
                                ,h5(textOutput('champ_B'))
                                ,h5('Source: Pôle Emploi - Dares, STMT')
                            )
                     )
                     
                   ),
                   
                   
                   br(), br(),
                   hr()
                   
                 )
)

