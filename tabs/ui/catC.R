catC <- tabPanel(title = "Catégorie C", 
                 value = "catC",
                 dashboardBody(
                   h1(paste0("Catégorie C")),
                   h2(textOutput('title_C')),
                   hr(),
                   fluidRow(
                     column(3,
                            radioButtons("mcvs_C", label = "Correction des variations saisonnières",
                                         choices = list("OUI" = 1, "NON" = 2),selected = 1)
                     ),
                     column(width = 3,
                            valueBoxOutput("bC_eff", width = NULL)
                     ),
                     column(width = 3,
                            valueBoxOutput("bC_var", width = NULL)
                     ),
                     column(width = 3,
                            valueBoxOutput("bC_evo", width = NULL)
                     )
                   ),
                   
                   fluidRow(
                     column(3
                            ,uiOutput("mgeo_C")
                            ,uiOutput("mcal_C")
                            ,uiOutput("mmot_C")
                            ,uiOutput("msex_C")
                            ,uiOutput("mage_C")
                     ),        
                     column(width = 9,
                            box(title = NULL
                                ,width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE
                                ,plotlyOutput('Graph_defmC', height = "100%", width = "100%")
                                ,h5(textOutput('type_C'))
                                ,h5(textOutput('champ_C'))
                                ,h5('Source: Pôle Emploi - Dares, STMT')
                            )
                     )
                     
                   ),
                   
                   
                   br(), br(),
                   hr()
                   
                 )
)

