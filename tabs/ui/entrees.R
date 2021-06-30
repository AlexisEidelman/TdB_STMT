entrees <- tabPanel(title = "Les entrées sur les listes", 
                    value = "Entrées",
                    dashboardBody(
                      h1(paste0("Entrées en catégories ABC")),
                      h2(textOutput('title_dee')),
                      hr(),
                      fluidRow(
                        column(3,
                               radioButtons("mcvs_dee", label = "Correction des variations saisonnières",
                                            choices = list("OUI" = 1, "NON" = 2),selected = 1)
                        ),
                        column(width = 3,
                               valueBoxOutput("bdee_eff", width = NULL)
                        ),
                        column(width = 3,
                               valueBoxOutput("bdee_var", width = NULL)
                        ),
                        column(width = 3,
                               valueBoxOutput("bdee_evo", width = NULL)
                        )
                      ),
                      
                      fluidRow(
                        column(3
                               ,uiOutput("mgeo_dee")
                               ,uiOutput("mcal_dee")
                               ,uiOutput("mmot_dee")
                               ,uiOutput("msex_dee")
                               ,uiOutput("mage_dee")
                               ,valueBoxOutput("bdee_par", width = NULL)
                        ),        
                        column(width = 9,
                               box(title = NULL
                                   ,width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE
                                   ,plotlyOutput('Graph_dee', height = "100%", width = "100%")
                                   ,h5(textOutput('type_dee'))
                                   ,h5(textOutput('champ_dee'))
                                   ,h5('Source: Pôle Emploi - Dares, STMT')
                               )
                        )
                        
                      ),
                      
                      
                      br(), br(),
                      hr()
                      
                    )
)

