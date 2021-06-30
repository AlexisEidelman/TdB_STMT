sorties <- tabPanel(title = "Les sorties des listes", 
                    value = "Sorties",
                    dashboardBody(
                      h1(paste0("Sorties de catégories ABC")),
                      h2(textOutput('title_des')),
                      hr(),
                      fluidRow(
                        column(3,
                               radioButtons("mcvs_des", label = "Correction des variations saisonnières",
                                            choices = list("OUI" = 1, "NON" = 2),selected = 1)
                        ),
                        column(width = 3,
                               valueBoxOutput("bdes_eff", width = NULL)
                        ),
                        column(width = 3,
                               valueBoxOutput("bdes_var", width = NULL)
                        ),
                        column(width = 3,
                               valueBoxOutput("bdes_evo", width = NULL)
                        )
                      ),
                      
                      fluidRow(
                        column(3
                               ,uiOutput("mgeo_des")
                               ,uiOutput("mcal_des")
                               ,uiOutput("mmot_des")
                               ,uiOutput("msex_des")
                               ,uiOutput("mage_des")
                               ,valueBoxOutput("bdes_par", width = NULL)
                        ),        
                        column(width = 9,
                               box(title = NULL
                                   ,width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE
                                   ,plotlyOutput('Graph_des', height = "100%", width = "100%")
                                   ,h5(textOutput('type_des'))
                                   ,h5(textOutput('champ_des'))
                                   ,h5('Source: Pôle Emploi - Dares, STMT')
                               )
                        )
                        
                      ),
                      
                      
                      br(), br(),
                      hr()
                      
                    )
)

