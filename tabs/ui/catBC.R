catBC <- tabPanel(title = "Catégories BC", 
                  value = "catBC",
                  dashboardBody(
                    h1(paste0("Catégories BC")),
                    h2(textOutput('title_BC')),
                    hr(),
                    fluidRow(
                      column(3,
                             radioButtons("mcvs_BC", label = "Correction des variations saisonnières",
                                          choices = list("OUI" = 1, "NON" = 2),selected = 1)
                      ),
                      column(width = 3,
                             valueBoxOutput("bBC_eff", width = NULL)
                      ),
                      column(width = 3,
                             valueBoxOutput("bBC_var", width = NULL)
                      ),
                      column(width = 3,
                             valueBoxOutput("bBC_evo", width = NULL)
                      )
                    ),
                    
                    fluidRow(
                      column(3,
                             uiOutput("mgeo_BC"),
                             uiOutput("mcal_BC"),
                             uiOutput("msex_BC"),
                             uiOutput("mage_BC")
                      ),        
                      column(width = 9,
                             box(title = NULL
                                 ,width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE
                                 ,plotlyOutput('Graph_defmBC', height = "100%", width = "100%")
                                 ,h5(textOutput('type_BC'))
                                 ,h5(textOutput('champ_BC'))
                                 ,h5('Source: Pôle Emploi - Dares, STMT')
                             )
                      )
                      
                    ),
                    
                    
                    br(), br(),
                    hr()
                    
                  )
)

