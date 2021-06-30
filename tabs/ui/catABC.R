catABC <- tabPanel(title = "Catégories ABC", 
                   value = "catABC",
                   dashboardBody(
                     h1(paste0("Catégories ABC")),
                     h2(textOutput('title_ABC')),
                     hr(),
                     fluidRow(
                       column(3,
                              radioButtons("mcvs_ABC", label = "Correction des variations saisonnières",
                                           choices = list("OUI" = 1, "NON" = 2),selected = 1)
                       ),
                       column(width = 3,
                              valueBoxOutput("bABC_eff", width = NULL)
                       ),
                       column(width = 3,
                              valueBoxOutput("bABC_var", width = NULL)
                       ),
                       column(width = 3,
                              valueBoxOutput("bABC_evo", width = NULL)
                       )
                     ),
                     
                     fluidRow(
                       column(3
                              ,uiOutput("mgeo_ABC")
                              ,uiOutput("mcal_ABC")
                              ,uiOutput("mmot_ABC")
                              ,uiOutput("msex_ABC")
                              ,uiOutput("mage_ABC")
                              ,valueBoxOutput("bABC_par", width = NULL)
                       ),        
                       column(width = 9,
                              box(title = NULL
                                  ,width = NULL, solidHeader = FALSE, status = "primary", collapsible = TRUE
                                  ,plotlyOutput('Graph_defmABC', height = "100%", width = "100%")
                                  ,h5(textOutput('type_ABC'))
                                  ,h5(textOutput('champ_ABC'))
                                  ,h5('Source: Pôle Emploi - Dares, STMT')
                              )
                       )
                       
                     ),
                     
                     
                     br(), br(),
                     hr()
                     
                   )
)

