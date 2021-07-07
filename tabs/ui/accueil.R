accueil <- tabPanel(title = "Accueil", 
                    value = "accueil",
                    dashboardBody(
                      h1("Inscrits tenus de rechercher un emploi (catégories A, B et C)"),
                      hr(),
                      h2(tags$p(mois_stat, style = "font-size: 20px;text-transform: uppercase;text-align: left;")),
                      h4("Données CVS-CJO, France métropolitaine"),
                      fluidRow(
                        column(width = 2
                               ,valueBoxOutput("box_A", width = NULL)
                               
                        ),
                        column(width = 3
                               ,plotOutput('Graph_A', width = NULL, height = 106)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput("box_effA", width = NULL)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput('box_evoA', width = NULL)
                               
                        ),
                        column(width = 2
                               ,valueBoxOutput('box_varA', width = NULL)
                        ),
                        column(width = 1
                               ,switchInput(inputId = "TabAviz"
                                            ,size = "mini"
                                            ,label = "<i class=\"fa fa-columns\"></i>"
                                            ,onLabel = "-"
                                            ,offLabel = "+"))
                      ),
                      DTOutput('TabdefmA', width = "100%"),
                      fluidRow(
                        column(width = 2
                               ,valueBoxOutput("box_B", width = NULL)
                               
                        ),
                        column(width = 3
                               ,plotOutput('Graph_B', width = NULL, height = 106)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput("box_effB", width = NULL)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput('box_evoB', width = NULL)
                               
                        ),
                        column(width = 2
                               ,valueBoxOutput('box_varB', width = NULL)
                        ),
                        column(width = 1
                               ,switchInput(inputId = "TabBviz"
                                            ,size = "mini"
                                            ,label = "<i class=\"fa fa-columns\"></i>"
                                            ,onLabel = "-"
                                            ,offLabel = "+"))
                      ),
                      DTOutput('TabdefmB', width = "100%"),
                      fluidRow(
                        column(width = 2
                               ,valueBoxOutput("box_C", width = NULL)
                               
                        ),
                        column(width = 3
                               ,plotOutput('Graph_C', width = NULL, height = 106)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput("box_effC", width = NULL)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput('box_evoC', width = NULL)
                               
                        ),
                        column(width = 2
                               ,valueBoxOutput('box_varC', width = NULL)
                        ),
                        column(width = 1
                               ,switchInput(inputId = "TabCviz"
                                            ,size = "mini"
                                            ,label = "<i class=\"fa fa-columns\"></i>"
                                            ,onLabel = "-"
                                            ,offLabel = "+"))
                      ),
                      DTOutput('TabdefmC', width = "100%"),
                      fluidRow(
                        column(width = 2
                               ,valueBoxOutput("box_ABC", width = NULL)
                               
                        ),
                        column(width = 3
                               ,plotOutput('Graph_ABC', width = NULL, height = 106)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput("box_effABC", width = NULL)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput('box_evoABC', width = NULL)
                               ,h5("Les évolutions et les variations sont mensuelles")
                               
                        ),
                        column(width = 2
                               ,valueBoxOutput('box_varABC', width = NULL)
                        ),
                        column(width = 1
                               ,switchInput(inputId = "TabABCviz"
                                            ,size = "mini"
                                            ,label = "<i class=\"fa fa-columns\"></i>"
                                            ,onLabel = "-"
                                            ,offLabel = "+"))
                      ),
                      DTOutput('TabdefmABC', width = "100%"),
                      
                      br(), br(), 
                      h1("Entrées et sorties de catégories A, B, C"),
                      hr(),
                      h2(tags$p(mois_stat, style = "font-size: 20px;text-transform: uppercase;text-align: left;")),
                      h4("Données CVS-CJO, France métropolitaine"),
                      fluidRow(
                        
                        column(width = 2
                               ,valueBoxOutput("box_DEE", width = NULL)
                               
                        ),
                        column(width = 3
                               ,plotOutput('Graph_DEE', width = NULL, height = 106)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput("box_effDEE", width = NULL)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput('box_evoDEE', width = NULL)
                               
                        ),
                        column(width = 2
                               ,valueBoxOutput('box_varDEE', width = NULL)
                               
                        ),
                        column(width = 1
                               ,switchInput(inputId = "TabDEEviz"
                                            ,size = "mini"
                                            ,label = "<i class=\"fa fa-columns\"></i>"
                                            ,onLabel = "-"
                                            ,offLabel = "+"))
                      ),
                      DTOutput('TabdeeABC', width = "100%"),
                      fluidRow(
                        
                        column(width = 2
                               ,valueBoxOutput("box_DES", width = NULL)
                               
                        ),
                        column(width = 3
                               ,plotOutput('Graph_DES', width = NULL, height = 106)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput("box_effDES", width = NULL)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput('box_evoDES', width = NULL)
                               ,h5("Les évolutions et les variations sont mensuelles")
                        ),
                        column(width = 2
                               ,valueBoxOutput('box_varDES', width = NULL)
                               
                        ),
                        column(width = 1
                               ,switchInput(inputId = "TabDESviz"
                                            ,size = "mini"
                                            ,label = "<i class=\"fa fa-columns\"></i>"
                                            ,onLabel = "-"
                                            ,offLabel = "+"))
                      ),
                      DTOutput('TabdesABC', width = "100%"),
                      
                      br(), br(), 
                      h1("Inscrits non tenus de rechercher un emploi (catégories D et E)"),
                      hr(),
                      h2(tags$p(mois_stat, style = "font-size: 20px;text-transform: uppercase;text-align: left;")),
                      h4("Données CVS-CJO, France métropolitaine"),
                      fluidRow(
                        column(width = 2
                               ,valueBoxOutput("box_D", width = NULL)
                               
                        ),
                        column(width = 3
                               ,plotOutput('Graph_D', width = NULL, height = 106)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput("box_effD", width = NULL)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput('box_evoD', width = NULL)
                               
                        ),
                        column(width = 2
                               ,valueBoxOutput('box_varD', width = NULL)
                        ),
                        column(width = 1
                               ,switchInput(inputId = "TabDviz"
                                            ,size = "mini"
                                            ,label = "<i class=\"fa fa-columns\"></i>"
                                            ,onLabel = "-"
                                            ,offLabel = "+"))
                      ),
                      DTOutput('TabdefmD', width = "100%"),
                      fluidRow(
                        column(width = 2
                               ,valueBoxOutput("box_E", width = NULL)
                               
                        ),
                        column(width = 3
                               ,plotOutput('Graph_E', width = NULL, height = 106)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput("box_effE", width = NULL)
                               
                        ),
                        
                        column(width = 2
                               ,valueBoxOutput('box_evoE', width = NULL)
                               ,h5("Les évolutions et les variations sont mensuelles")
                               
                        ),
                        column(width = 2
                               ,valueBoxOutput('box_varE', width = NULL)
                        ),
                        column(width = 1
                               ,switchInput(inputId = "TabEviz"
                                            ,size = "mini"
                                            ,label = "<i class=\"fa fa-columns\"></i>"
                                            ,onLabel = "-"
                                            ,offLabel = "+"))
                      ),
                      DTOutput('TabdefmE', width = "100%"),
                      br(), br()
                    )
)
