################################################################################
# UI
################################################################################
shinyUI <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        tags$head(tags$style(HTML('/* body */.content-wrapper, .right-side {background-color: #F5F5F5;}'))
                  
        ),
        
        fluidPage(
            useShinyjs(),
            tags$head(
                tags$link(rel = "shortcut icon", href = "img/Dares.png")
            ),
            ##-- Logo ----
            list(tags$head(HTML('<link rel="icon", href="img/Marianne.png",
                        type="image/png" />'))),
            div(style="padding: 1px 1px; width: '100%'",
                titlePanel(
                    title="", windowTitle = "Tableau de bord DEFM"
                )
            ),
            
            ##-- Header ----
            navbarPage(title = div(img(src="img/New_Logo_DARES.png",
                                       height = "105px", width = "312px")),
                       id = "navbar",
                       selected = "accueil",
                       theme = "styles.css", 
                       fluid = T,
                       windowTitle = "title",
                       ##-- Menu haut de page ----
                       accueil,
                       navbarMenu("Catégories ABC",
                                  catA,
                                  catB,
                                  catC,
                                  catBC,
                                  catABC,
                                  Ancien_ABC
                                  
                       ),
                       navbarMenu("Les Flux",
                                  entrees,
                                  sorties
                                  
                       ),
                       catD,
                       catE
                       
                       
            ),
            ##-- Footer ----
            div(class = "footer",
                includeHTML("html/site_footer.html")
                
            )
        )
        
    )
)