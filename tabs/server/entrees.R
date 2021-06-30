##-- menus ----
output$mgeo_dee = renderUI(selectInput("Geo_dee", label = "Champ :", c("France métropolitaine","France (hors Mayotte)"),"France métropolitaine"))

output$mcal_dee = renderUI(selectInput("Cal_dee", label = "Calculs :", c("sur 1 mois","sur 3 mois","sur 1 an"),"sur 1 mois"))

output$mmot_dee = renderUI(selectInput("Mot_dee", label = "Motif d'entrée :", c("Ensemble"
                                                                                ,"Fin de contrat"
                                                                                ,"Fin de mission d'intérim"
                                                                                ,"Démission"
                                                                                ,"Rupture conventionnelle"
                                                                                ,"Licenciement économique"
                                                                                ,"Autre licenciement"
                                                                                ,"Première entrée sur le marché du travail"
                                                                                ,"Retour d'inactivité"
                                                                                ,"Réinscription rapide"
                                                                                ,"Autres motifs"
                                                                                ,"Motif indéterminé")
                                       ,"Ensemble"))

output$msex_dee = renderUI({
  if(!is.null(input$Mot_dee)){
    if(input$Mot_dee=="Ensemble" & input$Geo_dee=="France métropolitaine"){
      selectInput("Sex_dee", label = "Sexe :", c("Ensemble","Hommes","Femmes"),"Ensemble")}
  }
})

output$mage_dee = renderUI({
  if(!is.null(input$Mot_dee)){
    if(input$Mot_dee=="Ensemble" & input$Geo_dee=="France métropolitaine"){
      selectInput("Age_dee", label = "Tranche d'âge :", c("Ensemble","moins de 25 ans","de 25 à 49 ans","50 ans ou plus"),"Ensemble")}
  }
})


##-- Maj titre, type de donnée et champ ----
output$title_dee <- renderText({ 
  paste(ser_dee$TG, " en ", mois_stat)
})

output$type_dee <- renderText({ 
  if(input$mcvs_dee==1){paste("Données corrigées des varaitions saisonnières et des jours ouvrables (CVS-CJO)")
  }else{paste("Données brutes")}
})

output$champ_dee <- renderText({ 
  paste("Champ: ", input$Geo_dee)
})


##-- Partie réactive ----
ser_dee <- reactiveValues(cvs = NULL, geo = NULL, var = NULL, evo = NULL, sex = NULL, age = NULL, mot = NULL, graph = "eff", TG = NULL)

observe({
  ##-- Actualisation des filtres ----
  if(!is.null(input$mcvs_dee)){if(input$mcvs_dee==2){ser_dee$cvs <- "N"}else{ser_dee$cvs <- "O"}}
  if(!is.null(input$Geo_dee)){if(input$Geo_dee=="France métropolitaine"){ser_dee$geo <- "Fm"}else{ser_dee$geo <- "Fr"}}
  if(!is.null(input$Cal_dee)){if(input$Cal_dee=="sur 1 mois"){
    ser_dee$var <- "V1m"
    ser_dee$evo <- "E1m"
  }else if(input$Cal_dee=="sur 3 mois"){
    ser_dee$var <- "V3m"
    ser_dee$evo <- "E3m"
  }else{
    ser_dee$var <- "V1a"
    ser_dee$evo <- "E1a"
  }}
  
  
  
  if(!is.null(input$Sex_dee)){
    if(input$Mot_dee!="Ensemble" | input$Geo_dee=="France (hors Mayotte)"){ser_dee$sex <- "E"
    }else if(input$Sex_dee=="Hommes"){ser_dee$sex <- "H"
    }else if(input$Sex_dee=="Femmes"){ser_dee$sex <- "F"
    }else{ser_dee$sex <- "E"}
  }
  
  if(!is.null(input$Age_dee)){
    if(input$Mot_dee!="Ensemble" | input$Geo_dee=="France (hors Mayotte)"){ser_dee$age <- "E"
    }else if(input$Age_dee=="moins de 25 ans"){ser_dee$age <- "J"
    }else if(input$Age_dee=="de 25 à 49 ans"){ser_dee$age <- "M"
    }else if(input$Age_dee=="50 ans ou plus"){ser_dee$age <- "S"
    }else{ser_dee$age <- "E"}
  }
  
  if(!is.null(ser_dee$sex)){if(ser_dee$sex == "E" & ser_dee$age == "E"){
    if(ser_dee$mot != ""){ser_dee$TG <- input$Mot_dee
    }else{ser_dee$TG <- "Les entrées"}
  }else if(ser_dee$sex != "E" & ser_dee$age == "E"){ser_dee$TG <- paste0(input$Sex_dee, "")}
    else if(ser_dee$sex == "E" & ser_dee$age != "E"){ser_dee$TG <- paste0("",input$Age_dee)}
    else {ser_dee$TG <- paste0(input$Sex_dee, " ayant ",input$Age_dee)}
  }
  
  if(!is.null(input$Mot_dee)){
    if(input$Mot_dee=="Fin de contrat"){ser_dee$mot <- "fincontrat"
    }else if(input$Mot_dee=="Fin de mission d'intérim"){ser_dee$mot <- "interi"
    }else if(input$Mot_dee=="Démission"){ser_dee$mot <- "demiss"
    }else if(input$Mot_dee=="Rupture conventionnelle"){ser_dee$mot <- "rupconv"
    }else if(input$Mot_dee=="Licenciement économique"){ser_dee$mot <- "licpap"
    }else if(input$Mot_dee=="Autre licenciement"){ser_dee$mot <- "autlic"
    }else if(input$Mot_dee=="Première entrée sur le marché du travail"){ser_dee$mot <- "1entre"
    }else if(input$Mot_dee=="Retour d'inactivité"){ser_dee$mot <- "repact"
    }else if(input$Mot_dee=="Réinscription rapide"){ser_dee$mot <- "RR"
    }else if(input$Mot_dee=="Autres motifs"){ser_dee$mot <- "autmot"
    }else if(input$Mot_dee=="Motif indéterminé"){ser_dee$mot <- "indeterm"
    }else{ser_dee$mot <- ""
    }}
  
  
  ##-- Actualisation des données des infos box ----
  if(!is.null(input$mcvs_dee)){
    dee_eff <- fp_dee[fp_dee$mois_stat==max(fp_dee$mois_stat, na.rm = TRUE) 
                      & fp_dee$geo==ser_dee$geo & fp_dee$cvs==ser_dee$cvs & fp_dee$type=="Eff" 
                      & fp_dee$cat=="ABC" & fp_dee$sexe==ser_dee$sex & fp_dee$age==ser_dee$age & fp_dee$motif==ser_dee$mot,9]
    infodee_eff <- format(dee_eff, big.mark = " ")
    
    dee_var <- fp_dee[fp_dee$mois_stat==max(fp_dee$mois_stat, na.rm = TRUE) 
                      & fp_dee$geo==ser_dee$geo & fp_dee$cvs==ser_dee$cvs & fp_dee$type==ser_dee$var 
                      & fp_dee$cat=="ABC" & fp_dee$sexe==ser_dee$sex & fp_dee$age==ser_dee$age & fp_dee$motif==ser_dee$mot,9]
    infodee_var <- format(dee_var, big.mark = " ")
    
    dee_evo <- fp_dee[fp_dee$mois_stat==max(fp_dee$mois_stat, na.rm = TRUE) 
                      & fp_dee$geo==ser_dee$geo & fp_dee$cvs==ser_dee$cvs & fp_dee$type==ser_dee$evo 
                      & fp_dee$cat=="ABC" & fp_dee$sexe==ser_dee$sex & fp_dee$age==ser_dee$age & fp_dee$motif==ser_dee$mot,9]
    infodee_evo <- paste0(format(dee_evo, decimal.mark = ",", digits = 2, nsmall = 1), " %")
    
    dee_par <- fp_dee[fp_dee$mois_stat==max(fp_dee$mois_stat, na.rm = TRUE) 
                      & fp_dee$geo==ser_dee$geo & fp_dee$cvs==ser_dee$cvs & fp_dee$type=="Par" 
                      & fp_dee$cat=="ABC" & fp_dee$sexe==ser_dee$sex & fp_dee$age==ser_dee$age & fp_dee$motif==ser_dee$mot,9]
    infodee_par <- paste0(format(dee_par, decimal.mark = ","), " %")
  }
  
  ##-- Création des infos box ---- 
  output$bdee_eff <- renderValueBox({
    bdeeeff<-valueBox(value=tags$p("Effectif", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                      ,width=NULL
                      ,icon = icon("users",lib="font-awesome")
                      ,color="orange"
                      ,href="#"
                      ,subtitle=tags$p(infodee_eff, style = "font-size: 28px;text-align: center;")
    )
    bdeeeff$children[[1]]$attribs$class<-"action-button"
    bdeeeff$children[[1]]$attribs$id<-"button_deeeff"
    return(bdeeeff)
  })
  
  output$bdee_var <- renderValueBox({
    bdeevar<-valueBox(value=tags$p("Variation ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                      ,width=NULL
                      ,icon = icon("chart-bar")
                      ,color="orange"
                      ,href="#"
                      ,subtitle=tags$p(paste(infodee_var, input$Cal_dee), style = "font-size: 24px;text-align: center;")
    )
    bdeevar$children[[1]]$attribs$class<-"action-button"
    bdeevar$children[[1]]$attribs$id<-"button_deevar"
    return(bdeevar)
  })
  
  output$bdee_evo <- renderValueBox({
    bdeeevo<-valueBox(value=tags$p("évolution ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                      ,width=NULL
                      ,icon = icon("chart-line")
                      ,color="orange"
                      ,href="#"
                      ,subtitle=tags$p(paste(infodee_evo, input$Cal_dee), style = "font-size: 24px;text-align: center;")
    )
    bdeeevo$children[[1]]$attribs$class<-"action-button"
    bdeeevo$children[[1]]$attribs$id<-"button_deeevo"
    return(bdeeevo)
  })
  
  observeEvent(input$Mot_dee,{
    if(input$Mot_dee!="Ensemble"){   
      output$bdee_par <- renderValueBox({
        
        bdeepar<-valueBox(value=tags$p("Part parmi les entrées", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                          ,width=NULL
                          ,icon = icon("chart-pie")
                          ,color="orange"
                          ,href="#"
                          ,subtitle=tags$p(infodee_par, style = "font-size: 24px;text-align: center;")
        )
        
        bdeepar$children[[1]]$attribs$class<-"action-button"
        bdeepar$children[[1]]$attribs$id<-"button_deepar"
        return(bdeepar)
        
      })
    }else{
      output$bdee_par <- NULL}
  })
  
  
  ##-- Actualisation des données ----
  if(!is.null(input$mcvs_dee)){
    dseff <- fp_dee[fp_dee$geo==ser_dee$geo & fp_dee$cvs==ser_dee$cvs & fp_dee$type=="Eff" & fp_dee$cat=="ABC" 
                    & fp_dee$sexe==ser_dee$sex & fp_dee$age==ser_dee$age & fp_dee$motif==ser_dee$mot,]
    
    dsvar <- fp_dee[fp_dee$geo==ser_dee$geo & fp_dee$cvs==ser_dee$cvs & fp_dee$type==ser_dee$var & fp_dee$cat=="ABC" 
                    & fp_dee$sexe==ser_dee$sex & fp_dee$age==ser_dee$age & fp_dee$motif==ser_dee$mot,] %>% filter(!is.na(valeur))
    
    dsevo <- fp_dee[fp_dee$geo==ser_dee$geo & fp_dee$cvs==ser_dee$cvs & fp_dee$type==ser_dee$evo & fp_dee$cat=="ABC" 
                    & fp_dee$sexe==ser_dee$sex & fp_dee$age==ser_dee$age & fp_dee$motif==ser_dee$mot,] %>% filter(!is.na(valeur))
    
    dspar <- fp_dee[fp_dee$geo==ser_dee$geo & fp_dee$cvs==ser_dee$cvs & fp_dee$type=="Par" & fp_dee$cat=="ABC" 
                    & fp_dee$sexe==ser_dee$sex & fp_dee$age==ser_dee$age & fp_dee$motif==ser_dee$mot,] %>% filter(!is.na(valeur))
    
  } 
  
  
  ##-- Choix du graphique ----
  observeEvent(input$button_deeeff,{
    ser_dee$graph <- "eff"
  })
  
  observeEvent(input$button_deevar,{
    ser_dee$graph <- "var"
  })
  
  observeEvent(input$button_deeevo,{
    ser_dee$graph <- "evo"
  })
  
  observeEvent(input$button_deepar,{
    ser_dee$graph <- "par"
  })
  
  if(!is.null(input$Mot_dee)){
    if(input$Mot_dee=="Ensemble" & ser_dee$graph == "par"){
      ser_dee$graph <- "eff"
    }}
  
  ##-- Actualisation du graphique ----
  if(ser_dee$graph=="eff"){ 
    output$Graph_dee <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("orange"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_dee$graph=="var"){ 
    output$Graph_dee <- renderPlotly({
      plot <- plot_ly(dsvar, x = ~mois_stat, y = ~dsvar$valeur, 
                      type = "bar", color = I("orange"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_dee$graph=="evo"){ 
    output$Graph_dee <- renderPlotly({
      plot <- plot_ly(dsevo, x = ~mois_stat, y = ~dsevo$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines+markers', color = I("orange"))
      plot %>%  
        layout(
          xaxis = list(title = "", range = c(paste0(ms_3_An, "-04-01"), paste0(ms_An,"-04-15"))),
          yaxis = list(title = "")
        )
    })
  }else if(ser_dee$graph=="par"){ 
    output$Graph_dee <- renderPlotly({
      plot <- plot_ly(dspar, x = ~mois_stat, y = ~dspar$valeur, name = 'série CSV-CJO', 
                      type = 'bar', color = I("orange"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else{ 
    output$Graph_dee <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("orange"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }
  
})