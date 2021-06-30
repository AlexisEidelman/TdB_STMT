##-- menus ----
output$mgeo_des = renderUI(selectInput("Geo_des", label = "Champ :", c("France métropolitaine","France (hors Mayotte)"),"France métropolitaine"))

output$mcal_des = renderUI(selectInput("Cal_des", label = "Calculs :", c("sur 1 mois","sur 3 mois","sur 1 an"),"sur 1 mois"))

output$mmot_des = renderUI(selectInput("Mot_des", label = "Motif d'entrée :", c("Ensemble"
                                                                                ,"Reprise d'emploi déclarée"
                                                                                ,"Entrée en stage"
                                                                                ,"Arrêt de recherche"
                                                                                ,"Arrêt de recherche pour maladie"
                                                                                ,"Arrêt de recherche pour retraite"
                                                                                ,"Arrêt de recherche hors retraite"
                                                                                ,"Autre arrêt de recherche"
                                                                                ,"Cessation d'inscription pour défaut d'actualisation"
                                                                                ,"Radiation administrative"
                                                                                ,"Autre cas")
                                       ,"Ensemble"))

output$msex_des = renderUI({
  if(!is.null(input$Mot_des)){
    if(input$Mot_des=="Ensemble" & input$Geo_des=="France métropolitaine"){
      selectInput("Sex_des", label = "Sexe :", c("Ensemble","Hommes","Femmes"),"Ensemble")}
  }
})

output$mage_des = renderUI({
  if(!is.null(input$Mot_des)){
    if(input$Mot_des=="Ensemble" & input$Geo_des=="France métropolitaine"){
      selectInput("Age_des", label = "Tranche d'âge :", c("Ensemble","moins de 25 ans","de 25 à 49 ans","50 ans ou plus"),"Ensemble")}
  }
})


##-- Maj titre, type de donnée et champ ----
output$title_des <- renderText({ 
  paste(ser_des$TG, " en ", mois_stat)
})

output$type_des <- renderText({ 
  if(input$mcvs_des==1){paste("Données corrigées des varaitions saisonnières et des jours ouvrables (CVS-CJO)")
  }else{paste("Données brutes")}
})

output$champ_des <- renderText({ 
  paste("Champ: ", input$Geo_des)
})


##-- Partie réactive ----
ser_des <- reactiveValues(cvs = NULL, geo = NULL, var = NULL, evo = NULL, sex = NULL, age = NULL, mot = NULL, graph = "eff", TG = NULL)

observe({
  ##-- Actualisation des filtres ----
  if(!is.null(input$mcvs_des)){if(input$mcvs_des==2){ser_des$cvs <- "N"}else{ser_des$cvs <- "O"}}
  if(!is.null(input$Geo_des)){if(input$Geo_des=="France métropolitaine"){ser_des$geo <- "Fm"}else{ser_des$geo <- "Fr"}}
  if(!is.null(input$Cal_des)){if(input$Cal_des=="sur 1 mois"){
    ser_des$var <- "V1m"
    ser_des$evo <- "E1m"
  }else if(input$Cal_des=="sur 3 mois"){
    ser_des$var <- "V3m"
    ser_des$evo <- "E3m"
  }else{
    ser_des$var <- "V1a"
    ser_des$evo <- "E1a"
  }}
  
  
  
  if(!is.null(input$Sex_des)){
    if(input$Mot_des!="Ensemble" | input$Geo_des=="France (hors Mayotte)"){ser_des$sex <- "E"
    }else if(input$Sex_des=="Hommes"){ser_des$sex <- "H"
    }else if(input$Sex_des=="Femmes"){ser_des$sex <- "F"
    }else{ser_des$sex <- "E"}
  }
  
  if(!is.null(input$Age_des)){
    if(input$Mot_des!="Ensemble" | input$Geo_des=="France (hors Mayotte)"){ser_des$age <- "E"
    }else if(input$Age_des=="moins de 25 ans"){ser_des$age <- "J"
    }else if(input$Age_des=="de 25 à 49 ans"){ser_des$age <- "M"
    }else if(input$Age_des=="50 ans ou plus"){ser_des$age <- "S"
    }else{ser_des$age <- "E"}
  }
  
  if(!is.null(ser_des$sex)){if(ser_des$sex == "E" & ser_des$age == "E"){
    if(ser_des$mot != ""){ser_des$TG <- input$Mot_des
    }else{ser_des$TG <- "Les sorties"}
  }else if(ser_des$sex != "E" & ser_des$age == "E"){ser_des$TG <- paste0(input$Sex_des, "")}
    else if(ser_des$sex == "E" & ser_des$age != "E"){ser_des$TG <- paste0("",input$Age_des)}
    else {ser_des$TG <- paste0(input$Sex_des, " ayant ",input$Age_des)}
  }
  
  if(!is.null(input$Mot_des)){
    if(input$Mot_des=="Reprise d'emploi déclarée"){ser_des$mot <- "remp"
    }else if(input$Mot_des=="Entrée en stage"){ser_des$mot <- "stag"
    }else if(input$Mot_des=="Arrêt de recherche"){ser_des$mot <- "arec"
    }else if(input$Mot_des=="Arrêt de recherche pour maladie"){ser_des$mot <- "mala"
    }else if(input$Mot_des=="Arrêt de recherche pour retraite"){ser_des$mot <- "retr"
    }else if(input$Mot_des=="Arrêt de recherche hors retraite"){ser_des$mot <- "aare_nretr"
    }else if(input$Mot_des=="Autre arrêt de recherche"){ser_des$mot <- "aare"
    }else if(input$Mot_des=="Cessation d'inscription pour défaut d'actualisation"){ser_des$mot <- "absc"
    }else if(input$Mot_des=="Radiation administrative"){ser_des$mot <- "radi"
    }else if(input$Mot_des=="Autre cas"){ser_des$mot <- "autr"
    }else{ser_des$mot <- ""
    }}
  
  
  ##-- Actualisation des données des infos box ----
  if(!is.null(input$mcvs_des)){
    des_eff <- fp_des[fp_des$mois_stat==max(fp_des$mois_stat, na.rm = TRUE) 
                      & fp_des$geo==ser_des$geo & fp_des$cvs==ser_des$cvs & fp_des$type=="Eff" 
                      & fp_des$cat=="ABC" & fp_des$sexe==ser_des$sex & fp_des$age==ser_des$age & fp_des$motif==ser_des$mot,9]
    infodes_eff <- format(des_eff, big.mark = " ")
    
    des_var <- fp_des[fp_des$mois_stat==max(fp_des$mois_stat, na.rm = TRUE) 
                      & fp_des$geo==ser_des$geo & fp_des$cvs==ser_des$cvs & fp_des$type==ser_des$var 
                      & fp_des$cat=="ABC" & fp_des$sexe==ser_des$sex & fp_des$age==ser_des$age & fp_des$motif==ser_des$mot,9]
    infodes_var <- format(des_var, big.mark = " ")
    
    des_evo <- fp_des[fp_des$mois_stat==max(fp_des$mois_stat, na.rm = TRUE) 
                      & fp_des$geo==ser_des$geo & fp_des$cvs==ser_des$cvs & fp_des$type==ser_des$evo 
                      & fp_des$cat=="ABC" & fp_des$sexe==ser_des$sex & fp_des$age==ser_des$age & fp_des$motif==ser_des$mot,9]
    infodes_evo <- paste0(format(des_evo, decimal.mark = ",", digits = 2, nsmall = 1), " %")
    
    des_par <- fp_des[fp_des$mois_stat==max(fp_des$mois_stat, na.rm = TRUE) 
                      & fp_des$geo==ser_des$geo & fp_des$cvs==ser_des$cvs & fp_des$type=="Par" 
                      & fp_des$cat=="ABC" & fp_des$sexe==ser_des$sex & fp_des$age==ser_des$age & fp_des$motif==ser_des$mot,9]
    infodes_par <- paste0(format(des_par, decimal.mark = ","), " %")
  }
  
  ##-- Création des infos box ---- 
  output$bdes_eff <- renderValueBox({
    bdeseff<-valueBox(value=tags$p("Effectif", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                      ,width=NULL
                      ,icon = icon("users",lib="font-awesome")
                      ,color="olive"
                      ,href="#"
                      ,subtitle=tags$p(infodes_eff, style = "font-size: 28px;text-align: center;")
    )
    bdeseff$children[[1]]$attribs$class<-"action-button"
    bdeseff$children[[1]]$attribs$id<-"button_deseff"
    return(bdeseff)
  })
  
  output$bdes_var <- renderValueBox({
    bdesvar<-valueBox(value=tags$p("Variation ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                      ,width=NULL
                      ,icon = icon("chart-bar")
                      ,color="olive"
                      ,href="#"
                      ,subtitle=tags$p(paste(infodes_var, input$Cal_des), style = "font-size: 24px;text-align: center;")
    )
    bdesvar$children[[1]]$attribs$class<-"action-button"
    bdesvar$children[[1]]$attribs$id<-"button_desvar"
    return(bdesvar)
  })
  
  output$bdes_evo <- renderValueBox({
    bdesevo<-valueBox(value=tags$p("évolution ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                      ,width=NULL
                      ,icon = icon("chart-line")
                      ,color="olive"
                      ,href="#"
                      ,subtitle=tags$p(paste(infodes_evo, input$Cal_des), style = "font-size: 24px;text-align: center;")
    )
    bdesevo$children[[1]]$attribs$class<-"action-button"
    bdesevo$children[[1]]$attribs$id<-"button_desevo"
    return(bdesevo)
  })
  
  observeEvent(input$Mot_des,{
    if(input$Mot_des!="Ensemble"){   
      output$bdes_par <- renderValueBox({
        
        bdespar<-valueBox(value=tags$p("Part parmi les sorties", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                          ,width=NULL
                          ,icon = icon("chart-pie")
                          ,color="olive"
                          ,href="#"
                          ,subtitle=tags$p(infodes_par, style = "font-size: 24px;text-align: center;")
        )
        
        bdespar$children[[1]]$attribs$class<-"action-button"
        bdespar$children[[1]]$attribs$id<-"button_despar"
        return(bdespar)
        
      })
    }else{
      output$bdes_par <- NULL}
  })
  
  
  ##-- Actualisation des données ----
  if(!is.null(input$mcvs_des)){
    dseff <- fp_des[fp_des$geo==ser_des$geo & fp_des$cvs==ser_des$cvs & fp_des$type=="Eff" & fp_des$cat=="ABC" 
                    & fp_des$sexe==ser_des$sex & fp_des$age==ser_des$age & fp_des$motif==ser_des$mot,]
    
    dsvar <- fp_des[fp_des$geo==ser_des$geo & fp_des$cvs==ser_des$cvs & fp_des$type==ser_des$var & fp_des$cat=="ABC" 
                    & fp_des$sexe==ser_des$sex & fp_des$age==ser_des$age & fp_des$motif==ser_des$mot,] %>% filter(!is.na(valeur))
    
    dsevo <- fp_des[fp_des$geo==ser_des$geo & fp_des$cvs==ser_des$cvs & fp_des$type==ser_des$evo & fp_des$cat=="ABC" 
                    & fp_des$sexe==ser_des$sex & fp_des$age==ser_des$age & fp_des$motif==ser_des$mot,] %>% filter(!is.na(valeur))
    
    dspar <- fp_des[fp_des$geo==ser_des$geo & fp_des$cvs==ser_des$cvs & fp_des$type=="Par" & fp_des$cat=="ABC" 
                    & fp_des$sexe==ser_des$sex & fp_des$age==ser_des$age & fp_des$motif==ser_des$mot,] %>% filter(!is.na(valeur))
    
  } 
  
  
  ##-- Choix du graphique ----
  observeEvent(input$button_deseff,{
    ser_des$graph <- "eff"
  })
  
  observeEvent(input$button_desvar,{
    ser_des$graph <- "var"
  })
  
  observeEvent(input$button_desevo,{
    ser_des$graph <- "evo"
  })
  
  observeEvent(input$button_despar,{
    ser_des$graph <- "par"
  })
  
  if(!is.null(input$Mot_des)){
    if(input$Mot_des=="Ensemble" & ser_des$graph == "par"){
      ser_des$graph <- "eff"
    }}
  
  ##-- Actualisation du graphique ----
  if(ser_des$graph=="eff"){ 
    output$Graph_des <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("chartreuse4"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_des$graph=="var"){ 
    output$Graph_des <- renderPlotly({
      plot <- plot_ly(dsvar, x = ~mois_stat, y = ~dsvar$valeur, 
                      type = "bar", color = I("chartreuse4"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_des$graph=="evo"){ 
    output$Graph_des <- renderPlotly({
      plot <- plot_ly(dsevo, x = ~mois_stat, y = ~dsevo$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines+markers', color = I("chartreuse4"))
      plot %>%  
        layout(
          xaxis = list(title = "", range = c(paste0(ms_3_An, "-04-01"), paste0(ms_An,"-04-15"))),
          yaxis = list(title = "")
        )
    })
  }else if(ser_des$graph=="par"){ 
    output$Graph_des <- renderPlotly({
      plot <- plot_ly(dspar, x = ~mois_stat, y = ~dspar$valeur, name = 'série CSV-CJO', 
                      type = 'bar', color = I("chartreuse4"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else{ 
    output$Graph_des <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("chartreuse4"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }
  
})