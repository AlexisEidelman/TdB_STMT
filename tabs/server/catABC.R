##-- menus ----
output$mgeo_ABC = renderUI({
  if(!is.null(input$Mot_ABC)){
    if(input$Mot_ABC=="Ensemble"){
      selectInput("Geo_ABC", label = "Champ :", c("France métropolitaine","France (hors Mayotte)"),"France métropolitaine")
    }else{selectInput("Geo_ABC", label = "Champ :", c("France métropolitaine"),"France métropolitaine")}
  }
})

output$mcal_ABC = renderUI(selectInput("Cal_ABC", label = "Calculs :", c("sur 1 mois","sur 3 mois","sur 1 an"),"sur 1 mois"))

output$mmot_ABC = renderUI(selectInput("Mot_ABC", label = "Selon l'ancienneté :", c("Ensemble"
                                                                                    ,"moins de 3 mois"
                                                                                    ,"3 mois et moins 6 mois"
                                                                                    ,"6 mois et moins 12 mois"
                                                                                    ,"moins d'un an"
                                                                                    ,"1 an et moins de 2 ans"
                                                                                    ,"2 ans et moins de 3 ans"
                                                                                    ,"3 ans ou plus"
                                                                                    ,"un an ou plus")
                                       ,"Ensemble"))

output$msex_ABC = renderUI({
  if(!is.null(input$Mot_ABC)){
    if(input$Mot_ABC=="Ensemble"){selectInput("Sex_ABC", label = "Sexe :", c("Ensemble","Hommes","Femmes"),"Ensemble")}
  }
})

output$mage_ABC = renderUI({
  if(!is.null(input$Mot_ABC)){
    if(input$Mot_ABC=="Ensemble" | input$Mot_ABC=="un an ou plus"){
      selectInput("Age_ABC", label = "Tranche d'âge :", c("Ensemble","moins de 25 ans","de 25 à 49 ans","50 ans ou plus"),"Ensemble")}
  }
})



##-- Maj titre, type de donnée et champ ----
output$title_ABC <- renderText({ 
  paste(ser_ABC$TG, " en ", mois_stat)
})

output$type_ABC <- renderText({ 
  if(input$mcvs_ABC==1){paste("Données corrigées des varaitions saisonnières et des jours ouvrables (CVS-CJO)")
  }else{paste("Données brutes")}
})

output$champ_ABC <- renderText({ 
  paste("Champ: ", input$Geo_ABC)
})



##-- Partie réactive ----
ser_ABC <- reactiveValues(cvs = NULL, geo = NULL, var = NULL, evo = NULL, sex = NULL, age = NULL, mot = NULL, graph = "eff",TG = NULL)

observe({
  ##-- Actualisation des filtres ----
  if(!is.null(input$mcvs_ABC)){if(input$mcvs_ABC==2){ser_ABC$cvs <- "N"}else{ser_ABC$cvs <- "O"}}
  if(!is.null(input$Geo_ABC)){if(input$Geo_ABC=="France métropolitaine"){ser_ABC$geo <- "Fm"}else{ser_ABC$geo <- "Fr"}}
  if(!is.null(input$Cal_ABC)){if(input$Cal_ABC=="sur 1 mois"){
    ser_ABC$var <- "V1m"
    ser_ABC$evo <- "E1m"
  }else if(input$Cal_ABC=="sur 3 mois"){
    ser_ABC$var <- "V3m"
    ser_ABC$evo <- "E3m"
  }else{
    ser_ABC$var <- "V1a"
    ser_ABC$evo <- "E1a"
  }}
  
  if(!is.null(input$Mot_ABC)){
    if(input$Mot_ABC!="Ensemble"){
      ser_ABC$geo <- "Fm"
      ser_ABC$sexe <- "E"
      if(input$Mot_ABC!="Un an ou plus"){
        ser_ABC$age <- "E"
      }
    }
  }
  
  if(!is.null(input$Sex_ABC)){if(input$Sex_ABC=="Hommes"){ser_ABC$sex <- "H"}else if(input$Sex_ABC=="Femmes"){ser_ABC$sex <- "F"}else{ser_ABC$sex <- "E"}}
  if(!is.null(input$Age_ABC)){if(input$Age_ABC=="moins de 25 ans"){ser_ABC$age <- "J"}
    else if(input$Age_ABC=="de 25 à 49 ans"){ser_ABC$age <- "M"}
    else if(input$Age_ABC=="50 ans ou plus"){ser_ABC$age <- "S"}
    else{ser_ABC$age <- "E"}}
  
  if(!is.null(ser_ABC$sex)){
    if(ser_ABC$mot!=""){
      if(ser_ABC$age == "E"){ser_ABC$TG <- paste0("Les inscrits depuis ", input$Mot_ABC)
      }else{ser_ABC$TG <- paste0("Les inscrits depuis ", input$Mot_ABC, " ayant ", input$Age_ABC)}
    }else{    
      if(ser_ABC$sex == "E" & ser_ABC$age == "E"){ser_ABC$TG <- "Les inscrits "}
      else if(ser_ABC$sex != "E" & ser_ABC$age == "E"){ser_ABC$TG <- input$Sex_ABC}
      else if(ser_ABC$sex == "E" & ser_ABC$age != "E"){ser_ABC$TG <- paste0("Les inscrits ayant ",input$Age_ABC)}
      else {ser_ABC$TG <- paste0(input$Sex_ABC, " ayant ",input$Age_ABC)}
    }}
  
  if(!is.null(input$Mot_ABC)){
    if(input$Mot_ABC=="moins de 3 mois"){ser_ABC$mot <- "0M"
    }else if(input$Mot_ABC=="3 mois et moins 6 mois"){ser_ABC$mot <- "3M"
    }else if(input$Mot_ABC=="6 mois et moins 12 mois"){ser_ABC$mot <- "6M"
    }else if(input$Mot_ABC=="moins d'un an"){ser_ABC$mot <- "0A"
    }else if(input$Mot_ABC=="1 an et moins de 2 ans"){ser_ABC$mot <- "1A"
    }else if(input$Mot_ABC=="2 ans et moins de 3 ans"){ser_ABC$mot <- "2A"
    }else if(input$Mot_ABC=="3 ans ou plus"){ser_ABC$mot <- "3A"
    }else if(input$Mot_ABC=="un an ou plus"){ser_ABC$mot <- "P1A"
    }else{ser_ABC$mot <- ""
    }}
  
  
  ##-- Actualisation des données des infos box ----
  if(!is.null(input$mcvs_ABC)){
    ABC_eff <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                       & fp_defm$geo==ser_ABC$geo & fp_defm$cvs==ser_ABC$cvs & fp_defm$type=="Eff" 
                       & fp_defm$cat=="ABC" & fp_defm$sexe==ser_ABC$sex & fp_defm$age==ser_ABC$age & fp_defm$motif==ser_ABC$mot,8]
    infoABC_eff <- format(ABC_eff, big.mark = " ")
    
    ABC_var <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                       & fp_defm$geo==ser_ABC$geo & fp_defm$cvs==ser_ABC$cvs & fp_defm$type==ser_ABC$var 
                       & fp_defm$cat=="ABC" & fp_defm$sexe==ser_ABC$sex & fp_defm$age==ser_ABC$age & fp_defm$motif==ser_ABC$mot,8]
    infoABC_var <- format(ABC_var, big.mark = " ")
    
    ABC_evo <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                       & fp_defm$geo==ser_ABC$geo & fp_defm$cvs==ser_ABC$cvs & fp_defm$type==ser_ABC$evo 
                       & fp_defm$cat=="ABC" & fp_defm$sexe==ser_ABC$sex & fp_defm$age==ser_ABC$age & fp_defm$motif==ser_ABC$mot,8]
    infoABC_evo <- paste0(format(ABC_evo, decimal.mark = ",", digits = 2, nsmall = 1), " %")
    
    ABC_par <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                       & fp_defm$geo==ser_ABC$geo & fp_defm$cvs==ser_ABC$cvs & fp_defm$type=="Par" 
                       & fp_defm$cat=="ABC" & fp_defm$sexe==ser_ABC$sex & fp_defm$age==ser_ABC$age & fp_defm$motif==ser_ABC$mot,8]
    infoABC_par <- paste0(format(ABC_par, decimal.mark = ","), " %")
  }
  
  ##-- Création des infos box ----
  output$bABC_eff <- renderValueBox({
    bABCeff<-valueBox(value=tags$p("Effectif", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                      ,width=NULL
                      ,icon = icon("users",lib="font-awesome")
                      ,color="navy"
                      ,href="#"
                      ,subtitle=tags$p(infoABC_eff, style = "font-size: 28px;text-align: center;")
    )
    bABCeff$children[[1]]$attribs$class<-"action-button"
    bABCeff$children[[1]]$attribs$id<-"button_ABCeff"
    return(bABCeff)
  })
  
  output$bABC_var <- renderValueBox({
    bABCvar<-valueBox(value=tags$p("Variation ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                      ,width=NULL
                      ,icon = icon("chart-bar")
                      ,color="navy"
                      ,href="#"
                      ,subtitle=tags$p(paste(infoABC_var, input$Cal_ABC), style = "font-size: 24px;text-align: center;")
    )
    bABCvar$children[[1]]$attribs$class<-"action-button"
    bABCvar$children[[1]]$attribs$id<-"button_ABCvar"
    return(bABCvar)
  })
  
  output$bABC_evo <- renderValueBox({
    bABCevo<-valueBox(value=tags$p("évolution ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                      ,width=NULL
                      ,icon = icon("chart-line")
                      ,color="navy"
                      ,href="#"
                      ,subtitle=tags$p(paste(infoABC_evo, input$Cal_ABC), style = "font-size: 24px;text-align: center;")
    )
    bABCevo$children[[1]]$attribs$class<-"action-button"
    bABCevo$children[[1]]$attribs$id<-"button_ABCevo"
    return(bABCevo)
  })
  
  observeEvent(input$Mot_ABC,{
    if(input$Mot_ABC!="Ensemble"){   
      output$bABC_par <- renderValueBox({
        
        bABCpar<-valueBox(value=tags$p("Part parmi les ABC", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                          ,width=NULL
                          ,icon = icon("chart-bar")
                          ,color="navy"
                          ,href="#"
                          ,subtitle=tags$p(infoABC_par, style = "font-size: 24px;text-align: center;")
        )
        
        bABCpar$children[[1]]$attribs$class<-"action-button"
        bABCpar$children[[1]]$attribs$id<-"button_ABCpar"
        return(bABCpar)
        
      })
    }else{
      output$bABC_par <- NULL}
  })
  
  
  ##-- Actualisation des données ----
  if(!is.null(input$mcvs_ABC)){
    dseff <- fp_defm[fp_defm$geo==ser_ABC$geo & fp_defm$cvs==ser_ABC$cvs & fp_defm$type=="Eff" & fp_defm$cat=="ABC" 
                     & fp_defm$sexe==ser_ABC$sex & fp_defm$age==ser_ABC$age & fp_defm$motif==ser_ABC$mot,]
    
    dsvar <- fp_defm[fp_defm$geo==ser_ABC$geo & fp_defm$cvs==ser_ABC$cvs & fp_defm$type==ser_ABC$var & fp_defm$cat=="ABC" 
                     & fp_defm$sexe==ser_ABC$sex & fp_defm$age==ser_ABC$age & fp_defm$motif==ser_ABC$mot,] %>% filter(!is.na(valeur))
    
    dsevo <- fp_defm[fp_defm$geo==ser_ABC$geo & fp_defm$cvs==ser_ABC$cvs & fp_defm$type==ser_ABC$evo & fp_defm$cat=="ABC" 
                     & fp_defm$sexe==ser_ABC$sex & fp_defm$age==ser_ABC$age & fp_defm$motif==ser_ABC$mot,] %>% filter(!is.na(valeur))
    
    dspar <- fp_defm[fp_defm$geo==ser_ABC$geo & fp_defm$cvs==ser_ABC$cvs & fp_defm$type=="Par" & fp_defm$cat=="ABC" 
                     & fp_defm$sexe==ser_ABC$sex & fp_defm$age==ser_ABC$age & fp_defm$motif==ser_ABC$mot,] %>% filter(!is.na(valeur))
  }
  
  
  ##-- Choix du graphique ----
  observeEvent(input$button_ABCeff,{
    ser_ABC$graph <- "eff"
  })
  
  observeEvent(input$button_ABCvar,{
    ser_ABC$graph <- "var"
  })
  
  observeEvent(input$button_ABCevo,{
    ser_ABC$graph <- "evo"
  })
  
  observeEvent(input$button_ABCpar,{
    ser_ABC$graph <- "par"
  })
  
  if(!is.null(input$Mot_ABC)){
    if(input$Mot_ABC=="Ensemble" & ser_ABC$graph == "par"){
      ser_ABC$graph <- "eff"
    }}
  
  
  ##-- Actualisation du graphique ----
  if(ser_ABC$graph=="eff"){ 
    output$Graph_defmABC <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("navy"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_ABC$graph=="var"){ 
    output$Graph_defmABC <- renderPlotly({
      plot <- plot_ly(dsvar, x = ~mois_stat, y = ~dsvar$valeur, 
                      type = "bar", color = I("navy"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_ABC$graph=="evo"){ 
    output$Graph_defmABC <- renderPlotly({
      plot <- plot_ly(dsevo, x = ~mois_stat, y = ~dsevo$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines+markers', color = I("navy"))
      plot %>%  
        layout(
          xaxis = list(title = "", range = c(paste0(ms_3_An, "-04-01"), paste0(ms_An,"-04-15"))),
          yaxis = list(title = "")
        )
    })
  }else if(ser_ABC$graph=="par"){ 
    output$Graph_defmABC <- renderPlotly({
      plot <- plot_ly(dspar, x = ~mois_stat, y = ~dspar$valeur, name = 'série CSV-CJO', 
                      type = 'bar', color = I("navy"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else{ 
    output$Graph_defmABC <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("navy"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }
  
})
