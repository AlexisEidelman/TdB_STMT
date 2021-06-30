##-- menus ----
output$mgeo_E = renderUI(selectInput("Geo_E", label = "Champ :", c("France métropolitaine", "France (hors Mayotte)"), "France métropolitaine"))
output$mcal_E = renderUI(selectInput("Cal_E", label = "Calculs :", c("sur 1 mois","sur 3 mois","sur 1 an"),"sur 1 mois"))
output$mmot_E = renderUI(selectInput("Mot_E", label = "Motif :", c("Ensemble"
                                                                   ,"Contrat aidé marchand"
                                                                   ,"Contrat aidé non marchand"
                                                                   ,"Créateur d'entreprise"
                                                                   ,"Autre cas")
                                     ,"Ensemble"))

output$msex_E = renderUI({
  if(!is.null(input$Mot_E)){
    if(input$Mot_E=="Ensemble" & input$Geo_E=="France métropolitaine"){
      selectInput("Sex_E", label = "Sexe :", c("Ensemble","Hommes","Femmes"),"Ensemble")}
  }
})

output$mage_E = renderUI({
  if(!is.null(input$Mot_E)){
    if(input$Mot_E=="Ensemble" & input$Geo_E=="France métropolitaine"){
      selectInput("Age_E", label = "Tranche d'âge :", c("Ensemble","moins de 25 ans","de 25 à 49 ans","50 ans ou plus"),"Ensemble")}
  }
})

output$title_E <- renderText({ 
  paste(ser_E$TG, " en ", mois_stat)
})

output$type_E <- renderText({ 
  if(input$mcvs_E==1){paste("Données corrigées des varaitions saisonnières et des jours ouvrables (CVS-CJO)")
  }else{paste("Données brutes")}
})

output$champ_E <- renderText({ 
  paste("Champ: ", input$Geo_E)
})

##-- Atualisation ----
ser_E <- reactiveValues(cvs = NULL, geo = NULL, var = NULL, evo = NULL, sex = NULL, age = NULL, mot = NULL, graph = "eff",TG = NULL)

observe({
  if(!is.null(input$mcvs_E)){if(input$mcvs_E==2){ser_E$cvs <- "N"}else{ser_E$cvs <- "O"}}
  
  if(!is.null(input$Geo_E)){if(input$Geo_E=="France métropolitaine"){ser_E$geo <- "Fm"}else{ser_E$geo <- "Fr"}}
  
  if(!is.null(input$Cal_E)){if(input$Cal_E=="sur 1 mois"){
    ser_E$var <- "V1m"
    ser_E$evo <- "E1m"
  }else if(input$Cal_E=="sur 3 mois"){
    ser_E$var <- "V3m"
    ser_E$evo <- "E3m"
  }else{
    ser_E$var <- "V1a"
    ser_E$evo <- "E1a"
  }}
  
  
  if(!is.null(input$Sex_E)){
    if(ser_E$geo=="Fm"){
      if(input$Sex_E=="Hommes"){ser_E$sex <- "H"
      }else if(input$Sex_E=="Femmes"){ser_E$sex <- "F"
      }else{ser_E$sex <- "E"}
    }else{ser_E$sex <- "E"}
  }
  
  if(!is.null(input$Age_E)){
    if(ser_E$geo=="Fm"){
      if(input$Age_E=="moins de 25 ans"){ser_E$age <- "J"
      }else if(input$Age_E=="de 25 à 49 ans"){ser_E$age <- "M"
      }else if(input$Age_E=="50 ans ou plus"){ser_E$age <- "S"
      }else{ser_E$age <- "E"}
    }else{ser_E$age <- "E"}
  }
  
  if(!is.null(ser_E$sex)){
    if(input$Mot_E=="Ensemble"){
      if(ser_E$sex == "E" & ser_E$age == "E"){ser_E$TG <- "Les inscrits"
      }else if(ser_E$sex != "E" & ser_E$age == "E"){ser_E$TG <- input$Sex_E
      }else if(ser_E$sex == "E" & ser_E$age != "E"){ser_E$TG <- paste0("Les inscrits ayant ",input$Age_E)
      }else {ser_E$TG <- paste0(input$Sex_E, " ayant ",input$Age_E)}
    }else{
      if(ser_E$sex == "E" & ser_E$age == "E"){ser_E$TG <- paste0("Les inscrits en ", input$Mot_E)
      }else if(ser_E$sex != "E" & ser_E$age == "E"){ser_E$TG <- paste0(input$Sex_E," en ", input$Mot_E)
      }else if(ser_E$sex == "E" & ser_E$age != "E"){ser_E$TG <- paste0("Les inscrits en ",  input$Mot_E," ayant ",input$Age_E)
      }else {ser_E$TG <- paste0(input$Sex_E, " en ", input$Mot_E," et ayant ",input$Age_E)}
    }
  }
  
  if(!is.null(input$Mot_E)){
    if(input$Mot_E=="Contrat aidé marchand"){ser_E$mot <- "MARCH"
    }else if(input$Mot_E=="Contrat aidé non marchand"){ser_E$mot <- "NON_MARCH"
    }else if(input$Mot_E=="Créateur d'entreprise"){ser_E$mot <- "ENT"
    }else if(input$Mot_E=="Autre cas"){ser_E$mot <- "AUTRES"
    }else{ser_E$mot <- ""
    }}
  
  
  if(!is.null(input$mcvs_E)){
    E_eff <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_E$geo & fp_defm$cvs==ser_E$cvs & fp_defm$type=="Eff" 
                     & fp_defm$cat=="E" & fp_defm$sexe==ser_E$sex & fp_defm$age==ser_E$age & fp_defm$motif==ser_E$mot, 8]
    infoE_eff <- format(E_eff, big.mark = " ")
    
    E_var <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_E$geo & fp_defm$cvs==ser_E$cvs & fp_defm$type==ser_E$var 
                     & fp_defm$cat=="E" & fp_defm$sexe==ser_E$sex & fp_defm$age==ser_E$age & fp_defm$motif==ser_E$mot, 8]
    infoE_var <- format(E_var, big.mark = " ")
    
    E_evo <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_E$geo & fp_defm$cvs==ser_E$cvs & fp_defm$type==ser_E$evo 
                     & fp_defm$cat=="E" & fp_defm$sexe==ser_E$sex & fp_defm$age==ser_E$age & fp_defm$motif==ser_E$mot, 8]
    infoE_evo <- paste0(format(E_evo, decimal.mark = ",", digits = 2, nsmall = 1), " %")
  }
  
  output$bE_eff <- renderValueBox({
    bEeff<-valueBox(value=tags$p("Effectif", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("users",lib="font-awesome")
                    ,color="aqua"
                    ,href="#"
                    ,subtitle=tags$p(infoE_eff, style = "font-size: 28px;text-align: center;")
    )
    bEeff$children[[1]]$attribs$class<-"action-button"
    bEeff$children[[1]]$attribs$id<-"button_Eeff"
    return(bEeff)
  })
  
  output$bE_var <- renderValueBox({
    bEvar<-valueBox(value=tags$p("Variation ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("chart-bar")
                    ,color="aqua"
                    ,href="#"
                    ,subtitle=tags$p(paste(infoE_var, input$Cal_E), style = "font-size: 24px;text-align: center;")
    )
    bEvar$children[[1]]$attribs$class<-"action-button"
    bEvar$children[[1]]$attribs$id<-"button_Evar"
    return(bEvar)
  })
  
  output$bE_evo <- renderValueBox({
    bEevo<-valueBox(value=tags$p("évolution ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("chart-line")
                    ,color="aqua"
                    ,href="#"
                    ,subtitle=tags$p(paste(infoE_evo, input$Cal_E), style = "font-size: 24px;text-align: center;")
    )
    bEevo$children[[1]]$attribs$class<-"action-button"
    bEevo$children[[1]]$attribs$id<-"button_Eevo"
    return(bEevo)
  })
  
  observeEvent(input$button_Eeff,{
    ser_E$graph <- "eff"
  })
  
  observeEvent(input$button_Evar,{
    ser_E$graph <- "var"
  })
  
  observeEvent(input$button_Eevo,{
    ser_E$graph <- "evo"
  })
  
  if(!is.null(input$mcvs_E)){
    dseff <- fp_defm[fp_defm$geo==ser_E$geo & fp_defm$cvs==ser_E$cvs & fp_defm$type=="Eff" & fp_defm$cat=="E" 
                     & fp_defm$sexe==ser_E$sex & fp_defm$age==ser_E$age & fp_defm$motif==ser_E$mot,]
    
  }else{dseff <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="Eff" & fp_defm$cat=="E" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",]
  }
  
  if(!is.null(input$mcvs_E)){
    dsvar <- fp_defm[fp_defm$geo==ser_E$geo & fp_defm$cvs==ser_E$cvs & fp_defm$type==ser_E$var & fp_defm$cat=="E" 
                     & fp_defm$sexe==ser_E$sex & fp_defm$age==ser_E$age & fp_defm$motif==ser_E$mot,] %>% filter(!is.na(valeur))
    
  }else{dsvar <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="V1m" & fp_defm$cat=="E" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",] %>% filter(!is.na(valeur))
  } 
  
  if(!is.null(input$mcvs_E)){
    dsevo <- fp_defm[fp_defm$geo==ser_E$geo & fp_defm$cvs==ser_E$cvs & fp_defm$type==ser_E$evo & fp_defm$cat=="E" 
                     & fp_defm$sexe==ser_E$sex & fp_defm$age==ser_E$age & fp_defm$motif==ser_E$mot,] %>% filter(!is.na(valeur))
    
  }else{dsevo <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="E1m" & fp_defm$cat=="E" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",] %>% filter(!is.na(valeur))
  } 
  
  if(ser_E$graph=="eff"){ 
    output$Graph_defmE <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_E$graph=="var"){ 
    output$Graph_defmE <- renderPlotly({
      plot <- plot_ly(dsvar, x = ~mois_stat, y = ~dsvar$valeur, 
                      type = "bar", color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_E$graph=="evo"){ 
    output$Graph_defmE <- renderPlotly({
      plot <- plot_ly(dsevo, x = ~mois_stat, y = ~dsevo$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines+markers', color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = "", range = c(paste0(ms_3_An, "-04-01"), paste0(ms_An,"-04-15"))),
          yaxis = list(title = "")
        )
    })
  }else{ 
    output$Graph_defmE <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }
  
})