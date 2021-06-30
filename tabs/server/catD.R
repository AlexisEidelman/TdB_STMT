##-- menus ----
output$mgeo_D = renderUI(selectInput("Geo_D", label = "Champ :", c("France métropolitaine", "France (hors Mayotte)"), "France métropolitaine"))
output$mcal_D = renderUI(selectInput("Cal_D", label = "Calculs :", c("sur 1 mois","sur 3 mois","sur 1 an"),"sur 1 mois"))
output$mmot_D = renderUI(selectInput("Mot_D", label = "Motif :", c("Ensemble"
                                                                   ,"CRP, CTP ou CSP"
                                                                   ,"formation ou autre cas")
                                     ,"Ensemble"))

output$msex_D = renderUI({
  if(!is.null(input$Geo_D)){
    if(input$Geo_D=="France métropolitaine"){selectInput("Sex_D", label = "Sexe :", c("Ensemble","Hommes","Femmes"),"Ensemble")}
  }
})

output$mage_D = renderUI({
  if(!is.null(input$Geo_D)){
    if(input$Geo_D=="France métropolitaine"){
      selectInput("Age_D", label = "Tranche d'âge :", c("Ensemble","moins de 25 ans","de 25 à 49 ans","50 ans ou plus"),"Ensemble")}
  }
})

output$title_D <- renderText({ 
  paste(ser_D$TG, " en ", mois_stat)
})

output$type_D <- renderText({ 
  if(input$mcvs_D==1){paste("Données corrigées des varaitions saisonnières et des jours ouvrables (CVS-CJO)")
  }else{paste("Données brutes")}
})

output$champ_D <- renderText({ 
  paste("Champ: ", input$Geo_D)
})

##-- Atualisation ----
ser_D <- reactiveValues(cvs = NULL, geo = NULL, var = NULL, evo = NULL, sex = NULL, age = NULL, mot = NULL, graph = "eff",TG = NULL)

observe({
  if(!is.null(input$mcvs_D)){if(input$mcvs_D==2){ser_D$cvs <- "N"}else{ser_D$cvs <- "O"}}
  
  if(!is.null(input$Geo_D)){if(input$Geo_D=="France métropolitaine"){ser_D$geo <- "Fm"}else{ser_D$geo <- "Fr"}}
  
  if(!is.null(input$Cal_D)){if(input$Cal_D=="sur 1 mois"){
    ser_D$var <- "V1m"
    ser_D$evo <- "E1m"
  }else if(input$Cal_D=="sur 3 mois"){
    ser_D$var <- "V3m"
    ser_D$evo <- "E3m"
  }else{
    ser_D$var <- "V1a"
    ser_D$evo <- "E1a"
  }}
  
  
  if(!is.null(input$Sex_D)){
    if(ser_D$geo=="Fm"){
      if(input$Sex_D=="Hommes"){ser_D$sex <- "H"
      }else if(input$Sex_D=="Femmes"){ser_D$sex <- "F"
      }else{ser_D$sex <- "E"}
    }else{ser_D$sex <- "E"}
  }
  
  if(!is.null(input$Age_D)){
    if(ser_D$geo=="Fm"){
      if(input$Age_D=="moins de 25 ans"){ser_D$age <- "J"
      }else if(input$Age_D=="de 25 à 49 ans"){ser_D$age <- "M"
      }else if(input$Age_D=="50 ans ou plus"){ser_D$age <- "S"
      }else{ser_D$age <- "E"}
    }else{ser_D$age <- "E"}
  }
  
  if(!is.null(ser_D$sex)){
    if(input$Mot_D=="Ensemble"){
      if(ser_D$sex == "E" & ser_D$age == "E"){ser_D$TG <- "Les inscrits"
      }else if(ser_D$sex != "E" & ser_D$age == "E"){ser_D$TG <- input$Sex_D
      }else if(ser_D$sex == "E" & ser_D$age != "E"){ser_D$TG <- paste0("Les inscrits ayant ",input$Age_D)
      }else {ser_D$TG <- paste0(input$Sex_D, " ayant ",input$Age_D)}
    }else{
      if(ser_D$sex == "E" & ser_D$age == "E"){ser_D$TG <- paste0("Les inscrits en ", input$Mot_D)
      }else if(ser_D$sex != "E" & ser_D$age == "E"){ser_D$TG <- paste0(input$Sex_D," en ", input$Mot_D)
      }else if(ser_D$sex == "E" & ser_D$age != "E"){ser_D$TG <- paste0("Les inscrits en ",  input$Mot_D," ayant ",input$Age_D)
      }else {ser_D$TG <- paste0(input$Sex_D, " en ", input$Mot_D," et ayant ",input$Age_D)}
    }
  }
  
  if(!is.null(input$Mot_D)){
    if(input$Mot_D=="CRP, CTP ou CSP"){ser_D$mot <- "CRPCTPCSP"
    }else if(input$Mot_D=="formation ou autre cas"){ser_D$mot <- "AUTRES"
    }else{ser_D$mot <- ""
    }}
  
  
  if(!is.null(input$mcvs_D)){
    D_eff <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_D$geo & fp_defm$cvs==ser_D$cvs & fp_defm$type=="Eff" 
                     & fp_defm$cat=="D" & fp_defm$sexe==ser_D$sex & fp_defm$age==ser_D$age & fp_defm$motif==ser_D$mot, 8]
    infoD_eff <- format(D_eff, big.mark = " ")
    
    D_var <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_D$geo & fp_defm$cvs==ser_D$cvs & fp_defm$type==ser_D$var 
                     & fp_defm$cat=="D" & fp_defm$sexe==ser_D$sex & fp_defm$age==ser_D$age & fp_defm$motif==ser_D$mot, 8]
    infoD_var <- format(D_var, big.mark = " ")
    
    D_evo <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_D$geo & fp_defm$cvs==ser_D$cvs & fp_defm$type==ser_D$evo 
                     & fp_defm$cat=="D" & fp_defm$sexe==ser_D$sex & fp_defm$age==ser_D$age & fp_defm$motif==ser_D$mot, 8]
    infoD_evo <- paste0(format(D_evo, decimal.mark = ",", digits = 2, nsmall = 1), " %")
  }
  
  output$bD_eff <- renderValueBox({
    bDeff<-valueBox(value=tags$p("Effectif", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("users",lib="font-awesome")
                    ,color="blue"
                    ,href="#"
                    ,subtitle=tags$p(infoD_eff, style = "font-size: 28px;text-align: center;")
    )
    bDeff$children[[1]]$attribs$class<-"action-button"
    bDeff$children[[1]]$attribs$id<-"button_Deff"
    return(bDeff)
  })
  
  output$bD_var <- renderValueBox({
    bDvar<-valueBox(value=tags$p("Variation ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("chart-bar")
                    ,color="blue"
                    ,href="#"
                    ,subtitle=tags$p(paste(infoD_var, input$Cal_D), style = "font-size: 24px;text-align: center;")
    )
    bDvar$children[[1]]$attribs$class<-"action-button"
    bDvar$children[[1]]$attribs$id<-"button_Dvar"
    return(bDvar)
  })
  
  output$bD_evo <- renderValueBox({
    bDevo<-valueBox(value=tags$p("évolution ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("chart-line")
                    ,color="blue"
                    ,href="#"
                    ,subtitle=tags$p(paste(infoD_evo, input$Cal_D), style = "font-size: 24px;text-align: center;")
    )
    bDevo$children[[1]]$attribs$class<-"action-button"
    bDevo$children[[1]]$attribs$id<-"button_Devo"
    return(bDevo)
  })
  
  observeEvent(input$button_Deff,{
    ser_D$graph <- "eff"
  })
  
  observeEvent(input$button_Dvar,{
    ser_D$graph <- "var"
  })
  
  observeEvent(input$button_Devo,{
    ser_D$graph <- "evo"
  })
  
  if(!is.null(input$mcvs_D)){
    dseff <- fp_defm[fp_defm$geo==ser_D$geo & fp_defm$cvs==ser_D$cvs & fp_defm$type=="Eff" & fp_defm$cat=="D" 
                     & fp_defm$sexe==ser_D$sex & fp_defm$age==ser_D$age & fp_defm$motif==ser_D$mot,]
    
  }else{dseff <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="Eff" & fp_defm$cat=="D" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",]
  }
  
  if(!is.null(input$mcvs_D)){
    dsvar <- fp_defm[fp_defm$geo==ser_D$geo & fp_defm$cvs==ser_D$cvs & fp_defm$type==ser_D$var & fp_defm$cat=="D" 
                     & fp_defm$sexe==ser_D$sex & fp_defm$age==ser_D$age & fp_defm$motif==ser_D$mot,] %>% filter(!is.na(valeur))
    
  }else{dsvar <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="V1m" & fp_defm$cat=="D" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",] %>% filter(!is.na(valeur))
  } 
  
  if(!is.null(input$mcvs_D)){
    dsevo <- fp_defm[fp_defm$geo==ser_D$geo & fp_defm$cvs==ser_D$cvs & fp_defm$type==ser_D$evo & fp_defm$cat=="D" 
                     & fp_defm$sexe==ser_D$sex & fp_defm$age==ser_D$age & fp_defm$motif==ser_D$mot,] %>% filter(!is.na(valeur))
    
  }else{dsevo <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="E1m" & fp_defm$cat=="D" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",] %>% filter(!is.na(valeur))
  } 
  
  if(ser_D$graph=="eff"){ 
    output$Graph_defmD <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_D$graph=="var"){ 
    output$Graph_defmD <- renderPlotly({
      plot <- plot_ly(dsvar, x = ~mois_stat, y = ~dsvar$valeur, 
                      type = "bar", color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_D$graph=="evo"){ 
    output$Graph_defmD <- renderPlotly({
      plot <- plot_ly(dsevo, x = ~mois_stat, y = ~dsevo$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines+markers', color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = "", range = c(paste0(ms_3_An, "-04-01"), paste0(ms_An,"-04-15"))),
          yaxis = list(title = "")
        )
    })
  }else{ 
    output$Graph_defmD <- renderPlotly({
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