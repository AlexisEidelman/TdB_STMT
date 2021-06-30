##-- menus ----
output$mgeo_C = renderUI({
  if(!is.null(input$Mot_C)){
    if(input$Mot_C=="79 heures ou plus"){
      selectInput("Geo_C", label = "Champ :", c("France métropolitaine","France (hors Mayotte)"),"France métropolitaine")
    }else{selectInput("Geo_C", label = "Champ :", c("France métropolitaine"),"France métropolitaine")}
  }
})

output$mcal_C = renderUI(selectInput("Cal_C", label = "Calculs :", c("sur 1 mois","sur 3 mois","sur 1 an"),"sur 1 mois"))

output$mmot_C = renderUI(selectInput("Mot_C", label = "Tranche d'heures d'activité :", c("79 heures ou plus"
                                                                                         ,"De 79 à 99 heures"
                                                                                         ,"De 100 à 119 heures"
                                                                                         ,"De 120 à 150 heures"
                                                                                         ,"151 heures ou plus")
                                     ,"79 heures ou plus"))


output$msex_C = renderUI({
  if(!is.null(input$Mot_C)){
    if(input$Mot_C=="79 heures ou plus"){selectInput("Sex_C", label = "Sexe :", c("Ensemble","Hommes","Femmes"),"Ensemble")}
  }
})

output$mage_C = renderUI(selectInput("Age_C", label = "Tranche d'âge :", c("Ensemble","moins de 25 ans","de 25 à 49 ans","50 ans ou plus"),"Ensemble"))

output$title_C <- renderText({ 
  paste(ser_C$TG, " en ", mois_stat)
})

output$type_C <- renderText({ 
  if(input$mcvs_C==1){paste("Données corrigées des varaitions saisonnières et des jours ouvrables (CVS-CJO)")
  }else{paste("Données brutes")}
})

output$champ_C <- renderText({ 
  paste("Champ: ", input$Geo_C)
})

##-- Atualisation ----
ser_C <- reactiveValues(cvs = NULL, geo = NULL, var = NULL, evo = NULL, sex = NULL, mot = NULL, graph = "eff",TG = NULL)

observe({
  if(!is.null(input$mcvs_C)){if(input$mcvs_C==2){ser_C$cvs <- "N"}else{ser_C$cvs <- "O"}}
  if(!is.null(input$Geo_C)){if(input$Geo_C=="France métropolitaine"){ser_C$geo <- "Fm"}else{ser_C$geo <- "Fr"}}
  if(!is.null(input$Cal_C)){if(input$Cal_C=="sur 1 mois"){
    ser_C$var <- "V1m"
    ser_C$evo <- "E1m"
  }else if(input$Cal_C=="sur 3 mois"){
    ser_C$var <- "V3m"
    ser_C$evo <- "E3m"
  }else{
    ser_C$var <- "V1a"
    ser_C$evo <- "E1a"
  }}
  
  if(!is.null(input$Mot_C)){
    if(input$Mot_C!="79 heures ou plus"){
      ser_C$geo <- "Fm"
      ser_C$sexe <- "E"
    }
  }
  
  if(!is.null(input$Sex_C)){if(input$Sex_C=="Hommes"){ser_C$sex <- "H"}else if(input$Sex_C=="Femmes"){ser_C$sex <- "F"}else{ser_C$sex <- "E"}}
  if(!is.null(input$Age_C)){if(input$Age_C=="moins de 25 ans"){ser_C$age <- "J"}
    else if(input$Age_C=="de 25 à 49 ans"){ser_C$age <- "M"}
    else if(input$Age_C=="50 ans ou plus"){ser_C$age <- "S"}
    else{ser_C$age <- "E"}}
  
  if(!is.null(ser_C$sex)){if(ser_C$sex == "E" & ser_C$age == "E"){ser_C$TG <- "Les inscrits"}
    else if(ser_C$sex != "E" & ser_C$age == "E"){ser_C$TG <- input$Sex_C}
    else if(ser_C$sex == "E" & ser_C$age != "E"){ser_C$TG <- paste0("Les inscrits ayant ",input$Age_C)}
    else {ser_C$TG <- paste0(input$Sex_C, " ayant ",input$Age_C)}
  }
  
  if(!is.null(input$Mot_C)){
    if(input$Mot_C=="De 79 à 99 heures"){ser_C$mot <- "HEURES_1"
    }else if(input$Mot_C=="De 100 à 119 heures"){ser_C$mot <- "HEURES_2"
    }else if(input$Mot_C=="De 120 à 150 heures"){ser_C$mot <- "HEURES_3"
    }else if(input$Mot_C=="151 heures ou plus"){ser_C$mot <- "HEURES_4"
    }else{ser_C$mot <- ""
    }}
  
  
  if(!is.null(input$mcvs_C)){
    C_eff <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_C$geo & fp_defm$cvs==ser_C$cvs & fp_defm$type=="Eff" 
                     & fp_defm$cat=="C" & fp_defm$sexe==ser_C$sex & fp_defm$age==ser_C$age & fp_defm$motif==ser_C$mot,8]
    infoC_eff <- format(C_eff, big.mark = " ")
    
    C_var <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_C$geo & fp_defm$cvs==ser_C$cvs & fp_defm$type==ser_C$var 
                     & fp_defm$cat=="C" & fp_defm$sexe==ser_C$sex & fp_defm$age==ser_C$age & fp_defm$motif==ser_C$mot,8]
    infoC_var <- format(C_var, big.mark = " ")
    
    C_evo <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_C$geo & fp_defm$cvs==ser_C$cvs & fp_defm$type==ser_C$evo 
                     & fp_defm$cat=="C" & fp_defm$sexe==ser_C$sex & fp_defm$age==ser_C$age & fp_defm$motif==ser_C$mot,8]
    infoC_evo <- paste0(format(C_evo, decimal.mark = ",", digits = 2, nsmall = 1), " %")
  }
  
  output$bC_eff <- renderValueBox({
    bCeff<-valueBox(value=tags$p("Effectif", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("users",lib="font-awesome")
                    ,color="aqua"
                    ,href="#"
                    ,subtitle=tags$p(infoC_eff, style = "font-size: 28px;text-align: center;")
    )
    bCeff$children[[1]]$attribs$class<-"action-button"
    bCeff$children[[1]]$attribs$id<-"button_Ceff"
    return(bCeff)
  })
  
  output$bC_var <- renderValueBox({
    bCvar<-valueBox(value=tags$p("Variation ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("chart-bar")
                    ,color="aqua"
                    ,href="#"
                    ,subtitle=tags$p(paste(infoC_var, input$Cal_C), style = "font-size: 24px;text-align: center;")
    )
    bCvar$children[[1]]$attribs$class<-"action-button"
    bCvar$children[[1]]$attribs$id<-"button_Cvar"
    return(bCvar)
  })
  
  output$bC_evo <- renderValueBox({
    bCevo<-valueBox(value=tags$p("évolution ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("chart-line")
                    ,color="aqua"
                    ,href="#"
                    ,subtitle=tags$p(paste(infoC_evo, input$Cal_C), style = "font-size: 24px;text-align: center;")
    )
    bCevo$children[[1]]$attribs$class<-"action-button"
    bCevo$children[[1]]$attribs$id<-"button_Cevo"
    return(bCevo)
  })
  
  observeEvent(input$button_Ceff,{
    ser_C$graph <- "eff"
  })
  
  observeEvent(input$button_Cvar,{
    ser_C$graph <- "var"
  })
  
  observeEvent(input$button_Cevo,{
    ser_C$graph <- "evo"
  })
  
  if(!is.null(input$mcvs_C)){
    dseff <- fp_defm[fp_defm$geo==ser_C$geo & fp_defm$cvs==ser_C$cvs & fp_defm$type=="Eff" & fp_defm$cat=="C" 
                     & fp_defm$sexe==ser_C$sex & fp_defm$age==ser_C$age & fp_defm$motif==ser_C$mot,]
    
  }else{dseff <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="Eff" & fp_defm$cat=="C" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",]
  }
  
  if(!is.null(input$mcvs_C)){
    dsvar <- fp_defm[fp_defm$geo==ser_C$geo & fp_defm$cvs==ser_C$cvs & fp_defm$type==ser_C$var & fp_defm$cat=="C" 
                     & fp_defm$sexe==ser_C$sex & fp_defm$age==ser_C$age & fp_defm$motif==ser_C$mot,] %>% filter(!is.na(valeur))
    
  }else{dsvar <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="V1m" & fp_defm$cat=="C" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",] %>% filter(!is.na(valeur))
  } 
  
  if(!is.null(input$mcvs_C)){
    dsevo <- fp_defm[fp_defm$geo==ser_C$geo & fp_defm$cvs==ser_C$cvs & fp_defm$type==ser_C$evo & fp_defm$cat=="C" 
                     & fp_defm$sexe==ser_C$sex & fp_defm$age==ser_C$age & fp_defm$motif==ser_C$mot,] %>% filter(!is.na(valeur))
    
  }else{dsevo <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="E1m" & fp_defm$cat=="C" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",] %>% filter(!is.na(valeur))
  } 
  
  if(ser_C$graph=="eff"){ 
    output$Graph_defmC <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_C$graph=="var"){ 
    output$Graph_defmC <- renderPlotly({
      plot <- plot_ly(dsvar, x = ~mois_stat, y = ~dsvar$valeur, 
                      type = "bar", color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_C$graph=="evo"){ 
    output$Graph_defmC <- renderPlotly({
      plot <- plot_ly(dsevo, x = ~mois_stat, y = ~dsevo$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines+markers', color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = "", range = c(paste0(ms_3_An, "-04-01"), paste0(ms_An,"-04-15"))),
          yaxis = list(title = "")
        )
    })
  }else{ 
    output$Graph_defmC <- renderPlotly({
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