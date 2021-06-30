##-- menus ----
output$mgeo_B = renderUI({
  if(!is.null(input$Mot_B)){
    if(input$Mot_B=="Moins de 79 heures"){
      selectInput("Geo_B", label = "Champ :", c("France métropolitaine","France (hors Mayotte)"),"France métropolitaine")
    }else{selectInput("Geo_B", label = "Champ :", c("France métropolitaine"),"France métropolitaine")}
  }
})

output$mcal_B = renderUI(selectInput("Cal_B", label = "Calculs :", c("sur 1 mois","sur 3 mois","sur 1 an"),"sur 1 mois"))

output$mmot_B = renderUI(selectInput("Mot_B", label = "Tranche d'heures d'activité :", c("Moins de 79 heures"
                                                                                         ,"Moins de 20 heures"
                                                                                         ,"De 20 à 39 heures"
                                                                                         ,"De 40 à 59 heures"
                                                                                         ,"De 60 à 78 heures")
                                     ,"Moins de 79 heures"))

output$msex_B = renderUI({
  if(!is.null(input$Mot_B)){
    if(input$Mot_B=="Moins de 79 heures"){selectInput("Sex_B", label = "Sexe :", c("Ensemble","Hommes","Femmes"),"Ensemble")}
  }
})

output$mage_B = renderUI(selectInput("Age_B", label = "Tranche d'âge :", c("Ensemble","moins de 25 ans","de 25 à 49 ans","50 ans ou plus"),"Ensemble"))

output$title_B <- renderText({ 
  paste(ser_B$TG, " en ", mois_stat)
})

output$type_B <- renderText({ 
  if(input$mcvs_B==1){paste("Données corrigées des varaitions saisonnières et des jours ouvrables (CVS-CJO)")
  }else{paste("Données brutes")}
})

output$champ_B <- renderText({ 
  paste("Champ: ", input$Geo_B)
})

##-- Atualisation ----
ser_B <- reactiveValues(cvs = NULL, geo = NULL, var = NULL, evo = NULL, sex = NULL, mot = NULL, graph = "eff",TG = NULL)

observe({
  if(!is.null(input$mcvs_B)){if(input$mcvs_B==2){ser_B$cvs <- "N"}else{ser_B$cvs <- "O"}}
  
  if(!is.null(input$Geo_B)){if(input$Geo_B=="France métropolitaine"){ser_B$geo <- "Fm"}else{ser_B$geo <- "Fr"}}
  
  if(!is.null(input$Cal_B)){if(input$Cal_B=="sur 1 mois"){
    ser_B$var <- "V1m"
    ser_B$evo <- "E1m"
  }else if(input$Cal_B=="sur 3 mois"){
    ser_B$var <- "V3m"
    ser_B$evo <- "E3m"
  }else{
    ser_B$var <- "V1a"
    ser_B$evo <- "E1a"
  }}
  
  if(!is.null(input$Mot_B)){
    if(input$Mot_B!="Moins de 79 heures"){
      ser_B$geo <- "Fm"
      ser_B$sexe <- "E"
    }
  }
  
  if(!is.null(input$Sex_B)){if(input$Sex_B=="Hommes"){ser_B$sex <- "H"}else if(input$Sex_B=="Femmes"){ser_B$sex <- "F"}else{ser_B$sex <- "E"}}
  if(!is.null(input$Age_B)){if(input$Age_B=="moins de 25 ans"){ser_B$age <- "J"}
    else if(input$Age_B=="de 25 à 49 ans"){ser_B$age <- "M"}
    else if(input$Age_B=="50 ans ou plus"){ser_B$age <- "S"}
    else{ser_B$age <- "E"}}
  
  if(!is.null(ser_B$sex)){if(ser_B$sex == "E" & ser_B$age == "E"){ser_B$TG <- "Les inscrits"}
    else if(ser_B$sex != "E" & ser_B$age == "E"){ser_B$TG <- input$Sex_B}
    else if(ser_B$sex == "E" & ser_B$age != "E"){ser_B$TG <- paste0("Les inscrits ayant ",input$Age_B)}
    else {ser_B$TG <- paste0(input$Sex_B, " ayant ",input$Age_B)}
  }
  
  if(!is.null(input$Mot_B)){
    if(input$Mot_B=="Moins de 20 heures"){ser_B$mot <- "HEURES_1"
    }else if(input$Mot_B=="De 20 à 39 heures"){ser_B$mot <- "HEURES_2"
    }else if(input$Mot_B=="De 40 à 59 heures"){ser_B$mot <- "HEURES_3"
    }else if(input$Mot_B=="De 60 à 78 heures"){ser_B$mot <- "HEURES_4"
    }else{ser_B$mot <- ""
    }}
  
  
  if(!is.null(input$mcvs_B)){
    B_eff <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_B$geo & fp_defm$cvs==ser_B$cvs & fp_defm$type=="Eff" 
                     & fp_defm$cat=="B" & fp_defm$sexe==ser_B$sex & fp_defm$age==ser_B$age & fp_defm$motif==ser_B$mot, 8]
    infoB_eff <- format(B_eff, big.mark = " ")
    
    B_var <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_B$geo & fp_defm$cvs==ser_B$cvs & fp_defm$type==ser_B$var 
                     & fp_defm$cat=="B" & fp_defm$sexe==ser_B$sex & fp_defm$age==ser_B$age & fp_defm$motif==ser_B$mot, 8]
    infoB_var <- format(B_var, big.mark = " ")
    
    B_evo <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_B$geo & fp_defm$cvs==ser_B$cvs & fp_defm$type==ser_B$evo 
                     & fp_defm$cat=="B" & fp_defm$sexe==ser_B$sex & fp_defm$age==ser_B$age & fp_defm$motif==ser_B$mot, 8]
    infoB_evo <- paste0(format(B_evo, decimal.mark = ",", digits = 2, nsmall = 1), " %")
  }
  
  output$bB_eff <- renderValueBox({
    bBeff<-valueBox(value=tags$p("Effectif", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("users",lib="font-awesome")
                    ,color="blue"
                    ,href="#"
                    ,subtitle=tags$p(infoB_eff, style = "font-size: 28px;text-align: center;")
    )
    bBeff$children[[1]]$attribs$class<-"action-button"
    bBeff$children[[1]]$attribs$id<-"button_Beff"
    return(bBeff)
  })
  
  output$bB_var <- renderValueBox({
    bBvar<-valueBox(value=tags$p("Variation ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("chart-bar")
                    ,color="blue"
                    ,href="#"
                    ,subtitle=tags$p(paste(infoB_var, input$Cal_B), style = "font-size: 24px;text-align: center;")
    )
    bBvar$children[[1]]$attribs$class<-"action-button"
    bBvar$children[[1]]$attribs$id<-"button_Bvar"
    return(bBvar)
  })
  
  output$bB_evo <- renderValueBox({
    bBevo<-valueBox(value=tags$p("évolution ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("chart-line")
                    ,color="blue"
                    ,href="#"
                    ,subtitle=tags$p(paste(infoB_evo, input$Cal_B), style = "font-size: 24px;text-align: center;")
    )
    bBevo$children[[1]]$attribs$class<-"action-button"
    bBevo$children[[1]]$attribs$id<-"button_Bevo"
    return(bBevo)
  })
  
  observeEvent(input$button_Beff,{
    ser_B$graph <- "eff"
  })
  
  observeEvent(input$button_Bvar,{
    ser_B$graph <- "var"
  })
  
  observeEvent(input$button_Bevo,{
    ser_B$graph <- "evo"
  })
  
  if(!is.null(input$mcvs_B)){
    dseff <- fp_defm[fp_defm$geo==ser_B$geo & fp_defm$cvs==ser_B$cvs & fp_defm$type=="Eff" & fp_defm$cat=="B" 
                     & fp_defm$sexe==ser_B$sex & fp_defm$age==ser_B$age & fp_defm$motif==ser_B$mot,]
    
  }else{dseff <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="Eff" & fp_defm$cat=="B" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",]
  }
  
  if(!is.null(input$mcvs_B)){
    dsvar <- fp_defm[fp_defm$geo==ser_B$geo & fp_defm$cvs==ser_B$cvs & fp_defm$type==ser_B$var & fp_defm$cat=="B" 
                     & fp_defm$sexe==ser_B$sex & fp_defm$age==ser_B$age & fp_defm$motif==ser_B$mot,] %>% filter(!is.na(valeur))
    
  }else{dsvar <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="V1m" & fp_defm$cat=="B" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",] %>% filter(!is.na(valeur))
  } 
  
  if(!is.null(input$mcvs_B)){
    dsevo <- fp_defm[fp_defm$geo==ser_B$geo & fp_defm$cvs==ser_B$cvs & fp_defm$type==ser_B$evo & fp_defm$cat=="B" 
                     & fp_defm$sexe==ser_B$sex & fp_defm$age==ser_B$age & fp_defm$motif==ser_B$mot,] %>% filter(!is.na(valeur))
    
  }else{dsevo <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="E1m" & fp_defm$cat=="B" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif=="",] %>% filter(!is.na(valeur))
  } 
  
  if(ser_B$graph=="eff"){ 
    output$Graph_defmB <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_B$graph=="var"){ 
    output$Graph_defmB <- renderPlotly({
      plot <- plot_ly(dsvar, x = ~mois_stat, y = ~dsvar$valeur, 
                      type = "bar", color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_B$graph=="evo"){ 
    output$Graph_defmB <- renderPlotly({
      plot <- plot_ly(dsevo, x = ~mois_stat, y = ~dsevo$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines+markers', color = I("blue"))
      plot %>%  
        layout(
          xaxis = list(title = "", range = c(paste0(ms_3_An, "-04-01"), paste0(ms_An,"-04-15"))),
          yaxis = list(title = "")
        )
    })
  }else{ 
    output$Graph_defmB <- renderPlotly({
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