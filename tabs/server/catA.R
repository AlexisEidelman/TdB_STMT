##-- menus ----
output$mgeo_A = renderUI(selectInput("Geo_A", label = "Champ :", c("France métropolitaine","France (hors Mayotte)"),"France métropolitaine"))
output$mcal_A = renderUI(selectInput("Cal_A", label = "Calculs :", c("sur 1 mois","sur 3 mois","sur 1 an"),"sur 1 mois"))
output$msex_A = renderUI(selectInput("Sex_A", label = "Sexe :", c("Ensemble","Hommes","Femmes"),"Ensemble"))
output$mage_A = renderUI(selectInput("Age_A", label = "Tranche d'âge :", c("Ensemble","moins de 25 ans","de 25 à 49 ans","50 ans ou plus"),"Ensemble"))

output$title_A <- renderText({ 
  paste(ser_A$TG, " en ", mois_stat)
})

output$type_A <- renderText({ 
  if(input$mcvs_A==1){paste("Données corrigées des varaitions saisonnières et des jours ouvrables (CVS-CJO)")
  }else{paste("Données brutes")}
})

output$champ_A <- renderText({ 
  paste("Champ: ", input$Geo_A)
})

##-- Atualisation ----
ser_A <- reactiveValues(cvs = NULL, geo = NULL, var = NULL, evo = NULL, sex = NULL, graph = "eff",TG = NULL)

observe({
  if(!is.null(input$mcvs_A)){if(input$mcvs_A==2){ser_A$cvs <- "N"}else{ser_A$cvs <- "O"}}
  if(!is.null(input$Geo_A)){if(input$Geo_A=="France métropolitaine"){ser_A$geo <- "Fm"}else{ser_A$geo <- "Fr"}}
  if(!is.null(input$Cal_A)){if(input$Cal_A=="sur 1 mois"){
    ser_A$var <- "V1m"
    ser_A$evo <- "E1m"
  }else if(input$Cal_A=="sur 3 mois"){
    ser_A$var <- "V3m"
    ser_A$evo <- "E3m"
  }else{
    ser_A$var <- "V1a"
    ser_A$evo <- "E1a"
  }}
  
  if(!is.null(input$Sex_A)){if(input$Sex_A=="Hommes"){ser_A$sex <- "H"}else if(input$Sex_A=="Femmes"){ser_A$sex <- "F"}else{ser_A$sex <- "E"}}
  if(!is.null(input$Age_A)){if(input$Age_A=="moins de 25 ans"){ser_A$age <- "J"}
    else if(input$Age_A=="de 25 à 49 ans"){ser_A$age <- "M"}
    else if(input$Age_A=="50 ans ou plus"){ser_A$age <- "S"}
    else{ser_A$age <- "E"}}
  
  if(!is.null(ser_A$sex)){if(ser_A$sex == "E" & ser_A$age == "E"){ser_A$TG <- "Les inscrits"}
    else if(ser_A$sex != "E" & ser_A$age == "E"){ser_A$TG <- input$Sex_A}
    else if(ser_A$sex == "E" & ser_A$age != "E"){ser_A$TG <- paste0("Les inscrits ayant ",input$Age_A)}
    else {ser_A$TG <- paste0(input$Sex_A, " ayant ",input$Age_A)}
  }
  
  
  
  if(!is.null(input$mcvs_A)){
    A_eff <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_A$geo & fp_defm$cvs==ser_A$cvs & fp_defm$type=="Eff" 
                     & fp_defm$cat=="A" & fp_defm$sexe==ser_A$sex & fp_defm$age==ser_A$age,8]
    infoA_eff <- format(A_eff, big.mark = " ")
    
    A_var <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_A$geo & fp_defm$cvs==ser_A$cvs & fp_defm$type==ser_A$var 
                     & fp_defm$cat=="A" & fp_defm$sexe==ser_A$sex & fp_defm$age==ser_A$age,8]
    infoA_var <- format(A_var, big.mark = " ")
    
    A_evo <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                     & fp_defm$geo==ser_A$geo & fp_defm$cvs==ser_A$cvs & fp_defm$type==ser_A$evo 
                     & fp_defm$cat=="A" & fp_defm$sexe==ser_A$sex & fp_defm$age==ser_A$age,8]
    infoA_evo <- paste0(format(A_evo, decimal.mark = ",", digits = 2, nsmall = 1), " %")
  }
  
  output$bA_eff <- renderValueBox({
    bAeff<-valueBox(value=tags$p("Effectif", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("users",lib="font-awesome")
                    ,color="purple"
                    ,href="#"
                    ,subtitle=tags$p(infoA_eff, style = "font-size: 28px;text-align: center;")
    )
    bAeff$children[[1]]$attribs$class<-"action-button"
    bAeff$children[[1]]$attribs$id<-"button_Aeff"
    return(bAeff)
  })
  
  output$bA_var <- renderValueBox({
    bAvar<-valueBox(value=tags$p("Variation ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("chart-bar")
                    ,color="purple"
                    ,href="#"
                    ,subtitle=tags$p(paste(infoA_var, input$Cal_A), style = "font-size: 24px;text-align: center;")
    )
    bAvar$children[[1]]$attribs$class<-"action-button"
    bAvar$children[[1]]$attribs$id<-"button_Avar"
    return(bAvar)
  })
  
  output$bA_evo <- renderValueBox({
    bAevo<-valueBox(value=tags$p("évolution ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                    ,width=NULL
                    ,icon = icon("chart-line")
                    ,color="purple"
                    ,href="#"
                    ,subtitle=tags$p(paste(infoA_evo, input$Cal_A), style = "font-size: 24px;text-align: center;")
    )
    bAevo$children[[1]]$attribs$class<-"action-button"
    bAevo$children[[1]]$attribs$id<-"button_Aevo"
    return(bAevo)
  })
  
  observeEvent(input$button_Aeff,{
    ser_A$graph <- "eff"
  })
  
  observeEvent(input$button_Avar,{
    ser_A$graph <- "var"
  })
  
  observeEvent(input$button_Aevo,{
    ser_A$graph <- "evo"
  })
  
  if(!is.null(input$mcvs_A)){
    dseff <- fp_defm[fp_defm$geo==ser_A$geo & fp_defm$cvs==ser_A$cvs & fp_defm$type=="Eff" & fp_defm$cat=="A" 
                     & fp_defm$sexe==ser_A$sex & fp_defm$age==ser_A$age,]
  }else{dseff <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="Eff" & fp_defm$cat=="A" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E",]
  }
  
  if(!is.null(input$mcvs_A)){
    dsvar <- fp_defm[fp_defm$geo==ser_A$geo & fp_defm$cvs==ser_A$cvs & fp_defm$type==ser_A$var 
                     & fp_defm$cat=="A" & fp_defm$sexe==ser_A$sex & fp_defm$age==ser_A$age,] %>% filter(!is.na(valeur))
    
  }else{dsvar <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="V1m" & fp_defm$cat=="A" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E",] %>% filter(!is.na(valeur))
  } 
  
  if(!is.null(input$mcvs_A)){
    dsevo <- fp_defm[fp_defm$geo==ser_A$geo & fp_defm$cvs==ser_A$cvs & fp_defm$type==ser_A$evo 
                     & fp_defm$cat=="A" & fp_defm$sexe==ser_A$sex & fp_defm$age==ser_A$age,] %>% filter(!is.na(valeur))
    
  }else{dsevo <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="E1m" & fp_defm$cat=="A" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E",]  %>% filter(!is.na(valeur))
  } 
  
  if(ser_A$graph=="eff"){ 
    output$Graph_defmA <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("purple"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_A$graph=="var"){ 
    output$Graph_defmA <- renderPlotly({
      plot <- plot_ly(dsvar, x = ~mois_stat, y = ~dsvar$valeur, 
                      type = "bar", color = I("purple"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_A$graph=="evo"){ 
    output$Graph_defmA <- renderPlotly({
      plot <- plot_ly(dsevo, x = ~mois_stat, y = ~dsevo$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines+markers', color = I("purple"))
      plot %>%  
        layout(
          xaxis = list(title = "", range = c(paste0(ms_3_An, "-04-01"), paste0(ms_An,"-04-15"))),
          yaxis = list(title = "")
        )
    })
  }else{ 
    output$Graph_defmA <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 'série CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("purple"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }
  
})