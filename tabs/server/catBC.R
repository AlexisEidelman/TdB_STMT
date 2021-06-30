##-- menus ----
output$mgeo_BC = renderUI(selectInput("Geo_BC", label = "Champ :", c("France m?tropolitaine","France (hors Mayotte)"),"France m?tropolitaine"))
output$mcal_BC = renderUI(selectInput("Cal_BC", label = "Calculs :", c("sur 1 mois","sur 3 mois","sur 1 an"),"sur 1 mois"))
output$msex_BC = renderUI(selectInput("Sex_BC", label = "Sexe :", c("Ensemble","Hommes","Femmes"),"Ensemble"))
output$mage_BC = renderUI(selectInput("Age_BC", label = "Tranche d'?ge :", c("Ensemble","moins de 25 ans","de 25 ? 49 ans","50 ans ou plus"),"Ensemble"))

output$title_BC <- renderText({ 
  paste(ser_BC$TG, " en ", mois_stat)
})

output$type_BC <- renderText({ 
  if(input$mcvs_BC==1){paste("Donn?es corrig?es des varaitions saisonni?res et des jours ouvrables (CVS-CJO)")
  }else{paste("Donn?es brutes")}
})

output$champ_BC <- renderText({ 
  paste("Champ: ", input$Geo_BC)
})

##-- Atualisation ----
ser_BC <- reactiveValues(cvs = NULL, geo = NULL, var = NULL, evo = NULL, sex = NULL, graph = "eff",TG = NULL)

observe({
  if(!is.null(input$mcvs_BC)){if(input$mcvs_BC==2){ser_BC$cvs <- "N"}else{ser_BC$cvs <- "O"}}
  if(!is.null(input$Geo_BC)){if(input$Geo_BC=="France m?tropolitaine"){ser_BC$geo <- "Fm"}else{ser_BC$geo <- "Fr"}}
  if(!is.null(input$Cal_BC)){if(input$Cal_BC=="sur 1 mois"){
    ser_BC$var <- "V1m"
    ser_BC$evo <- "E1m"
  }else if(input$Cal_BC=="sur 3 mois"){
    ser_BC$var <- "V3m"
    ser_BC$evo <- "E3m"
  }else{
    ser_BC$var <- "V1a"
    ser_BC$evo <- "E1a"
  }}
  
  if(!is.null(input$Sex_BC)){if(input$Sex_BC=="Hommes"){ser_BC$sex <- "H"}else if(input$Sex_BC=="Femmes"){ser_BC$sex <- "F"}else{ser_BC$sex <- "E"}}
  if(!is.null(input$Age_BC)){if(input$Age_BC=="moins de 25 ans"){ser_BC$age <- "J"}
    else if(input$Age_BC=="de 25 ? 49 ans"){ser_BC$age <- "M"}
    else if(input$Age_BC=="50 ans ou plus"){ser_BC$age <- "S"}
    else{ser_BC$age <- "E"}}
  
  if(!is.null(ser_BC$sex)){if(ser_BC$sex == "E" & ser_BC$age == "E"){ser_BC$TG <- "Les inscrits"}
    else if(ser_BC$sex != "E" & ser_BC$age == "E"){ser_BC$TG <- input$Sex_BC}
    else if(ser_BC$sex == "E" & ser_BC$age != "E"){ser_BC$TG <- paste0("Les inscrits ayant ",input$Age_BC)}
    else {ser_BC$TG <- paste0(input$Sex_BC, " ayant ",input$Age_BC)}
  }
  
  
  
  if(!is.null(input$mcvs_BC)){
    BC_eff <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                      & fp_defm$geo==ser_BC$geo & fp_defm$cvs==ser_BC$cvs & fp_defm$type=="Eff" 
                      & fp_defm$cat=="BC" & fp_defm$sexe==ser_BC$sex & fp_defm$age==ser_BC$age,8]
    infoBC_eff <- format(BC_eff, big.mark = " ")
    
    BC_var <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                      & fp_defm$geo==ser_BC$geo & fp_defm$cvs==ser_BC$cvs & fp_defm$type==ser_BC$var 
                      & fp_defm$cat=="BC" & fp_defm$sexe==ser_BC$sex & fp_defm$age==ser_BC$age,8]
    infoBC_var <- format(BC_var, big.mark = " ")
    
    BC_evo <- fp_defm[fp_defm$mois_stat==max(fp_defm$mois_stat, na.rm = TRUE) 
                      & fp_defm$geo==ser_BC$geo & fp_defm$cvs==ser_BC$cvs & fp_defm$type==ser_BC$evo 
                      & fp_defm$cat=="BC" & fp_defm$sexe==ser_BC$sex & fp_defm$age==ser_BC$age,8]
    infoBC_evo <- paste0(format(BC_evo, decimal.mark = ",", digits = 2, nsmall = 1), " %")
  }
  
  output$bBC_eff <- renderValueBox({
    bBCeff<-valueBox(value=tags$p("Effectif", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                     ,width=NULL
                     ,icon = icon("users",lib="font-awesome")
                     ,color="orange"
                     ,href="#"
                     ,subtitle=tags$p(infoBC_eff, style = "font-size: 28px;text-align: center;")
    )
    bBCeff$children[[1]]$attribs$class<-"action-button"
    bBCeff$children[[1]]$attribs$id<-"button_BCeff"
    return(bBCeff)
  })
  
  output$bBC_var <- renderValueBox({
    bBCvar<-valueBox(value=tags$p("Variation ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                     ,width=NULL
                     ,icon = icon("chart-bar")
                     ,color="orange"
                     ,href="#"
                     ,subtitle=tags$p(paste(infoBC_var, input$Cal_BC), style = "font-size: 24px;text-align: center;")
    )
    bBCvar$children[[1]]$attribs$class<-"action-button"
    bBCvar$children[[1]]$attribs$id<-"button_BCvar"
    return(bBCvar)
  })
  
  output$bBC_evo <- renderValueBox({
    bBCevo<-valueBox(value=tags$p("?volution ", style = "font-size: 20px;text-transform: uppercase;text-align: center;")
                     ,width=NULL
                     ,icon = icon("chart-line")
                     ,color="orange"
                     ,href="#"
                     ,subtitle=tags$p(paste(infoBC_evo, input$Cal_BC), style = "font-size: 24px;text-align: center;")
    )
    bBCevo$children[[1]]$attribs$class<-"action-button"
    bBCevo$children[[1]]$attribs$id<-"button_BCevo"
    return(bBCevo)
  })
  
  observeEvent(input$button_BCeff,{
    ser_BC$graph <- "eff"
  })
  
  observeEvent(input$button_BCvar,{
    ser_BC$graph <- "var"
  })
  
  observeEvent(input$button_BCevo,{
    ser_BC$graph <- "evo"
  })
  
  if(!is.null(input$mcvs_BC)){
    dseff <- fp_defm[fp_defm$geo==ser_BC$geo & fp_defm$cvs==ser_BC$cvs & fp_defm$type=="Eff" & fp_defm$cat=="BC" 
                     & fp_defm$sexe==ser_BC$sex & fp_defm$age==ser_BC$age,]
  }else{dseff <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="Eff" & fp_defm$cat=="BC" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E",]
  }
  
  if(!is.null(input$mcvs_BC)){
    dsvar <- fp_defm[fp_defm$geo==ser_BC$geo & fp_defm$cvs==ser_BC$cvs & fp_defm$type==ser_BC$var 
                     & fp_defm$cat=="BC" & fp_defm$sexe==ser_BC$sex & fp_defm$age==ser_BC$age,] %>% filter(!is.na(valeur))
    
  }else{dsvar <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="V1m" & fp_defm$cat=="BC" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E",] %>% filter(!is.na(valeur))
  } 
  
  if(!is.null(input$mcvs_BC)){
    dsevo <- fp_defm[fp_defm$geo==ser_BC$geo & fp_defm$cvs==ser_BC$cvs & fp_defm$type==ser_BC$evo 
                     & fp_defm$cat=="BC" & fp_defm$sexe==ser_BC$sex & fp_defm$age==ser_BC$age,] %>% filter(!is.na(valeur))
    
  }else{dsevo <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="E1m" & fp_defm$cat=="BC" 
                         & fp_defm$sexe=="E" & fp_defm$age=="E",]  %>% filter(!is.na(valeur))
  } 
  
  if(ser_BC$graph=="eff"){ 
    output$Graph_defmBC <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 's?rie CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("orange"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_BC$graph=="var"){ 
    output$Graph_defmBC <- renderPlotly({
      plot <- plot_ly(dsvar, x = ~mois_stat, y = ~dsvar$valeur, 
                      type = "bar", color = I("orange"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }else if(ser_BC$graph=="evo"){ 
    output$Graph_defmBC <- renderPlotly({
      plot <- plot_ly(dsevo, x = ~mois_stat, y = ~dsevo$valeur, name = 's?rie CSV-CJO', 
                      type = 'scatter', mode = 'lines+markers', color = I("orange"))
      plot %>%  
        layout(
          xaxis = list(title = "", range = c(paste0(ms_3_An, "-04-01"), paste0(ms_An,"-04-15"))),
          yaxis = list(title = "")
        )
    })
  }else{ 
    output$Graph_defmBC <- renderPlotly({
      plot <- plot_ly(dseff, x = ~mois_stat, y = ~dseff$valeur, name = 's?rie CSV-CJO', 
                      type = 'scatter', mode = 'lines', color = I("orange"))
      plot %>%  
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    })
  }
  
})