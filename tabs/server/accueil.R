##-- Atualisation bar menu ----
observeEvent(input$button_box_A,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "catA")
  shinyjs::runjs("window.scrollTo(0, 0)")
})
observeEvent(input$button_box_B,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "catB")
  shinyjs::runjs("window.scrollTo(0, 0)")
})
observeEvent(input$button_box_C,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "catC")
  shinyjs::runjs("window.scrollTo(0, 0)")
})
observeEvent(input$button_box_ABC,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "catABC")
  shinyjs::runjs("window.scrollTo(0, 0)")
})
observeEvent(input$button_box_DEE,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "Entrées")
  shinyjs::runjs("window.scrollTo(0, 0)")
})
observeEvent(input$button_box_DES,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "Sorties")
  shinyjs::runjs("window.scrollTo(0, 0)")
})
observeEvent(input$button_box_D,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "catD")
  shinyjs::runjs("window.scrollTo(0, 0)")
})
observeEvent(input$button_box_E,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "catE")
  shinyjs::runjs("window.scrollTo(0, 0)")
})

#--------------------- CATEGORIE A ---------------------
infos_mstat("defm", "A")

#-- Box A --
output$box_A <- renderValueBox({
  boxA<-valueBox(value=tags$p("", style = "font-size: 24px;")
                 ,icon = NULL
                 ,width=NULL
                 ,color = "purple"
                 ,href="#"
                 ,subtitle=tags$p("Catégorie A", style = "font-size: 24px;")
  )
  boxA$children[[1]]$attribs$class<-"action-button"
  boxA$children[[1]]$attribs$id<-"button_box_A"
  return(boxA)
})

#-- Graph A --
output$Graph_A <- renderPlot({miniplot("defm", "A", "purple")}, height = 90)

#-- Box effectif A --
output$box_effA <- renderValueBox({boxeff("defm", "A", "purple")})

#-- Box évolution A --
output$box_evoA <- renderValueBox({boxevo("defm", "A", "purple")})

#-- Box variation A --
output$box_varA <- renderValueBox({boxvar("defm", "A", "purple")})

#-- Tableau A --
observe({
  if(input$TabAviz){
    output$TabdefmA <- DT::renderDataTable({tableau(tabA)})
  }else{output$TabdefmA <- DT::renderDataTable({return()})}
})   

#--------------------- CATEGORIE B ---------------------
infos_mstat("defm", "B")

#-- Box B --
output$box_B <- renderValueBox({
  boxB<-valueBox(value=tags$p("", style = "font-size: 24px;")
                 ,icon = NULL
                 ,width=NULL
                 ,color = "blue"
                 ,href="#"
                 ,subtitle=tags$p("Catégorie B", style = "font-size: 24px;")
  )
  boxB$children[[1]]$attribs$class<-"action-button"
  boxB$children[[1]]$attribs$id<-"button_box_B"
  return(boxB)
})

#-- Graph B --
output$Graph_B <- renderPlot({miniplot("defm", "B", "blue")}, height = 90)

#-- Box effectif B --
output$box_effB <- renderValueBox({boxeff("defm", "B", "blue")})

#-- Box évolution B --
output$box_evoB <- renderValueBox({boxevo("defm", "B", "blue")})

#-- Box variation B --
output$box_varB <- renderValueBox({boxvar("defm", "B", "blue")})

#-- Tableau B --
observe({
  if(input$TabBviz){
    output$TabdefmB <- DT::renderDataTable({tableau(tabB)})
  }else{output$TabdefmB <- DT::renderDataTable({return()})}
}) 

#--------------------- CATEGORIE C ---------------------
#-- Infos C --
infos_mstat("defm", "C")

#-- Box C --
output$box_C <- renderValueBox({
  boxC<-valueBox(value=tags$p("", style = "font-size: 24px;")
                 ,icon = NULL
                 ,width=NULL
                 ,color = "aqua"
                 ,href="#"
                 ,subtitle=tags$p("Catégorie C", style = "font-size: 24px;")
  )
  boxC$children[[1]]$attribs$class<-"action-button"
  boxC$children[[1]]$attribs$id<-"button_box_C"
  return(boxC)
})

#-- Graph C --
output$Graph_C <- renderPlot({miniplot("defm", "C", "blue")}, height = 90)

#-- Box effectif C --
output$box_effC <- renderValueBox({boxeff("defm", "C", "aqua")})

#-- Box évolution C --
output$box_evoC <- renderValueBox({boxevo("defm", "C", "aqua")})

#-- Box variation C --
output$box_varC <- renderValueBox({boxvar("defm", "C", "aqua")})

#-- Tableau C --
observe({
  if(input$TabCviz){
    output$TabdefmC <- DT::renderDT({tableau(tabC)})
  }else{output$TabdefmC <- DT::renderDT({return()})}
}) 

#--------------------- CATEGORIES BC ---------------------
#-- Infos BC --
infos_mstat("defm", "BC")

#-- Box BC --
output$box_BC <- renderValueBox({
  boxBC<-valueBox(value=tags$p("", style = "font-size: 24px;")
                  ,icon = NULL
                  ,width=NULL
                  ,color = "light-blue"
                  ,href="#"
                  ,subtitle=tags$p("Catégories BC", style = "font-size: 24px;")
  )
  boxBC$children[[1]]$attribs$class<-"action-button"
  boxBC$children[[1]]$attribs$id<-"button_box_BC"
  return(boxBC)
})

#-- Graph BC --
output$Graph_BC <- renderPlot({miniplot("defm", "BC", "navy")}, height = 90)

#-- Box effectif BC --
output$box_effBC <- renderValueBox({boxeff("defm", "BC", "light-blue")})

#-- Box évolution BC --
output$box_evoBC <- renderValueBox({boxevo("defm", "BC", "light-blue")})

#-- Box variation BC --
output$box_varBC <- renderValueBox({boxvar("defm", "BC", "light-blue")})



#--------------------- CATEGORIES ABC ---------------------
#-- Infos ABC --
infos_mstat("defm", "ABC")

#-- Box ABC --
output$box_ABC <- renderValueBox({
  boxABC<-valueBox(value=tags$p("", style = "font-size: 24px;")
                   ,icon = NULL
                   ,width=NULL
                   ,color = "navy"
                   ,href="#"
                   ,subtitle=tags$p("Catégories ABC", style = "font-size: 24px;")
  )
  boxABC$children[[1]]$attribs$class<-"action-button"
  boxABC$children[[1]]$attribs$id<-"button_box_ABC"
  return(boxABC)
})

#-- Graph ABC --
output$Graph_ABC <- renderPlot({miniplot("defm", "ABC", "navy")}, height = 90)

#-- Box effectif ABC --
output$box_effABC <- renderValueBox({boxeff("defm", "ABC", "navy")})

#-- Box évolution ABC --
output$box_evoABC <- renderValueBox({boxevo("defm", "ABC", "navy")})

#-- Box variation ABC --
output$box_varABC <- renderValueBox({boxvar("defm", "ABC", "navy")})

#-- Tableau ABC --
observe({
  if(input$TabABCviz){
    output$TabdefmABC <- DT::renderDataTable({tableau(tabABC)})
  }else{output$TabdefmABC <- DT::renderDataTable({return()})}
}) 


#--------------------- LES ENTREES ---------------------
#-- Infos DEE --
infos_mstat("dee", "ABC")

#-- Box DEE --
output$box_DEE <- renderValueBox({
  boxDEE<-valueBox(value=tags$p("", style = "font-size: 24px;")
                   ,icon = NULL
                   ,width=NULL
                   ,color = "orange"
                   ,href="#"
                   ,subtitle=tags$p("Les entrées", style = "font-size: 24px;")
  )
  boxDEE$children[[1]]$attribs$class<-"action-button"
  boxDEE$children[[1]]$attribs$id<-"button_box_DEE"
  return(boxDEE)
})

#-- Graph DEE --
output$Graph_DEE <- renderPlot({miniplot("dee", "ABC", "orange")}, height = 90)

#-- Box effectif DEE --
output$box_effDEE <- renderValueBox({boxeff("dee", "ABC", "orange")})

#-- Box évolution DEE --
output$box_evoDEE <- renderValueBox({boxevo("dee", "ABC", "orange")})

#-- Box variation DEE --
output$box_varDEE <- renderValueBox({boxvar("dee", "ABC", "orange")})

#-- Tableau des entrées --
observe({
  if(input$TabDEEviz){
    output$TabdeeABC <- DT::renderDataTable({tableau(tabDEE)})
  }else{output$TabdeeABC <- DT::renderDataTable({return()})}
}) 

#--------------------- LES SORTIES ---------------------
#-- Infos DES --
infos_mstat("des", "ABC")

#-- Box DES --
output$box_DES <- renderValueBox({
  boxDES<-valueBox(value=tags$p("", style = "font-size: 24px;")
                   ,icon = NULL
                   ,width=NULL
                   ,color = "olive"
                   ,href="#"
                   ,subtitle=tags$p("Les sorties", style = "font-size: 24px;")
  )
  boxDES$children[[1]]$attribs$class<-"action-button"
  boxDES$children[[1]]$attribs$id<-"button_box_DES"
  return(boxDES)
})

#-- Graph DEE --
output$Graph_DES <- renderPlot({miniplot("des", "ABC", "chartreuse4")}, height = 90)

#-- Box effectif DES --
output$box_effDES <- renderValueBox({boxeff("des", "ABC", "olive")})

#-- Box évolution DES --
output$box_evoDES <- renderValueBox({boxevo("des", "ABC", "olive")})

#-- Box variation DES --
output$box_varDES <- renderValueBox({boxvar("des", "ABC", "olive")})

#-- Tableau des sorties --
observe({
  if(input$TabDESviz){
    output$TabdesABC <- DT::renderDataTable({tableau(tabDES)})
  }else{output$TabdesABC <- DT::renderDataTable({return()})}
}) 

#--------------------- CATEGORIE D ---------------------
infos_mstat("defm", "D")

#-- Box D --
output$box_D <- renderValueBox({
  boxD<-valueBox(value=tags$p("", style = "font-size: 24px;")
                 ,icon = NULL
                 ,width=NULL
                 ,color = "blue"
                 ,href="#"
                 ,subtitle=tags$p("Catégorie D", style = "font-size: 24px;")
  )
  boxD$children[[1]]$attribs$class<-"action-button"
  boxD$children[[1]]$attribs$id<-"button_box_D"
  return(boxD)
})

#-- Graph D --
output$Graph_D <- renderPlot({miniplot("defm", "D", "blue")}, height = 90)

#-- Box effectif D --
output$box_effD <- renderValueBox({boxeff("defm", "D", "blue")})

#-- Box évolution D --
output$box_evoD <- renderValueBox({boxevo("defm", "D", "blue")})

#-- Box variation D --
output$box_varD <- renderValueBox({boxvar("defm", "D", "blue")})

#-- Tableau D --
observe({
  if(input$TabDviz){
    output$TabdefmD <- DT::renderDataTable({tableau(tabD)})
  }else{output$TabdefmD <- DT::renderDataTable({return()})}
})   


#--------------------- CATEGORIE E ---------------------
infos_mstat("defm", "E")

#-- Box E --
output$box_E <- renderValueBox({
  boxE<-valueBox(value=tags$p("", style = "font-size: 24px;")
                 ,icon = NULL
                 ,width=NULL
                 ,color = "aqua"
                 ,href="#"
                 ,subtitle=tags$p("Catégorie E", style = "font-size: 24px;")
  )
  boxE$children[[1]]$attribs$class<-"action-button"
  boxE$children[[1]]$attribs$id<-"button_box_E"
  return(boxE)
})

#-- Graph E --
output$Graph_E <- renderPlot({miniplot("defm", "E", "blue")}, height = 90)

#-- Box effectif E --
output$box_effE <- renderValueBox({boxeff("defm", "E", "aqua")})

#-- Box évolution E --
output$box_evoE <- renderValueBox({boxevo("defm", "E", "aqua")})

#-- Box variation E --
output$box_varE <- renderValueBox({boxvar("defm", "E", "aqua")})

#-- Tableau E --
observe({
  if(input$TabEviz){
    output$TabdefmE <- DT::renderDataTable({tableau(tabE)})
  }else{output$TabdefmE <- DT::renderDataTable({return()})}
})   