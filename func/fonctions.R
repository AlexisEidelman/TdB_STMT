##-- Dernière observation ----
der_obs <- function(serie){
  serie[length(serie)]
}

##-- Derniers effectifs ----
der_eff <- function(serie){
  round( (serie[length(serie)] / 1000000),
         digits = 1)
}

##-- Dernières évolutions ----
der_evol <- function(serie){
  round( ((serie[length(serie)]-serie[length(serie)-1] ) / serie[length(serie)-1]) * 100,
         digits = 2)
}

##-- Graphique temporel simple ----
graph_ts <- function(serie1, serie2){
  plot <- plot_ly(defm_cat, x = ~mois_stat, y = ~serie1, name = 'série CSV-CJO', 
                  type = 'scatter', mode = 'lines', color = I("orange"))
  plot <- plot %>% add_trace(y = ~serie2, name = 'série brute', mode = 'lines', color = I("grey"))
  plot %>%  
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "")
    )
}

##-- Mini-Graph ----
miniplot <- function(type, cat, coul){
  if(type=="defm"){
    tab <- fp_defm 
  }else if(type=="dee"){
    tab <- fp_dee
  }else if(type=="des"){
    tab <- fp_des
  }
  
  don <- tab[tab$geo=="Fm" & tab$cvs=="O" & tab$type=="Eff" & tab$cat==cat
             & tab$sexe=="E" & tab$age=="E" & tab$mot=="",]
  
  #-- Graph_cat --
  plot <- ggplot(don) + geom_line(aes(x = mois_stat, y = valeur)
                                  ,color = coul)
  plot + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "White",
                                    colour = "White",
                                    size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) + xlab("") + ylab("")
  
}

#-- Infos mois stat ----
infos_mstat <- function(type, cat){
  if(type=="defm"){
    tab <- fp_defm 
  }else if(type=="dee"){
    tab <- fp_dee
  }else if(type=="des"){
    tab <- fp_des
  }
  
  doneff <- tab[tab$geo=="Fm" & tab$cvs=="O" & tab$type=="Eff" & tab$cat==cat
                & tab$sexe=="E" & tab$age=="E" & tab$mot=="",]
  
  donevo <- tab[tab$geo=="Fm" & tab$cvs=="O" & tab$type=="E1m" & tab$cat==cat
                & tab$sexe=="E" & tab$age=="E" & tab$mot=="",]
  
  donvar <- tab[tab$geo=="Fm" & tab$cvs=="O" & tab$type=="V1m" & tab$cat==cat
                & tab$sexe=="E" & tab$age=="E" & tab$mot=="",]
  
  assign(paste0("eff_", type, cat, "_cvs"), format(der_eff(doneff$valeur), decimal.mark=","), envir = .GlobalEnv)
  assign(paste0("evo_", type, cat, "_cvs"), format(der_obs(donevo$valeur), decimal.mark=",", digits = 2, nsmall = 1), envir = .GlobalEnv)
  assign(paste0("var_", type, cat, "_cvs"),format(der_obs(donvar$valeur), big.mark = " "), envir = .GlobalEnv)
}

#-- Box_eff ----
boxeff <- function(type, cat, coul){
  
  eff <- get(paste0("eff_", type, cat, "_cvs"))
  x <- type.convert(eff, dec=",")
  
  if(x < 2){unit <- "Million"}else{unit <- "Millions"}
  
  infoBox("effectif"
          ,value=tags$p(paste(eff, unit, sep = " "), style = "font-size: 20px;")
          ,icon = icon("users")
          ,fill = FALSE
          ,color = coul
  )
}

#-- Box_evo ----
boxevo <- function(type, cat, coul){
  infoBox("évolution"
          ,value=tags$p(paste0(get(paste0("evo_", type, cat, "_cvs"))," %"), style = "font-size: 20px;")
          ,icon = icon("chart-line")
          ,fill = FALSE
          ,color = coul
  )
}

#-- Box_var ----
boxvar <- function(type, cat, coul){
  infoBox("variation"
          ,value=tags$p(get(paste0("var_", type, cat, "_cvs")), style = "font-size: 20px;")
          ,icon = icon("chart-bar")
          ,fill = FALSE
          ,color = coul
  )
}

#-- tableau ----
tableau <- function(dontab){
  datatable(dontab, filter = 'none', rownames = FALSE
            ,extensions = c("Scroller", "FixedColumns", "Buttons")
            ,selection = "none"
            ,options = list(searching = FALSE
                            ,pageLength = 3
                            ,dom = "frtBip" 
                            ,scrollY = 200, scrollX = 400, scroller = TRUE
                            ,fixedColumns = list(leftColumns = 1)
                            ,buttons = c( 'copy', 'csv', 'excel', 'colvis'))
  )%>% formatRound(c(2:5), dec.mark = ",", mark = " ", digits = 1
  )%>% formatPercentage(c(6:15), dec.mark = ",", mark = " ", digits = 1
  )}



