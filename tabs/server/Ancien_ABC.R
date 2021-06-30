##-- Graph ABC selon l'ancienneté ----
df <- fp_defm[fp_defm$geo=="Fm" & fp_defm$cvs=="O" & fp_defm$type=="Par" & fp_defm$cat=="ABC" 
              & fp_defm$sexe=="E" & fp_defm$age=="E" & fp_defm$motif %in% c("0M", "3M", "6M", "1A","2A", "3A"),]%>% filter(mois_stat > '2009-12-31')

df$label = ""
df[df$motif=="0M",c("label")] <- c("[00 à 03 mois[")
df[df$motif=="3M",c("label")] <- c("[03 à 06 mois[")
df[df$motif=="6M",c("label")] <- c("[06 à 12 mois[")
df[df$motif=="1A",c("label")] <- c("[12 à 24 mois[")
df[df$motif=="2A",c("label")] <- c("[24 à 36 mois[")
df[df$motif=="3A",c("label")] <- c("[36 mois ou plus[")

df$mois <- substr(df$mois_stat,1,7)

df <- df[order(df$label),]

output$Anc_defmABC <- renderPlotly({
  fig <- df %>% plot_ly(x = ~valeur
                        ,y = ~label
                        ,frame = ~mois
                        ,type = 'bar'
                        ,text = ~paste0('<b>',format(df$valeur
                                                     ,decimal.mark=","
                                                     ,digits = 2
                                                     ,nsmall = 1)
                                        ," %")
                        ,textposition = 'auto'
                        ,showlegend = F
                        ,orientation='h'
                        ,marker = list(color = c('rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                                 'rgba(204,204,204,1)', 'rgba(222,45,38,0.8)',
                                                 'rgba(222,45,38,0.8)', 'rgba(222,45,38,0.8)')))
  
  fig %>% layout(yaxis = list(title = ''
                              ,tickvals = c(0,1,2,3,4,5)
                              ,ticktext = c('Moins de 3 mois'
                                            ,'Entre 3 et moins 6 mois'
                                            ,'Entre 6 et moins 12 mois'
                                            ,'Entre 1 an et moins de 2 ans'
                                            ,'Entre 2 ans et moins de 3 ans'
                                            ,'3 ans ou plus'))
                 ,xaxis = list(title = ''
                               ,ticksuffix = "%")
                 
  )%>% animation_opts( 100, easing = "elastic", redraw = FALSE, mode = "immediate"
  )%>% animation_slider(currentvalue = list(prefix = ""
                                            ,font = list(color="Blue", size = 30)
                                            ,xanchor = "center")
                        
  )%>% animation_button(x = 0
                        ,xanchor = "right"
                        ,y = 0
                        ,yanchor = "left")
  
})
