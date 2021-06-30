################################################################################
# Library
################################################################################
library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(DT)


load("data/DonEnv.Rda")
DonEnv$ms_An <- as.numeric(DonEnv$ms_An)
assign("mois_stat", DonEnv[,c("dateLettre_ms")], envir = .GlobalEnv)
assign("ms_An", DonEnv[,c("ms_An")], envir = .GlobalEnv)
assign("ms_3_An", DonEnv[,c("ms_An")]-3, envir = .GlobalEnv)

source("func/fonctions.R", local = TRUE)

##-- commande le comportement du menu dans le header ----
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
tab_files <- tab_files[-grep(x = tab_files, pattern = "server")]

suppressMessages(lapply(tab_files, source))

load("data/fp_defm.Rda")
load("data/fp_dee.Rda")
load("data/fp_des.Rda")
load("data/tabA.Rda")
load("data/tabB.Rda")
load("data/tabC.Rda")
load("data/tabABC.Rda")
load("data/tabDEE.Rda")
load("data/tabDES.Rda")
load("data/tabD.Rda")
load("data/tabE.Rda")