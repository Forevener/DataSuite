library(shiny)
library(shinyWidgets)
library(shinydashboard)
#library(shinydashboardPlus) #after they renamed functions to mirror shinydashboard all calls should be made explicitly
library(shinyjs)
library(shinyjqui)
library(shiny.i18n)
library(DT)
library(matrixStats)
library(glue)
# library(dplyr)
# library(haven)
# library(janitor)
# library(readxl)
# library(xlsx)
# library(rcompanion)
# library(questionr)
library(ggplot2)
# library(DescTools)
library(psych)
# library(parameters)
# library(fpc)
# library(cluster)
library(clustree) # clustree likes to be loaded
# library(factoextra)
# library(NbClust)
# library(GPArotation)
# library(ggraph)
# library(scam)
# library(leaps)
# library(bestglm)
# library(sjPlot)
# library(readr)
# library(readODS)
# library(moments)
# library(pwr)

# Hidden requirements from other packages - Shiny would like to know them
requireNamespace("ggraph", quietly = TRUE)
requireNamespace("GPArotation", quietly = TRUE)
requireNamespace("Rcsdp", quietly = TRUE)
requireNamespace("nFactors", quietly = TRUE)

source("functions_global.R", encoding = "utf-8")
