#START OF SCRIPT ----

library(dplyr)
#library(tidyr)
#library(purrr)
library(stringr)
library(lubridate)
#library(readr)
#library(forcats)

#library(labelled)
#library(data.table)
library(DT)
library(plotly)
#library(openxlsx)

library(shiny)
library(shinymanager)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(fresh)
#library(shinya11y)

#library(phsstyles)
#library(phsmethods)

source("functions.R")

#For PRA password protection
source("www/PRA_options.txt")
credentials <- data.frame(user = username, password = password, stringsAsFactors = FALSE)




#Bring in the dataset ----
load("www/dataset.Rdata")


definitions <- openxlsx::read.xlsx("www/definitions.xlsx")

names(definitions) <- names(definitions) |>
  str_replace_all("\\.", " ")





#mytheme ----
#must create a custom theme to make the colours on dashboard match PHS colours
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#1E7F84" # header bar = PHS-teal
  ),
  adminlte_sidebar( # sidebar colours
    width = "290px",
    dark_bg = "#1E7F84", # background colour (not selected) = PHS-teal
    dark_hover_bg = "#3F3685", # background colour (when hovering) = PHS-purple
    dark_color = "#FFFFFF", # text colour (not selected) = white
    dark_hover_color = "#FFFFFF", #hover colour = white
    dark_submenu_bg = "#4B999D", #sub-menu background colour = PHS-teal-80
    dark_submenu_color = "#FFFFFF" #sub-menu text color = white
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF",
    info_box_bg = "#FFF"
  )
  #adminlte_vars(
  #box_border_color = "#FFF"
  #)
)