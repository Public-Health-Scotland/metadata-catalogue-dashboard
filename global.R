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
dataset <- openxlsx::read.xlsx("../../data/20240318_masterfile.xlsx") |>
  tibble()
  
names(dataset) <- names(dataset) |>
  str_replace_all("\\.", " ") |>
  str_to_sentence() |>
  str_replace_all("[Pp][Hh][Ss]", "PHS")



tag_options <- get_options(dataset$Tags, "|")
hw_topic_options <- get_options(dataset$`Health & wellbeing topic`, "|")
phs_pub_topic_options <- get_options(dataset$`PHS publication topic`, "|")
equality_options <- get_options(dataset$Equality, "|")
geographies_options <- get_options(dataset$Geographies, "|")
sex_options <- c("[blank]", count(dataset, Sex)$Sex[-length(count(dataset, Sex)$Sex)])
int_ext_options <- c("[blank]", "Internal", "External")


##Testing extraction of link from Description column
# dataset |>
#   mutate(desc_link = str_extract(Description, "(^\\S*/+\\S*\\s)|(^\\S*$)"),
#          desc_sin_link = str_remove_all(Description, "(^\\S*/+\\S*\\s)|(^\\S*$)")) |>
#   select(Description, desc_link, desc_sin_link, `Link(s)`) |>
#   mutate(match = desc_link == `Link(s)`) |>
#   View()



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