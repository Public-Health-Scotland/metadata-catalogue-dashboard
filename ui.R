


#header ----
#makes the logo that goes in the top left corner of the dashboard
dashboardtitle <- tags$a(href = "https://www.publichealthscotland.scot/",
                         target="_blank",
                         tags$imag(src = "phs-logo.png",
                                   alt = "Public Health Scotland logo",
                                   width = 120
                         )
)


#makes the header bar, which includes the logo on the top left and title on the right
header <- dashboardHeader(
  title = dashboardtitle,
  titleWidth = 290,
  
  # tags$li(class = "dropdown",
  #   menuItem("Test",
  #            tabName = "test")
  # ),
  
  tags$li(class = "dropdown",
          tags$p("Health & Wellbeing Catalogue Dashboard v0.3")
  )
)





#sidebar ----
#creates the sidebar menu for the dashboard
sidebar <- dashboardSidebar(
  width = 280,
  
  #adds in the options for each page of the dashboard
  accessible_menu(    #function to please screen readers
    sidebarMenu(
      id = "menu",
      
      menuItem("Home",
               tabName = "home",
               icon = icon("house", verify_fa = FALSE) |> rem_aria_label(),
               selected = TRUE),
      
      menuItem("Catalogue",
               tabName = "catalogue",
               icon = icon("table", verify_fa = FALSE) |> rem_aria_label()),
      
      menuItem("Definitions",
               tabName = "definitions",
               icon = icon("info-circle", verify_fa = FALSE) |> rem_aria_label()),
      
      menuItem("Visualisation",
               tabName = "visualisation",
               icon = icon("image", verify_fa = FALSE) |> rem_aria_label()),
      
      menuItem("Version",
               tabName = "version",
               icon = icon("code", verify_fa = FALSE) |> rem_aria_label()),
      

      uiOutput("search_box"),
      uiOutput("column_selector"),
      uiOutput("filter_topic_selector"),
      uiOutput("label_selector"),
      uiOutput("item_type_selector"),
      uiOutput("tags_selector"),
      uiOutput("duplicate_selector"),
      uiOutput("equalities_selector"),
      uiOutput("geographies_selector"),
      uiOutput("hw_topic_selector"),
      uiOutput("int_ext_selector"),
      uiOutput("sex_selector"),
      uiOutput("source_selector"),
      
      uiOutput("reset_button"),
      uiOutput("download_table"),
      
      
      verbatimTextOutput("testing") #dev panel for testing things
      
      
    )    #sidebarMenu
  ),    #accessible_menu
  
  useShinyjs()    #must be included to make javascript stuff happen
)





#.----
#Home ----
home <- tabItem(
  tabName = "home",
  fluidRow(
    tabBox(title = "Home",
           # The id lets us use input$home_tab on the server to find the current tab
           id = "home_tab",
           width = 12,
           
           tabPanel(title = "Welcome",
                    fluidRow(
                      
                      box(width = 12,
                          solidHeader = TRUE,
                          h1("Welcome to the Health & Wellbeing Catalogue Dashboard")
                      ), #box
                      
                      
                      p("Click +/- to open and close the sections below",
                        style = "text-align: right;"),
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          collapsible = TRUE, collapsed = TRUE,
                          title = p(strong("What is this dashboard?")),
                          
                          p("This metadata catalogue provides a single source from which to search for 
                            all publicly available health and wellbeing indicators for Scotland. It has
                            been developed to make it easier to find and use existing intelligence for 
                            learning and decision making related to health and wellbeing."),
                          
                          p("The catalogue includes data from Public Health Scotland, as well as from 
                            other data owners such as the Office for National Statistics, National Records
                            of Scotland, and Scottish Government."),
                          
                          p("Indicators from each publication are grouped by category, providing an 
                            overview of available metrics by topic area. Each indicator has a metadata 
                            profile that provides available detail on that item, for example, frequency
                            of data updates, categorical breakdowns, and data source."),
                          
                          p("The dashboard allows quick identification of relevant metrics across wide 
                            range of topic areas and sources rather than searching multiple places.")
                      ),
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          collapsible = TRUE, collapsed = TRUE,
                          title = p(strong("Using the Data Catalogue")),
                          
                          p("The Catalogue tab displays a table, with each row being a dashboard, 
                            publication or indicator, and each column a category of metadata for the
                            row item. The default setting displays three columns (label, dashboard report 
                            name and health & wellbeing topic) but more metadata columns can be added by
                            selecting additional items from the ‘Columns to display’ drop downs."),
                          
                          p("Controls in the sidebar allow you to filter and search the catalogue, for 
                            example by geography, equalities, or data source."),
                          
                          p("There is a download button which looks like this:",
                            img(src = "download_button_sidebar.png", alt = "screenshot of the download button"), 
                            ". This will allow the user to download the metadata table as an Excel document.
                            The metadata downloaded will have the chosen filters applied."),
                          
                          p("The left-hand menu can be hidden and revealed by clicking on this toggle button ",
                            img(src = "sidebar_toggle.png", alt = "screenshot of the sidebar toggle button"),
                            " at the top of the screen.")
                          
                      ), 
                      
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          collapsible = TRUE, collapsed = TRUE,
                          title = p(strong("Definitions")),
                          
                          p("The Definitions tab provides a more detailed description of what is contained 
                            within each item. Where possible, this tab also provides a list of possible 
                            values the user can select from.")
                          
                      ), 
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          collapsible = TRUE, collapsed = TRUE,
                          title = p(strong("Visualisation")),
                          
                          p("The metadata available on this dashboard can be viewed in an interactive 
                            visualisation with several filtering and searching features. The Visualisation 
                            tab provides a link to an alternative way of interacting with the catalogue. 
                            This option is particularly useful for visualising connections between dashboards
                            and information gaps by topic area.")
                          
                      ), 
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          collapsible = TRUE, collapsed = FALSE,
                          title = p(strong("Tell us what you think")),
                          
                          p("The dashboard is still in development and is subject to changes and refinements 
                            in the future. Contact ",
                            tags$a(href = "mailto:phs.metadatacatalogue@phs.scot", 
                                   tags$u("phs.metadatacatalogue@phs.scot")), 
                            "for more information or to provide feedback.")
                          
                      ) 
                      
                    ) #fluidRow
           ) #tabPanel 
    ) #tabBox
  ) #fluidRow
) #tabItem
           










#Catalogue ----
catalogue <- tabItem(
  tabName = "catalogue",
  fluidRow(
    tabBox(title = "Catalogue",
           # The id lets us use input$home_tab on the server to find the current tab
           id = "catalogue_tab",
           width = 12,
           
           tabPanel(title = "Catalogue",
             fluidRow(
               DTOutput("main_table")
             )
           )
    )
  )
)






#Definitons ----
definitions <- tabItem(
  tabName = "definitions",
  fluidRow(
    tabBox(title = "Definitions",
           id = "definitions_tab",
           width = 12,
           
           tabPanel(title = "Definitions",
                    fluidRow(
                      box(width = 10,
                        h1("Definitions"),
                        p("Each of the columns in the data table are defined here."), 
                        tableOutput("definitions_table"),
                        p("Some columns contain multiple entries per row, and in these cases those entries are separated by a vertical bar (|)."),

                      )
                    ))
           
           )
  )
)




#Visualisation ----
visualisation <- tabItem(
  tabName = "visualisation",
  fluidRow(
    tabBox(title = "Visualisation",
           # The id lets us use input$home_tab on the server to find the current tab
           id = "visualisation_tab",
           width = 12,
           
           tabPanel(title = "Visualisation",
             fluidRow(
               p("The data available on this dashboard is also viewable in an interactive
                 visualisation with many useful filtering and searching features."),
               
               p("This can be found at ",
                 tags$a(
                   href = "https://kumu.io/graham09/phs-dashboard-indicators",##https://kumu.io/graham09/phs-dashboard-indicators #NR
                   tags$u("PHS Dashboard Indicators • Kumu"),##https://kumu.io/graham09/phs-dashboard-indicators #NR
                   class = "externallink", target = "_blank"),
                 ),
               
               tags$a(
                 href = "https://kumu.io/graham09/phs-dashboard-indicators",##https://kumu.io/graham09/phs-dashboard-indicators #NR
                 class = "externallink",
                 target = "_blank",
                 tags$img(src = "kumu_screenshot.png",
                          alt = "Screenshot of the interactive visualisation",
                          title = "PHS Dashboard Indicators • Kumu",##https://kumu.io/graham09/phs-dashboard-indicators #NR
                          width = "90%",
                          height = "90%")
               )
             )
           )
    )
  )
)





#Version ----
version <- tabItem(
  tabName = "version",
  fluidRow(
    tabBox(title = "Version",
           # The id lets us use input$home_tab on the server to find the current tab
           id = "version_tab",
           width = 12,
           tabPanel(title = "Version",
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          h1("Version information"),
                          br(),
                          p("Small changes to the format of the dashboard are noted 
                            with new version numbers as detailed below:"),
                          tableOutput("version_table")
                      )
                    ) #fluidRow
           ) #tabPanel
    ) #tabBox
  ) #fluidRow
) #tabItem








#Test tab ----
test <- tabItem(
  tabName = "test",
  fluidRow(
    tabBox(title = "Test",
           # The id lets us use input$home_tab on the server to find the current tab
           id = "test_tab",
           width = 12,
           
           tabPanel(title = "Test",
                    fluidRow(
                      p("test")
                    )
           )
    )
  )
)




#.----
#body ----
#put together each of the tabItems created above
body <-
  dashboardBody(
    use_theme(mytheme), # <-- use the theme to change colours
    tags$head(includeCSS("www/styles.css")),
    tags$head(
      tags$style(HTML(".sidebar {height: 90vh; overflow-y: auto;}",
                      ".box {-webkit-box-shadow: none;
                           -moz-box-shadow: none;
                           box-shadow: none;}",
                      ".nav-tabs-custom {box-shadow: none;}",
                      ".content-wrapper {overflow-y: hidden;}"
      ))),    #just a bunch of style stuff lifted from SPBaND
    
    #add dashboard pages to this list if we make more
    tabItems(home,
             catalogue,
             definitions,
             visualisation,
             version,
             test)
  )








#ui ----
#pulls together all the elements made in this script
ui <- tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  tags$style("@import url(https://use.fontawesome.com/releases/v6.0/css/all.css);"),
  tags$head(HTML("<html lang='en'>"),
            tags$link(rel="shortcut icon",
                      href="favicon_phs.ico"), # Icon for browser tab
            tags$title("Health & Wellbeing Catalogue Dashboard")
  ),
  
  #pull together the elements created above
  dashboardPage(
    header,
    sidebar,
    body
  ), # dashboardPage
  
  includeScript("www/script.js")
)


if(PRA) {
  ui <- ui |> secure_app()
}

ui



