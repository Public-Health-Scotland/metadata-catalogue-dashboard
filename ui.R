


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
          tags$p("Health & Wellbeing Catalogue Dashboard v0.2")
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
      
      menuItem("Data Table",
               tabName = "data_table",
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
                          h1("Welcome to the Health & Wellbeing Catalogue Dashboard"),
                      ), #box
                      
                      
                      # p("Click +/- to open and close the sections below", 
                      #   style = "text-align: right;"),
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          #collapsible = TRUE, collapsed = TRUE,
                          title = p(strong("What is this dashboard?")),
                          p("This dashboard aims to collect information on all dashboards 
                   and regular publications by PHS in one place, as well as which
                   indicators are shown in which dashboards/publications. On top of 
                   which indicators are associated with which dashboards/publications,
                   each dashboard, publication and indicator has a metadata profile
                   that gives more detail on that item."),
                          
                          p("The left-hand menu can also be hidden and revealed by 
                            clicking on this toggle button",
                            img(src = "sidebar_toggle.png", alt = "screenshot of the sidebar toggle button"),
                            "at the top of the screen. This can be useful if you are viewing charts on a small computer screen.")
                      ), 
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          #collapsible = TRUE, collapsed = TRUE,
                          title = p(strong("Data Table")),
                          p("The Data Table tab on this dashboard displays a table 
                            with each row being a dashboard, publication or indicator,
                            and each column being a catagory of metadata for these. 
                            Controls in the sidebar allow for filtering and searching
                            the table."),
                          
                          p("There is also a download button which looks like this:",
                            img(src = "download_button_sidebar.png", alt = "screenshot of the download button"), 
                            ". This will allow the user to download the metadata 
                            table as an xlsx document. The data downloaded will have 
                            the filters applied to it from the sidebar in.")
                      ), #function to remove unnecessary aria label on collapsible box
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          #collapsible = TRUE, collapsed = TRUE,
                          title = p(strong("Visualisation")),
                          
                          p("The data available on this dashboard is also viewable 
                            in an interactive visualisation with many useful filtering 
                            and searching features. This visualisation was built 
                            using Kumu rather than R Shiny, and so the 'visualisation'
                            tab in this dashboard provides a link to the Kumu page.")
                      ), 
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          #collapsible = TRUE, collapsed = FALSE,
                          title = p(strong("Tell us what you think")),
                          
                          p("The version of the dashboard available today is still in development 
                   and is subject to changes and refinements in the future. Contact ",
                            tags$a(href = "mailto:phs.metadatacatalogue@phs.scot", 
                                   tags$u("phs.metadatacatalogue@phs.scot")), 
                            "for more information or to provide feedback.")
                          
                      ) 
                      
                    ) #fluidRow
           ) #tabPanel 
    ) #tabBox
  ) #fluidRow
) #tabItem
           










#Data Table ----
data_table <- tabItem(
  tabName = "data_table",
  fluidRow(
    tabBox(title = "Data Table",
           # The id lets us use input$home_tab on the server to find the current tab
           id = "data_table_tab",
           width = 12,
           
           tabPanel(title = "Data Table",
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
             data_table,
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
  ) # dashboardPage
)


if(PRA) {
  ui <- ui |> secure_app()
}

ui



