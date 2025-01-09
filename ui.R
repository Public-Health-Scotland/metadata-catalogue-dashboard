


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
  tags$li(class = "dropdown",
          tags$p("Health & Wellbeing Metadata Catalogue v1.0")
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
      
      menuItem("Metadata catalogue",
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
                          h1("Welcome to the Health & Wellbeing Metadata Catalogue")
                      ), #box
                      
                      
                      p("Click +/- to open and close the sections below",
                        style = "text-align: right;"),
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          collapsible = TRUE, collapsed = TRUE,
                          title = p(strong("What is this dashboard?")),
                          
                          p("This catalogue provides metadata on easily accessible*, 
                          publicly available health and wellbeing indicators for Scotland.
                          It provides information on what data is available, but the 
                          metadata catalogue does not provide the actual data."),
		            
                          p("It has been developed to make it easier to find and use 
                            existing intelligence for learning and decision making related 
                            to health and wellbeing."),
                          
                          p("The catalogue includes data from Public Health Scotland 
                            (PHS), as well as from other data owners such as the Office 
                            for National Statistics, National Records of Scotland, and 
                            Scottish Government. "),
                            
                          p("Indicators from each publication are 
                            grouped by health and wellbeing category, providing an
                            overview of available metrics by topic. Each indicator has
                            a metadata profile that provides available detail on that 
                            item, for example, frequency of data updates, categorical 
                            breakdowns, and data source."),
                          
                          p("The dashboard allows quick identification of relevant 
                            metrics across wide range of topic areas and sources rather
                            than searching multiple places."),
                          
                          h3("How did we decide what indicators to include?"),
                          
                          div(HTML("<ul>
                          <li><strong>*Easily accessible:</strong> it is accessible in a dashboard or report.
                          Indicators that require data manipulation of the parent dataset to extract the data 
                          have not been included. </li>

                          <li><strong>Contemporary:</strong> only data that is routinely updated is included. 
                          Data that is no longer updated but is not older than 10 years old is also included 
                          (e.g. COVID-19 wider impacts dashboard). </li>

                          <li><strong>Scotland:</strong> only datasets that include Scotland are included. </li>

                          <li><strong>Population level:</strong> contains data representative of the population
                          it relates to, i.e. small scale research studies are not included. </li>
                                   </ul>")),
                          
                          
                      ) |> rem_button_aria_label(),
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          collapsible = TRUE, collapsed = TRUE,
                          title = p(strong("Using the data catalogue")),
                          
                          p("The metadata catalogue provides information in table format."),
                          
                          p("Each row is either a:"),
                          
                          div(HTML("<ul>
                          <li>dashboard, </li>
                          <li>indicator or </li>
                          <li>publication </li>
                                   </ul>")),
                          
                          
                          p("There is a range of metadata provided for the dashboards, 
                            publication or indicators. The default setting displays only 
                            three columns (label, dashboard or report name and health &
                            wellbeing topic). More metadata information can be displayed 
                            by selecting additional items from the ‘Columns to display’ 
                            drop downs."),
                          
                          p("Controls in the sidebar allow you to filter and search the 
                            catalogue, for example by geography, equalities, or data source."),
                          
                          p("There is a button which looks like this:",
                            img(src = "catalogue_buttons.png",
                                alt = "screenshot of the buttons found in each row of the catalogue"),
                            "Clicking on the blue part of the button will open a separate
                            tab which will take you to the website where the dashboard, 
                            indicator or publication is hosted. Clicking on the orange
                            part of the button will pop-up a summary of all metadata
                            related to that dashboard, indicator or publication."),
                          
                          p("There is a download button which looks like this:",
                            img(src = "download_button.png", 
                                alt = "screenshot of the download button"), 
                            "This will allow the user to download the metadata table 
                            as an Excel document. The metadata downloaded will have the 
                            chosen filters applied."),
                          
                          p("The left-hand menu can be hidden and revealed by clicking 
                            on this toggle button ",
                            img(src = "sidebar_toggle.png", 
                                alt = "screenshot of the sidebar toggle button"),
                            " at the top of the screen.")
                          
                      ) |> rem_button_aria_label(), 
                      
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          collapsible = TRUE, collapsed = TRUE,
                          title = p(strong("Definitions")),
                          
                          p("The Definitions tab provides a more detailed description of what is contained 
                            within each item. Where possible, this tab also provides a list of possible 
                            values the user can select from. Some do not have this as there are too many 
                            options to display.")
                          
                      ) |> rem_button_aria_label(), 
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          collapsible = TRUE, collapsed = TRUE,
                          title = p(strong("Visualisation")),
                          
                          p("The Visualisation tab provides a link to an alternative way of interacting with 
                            the catalogue, with many of the same filtering and searching options. This option
                            is particularly useful for visualising connections between dashboards and 
                            information gaps by topic area.")
                          
                      ) |> rem_button_aria_label(), 
                      
                      
                      #boxes with collapsible=TRUE below make the sections on the homepage collapsible
                      box(width = 12,
                          collapsible = TRUE, collapsed = FALSE,
                          title = p(strong("Tell us what you think")),
                          
                          p("The dashboard is still in development and is subject to changes and refinements 
                            in the future. Contact ",
                            tags$a(href = "mailto:phs.metadatacatalogue@phs.scot", 
                                   tags$u("phs.metadatacatalogue@phs.scot")), 
                            "for more information or to provide feedback.")
                          
                      ) |> rem_button_aria_label() 
                      
                    ) #fluidRow
           ), #tabPanel
           
           
           
           ##Summary ----
           tabPanel(title = "Summary",
                    
                    fluidRow(
                      box(width = 8,  solidHeader = TRUE,
                          
                          h1("Metadata Catalogue Summary"),
                          
                          p("This page gives a summary of the metadata stored in this catalogue. Each chart is interactive, 
                            hovering over each chart allows you to see more detail for each. Note that in many cases the total 
                            number of items included in the chart may seem to exceed the number of rows in the catalogue. This 
                            is caused by individual rows counting towards multiple health and wellbeing categories at once e.g., 
                            an indicator could be listed under Alcohol: Drugs: Smoking, Early Years & Young People and Health & 
                            Care: Acute & Emergency Services."),
                          
                          p("The metadata catalogue has a total of ", tags$b(as.character(length(dataset[[1]]))),
                            " rows, which are each either a dashboard, an indicator, or a statistical report. 
                            The proportions of these row types are shown in the pie chart to the right."),
                          
                          htmlOutput("int_ext_text")
                          
                      ), #box
                      
                      box(width = 4, solidHeader = TRUE,
                          plotlyOutput("type_chart") |> loading()
                      ) #box
                    ), #fluidRow
                    
                    
                    fluidRow(
                      
                      box(width = 8, solidHeader = TRUE,
                          plotlyOutput("hw_topic_chart")  |> loading()
                      ), #box
                      
                      box(width = 4, solidHeader = TRUE,
                          
                          h2("Health & wellbeing topic"),
                          
                          p("The indicators in this metadata catalogue are split between ",
                            tags$b(as.character(length(hw_topic_options)-1)),
                            " different health and wellbeing topics. The bar graph
                            to the left shows how many rows are related with each topic.")
                          
                          #htmlOutput("hw_topic_text")
                          
                      ) #box
                    ), #fluidRow
                    
                    
                    fluidRow(
                      
                      box(width = 5, solidHeader = TRUE,
                          h2("Tags"),
                          
                          p("The entries in the metadata catalogue are each given tags to help 
                            sort them into groups. There are currently ",
                            tags$b(as.character(length(tag_options)-1)),
                            " different tags throughout the catalogue."),
                          
                          p("A word cloud has been used to show the relative frequency
                            of each tag."),
                          
                          htmlOutput("tags_text") 
                      ), #box
                      
                      box(width = 7, solidHeader = TRUE,
                          plotOutput("tags_chart")  |> loading()
                      ) #box
                    ), #fluidRow
                    
                    
                    fluidRow(
                      box(width = 8, solidHeader = TRUE,
                          plotlyOutput("geographies_chart")  |> loading()
                      ), #box
                      
                      box(width = 4, solidHeader = TRUE,
                          h2("Geographies"),
                          
                          p("Each of the indicators, dashboard and statistical reports give 
                            breakdowns of different types of geography. In total there are ",
                            tags$b(as.character(length(geographies_options))),
                            " different geographical categorisations used.")
                          
                          #htmlOutput("geographies_text")
                      )
                    ), #fluidRow
                    
                    fluidRow(
                      box(width = 4, solidHeader = TRUE,
                          h2("Equality"),
                          
                          p("The equality column in the metadata catalogue shows what
                            equality-related characteristics each entry displays or addresses. 
                            There are currently ",
                            tags$b(as.character(length(equality_options)-1)),
                            " different types of equality-related characteristic represented here."),
                          
                          #htmlOutput("equality_text")
                          ),
                      
                      box(width = 8, solidHeader = TRUE, 
                          plotlyOutput("equality_chart")  |> loading()
                          ) #box
                    ), #fluidRow
                    
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          p("Last updated on ", last_updated)
                      ) #box
                    ) #fluidRow
                    
           ) #tabPanel
           
    ) #tabBox
  ) #fluidRow
) #tabItem
           










#Catalogue ----
catalogue <- tabItem(
  tabName = "catalogue",
  fluidRow(
    tabBox(title = "Metadata Catalogue",
           # The id lets us use input$home_tab on the server to find the current tab
           id = "catalogue_tab",
           width = 12,
           
           tabPanel(title = "Metadata catalogue",
             fluidRow(

                 downloadButton(outputId = "download", #button to download data
                                label = "Download table",
                                icon = shiny::icon("download") |> rem_aria_label(),
                                style = "float:right"
                                ),
                 
                 DTOutput("main_table"),
               
               br(),
               p("Last updated on ", last_updated)
             ) #fluidRow
           ) #tabPanel
    ) #tabBox
  ) #fluidRow
) #tabItem






#Definitons ----
definitions <- tabItem(
  tabName = "definitions",
  fluidRow(
    tabBox(title = "Definitions",
           id = "definitions_tab",
           width = 12,
           
           tabPanel(title = "Definitions",
                    fluidRow(
                      box(width = 10, solidHeader = TRUE,
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
             #summary,
             visualisation,
             version)
  )








#ui ----
#pulls together all the elements made in this script
ui <- tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  tags$style("@import url(https://use.fontawesome.com/releases/v6.0/css/all.css);"),
  tags$head(HTML("<html lang='en'>"),
            tags$link(rel="shortcut icon",
                      href="favicon_phs.ico"), # Icon for browser tab
            tags$title("Health & Wellbeing Metadata Catalogue")
  ),
  
  #pull together the elements created above
  dashboardPage(
    header,
    sidebar,
    body
  ), # dashboardPage
  
  includeScript("www/script.js")
)


# if(PRA) {
#   ui <- ui |> secure_app()
# }

ui



