#START OF SCRIPT ----

function(input, output, session){
  
  
  #Authentication for PRA ----
  res_auth <- shinymanager::secure_server(
    check_credentials = check_credentials(credentials)
  )
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth) 
  })
  
  
  #Selected values ----
  #this "selected" section makes variables not reset when user navigates to another page
  selected <- reactiveValues(columns = c("Label", "Dashboard or report name", "Health & wellbeing topic"),
                             filter_topics = character(0),
                             item_types = c("Indicator", "Dashboard", "Statistical report"),
                             tags = tag_options,
                             equalities = equality_options,
                             geographies = geographies_options,
                             hw_topics = hw_topic_options,
                             int_ext = int_ext_options,
                             sex = sex_options)
  
  
  observeEvent(input$selected_columns_open, 
               selected$columns <- input$selected_columns)
  
  observeEvent(input$selected_filter_topics_open, 
               selected$filter_topics <- input$selected_filter_topics)
  
  observeEvent(input$selected_item_types, 
               selected$item_types <- input$selected_item_types)
  
  observeEvent(input$selected_tags_open, 
               selected$tags <- input$selected_tags)
  
  observeEvent(input$selected_geographies_open,
               selected$geographies <- input$selected_geographies)
  
  observeEvent(input$selected_equalities_open,
               selected$equalities <- input$selected_equalities)
  
  observeEvent(input$selected_int_ext,
               selected$int_ext <- input$selected_int_ext)
  
  observeEvent(input$selected_hw_topics_open,
               selected$hw_topics <- input$selected_hw_topics)
  
  observeEvent(input$selected_sex_open, 
               selected$sex <- input$selected_sex)
  
  
  
  
  #Data table filters ----
  
  ##General search ----
  output$search_box <- renderUI({
    if("catalogue" %in% input$sidebarMenu){
      textInput(
        inputId = "general_search",
        label = "Search:",
        value = "",
        placeholder = "Search the entire metadata catalogue"
      )
    }
  }) 
  
  search_list <- reactive({
    if(length(input$general_search) == 1) {
      return(str_split_1(str_to_lower(input$general_search), " "))
    } else {
      return("")
    }
  })
  
  
  ##Columns ----
  output$column_selector <- renderUI({
    if("catalogue" %in% input$sidebarMenu){
      pickerInput(
        inputId = "selected_columns",
        label = "Columns to display:",
        choices = names(dataset)[!(names(dataset) %in% c("Buttons", "Chr_merge"))], 
        selected = selected$columns, 
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE,
                                selectedTextFormat = "count",
                                size = 10),
        choicesOpt = list(
          style = rep("color: #000000", length(names(dataset))-1) # PHS-purple text
        )
      )
    }
  })
  
  
  
  ##Filter topics ----
  output$filter_topic_selector <- renderUI({
    if("catalogue" %in% input$sidebarMenu){
      pickerInput(
        inputId = "selected_filter_topics",
        label = "Which categories to use for filtering:",
        choices = c("Label", "Health & wellbeing topic", "Tags",
                    "Type", "Produced by PHS", "Data source(s)", "Sex", "Equality", "Geographies"),
        selected = selected$filter_topics, 
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE,
                                selectedTextFormat = "count",
                                size = 10),
        choicesOpt = list(
          #content = tag_options |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>"),
          style = rep("color: #000000", length(names(dataset)))
        )
      )
    }
  })
  
  
  
  
  ##Label ----
  output$label_selector <- renderUI({
    
    labels <- textInput(
      inputId = "selected_labels",
      label = "Label:",
      value = "",
      placeholder = "Search only the Label column"
    )
    
    
    if(("catalogue" %in% input$sidebarMenu) & ("Label" %in% input$selected_filter_topics)) {
      labels
    } else if ("catalogue" %in% input$sidebarMenu) {
      labels |> hidden()
    }
  })
  
  label_search_list <- reactive({
    if(length(input$selected_labels) == 1) {
      return(str_split_1(str_to_lower(input$selected_labels), " "))
    } else {
      return("")
    }
  })
  
  
  ##Type ----
  output$item_type_selector <- renderUI({
    
    if ("Type" %in% input$selected_filter_topics) {
      selected <- selected$item_types
    } else {
      selected <- c("Indicator", "Dashboard", "Statistical report")
    }
    
    item_types <- checkboxGroupInput(
      inputId = "selected_item_types",
      label = "Types of item to display:",
      choices = c("Indicator", "Dashboard", "Statistical report"),
      selected = selected
    )
    
    
    if(("catalogue" %in% input$sidebarMenu) & ("Type" %in% input$selected_filter_topics)) {
      item_types
    } else if ("catalogue" %in% input$sidebarMenu) {
      item_types |> hidden()
    }
  })
  
  
  ##Tags ----
  output$tags_selector <- renderUI({
    
    if ("Tags" %in% input$selected_filter_topics) {
      selected <- selected$tags
    } else {
      selected <- tag_options
    }
    
    tags <- pickerInput(
      inputId = "selected_tags",
      label = "Tags:",
      choices = tag_options,
      selected = selected, 
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE,
                              selectedTextFormat = "count",
                              size = 10),
      choicesOpt = list(
        content = tag_options |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>"),
        style = rep("color: #000000", length(tag_options))
      )
    )
    
    if(("catalogue" %in% input$sidebarMenu) & ("Tags" %in% input$selected_filter_topics)){
      tags
    } else if ("catalogue" %in% input$sidebarMenu) {
      tags |> hidden()
    }
  })
  

  
  
  
  ##Equality ----
  output$equalities_selector <- renderUI({
    if ("Equality" %in% input$selected_filter_topics) {
      selected <- selected$equalities
    } else {
      selected <- equality_options
    }
    
    equalities <- pickerInput(
      inputId = "selected_equalities",
      label = "Equality:",
      choices = equality_options,
      selected = selected, 
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE,
                              selectedTextFormat = "count",
                              size = 10),
      choicesOpt = list(
        content = equality_options |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>"),
        style = rep("color: #000000", length(equality_options))
      )
    )
    
    if(("catalogue" %in% input$sidebarMenu) & ("Equality" %in% input$selected_filter_topics)){
      equalities
    } else if ("catalogue" %in% input$sidebarMenu) {
      equalities |> hidden()
    }
  })
  
  
  
  
  ##Geographies ----
  output$geographies_selector <- renderUI({
    if ("Geographies" %in% input$selected_filter_topics) {
      selected <- selected$geographies
    } else {
      selected <- geographies_options
    }
    
    geographies <- pickerInput(
      inputId = "selected_geographies",
      label = "Geographies:",
      choices = geographies_options,
      selected = selected, 
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE,
                              selectedTextFormat = "count",
                              size = 10),
      choicesOpt = list(
        content = geographies_options |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>"),
        style = rep("color: #000000", length(geographies_options))
      )
    )
    
    if(("catalogue" %in% input$sidebarMenu) & ("Geographies" %in% input$selected_filter_topics)){
      geographies
    } else if ("catalogue" %in% input$sidebarMenu) {
      geographies |> hidden()
    }
  })
  
  
  
  
  ##Health & wellbeing topic----
  output$hw_topic_selector <- renderUI({
    
    if ("Health & wellbeing topic" %in% input$selected_filter_topics) {
      selected <- selected$hw_topics
    } else {
      selected <- hw_topic_options
    }
    
    hw_topics <- pickerInput(
      inputId = "selected_hw_topics",
      label = "Health & wellbeing topics:",
      choices = hw_topic_options,
      selected = selected, 
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE,
                     selectedTextFormat = "count",
                     size = 10),
      choicesOpt = list(
        content = hw_topic_options |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>"),
        style = rep("color: #000000", length(hw_topic_options))
      )
    )
    
    if(("catalogue" %in% input$sidebarMenu) & ("Health & wellbeing topic" %in% input$selected_filter_topics)){
      hw_topics
    } else if ("catalogue" %in% input$sidebarMenu) {
      hw_topics |> hidden()
    }
  })
  
  
  
  
  ##Produced by PHS ----
  output$int_ext_selector <- renderUI({
    
    if ("Produced by PHS" %in% input$selected_filter_topics) {
      selected <- selected$int_ext
    } else {
      selected <- int_ext_options
    }

    int_ext <- checkboxGroupInput(
      inputId = "selected_int_ext",
      label = "Produced by PHS:",
      choices = int_ext_options,
      selected = selected
    )
    
    
    if(("catalogue" %in% input$sidebarMenu) & ("Produced by PHS" %in% input$selected_filter_topics)) {
      int_ext
    } else if ("catalogue" %in% input$sidebarMenu) {
      int_ext |> hidden()
    }
  })
  
  
  
  
  
  
  ##Sex ----
  output$sex_selector <- renderUI({
    
    if ("Sex" %in% input$selected_filter_topics) {
      selected <- selected$sex
    } else {
      selected <- sex_options
    }
    
    sex <- pickerInput(
      inputId = "selected_sex",
      label = "Sex:",
      choices = sex_options, 
      selected = selected, 
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE,
                              selectedTextFormat = "count",
                              size = 10),
      choicesOpt = list(
        style = rep("color: #000000", length(sex_options)) # PHS-purple text
      )
    )
    
    
    if(("catalogue" %in% input$sidebarMenu) & ("Sex" %in% input$selected_filter_topics)) {
      sex
    } else if ("catalogue" %in% input$sidebarMenu) {
      sex |> hidden()
    }
  })
  
  
  
  ##Source ----
  output$source_selector <- renderUI({
    
    sources <- textInput(
      inputId = "selected_sources",
      label = "Data source(s):",
      value = "",
      placeholder = "Search only the Data source(s) column"
    )
    
    
    if(("catalogue" %in% input$sidebarMenu) & ("Data source(s)" %in% input$selected_filter_topics)) {
      sources
    } else if ("catalogue" %in% input$sidebarMenu) {
      sources |> hidden()
    }
  })
  
  source_search_list <- reactive({
    if(length(input$selected_sources) == 1) {
      return(str_split_1(str_to_lower(input$selected_sources), " "))
    } else {
      return("")
    }
  })
  
  
  ##Reset button ----
  output$reset_button <- renderUI({
    if("catalogue" %in% input$sidebarMenu) {
      actionButton(inputId = "reset",
                 label = "Reset filters")
    }
  })
  
  observeEvent(input$reset, {
    selected$columns <- c("Label", "Dashboard or report name", "Health & wellbeing topic")
    selected$filter_topics <- character(0)
    }
    )
  
  
  
  
  #Output data table ----
  filtered_dataset <- reactive({
    dataset |>
      filter(search_string_and(string = Chr_merge, pattern_list = search_list()),

             search_string_and(string = str_to_lower(Label), pattern_list = label_search_list()),

             search_string_and(string = str_to_lower(`Data source(s)`), pattern_list = source_search_list()),

             search_string_or(string = Type, pattern_list = input$selected_item_types),
             
             search_string_or(string = Tags, pattern_list = input$selected_tags),
             search_string_or(string = Equality, pattern_list = input$selected_equalities),
             search_string_or(string = Geographies, pattern_list = input$selected_geographies),
             search_string_or(string = `Health & wellbeing topic`, pattern_list = input$selected_hw_topics),

             ((`Produced by PHS` %in% input$selected_int_ext) | (is.na(`Produced by PHS`) & "[blank]" %in% input$selected_int_ext)),

             ((Sex %in% input$selected_sex) | (is.na(Sex) & "[blank]" %in% input$selected_sex))
             ) |>
      select(Buttons, all_of(input$selected_columns))
  })
  
  
  
  output$main_table <- renderDT({
    if (length(filtered_dataset()) >= 1) {
      
      if("Link(s)" %in% input$selected_columns) {
        data <- filtered_dataset() |>
          mutate(`Link(s)` = str_replace_all(`Link(s)`, "(^|\\r\\n)(.*)(?=$|\\r\\n)", 
                                             '<a href = \\2 target = "_blank"><u>\\2</u></a> '))
      } else {
        data <- filtered_dataset()
      }
      
      
      datatable(data,
                escape = F,
                options = list(scrollX = TRUE,
                               scrollY = "50vh",
                               scrollCollapse = TRUE,
                               searching = FALSE
                               )
                )
    }
    
  })
  
  
  
  #Downloads ----
  output$download <- downloadHandler(
    
    filename = paste0("metadata_catalogue_download_", 
                      today() |> as.character() |> str_replace_all("-", "_"),
                      ".xlsx"),
    
    content = \(file) { 
      #openxlsx::write.xlsx(filtered_dataset() |> select(-Buttons), file) 
      
      
      #load in the download template
      wb <- loadWorkbook("www/catalogue_download_template.xlsx")
      
      
      
      #add the filtered data to the first page
      writeDataTable(wb, 
                     sheet = 1, 
                     startRow = 5,
                     x = filtered_dataset() |> select(-Buttons),
                     tableStyle = "TableStyleLight1", #table style with good colour contrast
                     tableName = "filtered_catalogue"
                     )
      addStyle(wb, 
               sheet = 1,
               rows = 5:(5+length(filtered_dataset()$Buttons)),
               cols = 1:length(names(filtered_dataset())),
               gridExpand = TRUE,
               stack = TRUE,
               style = createStyle(wrapText = TRUE, valign = "center")
               )
      
      if("Description" %in% names(filtered_dataset())) {
        setColWidths(wb,
                     sheet = 1,
                     cols = (which("Description" == names(filtered_dataset())) - 1),
                     width = 35
                     )
      }
      
      
      
      #add list of filters used to the second sheet
      writeDataTable(wb,
                     sheet = 2,
                     startRow = 3,
                     x = tibble(
                       `Filter name` = c(
                         "General Search", 
                         "Columns selected", 
                         "Label", 
                         "Health & wellbeing topic", 
                         "Tags",
                         "Type", 
                         "Produced by PHS", 
                         "Data source(s)", 
                         "Sex", 
                         "Equality", 
                         "Geographies"
                         ),
                       `Options included` = c(
                         if_else(input$general_search != "", paste0('"',input$general_search,'"'), "FILTER NOT USED"),
                         str_flatten_comma(input$selected_columns),
                         if_else(("Label" %in% input$selected_filter_topics & input$selected_labels != ""), paste0('"',input$selected_labels,'"'), "FILTER NOT USED"),
                         if_else("Health & wellbeing topic" %in% input$selected_filter_topics, str_flatten_comma(input$selected_hw_topics), "FILTER NOT USED"),
                         if_else("Tags" %in% input$selected_filter_topics, str_flatten_comma(input$selected_tags), "FILTER NOT USED"),
                         if_else("Type" %in% input$selected_filter_topics, str_flatten_comma(input$selected_item_types), "FILTER NOT USED"),
                         if_else("Produced by PHS" %in% input$selected_filter_topics, str_flatten_comma(input$selected_int_ext), "FILTER NOT USED"),
                         if_else(("Data source(s)" %in% input$selected_filter_topics & input$selected_sources != ""), paste0('"', input$selected_sources, '"'), "FILTER NOT USED"),
                         if_else("Sex" %in% input$selected_filter_topics, str_flatten_comma(input$selected_sex), "FILTER NOT USED"),
                         if_else("Equality" %in% input$selected_filter_topics, str_flatten_comma(input$selected_equalities), "FILTER NOT USED"),
                         if_else("Geographies" %in% input$selected_filter_topics, str_flatten_comma(input$selected_geographies), "FILTER NOT USED")
                         )
                       ), #tibble
                     tableStyle = "TableStyleLight1",
                     tableName = "filters_used"
                     ) #writeDataTable
      
      addStyle(wb, 
               sheet = 2,
               rows = 3:14,
               cols = 1:2,
               gridExpand = TRUE,
               stack = TRUE,
               style = createStyle(wrapText = TRUE, valign = "center")
      )
      
      
      
      
      #add the relevant definitions to the third sheet
      writeDataTable(wb,
                     sheet = 3,
                     startRow = 3,
                     x = definitions |> 
                       select(`Column name`, `Data type`, Definition, `Possible values`) |> 
                       filter(`Column name` %in% input$selected_columns),
                     tableStyle = "TableStyleLight1",
                     tableName = "definitions"
                     )
      
      addStyle(wb, 
               sheet = 3,
               rows = 3:(3+length(names(filtered_dataset()))),
               cols = 1:4,
               gridExpand = TRUE,
               stack = TRUE,
               style = createStyle(wrapText = TRUE, valign = "center")
      )
      
      
      saveWorkbook(wb, file)
      
      }
      
  )
  
  
  
  
  
  
  
  
  #Displaying regular tables ----
  output$definitions_table <- renderTable({
    
    definitions |>
      select(`Column name`, `Data type`, Definition, `Possible values`) |>
      filter(!(`Column name` %in% c("Last updated", "Next updated")))
    
  }, striped = TRUE, bordered = TRUE, na = "", width = "100%")
  
  
  
  
  output$version_table <- renderTable({
    
    versions <- c("0.1", 
                  "0.2",
                  "0.3",
                  "0.4",
                  "1.0"
                  )
    
    dates <- c("18 March 2024",
               "17 April 2024",
               "25 June 2024",
               "01 August 2024",
               "19 August 2024"
               )
    
    changes <- c("Basic skeleton of dashboard created.",
                 "Pre-release alpha build deployed.",
                 "Functionality of dashboard expanded, catalogue data put into more consistent and standard formats.",
                 "Added a summary tab to the home page to provide analytics for the metadata catalogue.",
                 "Official release."
                 )
    
    tibble(Version = versions,
           Date = dates,
           Changes = changes
           )
  },
  striped = TRUE,
  bordered = TRUE)
  
  
  
  
  
  
  
  

  #Pop-up boxes ----
  observeEvent(input$current_id, {

    if(is.character(input$current_id)) {
      row <- as.numeric(str_extract(input$current_id, "\\d*$")) #parse_number(input$current_id)
    } else {
      row <- 1
    }

    modalDialog(
      title = list(h1(dataset$Label[row]),
                   strong(dataset$Type[row] |> str_replace("[|]", "&"))),

      fluidRow(

        fluidRow(
          box(width = 12, solidHeader = TRUE,
            p(dataset$Description[row])
          )
        ),

        fluidRow(
          box(width = 6, solidHeader = TRUE,
              strong("Dashboard or report name:"),
              p(dataset$`Dashboard or report name`[row] |> str_replace_all("[|]", ", "))
          ),

          box(width = 6, solidHeader = TRUE,
              strong("Link(s):"),
              p(HTML(dataset$`Link(s)`[row] |> str_replace_all("(^|\\r\\n)(.*)(?=$|\\r\\n)", 
                                                               '<a href = \\2 target = "_blank"><u>\\2</u></a> '))),
              if(!is.na(dataset$`Link instructions`[row])) {
                p(dataset$`Link instructions`[row])
              }
          )
        ),


        fluidRow(
          box(width = 4, solidHeader = TRUE,
              strong("Produced by PHS:"),
              p(dataset$`Produced by PHS`[row])
          ),

          box(width = 4, solidHeader = TRUE,
              strong("Owner:"),
              p(dataset$Owner[row])
          ),

          box(width = 4, solidHeader = TRUE,
              strong("Data source(s):"),
              p(dataset$`Data source(s)`[row] |> str_replace_all("[|]", ", "))
          )
        ),


        fluidRow(
          box(width = 2, solidHeader = TRUE,
              # strong("Next updated:"),
              # p(dataset$`Next updated`[row])
          ),
          
          box(width = 4, solidHeader = TRUE,
              strong("Frequency:"),
              p(dataset$Frequency[row])
          ),

          box(width = 4, solidHeader = TRUE,
              strong("Trends:"),
              p(dataset$Trends[row])
          ),

          box(width = 2, solidHeader = TRUE,
              # strong("Last updated:"),
              # p(dataset$`Last updated`[row])
          )
        ),



        fluidRow(
          box(width = 4, solidHeader = TRUE,
              strong("Equality:"),
              p(dataset$Equality[row] |> str_replace_all("[|]", ", "))
          ),

          box(width = 4, solidHeader = TRUE,
              strong("Age:"),
              p(dataset$Age[row])
          ),

          box(width = 4, solidHeader = TRUE,
              strong("Sex:"),
              p(dataset$Sex[row])
          )
        ),




        fluidRow(
          box(width = 6, solidHeader = TRUE,
              strong("Geographies:"),
              p(dataset$Geographies[row] |> str_replace_all("[|]", ", "))
          ),

          box(width = 6, solidHeader = TRUE,
              strong("Health & wellbeing topic:"),
              p(dataset$`Health & wellbeing topic`[row] |> str_replace_all("[|]", ", ")),
              strong("Tags:"),
              p(dataset$Tags[row] |> str_replace_all("[|]", ", "))
          )
        )


      ),

      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ) |> showModal()
  })

  
  
  
  
  #Summary tab ----
  
  
  ## type ----
  output$type_chart <- renderPlotly({
    
    type_plot
    # types <- c("Indicator", "Dashboard", "Statistical report")
    # values <- map_int(types,
    #                 \(x) {str_detect((dataset$Type |> replace_na("")), x) |> sum()})
    # 
    # plot_ly(type = "pie",
    #         labels = types,
    #         values = values,
    #         textposition = "inside",
    #         textinfo = "label+percent",
    #         showlegend = FALSE,
    #         hovertemplate = "<b>%{label}</b><br>%{value} rows<extra></extra>",
    #         marker = list(colors = c("#83BB26", "#9B4393", "#3F3685"))
    #         ) |> 
    #   config(displayModeBar = FALSE)
  })
  
  
  output$int_ext_text <- renderText({
    
    int_ext_text
    # x <- dataset$`Produced by PHS` |> replace_na("") |> str_to_lower()
    # 
    # yes <- sum(x == "yes")
    # no <- sum(x == "no")
    # 
    # return(paste0("Of these, <b>", yes, "</b> are produced by PHS and <b>", no, "</b> are produced by other organisations."))
  })
  
  
  ##hw topic ----
  output$hw_topic_chart <- renderPlotly({
    
    hw_topic_plot
    # options <- hw_topic_options
    # options[1] <- "No topic"
    # options <- fct(options)
    # 
    # 
    # values <- map_int(hw_topic_options,
    #                          \(x) {str_detect((dataset$`Health & wellbeing topic` |> replace_na("")), x) |> sum()})
    # 
    # values[1] <- sum(is.na(dataset$`Health & wellbeing topic`))
    # 
    # 
    # plot_ly(type = "bar",
    #         x = options,
    #         y = values,
    #         hovertemplate = "<b>%{x}</b><br>%{y} rows<extra></extra>",
    #         marker = list(color = "#0078D4")
    #         ) |>
    #   layout(xaxis = list(title = "", tickangle = -30)) |> 
    #   config(displayModeBar = FALSE)
    
  })
  
  # output$hw_topic_text <- renderText({
  #   
  #   options <- hw_topic_options[-1]
  #   
  #   values <- map_int(options,
  #                     \(x) {str_detect((dataset$`Health & wellbeing topic` |> replace_na("")), x) |> sum()})
  #   
  #   max <- max(values)
  #   
  #   most_common <- options[values == max] |> str_flatten_comma(last = " and ")
  #   
  #   multiple <- length(options[values == max]) > 1
  #   
  #   plural <- if_else(multiple, "topics are", "topic is")
  #   
  #   
  #   return(paste0("The most common ", plural, " <b>", most_common, "</b> with <b>", max, "</b> entries."))
  #   
  # })


  
  ##tags ----
  output$tags_chart <- renderPlot({
    values <- map_int(tag_options, 
                      \(x) {str_detect((dataset$Tags |> replace_na("")), x) |> sum()})
    
    values[1] <- sum(is.na(dataset$Tags))
    
    set.seed(8)
    
    colours <- c("#3F3685", "#9B4393", "#0078D4", "#83BB26",
                "#948DA3", "#1E7F84", "#6B5C85", "#C73918")
    
    
    
    wordcloud::wordcloud(words = tag_options,
                         freq = values,
                         colors = colours,
                         random.color = TRUE,
                         random.order = FALSE,
                         rot.per = 0,
                         fixed.asp = FALSE
                         )
  })
  
  
  output$tags_text <- renderText({
    
    tags_text
    # options <- tag_options[-1]
    # 
    # values <- map_int(options, 
    #                   \(x) {str_detect((dataset$Tags |> replace_na("")), x) |> sum()})
    # 
    # max <- max(values)
    # 
    # most_common <- options[values == max] |> str_flatten_comma(last = " and ")
    # 
    # multiple <- length(options[values == max]) > 1
    # 
    # plural <- if_else(multiple, "tags are", "tag is")
    # 
    # 
    # return(paste0("The most frequent ", plural, " <b>", most_common, "</b> with <b>", max, "</b> entries."))
    
  })
  
  
  
  ##geographies ----
  output$geographies_chart <- renderPlotly({
    
    geographies_plot
    # values <- map_int(geographies_options, 
    #                   \(x) {str_detect((dataset$Geographies |> replace_na("")), x) |> sum()})
    # 
    # 
    # plot_ly(type = "bar",
    #         x = values,
    #         y = geographies_options |> reorder(length(geographies_options):1),
    #         hovertemplate = "<b>%{y}</b><br>%{x} rows<extra></extra>",
    #         marker = list(color = "#9B4393")
    # ) |>
    #   #layout(xaxis = list(title = "", tickangle = -30)) |> 
    #   config(displayModeBar = FALSE)
    
  })
  
  
  # output$geographies_text <- renderText({
  #   values <- map_int(geographies_options, 
  #                     \(x) {str_detect((dataset$Geographies |> replace_na("")), x) |> sum()})
  #   
  #   max <- max(values)
  #   
  #   most_common <- geographies_options[values == max] |> str_flatten_comma(last = " and ")
  #   
  #   multiple <- length(geographies_options[values == max]) > 1
  #   
  #   plural <- if_else(multiple, "geographical breakdowns are", "geographical breakdown is")
  #   
  #   
  #   return(paste0("The most common ", plural, " <b>", most_common, "</b> with <b>", max, "</b> entries."))
  #   
  # })
  
  
  
  
  ##equality ----
  output$equality_chart <- renderPlotly({
    
    equalities_plot
    # options <- equality_options
    # options[1] <- "None"
    # options <- reorder(options, length(options):1)
    # 
    # values <- map_int(equality_options, 
    #                   \(x) {str_detect((dataset$Equality |> replace_na("")), x) |> sum()})
    # 
    # values[1] <- sum(is.na(dataset$Equality))
    # 
    # 
    # plot_ly(type = "bar",
    #         x = values,
    #         y = options,
    #         hovertemplate = "<b>%{y}</b><br>%{x} rows<extra></extra>",
    #         marker = list(color = "#3F3685")
    # ) |>
    #   #layout(xaxis = list(title = "", tickangle = -30)) |> 
    #   config(displayModeBar = FALSE)
    
  })
  
  
  
  # output$equality_text <- renderText({
  #   
  #   options <- equality_options[-1]
  #   
  #   values <- map_int(options, 
  #                     \(x) {str_detect((dataset$Equality |> replace_na("")), x) |> sum()})
  #   
  #   
  #   max <- max(values)
  #   
  #   most_common <- options[values == max] |> str_flatten_comma(last = " and ")
  #   
  #   multiple <- length(options[values == max]) > 1
  #   
  #   plural <- if_else(multiple, "categories are", "category is")
  #   
  #   
  #   return(paste0("The most common ", plural, " <b>", most_common, "</b> with <b>", max, "</b> entries."))
  # })
  
  
  
  
  
  
  
  
  
  
  
  
  #Testing! ----
  #dev panel to display underlying values, uncomment to include
  
  # output$testing <- renderPrint({
  #   str_view(c(
  #     paste0("A: ", input$current_id),
  #     paste0("B: ", as.numeric(str_extract(input$current_id, "\\d*$")))
  #     ))
  # })

  
  
  
  
  
}