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
  selected <- reactiveValues(columns = c("Label", "Dashboard report name", "Health & wellbeing topic"),
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
        placeholder = "Search the entire catalogue"
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
        choices = names(dataset)[-((length(names(dataset))-1):length(names(dataset)))], 
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
      actionBttn(inputId = "reset",
                 label = "Reset filters",
                 color = "danger")
    }
  })
  
  observeEvent(input$reset, {
    selected$columns <- c("Label", "Dashboard report name", "Health & wellbeing topic")
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
      datatable(filtered_dataset(),
                escape = F,
                options = list(scrollX = TRUE,
                               scrollY = "50vh",
                               scrollCollapse = TRUE,
                               searching = FALSE
                               )
                )
    }
    
  })
  
  output$download_table <- renderUI({
    if("catalogue" %in% input$sidebarMenu){
      downloadBttn(outputId = "download", #button to download data
                   label = "Download table",
                   icon = shiny::icon("download") |> rem_aria_label(),
                   color = "royal"
      )
    }
  })
  
  
  output$download <- downloadHandler(
    
    filename = paste0("metadata_download_", 
                      today() |> as.character() |> str_replace_all("-", "_"),
                      ".xlsx"),
    
    content = \(file) { openxlsx::write.xlsx(filtered_dataset(), file) }
      
  )
  
  
  
  
  
  
  
  
  #Displaying regular tables ----
  output$definitions_table <- renderTable({
    
    definitions |>
      select(`Column name`, `Data type`, Definition, `Possible values`)
    
  }, striped = TRUE, bordered = TRUE, na = "", width = "100%")
  
  
  
  
  output$version_table <- renderTable({
    
    versions <- c("0.1", 
                  "0.2",
                  "0.3",
                  "1.0 (planned)"
                  )
    
    dates <- c("18 March 2024",
               "17 April 2024",
               "25 June 2024",
               "Autumn 2024"
               )
    
    changes <- c("Basic skeleton of dashboard created",
                 "Pre-release alpha build deployed",
                 "Functionality of dashboard expanded, catalogue data put into more consistent and standard formats.",
                 "Final release"
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
              strong("Dashboard report name:"),
              p(dataset$`Dashboard report name`[row] |> str_replace_all("[|]", ", "))
          ),

          box(width = 6, solidHeader = TRUE,
              strong("Link(s):"),
              p(dataset$`Link(s)`[row]),
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
          box(width = 3, solidHeader = TRUE,
              strong("Frequency:"),
              p(dataset$Frequency[row])
          ),

          box(width = 3, solidHeader = TRUE,
              strong("Trends:"),
              p(dataset$Trends[row])
          ),

          box(width = 3, solidHeader = TRUE,
              strong("Last updated:"),
              p(dataset$`Last updated`[row])
          ),

          box(width = 3, solidHeader = TRUE,
              strong("Next updated:"),
              p(dataset$`Next updated`[row])
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

  
  
  
  
  
  
  #Testing! ----
  #dev panel to display underlying values, uncomment to include
  
  # output$testing <- renderPrint({
  #   str_view(c(
  #     paste0("A: ", input$current_id),
  #     paste0("B: ", as.numeric(str_extract(input$current_id, "\\d*$")))
  #     ))
  # })

  
  
  
  
  
}