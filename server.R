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
  selected <- reactiveValues(columns = names(dataset),
                             filter_topics = character(0),
                             item_types = c("Indicator", "Dashboard", "Statistical report"),
                             tags = tag_options,
                             duplicate = TRUE,
                             equalities = equality_options,
                             geographies = geographies_options,
                             hw_topics = hw_topic_options,
                             int_ext = int_ext_options,
                             phs_pub_topics = phs_pub_topic_options,
                             sex = sex_options)
  
  
  observeEvent(input$selected_columns, 
               selected$columns <- input$selected_columns)
  
  observeEvent(input$selected_filter_topics, 
               selected$filter_topics <- input$selected_filter_topics)
  
  observeEvent(input$selected_item_types, 
               selected$item_types <- input$selected_item_types)
  
  observeEvent(input$selected_tags, 
               selected$tags <- input$selected_tags)
  
  observeEvent(input$selected_duplicate,
               selected$duplicate <- input$selected_duplicate)
  
  observeEvent(input$selected_geographies,
               selected$geographies <- input$selected_geographies)
  
  observeEvent(input$selected_equalities,
               selected$equalities <- input$selected_equalities)
  
  observeEvent(input$selected_int_ext,
               selected$int_ext <- input$selected_int_ext)
  
  observeEvent(input$selected_hw_topics,
               selected$hw_topics <- input$selected_hw_topics)
  
  observeEvent(input$selected_phs_pub_topics,
               selected$phs_pub_topics <- input$selected_phs_pub_topics)
  
  observeEvent(input$selected_sex, 
               selected$sex <- input$selected_sex)
  
  
  
  
  #Data table filters ----
  ##Columns ----
  output$column_selector <- renderUI({
    if("data_table" %in% input$sidebarMenu){
      pickerInput(
        inputId = "selected_columns",
        label = "Columns to display:",
        choices = names(dataset), 
        selected = selected$columns, 
        multiple = TRUE,
        options = list(`actions-box` = TRUE,
                       size = 10),
        choicesOpt = list(
          style = rep("color: #000000", length(names(dataset))) # PHS-purple text
        )
      )
    }
  })
  
  
  ##Filter topics ----
  output$filter_topic_selector <- renderUI({
    if("data_table" %in% input$sidebarMenu){
      pickerInput(
        inputId = "selected_filter_topics",
        label = "Which categories to use for filtering:",
        choices = c("Type", "Tags", "Duplicate", "Equality", "Geographies","Health & wellbeing topic", 
                    "Internal/external", "PHS publication topic", "Sex"), 
        selected = selected$filter_topics, 
        multiple = TRUE,
        options = list(`actions-box` = TRUE,
                       size = 10),
        choicesOpt = list(
          #content = tag_options |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>"),
          style = rep("color: #000000", length(names(dataset)))
        )
      )
    }
  })
  
  
  ##Type ----
  output$item_type_selector <- renderUI({
    
    if ("Type" %in% input$selected_filter_topics) {
      selected <- selected$item_types
    } else {
      selected <- c("Indicator", "Dashboard", "Statistical report")
    }
    
    # item_types <- pickerInput(
    #   inputId = "selected_item_types",
    #   label = "Types of item to display:",
    #   choices = c("Indicator", "Dashboard", "Statistical report"), 
    #   selected = selected, 
    #   multiple = TRUE,
    #   options = list(`actions-box` = TRUE,
    #                  size = 10),
    #   choicesOpt = list(
    #     style = rep("color: #000000", 3) # PHS-purple text
    #   )
    # )
    
    item_types <- checkboxGroupInput(
      inputId = "selected_item_types",
      label = "Types of item to display:",
      choices = c("Indicator", "Dashboard", "Statistical report"),
      selected = selected
    )
    
    
    if(("data_table" %in% input$sidebarMenu) & ("Type" %in% input$selected_filter_topics)) {
      item_types
    } else if ("data_table" %in% input$sidebarMenu) {
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
      options = list(`actions-box` = TRUE,
                     size = 10),
      choicesOpt = list(
        content = tag_options |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>"),
        style = rep("color: #000000", length(tag_options))
      )
    )
    
    if(("data_table" %in% input$sidebarMenu) & ("Tags" %in% input$selected_filter_topics)){
      tags
    } else if ("data_table" %in% input$sidebarMenu) {
      tags |> hidden()
    }
  })
  
  
  
  ##Duplicate ----
  output$duplicate_selector <- renderUI({
    if(("data_table" %in% input$sidebarMenu) & ("Duplicate" %in% input$selected_filter_topics)){
      radioButtons(
        inputId = "selected_duplicate",
        label = "Duplicate:",
        choices = c(TRUE, FALSE),
        selected = selected$duplicate,
        inline = TRUE
      )
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
      options = list(`actions-box` = TRUE,
                     size = 10),
      choicesOpt = list(
        content = equality_options |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>"),
        style = rep("color: #000000", length(equality_options))
      )
    )
    
    if(("data_table" %in% input$sidebarMenu) & ("Equality" %in% input$selected_filter_topics)){
      equalities
    } else if ("data_table" %in% input$sidebarMenu) {
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
      options = list(`actions-box` = TRUE,
                     size = 10),
      choicesOpt = list(
        content = geographies_options |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>"),
        style = rep("color: #000000", length(geographies_options))
      )
    )
    
    if(("data_table" %in% input$sidebarMenu) & ("Geographies" %in% input$selected_filter_topics)){
      geographies
    } else if ("data_table" %in% input$sidebarMenu) {
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
      options = list(`actions-box` = TRUE,
                     size = 10),
      choicesOpt = list(
        content = hw_topic_options |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>"),
        style = rep("color: #000000", length(hw_topic_options))
      )
    )
    
    if(("data_table" %in% input$sidebarMenu) & ("Health & wellbeing topic" %in% input$selected_filter_topics)){
      hw_topics
    } else if ("data_table" %in% input$sidebarMenu) {
      hw_topics |> hidden()
    }
  })
  
  
  
  
  ##Internal/external ----
  output$int_ext_selector <- renderUI({
    
    if ("Internal/external" %in% input$selected_filter_topics) {
      selected <- selected$int_ext
    } else {
      selected <- int_ext_options
    }
    
    # int_ext <- pickerInput(
    #   inputId = "selected_int_ext",
    #   label = "Internal/external:",
    #   choices = int_ext_options, 
    #   selected = selected, 
    #   multiple = TRUE,
    #   options = list(`actions-box` = TRUE,
    #                  size = 10),
    #   choicesOpt = list(
    #     style = rep("color: #000000", length(int_ext_options)) # PHS-purple text
    #   )
    # )
    
    int_ext <- checkboxGroupInput(
      inputId = "selected_int_ext",
      label = "Internal/external:",
      choices = int_ext_options,
      selected = selected
    )
    
    
    if(("data_table" %in% input$sidebarMenu) & ("Internal/external" %in% input$selected_filter_topics)) {
      int_ext
    } else if ("data_table" %in% input$sidebarMenu) {
      int_ext |> hidden()
    }
  })
  
  
  
  
  
  ##PHS publication topic ----
  output$phs_pub_topic_selector <- renderUI({
    
    if ("PHS publication topic" %in% input$selected_filter_topics) {
      selected <- selected$phs_pub_topics
    } else {
      selected <- phs_pub_topic_options
    }
    
    phs_pub_topics <- pickerInput(
      inputId = "selected_phs_pub_topics",
      label = "PHS publication topics:",
      choices = phs_pub_topic_options,
      selected = selected, 
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     size = 10),
      choicesOpt = list(
        content = phs_pub_topic_options |> str_wrap(width = 20) |> str_replace_all("\\n", "<br>"),
        style = rep("color: #000000", length(phs_pub_topic_options))
      )
    )
    
    if(("data_table" %in% input$sidebarMenu) & ("PHS publication topic" %in% input$selected_filter_topics)){
      phs_pub_topics
    } else if ("data_table" %in% input$sidebarMenu) {
      phs_pub_topics |> hidden()
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
      options = list(`actions-box` = TRUE,
                     size = 10),
      choicesOpt = list(
        style = rep("color: #000000", length(sex_options)) # PHS-purple text
      )
    )
    
    
    if(("data_table" %in% input$sidebarMenu) & ("Sex" %in% input$selected_filter_topics)) {
      sex
    } else if ("data_table" %in% input$sidebarMenu) {
      sex |> hidden()
    }
  })
  
  
  
  
  
  
  
  #Output data table ----
  filtered_dataset <- reactive({
    dataset |>
      filter(Type %in% input$selected_item_types,
             search_string(string = Tags,  
                           pattern_list = input$selected_tags),
             ((!is.na(Duplicate) %in% input$selected_duplicate) | !("Duplicate" %in% input$selected_filter_topics)),
             search_string(string = Equality,
                           pattern_list = input$selected_equalities),
             search_string(string = Geographies,
                           pattern_list = input$selected_geographies),
             search_string(string = `Health & wellbeing topic`,
                           pattern_list = input$selected_hw_topics),
             ((`Internal/external` %in% input$selected_int_ext) | (is.na(`Internal/external`) & "[blank]" %in% input$selected_int_ext)),
             search_string(string = `PHS publication topic`,
                           pattern_list = input$selected_phs_pub_topics),
             ((Sex %in% input$selected_sex) | (is.na(Sex) & "[blank]" %in% input$selected_sex))
             ) |>
      select(all_of(input$selected_columns))
  })
  
  
  
  output$main_table <- renderDT({
    
    datatable(filtered_dataset(),
              options = list(scrollX = TRUE,
                             scrollY = "55vh",
                             scrollCollapse = TRUE
                             )
              )
    
  })
  
  output$download_table <- renderUI({
    if("data_table" %in% input$sidebarMenu){
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
      select(`Column name`, `Data type`, Definition)
    
  }, striped = TRUE, bordered = TRUE, na = "")
  
  
  
  
  output$version_table <- renderTable({
    
    versions <- c("0.1", 
                  "1.0 (planned)"
                  )
    
    dates <- c("18 March 2024",
               "__ _____ 2024"
               )
    
    changes <- c("Basic skeleton of dashboard created",
                 "Final release"
                 )
    
    tibble(Version = versions,
           Date = dates,
           Changes = changes
           )
  },
  striped = TRUE,
  bordered = TRUE)
  
  
  
  
  #Testing! ----
  #dev panel to display underlying values, uncomment to include
  
  # output$testing <- renderPrint({
  #   str_view(c(
  #     paste0("A: ", input$selected_filter_topics),
  #     paste0("B: ", selected$filter_topics),
  #     paste0("C: "),
  #     paste0("D: ")
  #     ))
  # })

  
}