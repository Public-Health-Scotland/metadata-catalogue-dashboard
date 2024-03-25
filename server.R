function(input, output, session){
  
  
  
  output$main_table <- renderDT({
    
    datatable(dataset,
              options = list(scrollX = TRUE,
                             scrollY = "54vh",
                             scrollCollapse = TRUE),
              filter = "top"
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
    
    content = \(file) { openxlsx::write.xlsx(dataset, file) }
      
  )
  
  
  
  output$definitions_table <- renderTable({
    
    names <- c("Label",
               "Type",
               "Tags",
               "Description",
               "Age",              
               "Dashboard report name",
               "Data source(s)",
               "Duplicate",
               "Equality",
               "Frequency",
               "Geographies",
               "Health & wellbeing topic",
               "Internal/external",
               "Last updated",
               "Link instructions",
               "Link(s)",
               "Next updated",
               "Owner",
               "Phs publication topic",
               "Sex",
               "Trends from")
    
    definitions <- c("The name of the item.",
                     "Whether the item is a dashboard, a statistical report, or an indicator.",
                     "Tags on each item which aid filtering of types of item.",
                     "A description of the item.",
                     "The age ranges that the data the item applies to.",
                     "When the item is an indicator, this shows which dashboards/reports this indicator is displayed in.",
                     "Where the data for this item is drawn from.",
                     "Marker to show whether this item in the table is a duplicate? Unclear what this column is.",
                     "List of which equality characteristics the item concerns.",
                     "How frequently the data in the item is updated.",
                     "What geographical levels the data displayed by the item is broken down by.",
                     "Which health & wellbeing topics the item concerns.",
                     "Whether the item is produced internally (ie by PHS) or externally (ie by an organisation other than PHS).",
                     "When the data in the item was last updated.",
                     "In cases where following the link in the following column will not lead directly to the item, these instructions explain how to reach the item from where the link leads.",
                     "A link or links to where the data item can be found.",
                     "When the data in the item will next be updated.",
                     "Which organisation produces the item.",
                     "Which topic or topics the item concerns.",
                     "What sex or sexes the item concerns.",
                     "The time range that the data in the item includes."
                     )
    
    
    #names <- names(dataset)
    #definitions <- names(dataset) |> str_to_upper()
    
    
    tibble(`Column Name` = names,
           Definition = definitions)
    
  }, striped = TRUE, bordered = TRUE)
  
  
  
  
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
  
  
  
  
  
}