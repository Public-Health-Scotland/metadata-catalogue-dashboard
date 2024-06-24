
#function adds a loading spinner with phs-teal colour
#wrap any output in this function to make it show this while it loads
# loading <- function(whats_loading){
#   withSpinner(whats_loading, type = 5, color = "#1E7F84", size = 0.5)
# }



#function copied from internet that takes a shiny menu and deletes the unnecessary
#div element that could confuse screen readers
accessible_menu = function(bad_menu) {
  tab_input = tags$script(
    "
function customMenuHandleClick(e) {
  let n = $(e.currentTarget).find('a')[0].dataset.value;
  doSomethingWith(n);
}
function doSomethingWith(val) {
  Shiny.setInputValue('sidebarMenu', val);
}
$(document).ready(
  function() {
    $('ul.sidebar-menu li').click(customMenuHandleClick)
  });
"
  )
  bad_menu$children[[length(bad_menu$children)]] = NULL
  real_menu = tagList(bad_menu, tab_input)
  real_menu
}

#function which removes the aria-label attribute that shiny automatically
#puts on icons, despite it often not being needed
rem_aria_label <- function(icon) {
  icon[["attribs"]][["aria-label"]] = NULL
  return(icon)
}

#similar function to the above, but it operates on a tree-view menu
rem_menu_aria_label <- function(menu) {
  menu[["children"]][[1]][["children"]][[3]][["attribs"]][["aria-label"]] = NULL
  return(menu)
}

rem_button_aria_label <- function(box) {
  box[["children"]][[1]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["children"]][[1]][["attribs"]][["aria-label"]] = NULL
  box[["children"]][[1]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]][["title"]] = "open and close button" 
  return(box)
}







#takes a list of strings and single string, and searches the string to see if ANY patterns on the list match it.
search_string_or <- function(string, pattern_list){
  
  if(length(pattern_list) == 0) {
    return(FALSE)
  }
  
  
  return_when_empty <- "[blank]" %in% pattern_list
  empty <- is.na(string)
  
  pattern_match <- pattern_list |>
    str_replace_all("\\W", "\\\\W") |>
    str_flatten(")|(") %>%
    paste0("(", ., ")") %>%
    str_detect(string = string, pattern = .)
  
  return(pattern_match | (empty & return_when_empty))
  
}


#takes a list of strings and single string, and searches the string to see if ALL patterns on the list match it.
search_string_and <- function(string, pattern_list){
  
  if(length(pattern_list) == 0) {
    return(FALSE)
  }
  
  return_when_empty <- pattern_list == ""
  empty <- is.na(string)
  
  pattern_match <- pattern_list |>
    str_replace_all("\\W", "\\\\W") |>
    str_flatten(")(?=.*") %>%
    paste0("(?=.*", ., ").*") %>%
    str_detect(string = string, pattern = .)
  
  return(pattern_match | (empty & return_when_empty))
  
}










