#-------------------------------
#module help
#-------------------------------

help_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
  
    box(
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      title = "Observer data - example data set"
    ),
    box(
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      title = "Logbook data - example data set"
    )
  
  )
}