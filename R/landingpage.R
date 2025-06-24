#-------------------------------
#module Landing page
#-------------------------------


landingpage_UI <- function(id){
  
  ns <- NS(id)
  
  tagList( #does it need to be wrapped in tagList?
    
    box(
      collapsible = FALSE,
      solidHeader = TRUE,
      width = 12,
      title = "Welcome to the BycatchEstimator toolkit",
      fluidRow(
        column(12,
               "Add instructions for app here")
      )
    )
    
  
  )
}