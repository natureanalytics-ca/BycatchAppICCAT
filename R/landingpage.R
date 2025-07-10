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
               p("Add instructions for app here"),
               
               tags$img(
                 src = "workflow_diagram.png",  # Replace with your actual PNG file name
                 width = "50%",          # Or a fixed value like "300px"
                 alt = "Workflow")    
               
       )
      )
    )
    
  
  )
}