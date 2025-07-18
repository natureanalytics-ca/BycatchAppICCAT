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
               p("The BycatchEstimator app is an alternative interface to the BycatchEstimator R package. The app is organized as:"),
               tags$ol(
                 tags$li(strong("Data upload"),": where the user is required to attach their observer data and logbook data."),
                 tags$li(strong("Data checks setup"),": where the user needs to specify the data structures for both data sets and 
                 variables that are important to consider for bycatch estimation."),
                 tags$li(strong("Data checks results"),": after running data checks, the tool will produce a number of outputs that can be downloaded as a zip folder.")
               ),
               p("In the ",strong("Help"), "page, the user can find example data sets that can be downloaded, and links to the R package documentation."),
                 
                            
               tags$img(
                 src = "workflow_diagram.png",  
                 width = "50%",        
                 alt = "Workflow")    
               
       )
      )
    )
    
  
  )
}