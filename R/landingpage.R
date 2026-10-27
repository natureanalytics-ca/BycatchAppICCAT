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
                 tags$li(strong("Bycatch setup"),": upload observer data and logbook data."),
                 tags$li(strong("Data checks"),": review data inputs through statistical summaries and plots, making sure data are appropriate for bycatch estimation."),
                 tags$li(strong("Design-based estimators"),": produce estimates of bycatch by expanding a sample, such as an observer database to the total effort from logbooks or landings records."),
                 tags$li(strong("Download outputs"),": retain outputs that can be downloaded within a zip folder.")
               ),
               p("On the ",strong("Help"), "page, links are provided to this web application's user guide and related R package documentation."),
                 
                            
               tags$img(
                 src = "workflow_diagram2.png",  
                 style="display: block; padding-top: 20px; margin: 0 auto; width: 60%;",        
                 alt = "Workflow")    
               
       )
      )
    )
    
  
  )
}