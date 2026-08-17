#-------------------------------
#module help
#-------------------------------


############# UI #############

help_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    
    box(
      collapsible = FALSE,
      collapsed = FALSE,
      width = 12,
      title = "Online resources",
      tags$ul(
        tags$li(tags$a(href = "https://github.com/ebabcock/BycatchEstimator","BycatchEstimator R package github",target = "_blank")),
        tags$li(tags$a(href = "https://ebabcock.github.io/BycatchEstimator/","BycatchEstimator R package User Guide",target = "_blank"))
        
      )

    )
  
  )
}


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  