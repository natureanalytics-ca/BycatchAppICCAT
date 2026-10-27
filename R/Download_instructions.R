#-------------------------------
#Setup instructions
#-------------------------------


downloadInstuctions_UI <- function(id){
  
  ns <- NS(id)
  
  tagList( #does it need to be wrapped in tagList?
    
    ### -------------------------
    ### Info bar   
    ### -------------------------
    div (
      style="display: flex; flex-direction: row; flex-wrap: wrap; width: 100%; align-items: center; padding: 0px 0px 10px 0px;",
      # Dataset name
      div(
        style = "
          flex-grow: 100; 
          align-content: stretch; 
          background:
            linear-gradient(
              to right, 
              rgb(173, 181, 189, 0) 0%,
              #ADB5BD 100%
               
            )
            left 
            bottom
            no-repeat; 
          border-radius: 3px; 
          border-style: none; 
          color: #343a40;
          margin: 0px;"
        ,
        div(
          style = "float: left; padding: 4px 0px;",
          tags$strong(tags$em("Instructions"))
        )
      )
    ),
    
    box(
      collapsible = TRUE,
      collapsed = FALSE,
      width = 12,
      title = "Download",
      fluidRow(
        column(12,
               p("At minimum, make sure to complete Bycatch setup prior to accessing this module."),
               tags$ul(
                 tags$li(strong("Bycatch setup"),": Download will include all setup files."),
                 tags$li(strong("Data checks"),": Download will include the diagnostics and summary report."),
                 tags$li(strong("Design-based estimators"),": If an analysis has been completed, download will include analysis files and analysis report.")
               )
       )
      )
    )
  )
}


