#-------------------------------
#Setup instructions
#-------------------------------


datachecksInstuctions_UI <- function(id){
  
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
      collapsible = FALSE,
      collapsed = FALSE,
      width = 12,
      title = "Data checks",
      fluidRow(
        column(12,
               p("Make sure to complete Bycatch setup prior to accessing this module. Data checks provides the following:"),
               tags$ul(
                 tags$li(strong("Review of data"),": summaries of data quantity and quality, such as observer coverage levels."),
                 tags$li(strong("Plots and tables"),": always plot your data before proceeding to analysis!."),
                 tags$li(strong("Warnings"),": missing or likely problematic data points are flagged with warnings.")
               )
        )
      )
    )
  )
}


