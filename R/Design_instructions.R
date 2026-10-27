#-------------------------------
#Setup instructions
#-------------------------------


designInstuctions_UI <- function(id){
  
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
      title = "Design-based estimators",
      fluidRow(
        column(12,
               p("Make sure to complete Bycatch setup prior to accessing this module. Design-based estimators provide the following:"),
               tags$ul(
                 tags$li(strong("Estimates of bycatch"),": total annual bycatch produced by expanding a sample, such as an observer database to the total effort from logbooks or landings records."),
                 tags$li(strong("Multiple analytical methods"),": includes Ratio and Delta methods."),
                 tags$li(strong("Uncertainty envelopes"),":  bycatch estimates by year are provided along with standard errors and a figure showing the estimates by year with 95% confidence intervals.")
               )
       )
      )
    )
  )
}


