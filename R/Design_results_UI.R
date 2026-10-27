#-------------------------------
#module data checks setup
#-------------------------------

############# UI #############

designResults_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
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
          div(
            dashboardBadge("Part 2", color = "primary", position = "left"),
            HTML("&nbsp;"),
            tags$strong(tags$em("Analysis results"))
          )
        )
      )
    ),
    br(),
    ###--------------------------------------------
    ### Warning when steps 1 or 2 are not complete
    ###--------------------------------------------
    div(
      id = ns("warningPanelToggle"),
      valueBox(
        value = tags$strong("Analysis results are not available"),
        subtitle = tagList(
          br(),
          "Complete the specifications step before viewing Part 2.",
          br(),
          "If you have done so, review Part 1 for missing entries in mandatory fields and warning messages."
        ),
        icon = icon("info"),
        color = "secondary",
        width = 12,
        href = NULL,
        footer = NULL,
        gradient = TRUE,
        elevation = NULL
      )
    ),
    withSpinner(uiOutput(ns("report_ui")))
  )
  
}



