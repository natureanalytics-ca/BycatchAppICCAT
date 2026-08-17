#-------------------------------
#module data checks setup
#-------------------------------

############# UI #############

datachecksResults_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    ### -------------------------
    ### Info bar   
    ### -------------------------
    div (
      style="display: flex; flex-direction: row; flex-wrap: wrap; width: 100%; align-items: center; padding: 0px 32px 10px 0px;",
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
          tags$strong(tags$em("Diagnostics & summary"))
        )
      )
    ),
    br(),
    ###--------------------------------------------
    ### Warning when steps 1 or 2 are not complete
    ###--------------------------------------------
    div(
      id = ns("warningPanelToggle"),
      style = "margin: 0px 29px 0px 0px;",
      valueBox(
        value = tags$strong("Diagnostics & summary is not available"),
        subtitle = tagList(
          br(),
          "You must complete Bycatch Setup before accessing this module."
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
    uiOutput(ns("download_ui")),
    withSpinner(uiOutput(ns("report_ui")))
  )
  
}



