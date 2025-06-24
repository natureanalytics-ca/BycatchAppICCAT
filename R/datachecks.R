#-------------------------------
#module data checks
#-------------------------------

datachecks_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    box(
      title = "Data checks and diagnostics",
      width = 12,
      collapsible = FALSE,
      
      pickerInput(
        inputId = ns("observer_dataset"),
        label = "Observer data set",
        choices = "Name/title of observer data set"
      ),   
      pickerInput(
        inputId = ns("logbook_dataset"),
        label = "Logbook data set",
        choices = "Name/title of logbook data set"
      ),
      pickerInput(
        inputId = ns("factor_variables"),
        label = "Specify factor variables",
        choices = c("X1","X2","X3","X4")
      ),
      pickerInput(
        inputId = ns("numeric_variables"),
        label = "Specify numeric variables",
        choices = c("X1","X2","X3","X4")
      ),
      textInput(
        inputId = ns("spp_name"),
        label = "Species name",
        width = '100%'
      ),
      textInput(
        inputId = ns("datachecks_name"),
        label = "Add a brief description of data checks run",
        width = '100%'
      ),
      br(),
      br(),
      div(
        style = "display: flex; align-items: center; justify-content: center;",
        actionBttn(
          inputId = "run_datachecks",
          label = "Run data checks"
        ),
        actionBttn(
          inputId = "cancel_datachecks",
          label = "Cancel"
        ))
    )

  )
  
}
