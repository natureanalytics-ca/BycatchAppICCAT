#-------------------------------
#module data checks
#-------------------------------

############# UI #############

datachecks_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    box(
      title = "Data checks and diagnostics",
      width = 12,
      collapsible = FALSE,
      
      uiOutput(ns("observer_dataset_title")),
      
      uiOutput(ns("logbook_dataset_title")),
      
      uiOutput(ns("factorvariables_ui")),
      
      uiOutput(ns("numericvariables_ui")),

      textInput(
        inputId = ns("spp_name"),
        label = "Species common name",
        width = '100%'
      ),
      textInput(
        inputId = ns("spp_scientificname"),
        label = "Species scientific name",
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


############# SERVER #############


datachecks_SERVER <- function(id, observerdataInput = reactive(NULL), logbookdataInput = reactive(NULL)){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      observe({
        observerdataInput()$dt
        logbookdataInput()$dt
        print(logbookdataInput()$title)
        print(observerdataInput()$catchColumn)
        print(observerdataInput()$catchUnits)
      })
      
      output$observer_dataset_title <- renderUI({
        textInput(
          inputId = ns("observer_dataset"),
          label = "Observer data set",,
          value = observerdataInput()$title,
          width = '100%'
        )
      })
      
      output$logbook_dataset_title <- renderUI({
        textInput(
          inputId = ns("logbook_dataset"),
          label = "Logbook data set",,
          value = logbookdataInput()$title,
          width = '100%'
        )
      })

      output$factorvariables_ui <- renderUI({
        pickerInput(
          inputId = ns("factor_variables"),
          label = "Specify factor variables",
          choices = names(observerdataInput()$dt),
          width = '100%',
          options = pickerOptions(
            actionsBox = FALSE,
            size = 10,
            selectedTextFormat = "count > 4"
          ),
          multiple = TRUE
        )
      })
      
      output$numericvariables_ui <- renderUI({
        pickerInput(
          inputId = ns("numeric_variables"),
          label = "Specify numeric variables",
          choices = names(observerdataInput()$dt),
          width = '100%',
          options = pickerOptions(
            actionsBox = FALSE,
            size = 10,
            selectedTextFormat = "count > 4"
          ),
          multiple = TRUE
        )
      })
  
      ## Button for running data checks
      ## Call to bycatchSetup in BycatchEstimator package
      

      
    })
}