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
          inputId = ns("run_datachecks"),
          label = "Run data checks"
        ),
        # actionBttn(
        #   inputId = "cancel_datachecks",
        #   label = "Cancel"
        # )
        br(),br(),
        uiOutput(ns("download_ui")),
        )
    )

  )
  
}


############# SERVER #############


datachecks_SERVER <- function(id, observerdataInput = reactive(NULL), logbookdataInput = reactive(NULL)){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
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
          choices = c(NA, names(observerdataInput()$dt)),
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
      resultsDir <- reactiveVal(NULL)
      observeEvent(input$run_datachecks,{

        # request any reactive variables
        req(observerdataInput())
        req(logbookdataInput())

        # Create temp dir to hold output
        outDir <- tempfile("bycatch_output_")
        dir.create(outDir)
        
        # Run bycatchSetup function
        tryCatch({ # for debugging
          setupObj <- BycatchEstimator::bycatchSetup(
          obsdat = observerdataInput()$dt,
          logdat = logbookdataInput()$dt,
          yearVar = observerdataInput()$yearColumn,
          obsEffort = observerdataInput()$effortColumn,
          logEffort = logbookdataInput()$effortColumn,
          obsCatch = observerdataInput()$catchColumn,
          catchUnit = observerdataInput()$catchUnits,
          catchType = observerdataInput()$catchType,
          logNum = logbookdataInput()$aggregationColumn,
          sampleUnit = observerdataInput()$sampleUnits,
          factorVariables = input$factor_variables,
          numericVariables = input$numeric_variables,
          baseDir = outDir,
          runName = input$datachecks_name,
          runDescription = input$datachecks_name,
          common = input$spp_name,
          sp = input$spp_scientificname
        )
          print("bycatchSetup ran successfully")
        }, error = function(e) {
          showNotification(paste("Error running bycatchSetup:", e$message), type = "error")
          print(paste("bycatchSetup error:", e$message))
        })
        
        # Save path to generated output
        resultsDir(outDir)
        
        #print(resultsDir())
        #print(setupObj)
      })
      
      output$download_ui <- renderUI({
        req(resultsDir())
        downloadButton(ns("downloadZip"), "Download results as ZIP")
      })
      
      output$downloadZip <- downloadHandler(
        filename = function() {
          "bycatch_results.zip"
        },
        content = function(file) {
          outDir <- resultsDir()
          all_files <- list.files(outDir,recursive = TRUE,full.names = TRUE)
          files_to_zip <- all_files[file.info(all_files)$isdir == FALSE]
          zip(file, files = files_to_zip, flags = "-j")
        },
        contentType = "application/zip"
      )
      
      # observe({
      #   #observerdataInput()$dt
      #   #logbookdataInput()$dt
      #   print(class(observerdataInput()$dt$Catch))
      #   print(class(observerdataInput()$dt$sampled.sets))
      #   print(class(observerdataInput()$catchColumn))
      #   print(class(observerdataInput()$effortColumn))
      #   print(logbookdataInput()$aggregationColumn)
      # })

    })
}

