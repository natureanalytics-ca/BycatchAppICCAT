

server <- function(input, output, session) {

  #-------------------
  #Temp directory
  #-------------------
  #Created once per user session
  outDir <- tempfile(pattern = "bycatch_output_")
  dir.create(outDir)
  
  #Automatically destroyed when they close the browser
  session$onSessionEnded(function() {
    unlink(outDir, recursive = TRUE, force = TRUE)
  })
  
  #------------------
  #Bycatch setup module
  #------------------
  setupInstuctions_SERVER("setuppage")
  upload_observer_SERVER <- observerupload_SERVER("observerdata_upload")
  upload_logbook_SERVER <- logbookupload_SERVER("logbookdata_upload")
  
  setup_datachecks_SERVER <- datachecksSetup_SERVER(
    "datachecks_setup",
    observerdataInput = upload_observer_SERVER,
    logbookdataInput = upload_logbook_SERVER,
    outDir = outDir
  )
  
  shinyjs::onclick("observerdata_upload-observerupload_save", {
    updateControlbarMenu(
      session = session,
      inputId = "sidebar",
      selected = "Logbook_upload"
    )
    shinyjs::runjs("window.scrollTo(0, 0);")
  })
  
  shinyjs::onclick("logbookdata_upload-logbookupload_save", {
     updateControlbarMenu(
       session = session,
       inputId = "sidebar",
       selected = "setup_checks"
     )
     shinyjs::runjs("window.scrollTo(0, 0);")
  })
  
  observeEvent(upload_observer_SERVER()$formComplete, {
    if(upload_observer_SERVER()$formComplete) {
      shinyjs::html(id = "gather1", html = as.character(icon("check")))
      shinyjs::removeClass(id = "gather1", class = "badge-primary")
      shinyjs::addClass(id = "gather1", class = "badge-success")
    } else {
      shinyjs::html(id = "gather1", html = as.character(icon("1")))
      shinyjs::removeClass(id = "gather1", class = "badge-success")
      shinyjs::addClass(id = "gather1", class = "badge-primary")
    }
  })
  
  observeEvent(upload_logbook_SERVER()$formComplete, {
    if(upload_logbook_SERVER()$formComplete) {
      shinyjs::html(id = "gather2", html = as.character(icon("check")))
      shinyjs::removeClass(id = "gather2", class = "badge-primary")
      shinyjs::addClass(id = "gather2", class = "badge-success")
    } else {
      shinyjs::html(id = "gather2", html = as.character(icon("2")))
      shinyjs::removeClass(id = "gather2", class = "badge-success")
      shinyjs::addClass(id = "gather2", class = "badge-primary")
    }
  })
  
  observeEvent(setup_datachecks_SERVER()$active, {
    if(setup_datachecks_SERVER()$active) {
      shinyjs::html(id = "gather3", html = as.character(icon("check")))
      shinyjs::removeClass(id = "gather3", class = "badge-primary")
      shinyjs::addClass(id = "gather3", class = "badge-success")
    } else {
      shinyjs::html(id = "gather3", html = as.character(icon("3")))
      shinyjs::removeClass(id = "gather3", class = "badge-success")
      shinyjs::addClass(id = "gather3", class = "badge-primary")
    }
  }, ignoreNULL = FALSE)
  
  
  
  #------------------------
  #Data checks
  #------------------------
  
  results_datachecks_SERVER <- datachecksResults_SERVER(
                                "datachecks_results",
                                bycatchSetup = setup_datachecks_SERVER,
                                outDir = outDir
                                )
  

}