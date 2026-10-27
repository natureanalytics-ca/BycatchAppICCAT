

server <- function(input, output, session) {

  #-------------------
  #Temp directory
  #-------------------
  #Created once per user session
  outDir <- file.path(tempdir(), session$token)
  dir.create(outDir)
  print(outDir)
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
      updateTabsetPanel(
        session = session,
        inputId = "observerdata_upload-inputUIPanel", 
        selected = "freezeTab"
      )
      updateTabsetPanel(
        session = session,
        inputId = "logbookdata_upload-inputUIPanel", 
        selected = "freezeTab"
      )
    } else {
      shinyjs::html(id = "gather3", html = as.character(icon("3")))
      shinyjs::removeClass(id = "gather3", class = "badge-success")
      shinyjs::addClass(id = "gather3", class = "badge-primary")
      updateTabsetPanel(
        session = session,
        inputId = "observerdata_upload-inputUIPanel", 
        selected = "dataStructureTab"
      )
      updateTabsetPanel(
        session = session,
        inputId = "logbookdata_upload-inputUIPanel", 
        selected = "dataStructureTab"
      )
    }
  }, ignoreNULL = FALSE)
  
  
  
  #------------------------
  #Data checks
  #------------------------
  
  datachecksResults_SERVER(
    "datachecks_results",
    bycatchSetup = setup_datachecks_SERVER,
    outDir = outDir
  )
  
  observeEvent(setup_datachecks_SERVER()$active, {
    if(setup_datachecks_SERVER()$active) {
      shinyjs::html(id = "check1", html = as.character(icon("check")))
      shinyjs::removeClass(id = "check1", class = "badge-primary")
      shinyjs::addClass(id = "check1", class = "badge-success")
    } else {
      shinyjs::html(id = "check1", html = as.character(icon("1")))
      shinyjs::removeClass(id = "check1", class = "badge-success")
      shinyjs::addClass(id = "check1", class = "badge-primary")
    }
  }, ignoreNULL = FALSE)
  

  #-----------------------------
  #Design-based estimators
  #-----------------------------

  design_specifications_SERVER <- designSpecifications_SERVER(
    "designSpecifications_module",
    bycatchSetup = setup_datachecks_SERVER,
    outDir = outDir
  )
  
  designResults_SERVER(
    "designResults_module",
    inputSpecifications = design_specifications_SERVER,
    outDir = outDir
  )
  
  observeEvent(design_specifications_SERVER(), {
    if(design_specifications_SERVER()) {
      shinyjs::html(id = "design1", html = as.character(icon("check")))
      shinyjs::removeClass(id = "design1", class = "badge-primary")
      shinyjs::addClass(id = "design1", class = "badge-success")
      shinyjs::html(id = "design2", html = as.character(icon("check")))
      shinyjs::removeClass(id = "design2", class = "badge-primary")
      shinyjs::addClass(id = "design2", class = "badge-success")
    } else {
      shinyjs::html(id = "design1", html = as.character(icon("1")))
      shinyjs::removeClass(id = "design1", class = "badge-success")
      shinyjs::addClass(id = "design1", class = "badge-primary")
      shinyjs::html(id = "design2", html = as.character(icon("2")))
      shinyjs::removeClass(id = "design2", class = "badge-success")
      shinyjs::addClass(id = "design2", class = "badge-primary")
    }
  })
  
  #------------------------
  #Download
  #------------------------
  
  downloadResults_SERVER(
    "download_module",
    bycatchSetup = setup_datachecks_SERVER,
    outDir = outDir
  )
  
  observeEvent(setup_datachecks_SERVER()$active, {
    if(setup_datachecks_SERVER()$active) {
      shinyjs::html(id = "download1", html = as.character(icon("check")))
      shinyjs::removeClass(id = "download1", class = "badge-primary")
      shinyjs::addClass(id = "download1", class = "badge-success")
    } else {
      shinyjs::html(id = "download1", html = as.character(icon("1")))
      shinyjs::removeClass(id = "download1", class = "badge-success")
      shinyjs::addClass(id = "download1", class = "badge-primary")
    }
  }, ignoreNULL = FALSE)
  
}