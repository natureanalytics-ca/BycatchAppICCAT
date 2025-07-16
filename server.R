

server <- function(input, output, session) {

  #------------------
  #Data upload module
  #------------------
  upload_observer_SERVER <- observerupload_SERVER("observerdata_upload")
  upload_logbook_SERVER <- logbookupload_SERVER("logbookdata_upload")
  
  #-------------------
  #Data checks module
  #-------------------
  setup_datachecks_SERVER <- datachecksSetup_SERVER(
                                "datachecks_setup",
                                observerdataInput = reactive(upload_observer_SERVER),
                                logbookdataInput = reactive(upload_logbook_SERVER)
  )
  
  results_datachecks_SERVER <- datachecksResults_SERVER(
                                "datachecks_results",
                                 resultsDir = reactive(setup_datachecks_SERVER)
                                )
  
  #------------------
  #Help module
  #------------------
  help_SERVER("help_module")
  
}