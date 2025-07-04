

server <- function(input, output, session) {

  upload_observer_SERVER <- observerupload_SERVER("observerdata_upload")
  
  upload_logbook_SERVER <- logbookupload_SERVER("logbookdata_upload")
  
  datachecks_SERVER("datachecks",observerdataInput = reactive(upload_observer_SERVER),
                    logbookdataInput = reactive(upload_logbook_SERVER) )
  
}