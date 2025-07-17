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
      style="display: flex; flex-direction: row; flex-wrap: wrap; width: 100%; align-items: center; padding: 0px 32px 0px 0px;",
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
          margin: 10px 0px;"
        ,
        div(
          style = "float: right; padding: 4px 20px;",
          tags$strong(tags$em("Data checks results"))
        )
      )
    ),
    
    br(),
    uiOutput(ns("download_ui")),
    uiOutput(ns("report_ui"))
  )
  
}


############# SERVER #############


datachecksResults_SERVER <- function(id, resultsDir = reactive(NULL)){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      observeEvent(resultsDir()$output, {
        print(resultsDir()$output)
      })
      
      output$download_ui <- renderUI({
        req(resultsDir()$output)
        downloadButton(ns("downloadZip"), "Download results as ZIP")
      })
      
      output$report_ui <- renderUI({
        req(resultsDir()$output)
        outDir <- resultsDir()$output
        #outDir <- "C:\\Users\\WILLIA~1\\AppData\\Local\\Temp\\RtmpKk9b54\\bycatch_output_760c343f6614"
        all_files <- data.frame(nm = list.files(outDir,recursive = TRUE,full.names = TRUE))
        src <- all_files %>%
          filter(stringr::str_detect(nm, pattern = ".html"))

        tagList(
          div(
            style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px; margin: 20px 32px 0px 0px; background: white;",
            tags$iframe(
              src = base64enc::dataURI(file=src$nm, mime="text/html; charset=UTF-8"),
              style="border:0; width:100%; height: 10000px;"
            )
          )
        )
      })
      
      output$downloadZip <- downloadHandler(
        filename = function() {
          "bycatch_results.zip"
        },
        content = function(file) {
          outDir <- resultsDir()$output
          all_files <- list.files(outDir,recursive = TRUE,full.names = TRUE)
          files_to_zip <- all_files[file.info(all_files)$isdir == FALSE]
          zip(file, files = files_to_zip, flags = "-j")
        },
        contentType = "application/zip"
      )
     
    }
  )
}

