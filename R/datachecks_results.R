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
        #req(resultsDir()$output)
        tagList(
        div(style = "display: inline-block;",
        downloadButton(ns("downloadZip"), "Download results as ZIP")),
        div(style = "display: inline-block;",
          shinyWidgets::dropdown(
            style = "simple",
            status = "royal",
            icon = icon('circle-info'),
            right = FALSE,
            size = "sm",
            div(
              style = "width: 600px; font-weight: normal;",
              "The html report in display contains warning messages about missing data or NAs, summary figures and tables showing the sample size and 
              presence/absence of the bycatch species across levels of exploratory variables, observer coverage levels, raw trends in CPUE, and a 
              summary of the available data. The zip folder contains this html report together with: observer and logbook data sets uploaded by the user 
              (logbook_dataset and observer_dataset); a summary of the inputs defined in the app by the user (bycatch_inputs); two files labelled as 
              DataSummary and StrataSummary which contain data summaries by year and factor variables; and a .rds file that can be read in R."
            )))
        )
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

