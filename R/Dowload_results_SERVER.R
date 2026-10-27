#-------------------------------
#module data checks setup
#-------------------------------

############# SERVER #############


downloadResults_SERVER <- function(id, bycatchSetup, outDir){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      observe({
        shinyjs::toggle("warningPanelToggle", condition = !bycatchSetup()$active)  
      }) 
        
      output$download_ui <- renderUI({
        req(bycatchSetup()$active)
        tagList(
          div(style = "display: inline-block;",
            downloadButton(
              ns("downloadZip"), 
              "Download results as ZIP",
              onclick = "
                document.getElementById('ui-shield').style.display = 'block';
                setTimeout(function() { 
                  document.getElementById('ui-shield').style.display = 'none'; 
                }, 3000);"
            )
          ),
          div(style = "display: inline-block;",
            shinyWidgets::dropdown(
              style = "simple",
              status = "royal",
              icon = icon('circle-info'),
              right = FALSE,
              size = "sm",
              div(
                style = "width: 600px; font-weight: normal;",
                tags$strong("Bycatch setup"),
                tags$ul(
                  tags$li("The zip folder contains observer and logbook data sets uploaded by the user (logbook_dataset and observer_dataset)."), 
                  tags$li("A summary of the inputs defined in the app by the user (bycatch_inputs)."),
                  tags$li("Two files labelled as DataSummary and StrataSummary which contain data summaries by year and factor variables."),
                  tags$li("An .rds file that can be read in R.")
                ),
                br(),
                tags$strong("Data checks"),
                tags$ul(
                  tags$li("The html report in display contains warning messages about missing data or NAs, summary figures and tables showing the sample size and presence/absence of the bycatch species across levels of exploratory variables, observer coverage levels, raw trends in CPUE, and a summary of the available data.")
                ),
                br(),
                tags$strong("Design-based estimators"),
                tags$ul(
                  tags$li("An html report with bycatch estimates."),
                  tags$li("Tables with the total estimates by year for each method, along with standard errors, and the same by the groups defined by groupVar (if requested)."),
                  tags$li("An .rds file that can be read in R.")
                )
              )
            )
          )
        )
      })
      
      # output$downloadZip <- downloadHandler(
      #   
      #   filename = function() {
      #     "bycatch_results.zip"
      #   },
      #   content = function(file) {
      #     all_files <- list.files(outDir,recursive = TRUE,full.names = TRUE)
      #     files_to_zip <- all_files[file.info(all_files)$isdir == FALSE]
      #     zip(file, files = files_to_zip, flags = "-j")
      #   },
      #   contentType = "application/zip"
      # )
      
      
      output$downloadZip <- downloadHandler(
        filename = function() {
          "bycatch_results.zip"
        },
        content = function(file) {
          # Execute zip inside outDir so paths remain relative
          withr::with_dir(outDir, {
            # Get relative paths of all files within outDir
            files_to_zip <- list.files(recursive = TRUE, full.names = FALSE)
            # Filter out directories if any exist in the list
            files_to_zip <- files_to_zip[!file.info(files_to_zip)$isdir]
            
            # Zip without the -j flag to preserve folder hierarchy
            zip(zipfile = file, files = files_to_zip)
          })
        },
        contentType = "application/zip"
      )
    }
  )
}

