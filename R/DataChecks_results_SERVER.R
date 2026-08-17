#-------------------------------
#module data checks setup
#-------------------------------

############# SERVER #############


datachecksResults_SERVER <- function(id, bycatchSetup, outDir){
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
                "The html report in display contains warning messages about missing data or NAs, summary figures and tables showing the sample size and 
                presence/absence of the bycatch species across levels of exploratory variables, observer coverage levels, raw trends in CPUE, and a 
                summary of the available data. The zip folder contains this html report together with: observer and logbook data sets uploaded by the user 
                (logbook_dataset and observer_dataset); a summary of the inputs defined in the app by the user (bycatch_inputs); two files labelled as 
                DataSummary and StrataSummary which contain data summaries by year and factor variables; and a .rds file that can be read in R."
              )
            )
            )
        )
      })
      
      output$report_ui <- renderUI({
        req(bycatchSetup()$active)
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
          all_files <- list.files(outDir,recursive = TRUE,full.names = TRUE)
          files_to_zip <- all_files[file.info(all_files)$isdir == FALSE]
          zip(file, files = files_to_zip, flags = "-j")
        },
        contentType = "application/zip"
      )
    }
  )
}

