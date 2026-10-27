#-------------------------------
#module data checks setup
#-------------------------------

############# SERVER #############


designResults_SERVER <- function(id, inputSpecifications, outDir){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      observe({
        shinyjs::toggle("warningPanelToggle", condition = !inputSpecifications())  
      }) 
        
      output$report_ui <- renderUI({
        req(inputSpecifications())
        all_files <- data.frame(nm = list.files(outDir, recursive = TRUE, full.names = TRUE))
        src <- all_files %>%
          filter(stringr::str_detect(nm, pattern = "DesignResults.html"))

        if(NROW(src) > 0) {
          return(
            tagList(
              div(
                style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px; background: white;",
                tags$iframe(
                  src = base64enc::dataURI(file=src$nm, mime="text/html; charset=UTF-8"),
                  style="border:0; width:100%; height: 10000px;"
                )
              )
            )
          )
        }
      })
    }
  )
}

