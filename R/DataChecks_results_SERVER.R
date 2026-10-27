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
        
      output$report_ui <- renderUI({
        req(bycatchSetup()$active)
        all_files <- data.frame(nm = list.files(outDir,recursive = TRUE,full.names = TRUE))
        src <- all_files %>%
          filter(stringr::str_detect(nm, pattern = "DataChecks.html"))

        tagList(
          div(
            style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px; background: white;",
            tags$iframe(
              src = base64enc::dataURI(file=src$nm, mime="text/html; charset=UTF-8"),
              style="border:0; width:100%; height: 10000px;"
            )
          )
        )
      })
    }
  )
}

