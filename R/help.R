#-------------------------------
#module help
#-------------------------------


############# UI #############

help_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
  
    box(
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      title = "Observer data - example data set",
      
      downloadBttn(ns("download_observerdata_example"),"Download")
    ),
    box(
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      title = "Logbook data - example data set",
      
      downloadBttn(ns("download_logbookdata_example"),"Download")
    ),
    box(
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      title = "Online resources",
      tags$ul(
        tags$li(tags$a(href = "https://github.com/ebabcock/BycatchEstimator","BycatchEstimator R package github",target = "_blank")),
        tags$li(tags$a(href = "https://ebabcock.github.io/BycatchEstimator/","BycatchEstimator R package User Guide",target = "_blank"))
        
      )

    )
  
  )
}

############# SERVER #############

help_SERVER <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
  
      # create reactive that reads observer example data
      observerData_example <- reactive({
        dt<-tryCatch(
          {
            read.csv(paste0("www/example_datasets/observerdata_example.csv"), header = TRUE)
          },
          error = function(e) {
            NULL
          }
        )
        return(dt)
      })
      
      # create reactive that reads logbook example data
      logbookData_example <- reactive({
        dt<-tryCatch(
          {
            read.csv(paste0("www/example_datasets/logbookdata_example.csv"), header = TRUE)
          },
          error = function(e) {
            NULL
          }
        )
        return(dt)
      })
      
      output$download_observerdata_example <- downloadHandler(
        filename = "observerdata_example.csv",
        content = function(file){
          write.csv(observerData_example(), file, row.names = FALSE)
        }
        
      )
      
      output$download_logbookdata_example <- downloadHandler(
        filename = "logbookdata_example.csv",
        content = function(file){
          write.csv(logbookData_example(), file, row.names = FALSE)
        }
        
      )
  

    }) #close function moduleServer
} #close help_SERVER
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  