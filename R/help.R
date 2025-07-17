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
      tagList(
      selectInput(ns("exampleObserverData"),
                  label = "Select an example data set",
                  choices = c("Simple data" = "observerdata_example",
                              "LLSIM data" = "LLSIM_observerdata_example")
                  ),
      
      downloadButton(ns("download_observerdata_example"),"Download"),
      div(style = "margin-top: 20px"),
      box(
        width = 12,
        title = "Example observer data preview",
        collapsible = TRUE,
        collapsed = TRUE,
        DTOutput(ns("exampleObserverDataDT"))
      )
    )),
    
    box(
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      title = "Logbook data - example data set",
      tagList(
      selectInput(ns("exampleLogbookData"),
                  label = "Select an example data set",
                  choices = c("Simple data" = "logbookdata_example",
                              "LLSIM data" = "LLSIM_logbookdata_example")
      ),
      downloadButton(ns("download_logbookdata_example"),"Download"),
      div(style = "margin-top: 20px"),
      box(
        width = 12,
        title = "Example logbook data preview",
        collapsible = TRUE,
        collapsed = TRUE,
        DTOutput(ns("exampleLogbookDataDT"))
      )
    )),
    
    box(
      collapsible = FALSE,
      collapsed = FALSE,
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
            read.csv(paste0("www/example_datasets/",input$exampleObserverData,".csv"), header = TRUE)
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
            read.csv(paste0("www/example_datasets/",input$exampleLogbookData,".csv"), header = TRUE)
          },
          error = function(e) {
            NULL
          }
        )
        return(dt)
      })
      
      output$download_observerdata_example <- downloadHandler(
        filename = function(){
          paste0(input$exampleObserverData, ".csv")
          },
        content = function(file){
          write.csv(observerData_example(), file, row.names = FALSE)
        }
        
      )
      
      output$download_logbookdata_example <- downloadHandler(
        filename = function(){
          paste0(input$exampleLogbookData, ".csv")
        },
        content = function(file){
          write.csv(logbookData_example(), file, row.names = FALSE)
        }
        
      )
  
      output$exampleObserverDataDT <- renderDT({
        req(observerData_example())
        datatable(observerData_example(),
                  rownames=FALSE)
      })
      
      output$exampleLogbookDataDT <- renderDT({
        req(logbookData_example())
        datatable(logbookData_example(),
                  rownames=FALSE)
      })

    }) #close function moduleServer
} #close help_SERVER
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  