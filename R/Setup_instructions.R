#-------------------------------
#Setup instructions
#-------------------------------


setupInstuctions_UI <- function(id){
  
  ns <- NS(id)
  
  tagList( #does it need to be wrapped in tagList?
    
    ### -------------------------
    ### Info bar   
    ### -------------------------
    div (
      style="display: flex; flex-direction: row; flex-wrap: wrap; width: 100%; align-items: center; padding: 0px 0px 10px 0px;",
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
          margin: 0px;"
        ,
        div(
          style = "float: left; padding: 4px 0px;",
          tags$strong(tags$em("Instructions"))
        )
      )
    ),
    
    box(
      collapsible = FALSE,
      collapsed = FALSE,
      width = 12,
      title = "Bycatch setup",
      fluidRow(
        column(12,
               p("Bycatch setup is required prior to accessing any other module. Setup is completed in three steps:"),
               tags$ol(
                 tags$li(strong("Gather observer data"),": where the user is required to attach their observer data and select required inputs."),
                 tags$li(strong("Gather logbook data"),": where the user is required to attach their logbook data and select required inputs."),
                 tags$li(strong("Align data & activate"),": after aligning effort records between observer and logbook data, the user will activate bycatch setup and proceed to other modules.")
               ),
               p("In the boxes found below, the user can find example datasets that can be downloaded and used as practice.")
       )
      )
    ),
    box(
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      title = "Simple observer and logbook datasets",
      div(
        style = "margin: 0px 7.5px 10px; 0px;",
        h6(tags$strong("Observer data")),
        downloadButton(
          ns("download_observerdata_simple"),
          "Download observer data",
          onclick = "
                document.getElementById('ui-shield').style.display = 'block';
                setTimeout(function() { 
                  document.getElementById('ui-shield').style.display = 'none'; 
                }, 3000);"
        )
      ),
      box(
        width = 12,
        title = "Preview simple observer dataset",
        collapsible = TRUE,
        collapsed = TRUE,
        "First 10 rows of data shown.",
        br(),
        "Dowload file to see entire dataset.",
        DTOutput(ns("exampleObserverDataSimpleDT"))
      ),
      div(
        style = "margin: 30px 7.5px 10px 7.5px;",
        h6(tags$strong("Logbook data")),
        downloadButton(
          ns("download_logbookdata_simple"),
          "Download logbook data",
          onclick = "
                document.getElementById('ui-shield').style.display = 'block';
                setTimeout(function() { 
                  document.getElementById('ui-shield').style.display = 'none'; 
                }, 3000);"
        )
      ),
      box(
        width = 12,
        title = "Preview simple logbook dataset",
        collapsible = TRUE,
        collapsed = TRUE,
        "First 10 rows of data shown.",
        br(),
        "Dowload file to see entire dataset.",
        DTOutput(ns("exampleLogbookDataSimpleDT"))
      )
    ),
    box(
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      title = "Simple observer with aggregated logbook datasets",
      div(
        style = "margin: 0px 7.5px 10px; 0px;",
        h6(tags$strong("Observer data")),
        downloadButton(
          ns("download_observerdata_simple2"),
          "Download observer data",
          onclick = "
                document.getElementById('ui-shield').style.display = 'block';
                setTimeout(function() { 
                  document.getElementById('ui-shield').style.display = 'none'; 
                }, 3000);"
        )
      ),
      box(
        width = 12,
        title = "Preview simple observer dataset",
        collapsible = TRUE,
        collapsed = TRUE,
        "First 10 rows of data shown.",
        br(),
        "Dowload file to see entire dataset.",
        DTOutput(ns("exampleObserverDataSimpleDT2"))
      ),
      div(
        style = "margin: 30px 7.5px 10px 7.5px;",
        h6(tags$strong("Logbook aggredated data")),
        downloadButton(
          ns("download_logbookdata_aggregated"),
          "Download aggregated logbook data",
          onclick = "
                document.getElementById('ui-shield').style.display = 'block';
                setTimeout(function() { 
                  document.getElementById('ui-shield').style.display = 'none'; 
                }, 3000);"
        )
      ),
      box(
        width = 12,
        title = "Preview aggregated logbook dataset",
        collapsible = TRUE,
        collapsed = TRUE,
        "First 10 rows of data shown.",
        br(),
        "Dowload file to see entire dataset.",
        DTOutput(ns("exampleLogbookDataAggregatedDT"))
      )
    ),
    box(
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      title = "Complex observer and logbook datasets from LLSIM",
      div(
        style = "margin: 0px 7.5px 10px; 0px;",
        h6(tags$strong("Observer data")),
        downloadButton(
          ns("download_observerdata_LLSIM"),
          "Download observer data",
          onclick = "
                document.getElementById('ui-shield').style.display = 'block';
                setTimeout(function() { 
                  document.getElementById('ui-shield').style.display = 'none'; 
                }, 3000);"
        )
      ),
      box(
        width = 12,
        title = "Preview complex observer dataset",
        collapsible = TRUE,
        collapsed = TRUE,
        "First 10 rows of data shown.",
        br(),
        "Dowload file to see entire dataset.",
        DTOutput(ns("exampleObserverDataLLSIMDT"))
      ),
      div(
        style = "margin: 30px 7.5px 10px; 7.5px",
        h6(tags$strong("Logbook data")),
        downloadButton(
          ns("download_logbookdata_LLSIM"),
          "Download logbook data",
          onclick = "
                document.getElementById('ui-shield').style.display = 'block';
                setTimeout(function() { 
                  document.getElementById('ui-shield').style.display = 'none'; 
                }, 3000);"
        )
      ),
      box(
        width = 12,
        title = "Preview complex logbook dataset",
        collapsible = TRUE,
        collapsed = TRUE,
        "First 10 rows of data shown.",
        br(),
        "Dowload file to see entire dataset.",
        DTOutput(ns("exampleLogbookDataLLSIMDT"))
      )
    )
  )
}

############# SERVER #############

setupInstuctions_SERVER <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      #----------------
      # Simple datasets
      #----------------
      observerDataSimple_example <- reactive({
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
      
      logbookDataSimple_example <- reactive({
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
      
      output$download_observerdata_simple <- output$download_observerdata_simple2 <- downloadHandler(
        filename = function(){
          "observerdata_simple.csv"
        },
        content = function(file){
          write.csv(observerDataSimple_example(), file, row.names = FALSE)
        }
        
      )
      
      output$download_logbookdata_simple <- downloadHandler(
        filename = function(){
          "logbookdata_simple.csv"
        },
        content = function(file){
          write.csv(logbookDataSimple_example(), file, row.names = FALSE)
        }
        
      )
      
      output$exampleObserverDataSimpleDT <- output$exampleObserverDataSimpleDT2 <- renderDT({
        req(observerDataSimple_example())
        datatable(head(observerDataSimple_example(), n = 10L),
                  options = list(dom = 't', pageLength = 10),
                  rownames=FALSE)
      })
      
      output$exampleLogbookDataSimpleDT <- renderDT({
        req(logbookDataSimple_example())
        datatable(head(logbookDataSimple_example(), n = 10L),
                  options = list(dom = 't', pageLength = 10),
                  rownames=FALSE
                  )
      })
      
      #------------------
      #Aggregated logbook
      #------------------
      
      logbookDataAggregated_example <- reactive({
        dt<-tryCatch(
          {
            read.csv(paste0("www/example_datasets/logbookdata_example_aggregate.csv"), header = TRUE)
          },
          error = function(e) {
            NULL
          }
        )
        return(dt)
      })
      
      output$exampleLogbookDataAggregatedDT <- renderDT({
        req(logbookDataAggregated_example())
        datatable(head(logbookDataAggregated_example(), n = 10L),
                  options = list(dom = 't', pageLength = 10),
                  rownames=FALSE)
      })
      
      output$download_logbookdata_aggregated <- downloadHandler(
        filename = function(){
          "logbookdata_aggregate.csv"
        },
        content = function(file){
          write.csv(logbookDataAggregated_example(), file, row.names = FALSE)
        }
        
      )
      
      #----------------
      #LLSIM datasets
      #----------------
      observerDataLLSIM_example <- reactive({
        dt<-tryCatch(
          {
            read.csv(paste0("www/example_datasets/LLSIM_observerdata_example.csv"), header = TRUE)
          },
          error = function(e) {
            NULL
          }
        )
        return(dt)
      })
      
      logbookDataLLSIM_example <- reactive({
        dt<-tryCatch(
          {
            read.csv(paste0("www/example_datasets/LLSIM_logbookdata_example.csv"), header = TRUE)
          },
          error = function(e) {
            NULL
          }
        )
        return(dt)
      })
      
      output$download_observerdata_LLSIM <- downloadHandler(
        filename = function(){
          "observerdata_LLSIM.csv"
        },
        content = function(file){
          write.csv(observerDataLLSIM_example(), file, row.names = FALSE)
        }
        
      )
      
      output$download_logbookdata_LLSIM <- downloadHandler(
        filename = function(){
          "logbookdata_LLSIM.csv"
        },
        content = function(file){
          write.csv(logbookDataLLSIM_example(), file, row.names = FALSE)
        }
      )
      
      output$exampleObserverDataLLSIMDT <- renderDT({
        req(observerDataLLSIM_example())
        datatable(head(observerDataLLSIM_example(), n = 10L),
                  options = list(dom = 't', pageLength = 10),
                  rownames=FALSE)
      })
      
      output$exampleLogbookDataLLSIMDT <- renderDT({
        req(logbookDataLLSIM_example())
        datatable(head(logbookDataLLSIM_example(), n = 10L),
                  options = list(dom = 't', pageLength = 10),
                  rownames=FALSE)
      })
      
    }) #close function moduleServer
} #close help_SERVER
