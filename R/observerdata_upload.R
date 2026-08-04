#-------------------------------
#module upload observer data
#-------------------------------


############# UI #############

observerupload_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    ### -------------------------
    ### Info bar
    ### -------------------------
    div (
      style="display: flex; flex-direction: row; flex-wrap: wrap; width: 100%; align-items: center; padding: 0px 32px 10px 18px;",
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
          div(
            dashboardBadge("Part 1", color = "primary", position = "left"),
            HTML("&nbsp;"),
            tags$strong(tags$em("Gather observer data"))
          )
        ),
        div("*Mandatory fields", style = "float: right; color: red; padding: 4px 4px;"),
      )
    ),
    br(),
    timelineBlock(
      width = 12,
      reversed = FALSE,
      timelineItem(
        title = div(
          div(
            style = "float: right;",
            shinyWidgets::dropdown(
              style = "simple",
              status = "royal",
              icon = icon('circle-info'),
              right = TRUE,
              size = "sm",
              div(
                style = "width: 600px; font-weight: normal;",
                tags$ul(
                  tags$li("An observer data set is mandatory."),
                  tags$li("CSV file format only"),
                  tags$li("Example data sets are available in the Help page."),
                  tags$li("Files larger than 100 MB will not be uploaded.")
                )
              )
            )
          ),
          HTML(paste("Start by attaching", tags$strong("observer data")))
        ),
        icon = icon("1"),
        tagList("Attach a file in csv format", div("*", style = "display: inline; color: red;")),
        fileInput(
          inputId = ns("customObserverData"),
          label = NULL,
          buttonLabel = "Attach",
          accept = c('.csv'),
          multiple = FALSE,
          width = "100%",
        )
      ),
      
      hidden(
        timelineItem(
          title = div(
            "Review your observer data"
          ),
          icon = icon("2"),
          DTOutput(ns("previewObserverDT")),
        )
      )%>% shiny::tagAppendAttributes(id = ns("preview")),
      hidden(
        timelineItem(
          title = div(
            div(
              style = "float: right;",
              shinyWidgets::dropdown(
                style = "simple",
                status = "royal",
                icon = icon('circle-info'),
                right = TRUE,
                size = "sm",
                div(
                  style = "width: 600px; font-weight: normal;",
                  tags$ul(
                    tags$li("Maximum of 50 characters for the title.")
                  )
                )
              )
            ),
            "Add a brief title for your observer data set"
          ),
          icon = icon("3"),
          textInput(
            inputId = ns("observerdata_title"),
            label = tagList("Title", div("*", style = "display: inline; color: red;")),
            width = '100%'
          ),
          br(),
          div(
            style = "display: flex; align-items: center; justify-content: center;",
            actionBttn(
              inputId = ns("observerupload_save"),
              label = "Advance to part 2: gather logbook data",
              width='100%'
            )
          )
        )%>% shiny::tagAppendAttributes(id = ns("finalize")) #close timelineItem
      )
    ) #close timelineBlock
  )
}


############# SERVER #############


observerupload_SERVER <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      #---------------------------
      #Input contraints on nchar
      #---------------------------
      shinyjs::runjs("$('#observerdata_upload-observerdata_title').attr('maxlength', 50)")      
      
      #Holds the file uploaded by user
      previewObserverdata<-reactiveVal()
      
      #Show input
      observeEvent(input$customObserverData, {
        shinyjs::hide("preview")
        shinyjs::hide("finalize")
        previewObserverdata(tryCatch(
          {
            read_csv(input$customObserverData$datapath,
                     col_names = TRUE, show_col_types = FALSE)
          },
          error = function(e) {
            NULL
          }
        ))
        shinyjs::show("preview")
        shinyjs::show("finalize")
      }, ignoreInit = TRUE)
      
      #Creates a temporary object
      previewObserverObj <- reactive({
        dt <- data.frame(previewObserverdata())
        return(dt)
      })
      
      #Data table output
      output$previewObserverDT <- renderDT({
        req(previewObserverObj())
        n<-NCOL(previewObserverObj())-1
        datatable(previewObserverObj(),
                  rownames=FALSE)
      })
     
      #Save button for observer data
      observerInputsInfo <- reactiveValues(dt = NULL, title = NULL)
      
      observe({
        observerInputsInfo$dt <- previewObserverObj()
        observerInputsInfo$title <- input$observerdata_title
      })
      
      
      # observeEvent(input$observerupload_save,{ 
      #   #Statement in case there is no data -> modal for user to input data before finalizing
      #   if(is.null(previewObserverObj()) || nchar(input$observerdata_title) < 1){
      #     showModal(
      #       modalDialog(
      #         title = div(icon("circle-xmark", style = "color: red;"), div("Logbook data failed", style = "display: inline; padding-left: 5px;")),
      #         tags$ul(
      #           tags$li("An observer dataset is required."),
      #           tags$li("A title for your dataset is required.")
      #         ),
      #         easyClose = TRUE,
      #         footer = NULL
      #       )
      #     )
      #   } else {
      #     observerInputsInfo$dt <- previewObserverObj()
      #     observerInputsInfo$title <- input$observerdata_title
      #     shinyjs::onclick("observerdata_upload-observerupload_save", updateControlbarMenu(
      #       session = session,
      #       inputId = "sidebar",
      #       selected = "Logbook_upload"
      #     ))
      #     showModal(
      #       modalDialog(
      #         title = div(icon("binoculars", style = "color: #007bff;"), div("Observer data finalized", style = "display: inline; padding-left: 5px;")),
      #         tags$ul(
      #           tags$li("Remember to load logbook data"),
      #           tags$li("Proceed to data checks")
      #         ),
      #         easyClose = TRUE,
      #         footer = NULL
      #       )
      #     )
      #   }
      # 
      # })
      
      #-----------------------------
      #Reactives returned by module
      #-----------------------------
      observerInputsInfo
      
    }) #close function moduleServer
} #close observerupload_SERVER


