#-------------------------------
#module upload logbook data
#-------------------------------

############# UI #############

logbookupload_UI <- function(id){
  
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
            dashboardBadge("Part 2", color = "primary", position = "left"),
            HTML("&nbsp;"),
            tags$strong(tags$em("Gather logbook data"))
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
                  tags$li("A logbook data set is mandatory."),
                  tags$li("CSV file format only"),
                  tags$li("Example data sets are available in the Help page."),
                  tags$li("Files larger than 100 MB will not be uploaded.")
                )
              )
            )
          ),
          HTML(paste("Start by attaching", tags$strong("logbook data")))
        ),
        icon = icon("1"),
        tagList(tags$strong("Attach a file in csv format"), div("*", style = "display: inline; color: red;")),
        fileInput(
          inputId = ns("customLogbookData"),
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
            # div(
            #   style = "float: right;",
            #   shinyWidgets::dropdown(
            #     style = "simple",
            #     status = "royal",
            #     icon = icon('circle-info'),
            #     right = TRUE,
            #     size = "sm",
            #     div(
            #       style = "width: 600px; font-weight: normal;",
            #       "Guidance add here."
            #     ))
            # ),
            "Review your logbook data"
          ),
          icon = icon("2"),
          DTOutput(ns("previewLogbookDT")),
        )%>% shiny::tagAppendAttributes(id = ns("preview"))
      ),
      
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
                ))
            ),
            "Add a brief title for your logbook data set"
          ),
          icon = icon("3"),
          textInput(
            inputId = ns("logbookdata_title"),
            label = tagList("Title", div("*", style = "display: inline; color: red;")),
            width = '100%'
          ),
          br(),
          div(
            style = "display: flex; align-items: center; justify-content: center;",
            actionBttn(
              inputId = ns("logbookupload_save"),
              label = "Advance to part 3: setup and activation",
              width='100%'
            )
          )
        )%>% shiny::tagAppendAttributes(id = ns("finalize")) #close timelineItem
      )
    )#close timelineBlock

  )
  
}


############# SERVER #############

logbookupload_SERVER <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      #---------------------------
      #Input contraints on nchar
      #---------------------------
      shinyjs::runjs("$('#logbookdata_upload-logbookdata_title').attr('maxlength', 50)")
      
      #Holds the file uploaded by user
      previewLogbookdata<-reactiveVal()
      
      #Show input
      observeEvent(input$customLogbookData, {
        shinyjs::hide("preview")
        shinyjs::hide("finalize")
        previewLogbookdata(tryCatch(
          {
            read_csv(input$customLogbookData$datapath,
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
      previewLogbookObj <- reactive({
        dt <- previewLogbookdata()
        return(dt)
      })
      
      #Data table output
      output$previewLogbookDT <- renderDT({
        req(previewLogbookObj())
        n<-NCOL(previewLogbookObj())-1
        datatable(previewLogbookObj(),
                  rownames=FALSE)
      })
      
      #Save button for logbook data
      logbookInputsInfo <- reactiveValues(dt = NULL, title = NULL)
      
      observe({
        logbookInputsInfo$dt <- previewLogbookObj()
        logbookInputsInfo$title <- input$logbookdata_title
      })
      
      # observeEvent(input$logbookupload_save,{ 
      #   if(is.null(previewLogbookObj())){
      #     showModal(
      #       modalDialog(
      #         title = div(icon("circle-xmark", style = "color: red;"), div("Logbook data failed", style = "display: inline; padding-left: 5px;")),
      #         tags$ul(
      #           tags$li("Start by attaching and previewing your data")
      #         ),
      #         easyClose = TRUE,
      #         footer = NULL
      #       )
      #     )
      #   } else {
      #     logbookInputsInfo$dt <- previewLogbookObj()
      #     logbookInputsInfo$title <- input$logbookdata_title
      #     showModal(
      #       modalDialog(
      #         title = div(icon("ship", style = "color: #007bff;"), div("Logbook data finalized", style = "display: inline; padding-left: 5px;")),
      #         tags$ul(
      #           tags$li("Remember to load observer data"),
      #           tags$li("Proceed to data checks")
      #         ),
      #         easyClose = TRUE,
      #         footer = NULL
      #       )
      #     )
      #   }
      # })
      
      #-----------------------------
      #Reactives returned by module
      #-----------------------------
      logbookInputsInfo
      
    }) #close function moduleServer
} #close logbookupload_SERVER
