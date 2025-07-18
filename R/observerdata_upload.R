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
      style="display: flex; flex-direction: row; flex-wrap: wrap; width: 100%; align-items: center; padding: 0px 32px 10px 66px;",
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
          style = "float: right; padding: 4px 20px;",
          tags$strong(tags$em("Load observer data"))
        )
      )
    ),
    div("*Mandatory field", style = "float: right; color: red; padding: 0px 32px 0px 0px;"),
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
                "Mandatory to attach a observer data set. Example data sets are available in the Help page. Files larger than 100 MB will not be uploaded."
              ))
          ),
          "Start by attaching observer data"
        ),
        icon = icon("binoculars"),
        tagList(tags$strong("Attach a file in csv format"), div("*", style = "display: inline; color: red;")),
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
            "Preview observer data"
          ),
          icon = icon("binoculars"),
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
                  "Maximum of 50 characters for the title."
                ))
            ),
            "Finalize and save observer data"
          ),
          icon = icon("binoculars"),
          textInput(
            inputId = ns("observerdata_title"),
            label = "Add a brief title for your observer data set",
            width = '100%'
          ),
          br(),
          div(
            style = "display: flex; align-items: center; justify-content: center;",
            actionBttn(
              inputId = ns("observerupload_save"),
              label = tagList("Finalize and save data", div("*", style = "display: inline; color: red;")),
              icon=icon('floppy-disk'),
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
      
      
      observeEvent(input$observerupload_save,{ 
        #Statement in case there is no data -> modal for user to input data before finalizing
        if(is.null(previewObserverObj())){
          showModal(
            modalDialog(
              title = div(icon("circle-xmark", style = "color: red;"), div("Logbook data failed", style = "display: inline; padding-left: 5px;")),
              tags$ul(
                tags$li("Start by attaching and previewing your data")
              ),
              easyClose = TRUE,
              footer = NULL
            )
          )
        } else {
          observerInputsInfo$dt <- previewObserverObj()
          observerInputsInfo$title <- input$observerdata_title
          showModal(
            modalDialog(
              title = div(icon("binoculars", style = "color: #007bff;"), div("Observer data finalized", style = "display: inline; padding-left: 5px;")),
              tags$ul(
                tags$li("Remember to load logbook data"),
                tags$li("Proceed to data checks")
              ),
              easyClose = TRUE,
              footer = NULL
            )
          )
        }

      })
      
      #-----------------------------
      #Reactives returned by module
      #-----------------------------
      observerInputsInfo
      
    }) #close function moduleServer
} #close observerupload_SERVER


