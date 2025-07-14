#-------------------------------
#module upload logbook data
#-------------------------------

############# UI #############

logbookupload_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(

        timelineBlock(
          width = 12,
          reversed = FALSE,
          timelineItem(
            title = "Start by attaching and previewing your data",
            icon = icon("paperclip"),
            tags$strong("Attach a file in csv format"),
            div(
              style = "float: right;",
              shinyWidgets::dropdown(
                style = "simple",
                status = "royal",
                icon = icon('circle-info'),
                right = TRUE,
                size = "md",
                div(
                  style = "width: 600px; font-weight: normal;",
                  "Guidance add here."
                ))
            ),
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
              title = "Preview logbook data set",
              icon = icon("eye"),
              DTOutput(ns("previewLogbookDT")),
            )%>% shiny::tagAppendAttributes(id = ns("preview")) #timeline end,
          ),
          
          hidden(
            timelineItem(
              title = "How is your data set structured?",
              icon = icon("file"),
              
              # prettyRadioButtons(
              #   inputId = ns("header"),
              #   label = "Does your data have a header row?",
              #   choiceNames = c("Yes", "No"),
              #   choiceValues = c(TRUE, FALSE),
              #   icon = icon("check"),
              #   animation = "jelly",
              #   inline=TRUE,
              #   status = "default"
              # ),
              
              uiOutput(ns("logbook_year_ui")),
              br(),
              uiOutput(ns("logbook_effort_ui")),
              br(),
              div(
                style = "float: right;",
                shinyWidgets::dropdown(
                  style = "simple",
                  status = "royal",
                  icon = icon('circle-info'),
                  right = TRUE,
                  size = "md",
                  div(
                    style = "width: 600px; font-weight: normal;",
                    "Guidance add here."
                  ))
              ),
              prettyRadioButtons(
                inputId = ns("logbook_sampleunit"),
                label = "What is the sample unit (i.e. what each row represents)?",
                choices = c("Sets", "Trips","Other (aggregated data)"), #TO ADD: textbox to specify what Other is
                selected = "Trips",
                icon = icon("check"),
                animation = "jelly",
                inline=TRUE,
                status = "default"
              ),
              uiOutput(ns("logbook_aggregationcolumn_ui")),
              br(),
              div(
                style = "float: right;",
                shinyWidgets::dropdown(
                  style = "simple",
                  status = "royal",
                  icon = icon('circle-info'),
                  right = TRUE,
                  size = "md",
                  div(
                    style = "width: 600px; font-weight: normal;",
                    "Guidance add here."
                  ))
              ),
              textInput(
                inputId = ns("logbookdata_title"),
                label = "Add a brief title for your logbook data set",
                width = '100%'
              ),
              br(),
              div(
                style = "display: flex; align-items: center; justify-content: center;",
                actionBttn(
                  inputId = ns("logbookupload_save"),
                  label = "Finalize and save data",
                  icon=icon('floppy-disk'),
                  width='100%'
                )
                # actionBttn(
                #   inputId = "logbookupload_cancel",
                #   label = "Cancel"
                # )
              )
              
            )%>% shiny::tagAppendAttributes(id = ns("structure")) #close timelineItem
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
      
      #Holds the file uploaded by user
      previewLogbookdata<-reactiveVal()
      
      #Show input
      observeEvent(input$customLogbookData, {
        shinyjs::hide("preview")
        shinyjs::hide("structure")
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
        shinyjs::show("structure")
      }, ignoreInit = TRUE)
      
      #Creates a temporary object
      previewLogbookObj <- reactive({
        #req(previewLogbookdata())
        dt <- previewLogbookdata()
        # if(as.logical(input$header)){
        #   names(dt) <- dt[1,]
        #   dt <- dt[-1, , drop = FALSE]
        # }
        return(dt)
      })
      
      ### dynamic pickerInputs that read data set
      #Year Column
      output$logbook_year_ui <- renderUI({
        req(previewLogbookObj())
        tagList(
          div(
            style = "float: right;",
            shinyWidgets::dropdown(
              style = "simple",
              status = "royal",
              icon = icon('circle-info'),
              right = TRUE,
              size = "md",
              div(
                style = "width: 600px; font-weight: normal;",
                "Guidance add here."
              ))
          ),
          pickerInput(
            inputId = ns("logbook_year"),
            label = "Select column that contains year (needs to be named the same as in observer data)",
            choices = names(previewLogbookObj()),
            width = '100%'
          )
        )
      })

      #Effort column
      output$logbook_effort_ui <- renderUI({
        req(previewLogbookObj())
        tagList(
          div(
            style = "float: right;",
            shinyWidgets::dropdown(
              style = "simple",
              status = "royal",
              icon = icon('circle-info'),
              right = TRUE,
              size = "md",
              div(
                style = "width: 600px; font-weight: normal;",
                "Guidance add here."
              ))
          ),
          pickerInput(
            inputId = ns("logbook_effort"),
            label = "Select column that contains effort",
            choices = names(previewLogbookObj()),
            width = '100%'
          )
        )
      })
      #Aggregation column
      output$logbook_aggregationcolumn_ui <- renderUI({
        req(previewLogbookObj())
        pickerInput(
          inputId = ns("logbook_aggregationcolumn"),
          label = "If data is aggregated, indicate column that gives number of sample units",
          choices = c("NA",names(previewLogbookObj())),
          selected = "NA",
          width = '100%'
        )
      })
      
      #Data table output
      output$previewLogbookDT <- renderDT({
        req(previewLogbookObj())
        n<-NCOL(previewLogbookObj())-1
        datatable(previewLogbookObj(),
                  rownames=FALSE)
      })
      
      #Save button for logbook data
      logbookInputsInfo <- reactiveValues(dt = NULL, title = NULL, header = NULL, 
                                           yearColumn = NULL, effortColumn = NULL,
                                           sampleUnits = NULL, aggregationColumn = NULL)
      
      
      observeEvent(input$logbookupload_save,{ 
        if(is.null(previewLogbookObj())){
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
          logbookInputsInfo$dt <- previewLogbookObj()
          logbookInputsInfo$title <- input$logbookdata_title
          logbookInputsInfo$header <- input$header
          logbookInputsInfo$yearColumn <- input$logbook_year
          logbookInputsInfo$effortColumn <- input$logbook_effort
          logbookInputsInfo$sampleUnits <- input$logbook_sampleunit
          logbookInputsInfo$aggregationColumn <- if (input$logbook_aggregationcolumn == "NA") NA else input$logbook_aggregationcolumn
          showModal(
            modalDialog(
              title = div(icon("circle-check", style = "color: #007bff;"), div("Logbook data finalized", style = "display: inline; padding-left: 5px;")),
              
              tags$ul(
                tags$li("Optionally, load observer data"),
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
      logbookInputsInfo
      
    }) #close function moduleServer
} #close logbookupload_SERVER
