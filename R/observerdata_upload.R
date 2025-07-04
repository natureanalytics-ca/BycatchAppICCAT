#-------------------------------
#module upload observer data
#-------------------------------


############# UI #############

observerupload_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(

    #fluidRow(
      #column(
        #12,
        timelineBlock(
          width = 12,
          reversed = FALSE,
          timelineItem(
            title = "Start by attaching your data",
            icon = icon("paperclip"),
            fileInput(
              inputId = ns("customObserverData"),
              label = tags$small("Attach a file in csv format"),
              buttonLabel = "Attach",
              accept = c('.csv'),
              multiple = FALSE,
              width = "100%",
            )
          ),
          
          timelineItem(
            title = "Preview observer data set",
            icon = icon("eye"),
            DTOutput(ns("previewObserverDT")),
            br(),
            br(),
            textInput(
              inputId = ns("observerdata_title"),
              label = "Add a brief title for your observer data set",
              width = '100%'
            )
          ),

          timelineItem(
            title = "How is your data set structured?",
            icon = icon("file"),
            prettyRadioButtons(
              inputId = ns("header"),
              label = "Does your data have a header row?",
              choiceNames = c("Yes", "No"),
              choiceValues = c(TRUE, FALSE),
              icon = icon("check"),
              animation = "jelly",
              inline=TRUE,
              status = "default"
            ),
            # pickerInput(
            #   inputId = ns("observer_catch"),
            #   label = "Select column that contains catch",
            #   choices = c("X1","X2","X3","X4"),
            #   width = '100%'
            # ),
            uiOutput(ns("observer_catch_ui")),
            
           pickerInput(
              inputId = ns("observer_catchunits"),
              label = "Units of catch",
              choices = c("Numbers", "Weight in kg","Weight in tonnes"),
              width = '100%'
            ),
           pickerInput(
              inputId = ns("observer_catchtype"),
              label = "What is the catch type?",
              choices = c("Dead discards", "Live discards","Discards","Bycatch"), #add more catch types?
              width = '100%'
            ),
           
           uiOutput(ns("observer_year_ui")),
           
           uiOutput(ns("observer_effort_ui")),

            prettyRadioButtons(
              inputId = ns("observer_sampleunit"),
              label = "What is the sample unit (i.e. what each row represents)?",
              choices = c("Sets", "Trips"),
              selected = "Trips",
              icon = icon("check"),
              animation = "jelly",
              inline=TRUE,
              status = "default"
            ),
            br(),
            br(),
            div(
              style = "display: flex; align-items: center; justify-content: center;",
              actionBttn(
                inputId = ns("observerupload_save"),
                label = "Finalize and save data",
                icon=icon('floppy-disk'),
                width='100%'
              )
              # actionBttn(
              #   inputId = "observerupload_cancel",
              #   label = "Cancel"
              # )
            )
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
      
      #Holds the file uploaded by user
      previewObserverdata<-reactiveVal()
      
      #Show input
      observeEvent(input$customObserverData, {
        previewObserverdata(tryCatch(
          {
            read_csv(input$customObserverData$datapath,
                     col_names = FALSE, show_col_types = FALSE)
          },
          error = function(e) {
            NULL
          }
        ))}, ignoreInit = TRUE)
      
      #Creates a temporary object
      previewObserverObj <- reactive({
        req(previewObserverdata())
        dt <- data.frame(previewObserverdata())
        if(as.logical(input$header)){
          names(dt) <- dt[1,]
          dt <- dt[-1, , drop = FALSE]
        }
        return(dt)
      })
      
      ### dynamic pickerInputs that read data set
      #CatchColumn
      output$observer_catch_ui <- renderUI({
        req(previewObserverObj())
        pickerInput(
          inputId = ns("observer_catch"),
          label = "Select column that contains catch",
          choices = names(previewObserverObj()),
          width = '100%'
        )
      })
      #Year Column
      output$observer_year_ui <- renderUI({
        req(previewObserverObj())
        pickerInput(
          inputId = ns("observer_year"),
          label = "Select column that contains year",
          choices = names(previewObserverObj()),
          width = '100%'
        )
      })
      #Effort column
      output$observer_effort_ui <- renderUI({
        req(previewObserverObj())
        pickerInput(
          inputId = ns("observer_effort"),
          label = "Select column that contains effort",
          choices = names(previewObserverObj()),
          width = '100%'
        )
      })
      
      #Data table output
      output$previewObserverDT <- renderDT({
        req(previewObserverObj())
        n<-NCOL(previewObserverObj())-1
        datatable(previewObserverObj(),
                  rownames=FALSE)
      })
     
      #Save button for observer data
      observerInputsInfo <- reactiveValues(dt = NULL, title = NULL, header = NULL, catchColumn = NULL,
                                         catchUnits = NULL, catchType = NULL, yearColumn = NULL, effortColumn = NULL,
                                         sampleUnits = NULL)
      
      
      observeEvent(input$observerupload_save,{ #maybe add if statement in case there is no data -> modal for user to input data before finalizing
        
        observerInputsInfo$dt <- previewObserverObj()
        observerInputsInfo$title <- input$observerdata_title
        observerInputsInfo$header <- input$header
        observerInputsInfo$catchColumn <- input$observer_catch
        observerInputsInfo$catchUnits <- input$observer_catchunits
        observerInputsInfo$catchType <- input$observer_catchtype
        observerInputsInfo$yearColumn <- input$observer_year
        observerInputsInfo$effortColumn <- input$observer_effort
        observerInputsInfo$sampleUnits <- input$observer_sampleunit

      })
      
      #-----------------------------
      #Reactives returned by module
      #-----------------------------
      observerInputsInfo
      
    }) #close function moduleServer
} #close observerupload_SERVER


