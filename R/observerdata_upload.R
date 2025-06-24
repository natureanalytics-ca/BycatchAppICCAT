#-------------------------------
#module upload observer data
#-------------------------------

observerupload_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(

    fluidRow(
      column(
        6,
        timelineBlock(
          width = 12,
          reversed = FALSE,
          timelineItem(
            title = "Start by attaching and previewing your data",
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
            pickerInput(
              inputId = ns("observer_catch"),
              label = "Select column that contains catch",
              choices = c("X1","X2","X3","X4")
            ),
            pickerInput(
              inputId = ns("observer_catchunits"),
              label = "Units of catch",
              choices = c("Numbers", "Weight in kg","Weight in tonnes")
            ),
            textInput(
              inputId = ns("observer_catchtype"),
              label = "What is the catch type (live discards, dead discards, etc.)?"
            ),
            pickerInput(
              inputId = ns("observer_year"),
              label = "Select column that contains year",
              choices = c("X1","X2","X3","X4")
            ),
            pickerInput(
              inputId = ns("observer_effort"),
              label = "Select column that contains effort",
              choices = c("X1","X2","X3","X4")
            ),
            prettyRadioButtons(
              inputId = ns("observer_sampleunit"),
              label = "What is the sample unit (i.e. what each row represents)?",
              choices = c("Sets", "Trips"),
              selected = FALSE,
              icon = icon("check"),
              animation = "jelly",
              inline=TRUE,
              status = "default"
            )

          )
          
        ) #close timelineBlock

      ), #close column
    
    column(
      6,
      box(
        width = 12,
        title = "Preview observer data set",
        collapsible = FALSE,
        br(),
        textInput(
          inputId = ns("observerdata_title"),
          label = "Add a brief title for your observer data set",
          width = '100%'
        ),
        br(),
        br(),
        div(
          style = "display: flex; align-items: center; justify-content: center;",
          actionBttn(
            inputId = "observerupload_save",
            label = "Save"
          ),
          actionBttn(
            inputId = "observerupload_cancel",
            label = "Cancel"
          ))
      )
    )

    ) #close fluidRow
    
    
    
  )

}

