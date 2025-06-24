#-------------------------------
#module upload logbook data
#-------------------------------

logbookupload_UI <- function(id){
  
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
              inputId = ns("customLogbookData"),
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
              inputId = ns("logbook_year"),
              label = "Select column that contains year",
              choices = c("X1","X2","X3","X4")
            ),
            pickerInput(
              inputId = ns("logbook_effort"),
              label = "Select column that contains effort",
              choices = c("X1","X2","X3","X4")
            ),
            prettyRadioButtons(
              inputId = ns("logbook_sampleunit"),
              label = "What is the sample unit (i.e. what each row represents)?",
              choices = c("Sets", "Trips","Other (aggregated data)"), #TO ADD: textbox to specify what Other is
              selected = FALSE,
              icon = icon("check"),
              animation = "jelly",
              inline=TRUE,
              status = "default"
            ),
            pickerInput(
              inputId = ns("logbook_aggregationcolumn"),
              label = "If data is aggregated, indicate column that gives number of sample units",
              choices = c("X1","X2","X3","X4")
            )
          )
        ) #close timelineBlock
      ), #close column
      
      column(
        6,
        box(
          width = 12,
          title = "Preview logbook data set",
          collapsible = FALSE,
          br(),
          textInput(
            inputId = ns("logbookdata_title"),
            label = "Add a brief title for your logbook data set",
            width = '100%'
          ),
          br(),
          br(),
          div(
            style = "display: flex; align-items: center; justify-content: center;",
            actionBttn(
              inputId = "logbookupload_save",
              label = "Save"
            ),
            actionBttn(
              inputId = "logbookupload_cancel",
              label = "Cancel"
            ))
        )
      )
      
    ) #close fluidRow
  )
  
}

