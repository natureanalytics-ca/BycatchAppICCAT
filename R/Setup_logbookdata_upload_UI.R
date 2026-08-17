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
      id = ns("logbookupload"),
      style="display: flex; flex-direction: row; flex-wrap: wrap; width: 100%; align-items: center; padding: 0px 32px 10px 0px;",
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
        tagList("Attach a file in csv format", div("*", style = "display: inline; color: red;")),
        fileInput(
          inputId = ns("customLogbookData"),
          label = NULL,
          buttonLabel = tags$strong("Attach", style = "color: #0d6efd;"),
          accept = c(".csv", "text/csv"),
          multiple = FALSE,
          width = "100%",
        ),
        actionBttn(
          inputId = ns("resetLogbookData"),
          label = "Clear file",
          size = "xs"
        )
      ),
      
      hidden(
        timelineItem(
          title = div(
            "Review your logbook dataset"
          ),
          icon = icon("2"),
          withSpinner(DTOutput(ns("previewLogbookDT"))),
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
                  h6("Select the name of the column in the logbook dataset that contains:"),
                  tags$ul(
                    tags$li("Year")
                  )
                ))
            ),
            "Assign required column names"
          ),
          icon = icon("3"),
          fluidRow(
            column(
              12,
              pickerInput(
                inputId = ns("logbook_year"),
                label = tagList("Select column that contains year", div("*", style = "display: inline; color: red;")),
                choices = NULL,
                width = '100%',
                multiple = TRUE,
                options = pickerOptions(
                  maxOptions = 1
                )
              ),
              #Warning text (initially empty)
              tags$div(
                id = ns("logbook_yearWarning"),
                style = "color: red; font-weight: bold; margin: 5px; display: none;",
                "Warning: Year is available but you selected something else!",
                br(),
                div(
                  style = "color: black; font-weight: normal;",
                  "This action is not allowed because 'Year' is a protected column name.",
                  br(),
                  "To correct this issue, do the following:",
                  tags$ul(
                    tags$li("Option 1: Select 'Year' as your year variable."),
                    tags$li("Option 2: Remove or rename 'Year' column in your dataset, re-upload file, and select your preferred year variable.")
                  )
                )
              )
            )
          )
        )
      )%>% shiny::tagAppendAttributes(id = ns("logBookInputs")),
      
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
            "Add a brief title for your logbook dataset"
          ),
          icon = icon("4"),
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
              label = "Advance to part 3: Align data and activate",
              width='100%'
            )
          )
        )%>% shiny::tagAppendAttributes(id = ns("finalize")) #close timelineItem
      )
    )#close timelineBlock
  )
}


