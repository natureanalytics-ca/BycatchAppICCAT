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
          div(
            dashboardBadge("Part 1", color = "primary", position = "left"),
            HTML("&nbsp;"),
            tags$strong(tags$em("Gather observer data"))
          )
        ),
        div("*Mandatory fields", style = "float: right; color: red; padding: 4px 10px;"),
      )
    ),
    br(),
    bs4Dash::tabsetPanel(
      id = ns("inputUIPanel"),
      type = "hidden",
      selected = "dataStructureTab",
      tabPanel(
        title = NULL,
        value = "dataStructureTab",
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
                      tags$li("Example data sets are available on the Instructions page."),
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
              buttonLabel = tags$strong("Attach", style = "color: #0d6efd;"),
              accept = c(".csv", "text/csv"),
              multiple = FALSE,
              width = "100%",
            ),
            actionBttn(
              inputId = ns("resetObserverData"),
              label = "Clear file",
              size = "xs"
            )
          ),
          hidden(
            timelineItem(
              title = div(
                "Review your observer dataset"
              ),
              icon = icon("2"),
              withSpinner(DTOutput(ns("previewObserverDT"))),
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
                      h6("Select the name of the column in the observer dataset that contains:"),
                      tags$ul(
                        tags$li("Year"),
                        tags$li("If your variable name is missing from the list, review user guide section on Protected names.")
                      )
                    ))
                ),
                "Assign required year column"
              ),
              icon = icon("3"),
              fluidRow(
                column(
                  12,
                  pickerInput(
                    inputId = ns("observer_year"),
                    label = tagList("Select data column that contains year", div("*", style = "display: inline; color: red;")),
                    choices = NULL,
                    width = '100%',
                    multiple = TRUE,
                    options = pickerOptions(
                      maxOptions = 1
                    )
                  ),
                  #Warning text (initially empty)
                  tags$div(
                    id = ns("observer_yearWarning"),
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
          )%>% shiny::tagAppendAttributes(id = ns("observerInputs")),
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
                      h6("Select the name of the column in the observer dataset that contains:"),
                      tags$ul(
                        tags$li("Catch."),                        
                        tags$li("If your variable name is missing from the list, review user guide section on Protected names.")

                      )
                    ))
                ),
                "Assign required catch details"
              ),
              icon = icon("4"),
              fluidRow(
                column(
                  12,
                  pickerInput(
                    inputId = ns("observer_catch"),
                    label = tagList("Select data column that contains catch", div("*", style = "display: inline; color: red;")),
                    choices = NULL,
                    width = '100%',
                    multiple = TRUE,
                    options = pickerOptions(
                      maxOptions = 1
                    )
                  ),
                  #Warning text (initially empty)
                  tags$div(
                    id = ns("observer_catchWarning"),
                    style = "color: red; font-weight: bold; margin: 5px 5px 20px 5px; display: none;",
                    "Warning: catch column and year column are identical.",
                    div(
                      style = "color: black; font-weight: normal;",
                      "This action is not allowed because catch column and year column must be different."
                    ),
                    br()
                  ),
                  #Warning text (initially empty)
                  tags$div(
                    id = ns("observer_catchWarning2"),
                    style = "color: red; font-weight: bold; margin: 5px; display: none;",
                    "Warning: Catch is available but you selected something else!",
                    br(),
                    div(
                      style = "color: black; font-weight: normal;",
                      "This action is not allowed because 'Catch' is a protected column name.",
                      br(),
                      "To correct this issue, do the following:",
                      tags$ul(
                        tags$li("Option 1: Select 'Catch' as your catch variable."),
                        tags$li("Option 2: Remove or rename 'Catch' column in your dataset, re-upload file, and select your preferred catch variable.")
                      )
                    ),
                    br()
                  ),
                  pickerInput(
                    inputId = ns("observer_catchunits"),
                    label = tagList("Create label for units of catch", div("*", style = "display: inline; color: red;")),
                    choices = c("Numbers", "Weight in kg","Weight in tonnes","Other"),
                    selected = character(0),
                    width = '100%',
                    multiple = TRUE,
                    options = pickerOptions(
                      maxOptions = 1
                    )
                  ),
                  pickerInput(
                    inputId = ns("observer_catchtype"),
                    label = tagList("Create label for catch type", div("*", style = "display: inline; color: red;")),
                    choices = c("Dead discards", "Live discards","Discards","Bycatch","Other"),
                    selected = character(0),
                    width = '100%',
                    multiple = TRUE,
                    options = pickerOptions(
                      maxOptions = 1
                    )
                  )
                )
              )
            )
          )%>% shiny::tagAppendAttributes(id = ns("observerInputs2")),
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
                "Add a brief title for your observer dataset"
              ),
              icon = icon("5"),
              textInput(
                inputId = ns("observerdata_title"),
                label = tagList("Create label for observer dataset", div("*", style = "display: inline; color: red;")),
                width = '100%'
              ),
              br(),
              div(
                style = "display: flex; align-items: center; justify-content: center;",
                actionBttn(
                  inputId = ns("observerupload_save"),
                  label = "Advance to part 2: Gather logbook data",
                  width='100%'
                )
              )
            )%>% shiny::tagAppendAttributes(id = ns("finalize")) #close timelineItem
          )
        )
      ), #close timelineBlock
      tabPanel(
        title = NULL,
        value = "freezeTab",
        valueBox(
          value = tags$strong("Gather observer data complete"),
          subtitle = tagList(
            br(),
            "To modify observer data, deactiviate your bycatch setup within the 'Align and activate' tab.",
          ),
          icon = icon("check"),
          color = "success",
          width = 12,
          href = NULL,
          footer = NULL,
          gradient = TRUE,
          elevation = NULL
        )
      )
    )
  )
}

