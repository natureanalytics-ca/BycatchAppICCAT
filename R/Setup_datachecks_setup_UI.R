#-------------------------------
#module data checks setup
#-------------------------------

############# UI #############

datachecksSetup_UI <- function(id){
  
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
            dashboardBadge("Part 3", color = "primary", position = "left"),
            HTML("&nbsp;"),
            tags$strong(tags$em("Align data & activate"))
          )
        ),
        div("*Mandatory fields", style = "float: right; color: red; padding: 4px 10px;"),
      )
    ),
    br(),
    ###--------------------------------------------
    ### Warning when steps 1 or 2 are not complete
    ###--------------------------------------------
    div(
      id = ns("warningPanelToggle"),
      valueBox(
        value = tags$strong("Align data & activate is not available"),
        subtitle = tagList(
          br(),
          "Gather an observer dataset and a logbook dataset before completing Part 3.",
          br(),
          "If you have done so, review Parts 1 & 2 for missing entries in mandatory fields and warning messages."
        ),
        icon = icon("info"),
        color = "secondary",
        width = 12,
        href = NULL,
        footer = NULL,
        gradient = TRUE,
        elevation = NULL
      )
    ),
    #---------------------------
    #Main input panel
    #--------------------------
    hidden(div(
      id = ns("inputPanelToggle"),
      bs4Dash::tabsetPanel(
        id = ns("inputUIPanel"),
        type = "hidden",
        selected = "dataStructureTab",
        tabPanel(
          title = "Align effort",
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
                      "Tips for aligning fishing effort:",
                      tags$ul(
                        tags$li("Select the column names in the observer data and corresponding names in the logbook data for fishing effort. Another time step other than year can be used."),
                        tags$li("Indicate what is the sample unit is in the observer data (each row should represent a trip or set)."),
                        tags$li("Indicate whether the logbook data is structured in the same way. If not, the logbook data is likely to be further aggregated."),
                        tags$li("If further aggregated, indicate what is the column in the logbook data that contains the number of sample units per row."),
                        tags$li("For example, if the sample unit in the observer data is trips, but the logbook data is aggregated by year and area, the user should have a column that defines the number of logbook trips by year and area."),
                        tags$li(tags$strong("Setting up fishing effort is tricky!"), "See the expandable list of alternative effort configuration for assistance in setting up your own datasets.")
                      )
                    ))
                ),
                "Specify and align fishing effort"
              ),
              icon = icon("1"),
              fluidRow(
                column(
                  6,
                  div(
                    style = "margin: 10px 7.5px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
                    div(icon("binoculars"), HTML("&nbsp;"), tags$strong(tags$em("Observer effort")), style = "margin-bottom: 10px;"),
                    pickerInput(
                      inputId = ns("observer_effort"),
                      label = tagList("Select column that contains effort", div("*", style = "display: inline; color: red;")),
                      choices = NULL,
                      width = '100%',
                      multiple = TRUE,
                      options = pickerOptions(
                        maxOptions = 1
                      )
                    ),
                    tags$div(
                      id = ns("observer_effortWarning"),
                      style = "color: red; font-weight: bold; margin: 5px; display: none;",
                      "Warning: Effort is available but you selected something else!",
                      br(),
                      div(
                        style = "color: black; font-weight: normal;",
                        "This action is not allowed because 'Effort' is a protected column name.",
                        br(),
                        "To correct this issue, do the following:",
                        tags$ul(
                          tags$li("Option 1: Select 'Effort' as your effort variable."),
                          tags$li("Option 2: Remove or rename 'Effort' column in your dataset, re-upload file, and select your preferred effort variable.")
                        )
                      ),
                      br()
                    ),
                    pickerInput(
                      inputId = ns("observer_sampleunit"),
                      label = tagList("Specify sample unit of observer effort", div("*", style = "display: inline; color: red;"), br(), "(i.e., what does each row represent?)"),
                      choices = c("Trip", "Set"),
                      selected = NULL,
                      width = '100%',
                      multiple = TRUE,
                      options = pickerOptions(
                        maxOptions = 1
                      )
                    )
                  )
                  
                ),
                column(
                  6,
                  div(
                    style = "margin: 10px 7.5px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
                    div(icon("ship"), HTML("&nbsp;"), tags$strong(tags$em("Logbook effort")), style = "margin-bottom: 10px;"),
                    pickerInput(
                      inputId = ns("logbook_effort"),
                      label = tagList("Select column that contains effort", div("*", style = "display: inline; color: red;")),
                      choices = NULL,
                      width = '100%',
                      multiple = TRUE,
                      options = pickerOptions(
                        maxOptions = 1
                      )
                    ),
                    tags$div(
                      id = ns("logbook_effortWarning"),
                      style = "color: red; font-weight: bold; margin: 5px; display: none;",
                      "Warning: Effort is available but you selected something else!",
                      br(),
                      div(
                        style = "color: black; font-weight: normal;",
                        "This action is not allowed because 'Effort' is a protected column name.",
                        br(),
                        "To correct this issue, do the following:",
                        tags$ul(
                          tags$li("Option 1: Select 'Effort' as your effort variable."),
                          tags$li("Option 2: Remove or rename 'Effort' column in your dataset, re-upload file, and select your preferred effort variable.")
                        )
                      ),
                      br()
                    ),
                    pickerInput(
                      inputId = ns("logbook_sampleunit"),
                      label = tagList("Specify aggregation level of logbook effort", div("*", style = "display: inline; color: red;"), br(), "(i.e., is logbook data aggregated further than observer data?)"),
                      choices = c("Same aggregation as observer data", "Aggregated further"),
                      selected = NULL,
                      width = '100%',
                      multiple = TRUE,
                      options = pickerOptions(
                        maxOptions = 1
                      )
                      
                    ),
                    hidden(
                      div(
                        id = ns("logbook_aggregation"),
                        pickerInput(
                          inputId = ns("logbook_aggregationcolumn"),
                          label = tagList("Specify column containing counts of sample units", div("*", style = "display: inline; color: red;")),
                          choices = NULL,
                          width = '100%',
                          multiple = TRUE,
                          options = pickerOptions(
                            maxOptions = 1
                          )
                        ),
                        #Warning text (initially empty)
                        tags$div(
                          id = ns("logbook_aggregationcolumnWarning2"),
                          style = "color: red; font-weight: bold; margin: 5px; display: none;",
                          "Warning: SampleUnits is available but you selected something else!",
                          br(),
                          div(
                            style = "color: black; font-weight: normal;",
                            "This action is not allowed because 'SampleUnits' is a protected column name.",
                            br(),
                            "To correct this issue, do the following:",
                            tags$ul(
                              tags$li("Option 1: Select 'SampleUnits' as your counts variable."),
                              tags$li("Option 2: Remove or rename 'SampleUnits' column in your dataset, re-upload file, and select your preferred counts variable.")
                            )
                          ),
                          br()
                        ),
                        #Warning text (initially empty)
                        tags$div(
                          id = ns("logbook_aggregationcolumnWarning"),
                          style = "color: red; font-weight: bold; margin: 5px 5px 20px 5px; display: none;",
                          "Warning: Counts of sample units and effort columns are identical.",
                          div(
                            style = "color: black; font-weight: normal;",
                            "This action is not allowed because counts of sample units and effort columns must be different."
                          ),
                          br()
                        )
                      )
                    )
                  )
                )
              ),
              box(
                title = tagList(
                  div(
                    tags$strong("Help! Setting up fishing effort is tricky"),
                    br(),
                    tags$em("Expand tab to find an example that aligns with your data structure:")
                  )
                ),
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                div(
                  style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
                  div(icon("a"), style = "margin-left: 7.5px;"),
                  tags$img(
                    src = "trips_sameAgg.png",  
                    width = "100%"
                  )
                ),
                div(
                  style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
                  div(icon("b"), style = "margin-left: 7.5px;"),
                  tags$img(
                    src = "sets_sameAgg.png",  
                    width = "100%"
                  )
                ),
                div(
                  style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
                  div(icon("c"), style = "margin-left: 7.5px;"),
                  tags$img(
                    src = "trips_furtherAgg.png",  
                    width = "100%"
                  )
                ),
                div(
                  style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
                  div(icon("d"), style = "margin-left: 7.5px;"),
                  tags$img(
                    src = "sets_furtherAgg.png",  
                    width = "100%"
                  )
                )
              )
            ),
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
                      "Select factor variables:",
                      tags$ul(
                        tags$li("Specify variables that should be interpreted as factors."),
                        tags$li("Year is required and has been populated for you (no need to repeat this entry)."),
                        br(),
                        br(),
                        tags$li("Factors are selected by clicking on add a variable and choosing a column name from the observer dataset and matching it with the corresponding column name in the logbook dataset."),
                        tags$li("Column names from the observer dataset and logbook dataset should almost always be identical."),
                        tags$li("In cases where names do not match, re-naming of the logbook factor will occur using the name from the observer dataset."),
                        tags$li("Matching factor names are required for analysis."),
                        tags$li("Re-naming can sometimes cause data processing errors. Please use care when choosing to re-name variables.")
                      )
                    ))
                ),
                "Select factor variables (optional)"
              ),
              icon = icon("2"),
              fluidRow(
                column(
                  12,
                  div(
                    style = "display: flex; flex-direction: row; justify-content: space-between; align-items: flex-start;",
                    div(
                      style = "margin-bottom: 10px;",
                      actionBttn(
                        inputId = ns("addFactor"),
                        label = "Add variable"
                      )
                    ),
                    div(
                      div(
                        id = ns("warningFactorObserver"),
                        style = "color: red; font-weight: bold; margin-right: 5px; display: none;",
                        "Warning: observer effort cannot be selected as a factor variable!"
                      ),
                      div(
                        id = ns("warningFactorLogbook"),
                        style = "color: red; font-weight: bold; margin-right: 5px; display: none;",
                        "Warning: logbook effort cannot be selected as a factor variable!"
                      )
                    )
                  ),
                  uiOutput(ns("yearPlaceholder")),
                  tags$div(id = ns("factor_insertUI"))
                )
              )
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
                        tags$li("Specify which variables should be interpreted as numeric/continuous. These are optional."),
                        tags$li("Examples of numeric variablesmay include: latitude, longitude, year, or month.")
                      )
                    ))
                ),
                "Select numeric variables (optional)"
              ),
              icon = icon("3"),
              fluidRow(
                column(
                  12,
                  actionBttn(
                    inputId = ns("addNumeric"),
                    label = "Add variable"
                  ),
                  tags$div(id = ns("numeric_insertUI"))
                )
              )
            )),
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
                        tags$li("Provide a common name and a scientific name for the species used (maximum of 20 characters)."),
                        tags$li("Provide a brief description (maximum of 50 characters).")
                      )
                    )
                  )
                ),
                "Specify metadata"
              ),
              icon = icon("3"),
              fluidRow(
                column(
                  12,
                  textInput(
                    inputId = ns("spp_name"),
                    label = tagList(tags$strong("Add a species common name"), div("*", style = "display: inline; color: red;")),
                    width = '100%'
                  ),
                  textInput(
                    inputId = ns("spp_scientificname"),
                    label = tagList(tags$strong("Add a species scientific name"), div("*", style = "display: inline; color: red;")),
                    width = '100%'
                  ),
                  textInput(
                    inputId = ns("datachecks_name"),
                    label = tagList(tags$strong("Add a brief description"), div("*", style = "display: inline; color: red;")),
                    width = '100%'
                  )
                )
              )
            ),
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
                        tags$li("Required before advancing to other modules."),
                        tags$li("If activation is unsuccessful, follow suggestions in the modal to resolve issues.")
                      )
                    )
                  )
                ),
                "Activate bycatch setup"
              ),
              icon = icon("4"),
              fluidRow(
                column(
                  12,
                  div(
                    style = "display: flex; align-items: center; justify-content: center;",
                    actionBttn(
                      inputId = ns("run_datachecks"),
                      label = tagList("Activate", div("*", style = "display: inline; color: red;")),
                      onclick = "document.getElementById('ui-shield').style.display = 'block';"
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          title = "Activation summary",
          value = "runChecksTab",
          div(
            style = "margin-top: 20px;",
            uiOutput(ns("checksPreview"))
          ),
          fluidRow(
            column(
              6,
              reactableOutput(ns("observer_effortLinkage"))
            ),
            column(
              6,
              reactableOutput(ns("logbook_effortLinkage"))
            )
          ),
          fluidRow(
            column(
              12,
              div(
                style = "display: flex; align-items: center; justify-content: center; margin-top: 20px;",
                actionBttn(
                  inputId = ns("deactivate"),
                  label = "Deactivate and return to previous",
                  onclick = "document.getElementById('ui-shield').style.display = 'block';"
                )
              )
            )
          )
        )
      )
    ))
  )
}




