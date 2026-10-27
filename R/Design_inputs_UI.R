#-------------------------------
#module data checks setup
#-------------------------------

############# UI #############

designSpecifications_UI <- function(id){
  
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
            tags$strong(tags$em("Specifications"))
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
        value = tags$strong("Specifications is not available"),
        subtitle = tagList(
          br(),
          "You must complete Bycatch setup before accessing this module."
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
        selected = "specificationsTab",
        tabPanel(
          title = "Specifications",
          value = "specificationsTab",
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
                        tags$li("Select one or more design stratifiction variables to be used in design-based stratification."),
                        tags$li("Total bycatch estimates are made at each level of the selected stratification variables."),
                        
                      )
                    ))
                ),
                "Select design stratification variable(s)"
              ),
              icon = icon("1"),
              fluidRow(
                column(
                  12,
                  pickerInput(
                    inputId = ns("strat_var"),
                    label = tagList("Select design stratification variable(s)", div("*", style = "display: inline; color: red;")),
                    choices = NULL,
                    width = '100%',
                    multiple = TRUE
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
                      "Tips for aligning fishing effort:",
                      tags$ul(
                        tags$li("Grouping variable(s) are the factor variables used to compute bycatch summary statistics."),
                        tags$li("For example, this can be used to calculate total bycatch within ICCAT statistical areas in each year."),
                        tags$li("Or, to summarize by a variable other than Year."),
                        tags$li("Grouping variable(s) are not required to also be stratification variables. Bycatch estimates are allocated across grouping variables that are not included as stratification variables in proportion to total logbook effort."),
                       )
                    ))
                ),
                "Grouping variable(s) (optional)"
              ),
              icon = icon("2"),
              fluidRow(
                column(
                  12,
                  pickerInput(
                    inputId = ns("group_var"),
                    label = "Select grouping variable(s)",
                    choices = NULL,
                    width = '100%',
                    multiple = TRUE
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
                      "Tips for aligning fishing effort:",
                      tags$ul(
                        tags$li("Two methods are available: Ratio and Delta."),
                        tags$li("You can include one or both methods in your analysis."),
                        tags$li("Refer to the user guide for technical details."),
                      )
                    ))
                ),
                "Analysis method(s)"
              ),
              icon = icon("3"),
              fluidRow(
                column(
                  12,
                  pickerInput(
                    inputId = ns("analysis_method"),
                    label = tagList("Select one or more analysis methods", div("*", style = "display: inline; color: red;")),
                    choices = c("Ratio", "Delta"),
                    selected = c("Ratio", "Delta"),
                    width = '100%',
                    multiple = TRUE,
                    options = pickerOptions(
                      maxOptions = 2
                    )
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
                      "Tips for aligning fishing effort:",
                      tags$ul(
                        tags$li("Maximum of 100 characters for the title.")
                      )
                    ))
                ),
                "Scenario name"
              ),
              icon = icon("5"),
              fluidRow(
                column(
                  12,
                  textInput(
                    inputId = ns("design_name"),
                    label = tagList(tags$strong("Specify a scenario name"), div("*", style = "display: inline; color: red;")),
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
                        tags$li("Required to complete the analysis."),
                        tags$li("If analysis is unsuccessful, follow suggestions in the modal to resolve issues.")
                      )
                    )
                  )
                ),
                "Run analysis"
              ),
              icon = icon("6"),
              fluidRow(
                column(
                  12,
                  div(
                    style = "display: flex; align-items: center; justify-content: center;",
                    actionBttn(
                      inputId = ns("run_design"),
                      label = tagList("Run analysis", div("*", style = "display: inline; color: red;")),
                      onclick = "document.getElementById('ui-shield').style.display = 'block';"
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          title = "Analysis summary",
          value = "analysisTab",
          div(
            style = "margin-top: 20px;",
            uiOutput(ns("analysisPreview"))
          ),
          fluidRow(
            column(
              12,
              div(
                style = "display: flex; align-items: center; justify-content: center; margin-top: 20px;",
                actionBttn(
                  inputId = ns("deactivate"),
                  label = "Reset analysis and return to previous",
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




