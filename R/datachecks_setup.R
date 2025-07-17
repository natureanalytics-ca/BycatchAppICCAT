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
      style="display: flex; flex-direction: row; flex-wrap: wrap; width: 100%; align-items: center; padding: 0px 32px 0px 0px;",
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
          margin: 10px 0px;"
        ,
        div(
          style = "float: right; padding: 4px 20px;",
          tags$strong(tags$em("Data checks setup"))
        )
      )
    ),
    hidden(
      div(
        id = ns("warningPanelToggle"),
        style = "margin: 0px 25px 0px 0px;",
        valueBox(
          value = tags$strong("Run checks is not available"),
          subtitle = "To run checks, first load both observer data and logbook data.",
          icon = icon("info"),
          color = "secondary",
          width = 12,
          href = NULL,
          footer = NULL,
          gradient = TRUE,
          elevation = NULL
        )
      )
    ),
    div(
      id = ns("inputPanelToggle"),
      div("*Mandatory field", style = "float: right; color: red; padding: 0px 32px 0px 0px;"),
      bs4Dash::tabsetPanel(
        id = ns("inputUIPanel"),
        type = "pills",
        tabPanel(
          title = div(
            strong("Data structure"),
            HTML("&nbsp;"),
            icon("play")
          ),
          value = "dataStructureTab",
          div(
            style = "margin-top: 20px;",
            uiOutput(ns("dataStructure"))
          )
        ),
        tabPanel(
          title = div(
            strong("Exploratory variables"),
            HTML("&nbsp;"),
            icon("play")
          ),
          value = "exploratoryVariablesTab",
          div(
            style = "margin-top: 20px;",
            uiOutput(ns("exploratoryVariables"))
          )
        ),
        tabPanel(
          title = div(
            strong("Run checks"),
            HTML("&nbsp;")
          ),
          value = "runChecksTab",
          div(
            style = "margin-top: 20px;",
            uiOutput(ns("runChecks"))
          )
        )
      )
    )
  )
}


############# SERVER #############
datachecksSetup_SERVER <- function(id, observerdataInput = reactive(NULL), logbookdataInput = reactive(NULL)){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      #---------------------------
      #Input constraints on nchar
      #---------------------------
      shinyjs::runjs("$('#datachecks_setup-spp_name').attr('maxlength', 20)")
      shinyjs::runjs("$('#datachecks_setup-spp_scientificname').attr('maxlength', 20)")
      shinyjs::runjs("$('#datachecks_setup-datachecks_name').attr('maxlength', 50)")
      
      #-----------------------------
      #Reset when new dataset loaded
      #-----------------------------
      observeEvent(
        c(observerdataInput()$dt, logbookdataInput()$dt), {
          updateTabsetPanel(
            session, 
            inputId = "inputUIPanel", 
            selected = "dataStructureTab"
          )
        shinyjs::toggle("warningPanelToggle", condition = isTRUE(is.null(observerdataInput()$dt) | is.null(logbookdataInput()$dt)))  
        shinyjs::toggle("inputPanelToggle", condition = isTRUE(!is.null(observerdataInput()$dt) & !is.null(logbookdataInput()$dt)))  
        
      }, ignoreNULL = FALSE)
      
      #--------------
      #Data structure
      #--------------
      output$dataStructure <- renderUI({
       tagList(
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
                      "Select the name of the column in the observer data that contains catch, and select the units of catch
                      and catch type."
                    ))
                ),
                "Observer data"
              ),
              icon = icon("binoculars"),
              fluidRow(
                column(
                  12,
                  pickerInput(
                    inputId = ns("observer_catch"),
                    label = tagList("Select column that contains catch", div("*", style = "display: inline; color: red;")),
                    choices = names(observerdataInput()$dt),
                    width = '100%'
                  ),
                  pickerInput(
                    inputId = ns("observer_catchunits"),
                    label = tagList("Units of catch", div("*", style = "display: inline; color: red;")),
                    choices = c("Numbers", "Weight in kg","Weight in tonnes","Other"),
                    width = '100%'
                  ),
                  pickerInput(
                    inputId = ns("observer_catchtype"),
                    label = tagList("Catch type", div("*", style = "display: inline; color: red;")),
                    choices = c("Dead discards", "Live discards","Discards","Bycatch","Other"), 
                    width = '100%'
                  ),
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
                      "Select the column names in the observer data and corresponding names in the logbook data for the year variable
                      and effort variable. Another time step other than year can be used. Indicate what is the sample unit in the observer data
                      (each row should represent a trip or set) and if the logbook data is structured in the same way. If not, the logbook data is
                      likely to be aggreated, and the user should indicate what is the column in the logbook data that contains the number of sample 
                      units. For example, if the sample unit in the observer data is trips, but the logbook data is aggregated by year and area, the user 
                      should have a column that defines the number of logbook trips by year and area."
                    ))
                ),
                "Specify corresponding data structures"
              ),
              icon = icon("clipboard-check"),
              fluidRow(
                column(
                  6,
                  div(icon("binoculars"), HTML("&nbsp;"), tags$strong(tags$em("Observer data"))),
                ),
                column(
                  6,
                  div(icon("ship"), HTML("&nbsp;"), tags$strong(tags$em("Corresponding logbook data"))),
                )
              ),
              div(
                style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
                fluidRow(
                  column(
                    6,
                    pickerInput(
                      inputId = ns("observer_year"),
                      label = tagList("Select column that contains year", div("*", style = "display: inline; color: red;")),
                      choices = names(observerdataInput()$dt),
                      width = '100%'
                    )
                  ),
                  column(
                    6,
                    pickerInput(
                      inputId = ns("logbook_year"),
                      label = tagList("Select column that contains year", div("*", style = "display: inline; color: red;")),
                      choices = names(logbookdataInput()$dt),
                      width = '100%'
                    )
                  )
                )
              ),
              div(
                style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
                fluidRow(
                  column(
                    6,
                    pickerInput(
                      inputId = ns("observer_effort"),
                      label = tagList("Select column that contains effort", div("*", style = "display: inline; color: red;")),
                      choices = names(observerdataInput()$dt),
                      width = '100%'
                    )
                  ),
                  column(
                    6,
                    pickerInput(
                      inputId = ns("logbook_effort"),
                      label = tagList("Select column that contains effort", div("*", style = "display: inline; color: red;")),
                      choices = names(logbookdataInput()$dt),
                      width = '100%'
                    ),
                  )
                )
              ),   
              div(
                style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
                fluidRow(
                  column(
                    6,
                    prettyRadioButtons(
                      inputId = ns("observer_sampleunit"),
                      label = tagList("What is the sample unit (i.e. what each row represents)?", div("*", style = "display: inline; color: red;")),
                      choices = c("Trips", "Sets"),
                      selected = "Trips",
                      icon = icon("check"),
                      animation = "jelly",
                      inline=TRUE,
                      status = "default"
                    )
                  ),
                  column(
                    6,
                    prettyRadioButtons(
                      inputId = ns("logbook_sampleunit"),
                      label = tagList("Same sample units as in observer data?", div("*", style = "display: inline; color: red;")),
                      choiceNames = c("Yes", "No (aggregated data)"),
                      choiceValues = c("Yes", "No"),
                      selected = "Yes",
                      icon = icon("check"),
                      animation = "jelly",
                      inline=TRUE,
                      status = "default"
                    ),
                    hidden(
                      div(
                        id = ns("logbook_aggregation"),
                        pickerInput(
                          inputId = ns("logbook_aggregationcolumn"),
                          label = "Indicate column that gives number of sample units",
                          choices = c("NA", names(logbookdataInput()$dt)),
                          selected = "NA",
                          width = '100%'
                        )
                      )
                    )
                  )
                )
              )   
            )
          )
        )
      })
      
      observeEvent(input$logbook_sampleunit, {
        shinyjs::toggle("logbook_aggregation", condition = input$logbook_sampleunit == "No")
        updatePickerInput(
          inputId = "logbook_aggregationcolumn",
          selected = "NA"
        )
      })
      
      #---------------------
      #Exploratory variables
      #---------------------
      #Factors
      factorNames <- reactiveValues()
      #Numeric 
      numericNames <- reactiveValues()
      
      output$exploratoryVariables <- renderUI({
        tagList(
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
                      "Guidance add here."
                    ))
                ),
                div("Select factor variables", div("*", style = "display: inline; color: red;"))
              ),
              icon = icon("clipboard-check"),
              fluidRow(
                column(
                  12,
                  actionBttn(
                    inputId = ns("addFactor"),
                    label = "Add variable"
                  ),
                  tags$div(id = ns("factor_insertUI")),
                  div(
                    style = "margin-top: 30px;",
                    DTOutput(ns("factorDT"))
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
                      "Guidance add here."
                    ))
                ),
                "Select numeric variables (optional)"
              ),
              icon = icon("clipboard-check"),
              fluidRow(
                column(
                  12,
                  actionBttn(
                    inputId = ns("addNumeric"),
                    label = "Add variable"
                  ),
                  tags$div(id = ns("numeric_insertUI")),
                  div(
                    style = "margin-top: 30px;",
                    DTOutput(ns("numericDT"))
                  )
                )
              )
            )
          )  
        )
      })
      
      ### Factors
      observeEvent(input$addFactor, {
        divID <- gsub("\\.", "", format(Sys.time(), "%H%M%OS3"))
        btnID <- paste0(divID, "remove")
        layoutID <- paste0(divID, "layout")
        picker1ID <- paste0(divID, "picker1")
        picker2ID <- paste0(divID, "picker2")
        
        #remove
        observeEvent(input[[btnID]], {
          removeUI(selector = paste0("#", session$ns(divID)))
          factorNames[[divID]] <- NULL
        }, ignoreInit = TRUE, once = TRUE)
        
        #Update paired factors
        observeEvent(
          c(input[[picker1ID]], input[[picker2ID]]), {
            factorNames[[divID]] <- c(input[[picker1ID]], input[[picker2ID]])
        })
      
        #Layout to display
        output[[layoutID]] <-renderUI({
          div(
            style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px;",
            div(
              style = "float: right; margin: 8px;",
              actionBttn(
                inputId = ns(btnID),
                label = "Remove",
                size = "xs",
              )
            ),
            div(
              style = "margin: 8px;",
              tags$strong(tags$em("Select corresponding variables"))
            ),
            
            fluidRow(
              column(
                6,
                pickerInput(
                  inputId = ns(picker1ID),
                  label = div(icon("binoculars"), HTML("&nbsp;"), tags$strong("Observer data")),
                  choices = names(observerdataInput()$dt),
                  width = '100%',
                  options = pickerOptions(
                    actionsBox = FALSE,
                    size = 10,
                    selectedTextFormat = "count > 4"
                  ),
                  multiple = FALSE
                )
              ),
              column(
                6,
                pickerInput(
                  inputId = ns(picker2ID),
                  label = div(icon("ship"), HTML("&nbsp;"), tags$strong("Logbook data")),
                  choices = names(logbookdataInput()$dt),
                  width = '100%',
                  options = pickerOptions(
                    actionsBox = FALSE,
                    size = 10,
                    selectedTextFormat = "count > 4"
                  ),
                  multiple = FALSE
                )
              )
            )
          )
        })
        
        #Insert UI
        insertUI(
          selector = paste0("#", session$ns("factor_insertUI")),
          where = "afterBegin",
          immediate = TRUE,
          ui = tags$div(id = session$ns(divID),
                        uiOutput(session$ns(layoutID))  
          ),
          session = session
        )
      })
        
      #Selected factors table
      output$factorDT <- renderDT({
        nm<-names(factorNames)
        bld <-data.frame()
        for (i in nm){
          if(!is.null(factorNames[[i]])){
            bld<-rbind(bld, 
                       list(
                         id=i, 
                         observer = factorNames[[i]][1],
                         logbook = factorNames[[i]][2],
                         newLogbook = factorNames[[i]][1])
            )
          }
        }
        datatable(bld[,-1],
                  options = list(
                    columnDefs = list(list(className = 'dt-left', targets = c(0,1,2))),
                    dom = 't',
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#ADB5BD', 'color': '#343a40'});",
                      "}")
                  ),
                  colnames=c("Observer variable", "Logbook variable", "Logbook renaming"),
                  rownames=FALSE,
                  selection = 'none',
                  caption = tags$strong(tags$em("Selected pairings"))
                )
      })
      
      #### Numeric
      observeEvent(input$addNumeric, {
        divID <- gsub("\\.", "", format(Sys.time(), "%H%M%OS3"))
        btnID <- paste0(divID, "remove")
        layoutID <- paste0(divID, "layout")
        picker1ID <- paste0(divID, "picker1")
        picker2ID <- paste0(divID, "picker2")
        
        #remove
        observeEvent(input[[btnID]], {
          removeUI(selector = paste0("#", session$ns(divID)))
          numericNames[[divID]] <- NULL
        }, ignoreInit = TRUE, once = TRUE)
        
        #Update paired factors
        observeEvent(
          c(input[[picker1ID]], input[[picker2ID]]), {
            numericNames[[divID]] <- c(input[[picker1ID]], input[[picker2ID]])
        })
        
        #Layout to display
        output[[layoutID]] <-renderUI({
          div(
            style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px;",
            div(
              style = "float: right; margin: 8px;",
              actionBttn(
                inputId = ns(btnID),
                label = "Remove",
                size = "xs",
              )
            ),
            div(
              style = "margin: 8px;",
              tags$strong(tags$em("Select corresponding variables"))
            ),
            
            fluidRow(
              column(
                6,
                pickerInput(
                  inputId = ns(picker1ID),
                  label = div(icon("binoculars"), HTML("&nbsp;"), tags$strong("Observer data")),
                  choices = names(observerdataInput()$dt),
                  width = '100%',
                  options = pickerOptions(
                    actionsBox = FALSE,
                    size = 10,
                    selectedTextFormat = "count > 4"
                  ),
                  multiple = FALSE
                )
              ),
              column(
                6,
                pickerInput(
                  inputId = ns(picker2ID),
                  label = div(icon("ship"), HTML("&nbsp;"), tags$strong("Logbook data")),
                  choices = names(logbookdataInput()$dt),
                  width = '100%',
                  options = pickerOptions(
                    actionsBox = FALSE,
                    size = 10,
                    selectedTextFormat = "count > 4"
                  ),
                  multiple = FALSE
                )
              )
            )
          )
        })
        
        #Insert UI
        insertUI(
          selector = paste0("#", session$ns("numeric_insertUI")),
          where = "afterBegin",
          immediate = TRUE,
          ui = tags$div(id = session$ns(divID),
                        uiOutput(session$ns(layoutID))  
          ),
          session = session
        )
      })
      
      #Selected factors table
      output$numericDT <- renderDT({
        nm<-names(numericNames)
        bld <-data.frame()
        for (i in nm){
          if(!is.null(numericNames[[i]])){
            bld<-rbind(bld, 
                       list(
                         id=i, 
                         observer = numericNames[[i]][1],
                         logbook = numericNames[[i]][2],
                         newLogbook = numericNames[[i]][1])
            )
          }
        }
        datatable(bld[,-1],
                  options = list(
                    columnDefs = list(list(className = 'dt-left', targets = c(0,1,2))),
                    dom = 't',
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#ADB5BD', 'color': '#343a40'});",
                      "}")
                  ),
                  colnames=c("Observer variable", "Logbook variable", "Logbook renaming"),
                  rownames=FALSE,
                  selection = 'none',
                  caption = tags$strong(tags$em("Selected pairings"))
        )
      })
      
      
      ###########
      #Run checks
      ###########
      output$runChecks <- renderUI({
        tagList(
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
                      "Guidance add here."
                    ))
                ),
                "Add metadata"
              ),
              icon = icon("clipboard-check"),
              fluidRow(
                column(
                  12,
                  textInput(
                    inputId = ns("spp_name"),
                    label = tagList(tags$strong("Species common name"), div("*", style = "display: inline; color: red;")),
                    width = '100%'
                  ),
                  textInput(
                    inputId = ns("spp_scientificname"),
                    label = tagList(tags$strong("Species scientific name"), div("*", style = "display: inline; color: red;")),
                    width = '100%'
                  ),
                  textInput(
                    inputId = ns("datachecks_name"),
                    label = tagList(tags$strong("Add a brief description of data checks run"), div("*", style = "display: inline; color: red;")),
                    width = '100%'
                  ),
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
                      "Guidance add here."
                    ))
                ),
                "Run checks & get results"
              ),
              icon = icon("chart-simple"),
              fluidRow(
                column(
                  12,
                  uiOutput(ns("checksPreview")),
                )
              ),
              fluidRow(
                column(
                  12,
                  div(
                    style = "display: flex; align-items: center; justify-content: center;",
                    actionBttn(
                      inputId = ns("run_datachecks"),
                      label = tagList("Run data checks", div("*", style = "display: inline; color: red;")),
                      
                    )
                  )
                )
              )
            )
          )
        )
      })
      
      ##Summary of input selections
      output$checksPreview <- renderUI({
        
        #Get factor names
        nm<-names(factorNames)
        bldFactor <-data.frame()
        for (i in nm){
          if(!is.null(factorNames[[i]])){
            bldFactor<-rbind(bldFactor, 
                       list(
                         id=i, 
                         observer = factorNames[[i]][1],
                         logbook = factorNames[[i]][2]
                       )
            )
          }
        }

        #Get numeric names
        nm<-names(numericNames)
        bldNumeric <-data.frame()
        for (i in nm){
          if(!is.null(numericNames[[i]])){
            bldNumeric<-rbind(bldNumeric, 
                             list(
                               id=i, 
                               observer = numericNames[[i]][1],
                               logbook = numericNames[[i]][2]
                             )
            )
          }
        }

        tagList(
          tags$strong(tags$em("Input summary")),
          div(
            style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
            fluidRow(
              column(
                4,
                div(icon("clipboard-check"), HTML("&nbsp;"), tags$strong(tags$em("Input"))),
              ),
              column(
                4,
                div(icon("binoculars"), HTML("&nbsp;"), tags$strong(tags$em("Observer data"))),
              ),
              column(
                4,
                div(icon("ship"), HTML("&nbsp;"), tags$strong(tags$em("Logbook data"))),
              )
            ),
            fluidRow(
              column(
                4,
                tags$strong("Title:")
              ),
              column(
                4,
                div(style = "color: #007bff;", observerdataInput()$title)
              ),
              column(
                4,
                div(style = "display: inline; color: #007bff;", logbookdataInput()$title)
              )
            ),
            fluidRow(
              column(
                4,
                tags$strong("Catch:")
              ),
              column(
                4,
                div(style = "color: #007bff;", input$observer_catch)
              ),
              column(
                4,
              )
            ),
            fluidRow(
              column(
                4,
                tags$strong("Units of catch:")
              ),
              column(
                4,
                div(style = "color: #007bff;", input$observer_catchunits)
              ),
              column(
                4,
              )
            ),
            fluidRow(
              column(
                4,
                tags$strong("Catch type:")
              ),
              column(
                4,
                div(style = "color: #007bff;", input$observer_catchtype)
              ),
              column(
                4,
              )
            ),
            fluidRow(
              column(
                4,
                tags$strong("Year:")
              ),
              column(
                4,
                div(style = "color: #007bff;", input$observer_year)
              ),
              column(
                4,
                if(input$observer_year == input$logbook_year) {div(style = "color: #007bff;", input$logbook_year)},
                if(input$observer_year != input$logbook_year) {div(style = "color: #007bff;", input$logbook_year, icon("arrow-right"), input$observer_year)}
              )
            ),
            fluidRow(
              column(
                4,
                tags$strong("Effort:")
              ),
              column(
                4,
                div(style = "color: #007bff;", input$observer_effort)
              ),
              column(
                4,
                div(style = "color: #007bff;", input$logbook_effort)
              )
            ),
            fluidRow(
              column(
                4,
                tags$strong("Sample unit:")
              ),
              column(
                4,
                div(style = "color: #007bff;", input$observer_sampleunit)
              ),
              column(
                4,
                div(style = "color: #007bff;", input$logbook_sampleunit)
              )
            ),
            fluidRow(
              column(
                4,
                tags$strong("Factor variables:")
              ),
              column(
                4,
                if(NROW(bldFactor) > 0){
                  lapply(1:NROW(bldFactor), FUN=function(x){
                    div(style = "display: block; color: #007bff;", bldFactor$observer[x])
                  })
                }
              ),
              column(
                4,
                if(NROW(bldFactor) > 0){
                  lapply(1:NROW(bldFactor), FUN=function(x){
                    if(bldFactor$observer[x] == bldFactor$logbook[x]) return(div(style = "color: #007bff;", bldFactor$logbook[x]))
                    if(bldFactor$observer[x] != bldFactor$logbook[x]) return(div(style = "color: #007bff;", bldFactor$logbook[x], icon("arrow-right"), bldFactor$observer[x]))
                  })
                }
              )
            ),
            fluidRow(
              column(
                4,
                tags$strong("Numeric variables:")
              ),
              column(
                4,
                if(NROW(bldNumeric) > 0){
                  lapply(1:NROW(bldNumeric), FUN=function(x){
                    div(style = "display: block; color: #007bff;", bldNumeric$observer[x])
                  })
                }
              ),
              column(
                4,
                if(NROW(bldNumeric) > 0){
                  lapply(1:NROW(bldNumeric), FUN=function(x){
                    if(bldNumeric$observer[x] == bldNumeric$logbook[x]) return(div(style = "color: #007bff;", bldNumeric$logbook[x]))
                    if(bldNumeric$observer[x] != bldNumeric$logbook[x]) return(div(style = "color: #007bff;", bldNumeric$logbook[x], icon("arrow-right"), bldNumeric$observer[x]))
                  })
                }
              )
            )
          )
        )
      })
      
      
      
      ## Button for running data checks
      resultsDir <- reactiveValues(output = NULL)

      observeEvent(input$run_datachecks, {

        #Re-name logbook factors, if applicable
        factorCheck <- ifelse(NROW(names(factorNames)) > 0, sum(sapply(names(factorNames), function(x){!is.null(factorNames[[x]])})), 0)
      
        if(
          is.null(logbookdataInput()$dt) ||
          is.null(observerdataInput()$dt) ||
          is.null(input$observer_year) ||
          is.null(input$logbook_year) ||
          is.null(input$observer_effort) ||
          is.null(input$observer_catch) ||
          is.null(input$logbook_effort) ||
          is.null(input$observer_catch) ||
          nchar(input$spp_scientificname) == 0 ||
          nchar(input$spp_name) == 0 ||
          nchar(input$datachecks_name) == 0 ||
          factorCheck < 1
        ){
          showModal(
            modalDialog(
              title = div(icon("circle-xmark", style = "color: red;"), div("Run checks failed", style = "display: inline; padding-left: 5px;")),
              tags$ul(
                tags$li("Make sure all required fields are entered")
              ),
              easyClose = TRUE,
              footer = NULL
            )
          )
        } else {
          waitScreen$show()
          #Data processing - Data stucture step.
          #Re-name logbook Year, if applicable
          logbook_dt <- logbookdataInput()$dt %>%
            dplyr::rename(!!input$observer_year := !!input$logbook_year)

          #Re-name logbook factors, if applicable
          nm<-names(factorNames)
          bldFactor <-data.frame()
          for (i in nm){
            if(!is.null(factorNames[[i]])){
              bldFactor<-rbind(bldFactor,
                         list(
                           id=i,
                           observer = factorNames[[i]][1],
                           logbook = factorNames[[i]][2],
                           newLogbook = factorNames[[i]][1])
              )
              logbook_dt <- logbook_dt %>%
                rename(!!factorNames[[i]][1] := !!factorNames[[i]][2])
            }
          }

          #Re-name logbook numeric, if applicable
          nm<-names(numericNames)
          bldNumeric <-data.frame()
          for (i in nm){
            if(!is.null(numericNames[[i]])){
              bldNumeric<-rbind(bldNumeric,
                         list(
                           id=i,
                           observer = numericNames[[i]][1],
                           logbook = numericNames[[i]][2],
                           newLogbook = numericNames[[i]][1])
              )
              logbook_dt <- logbook_dt %>%
                rename(!!numericNames[[i]][1] := !!numericNames[[i]][2])
            }
          }

          # Create temp dir to hold output
          outDir <- tempfile("bycatch_output_")
          dir.create(outDir)
          print(paste("Dir exists:", dir.exists(outDir)))
          print(list.files(outDir))
          # Run bycatchSetup function
          tryCatch({ # for debugging
            setupObj <- BycatchEstimator::bycatchSetup(
              obsdat = observerdataInput()$dt, #Yes
              logdat = logbook_dt, #Yes
              yearVar = input$observer_year, #Yes
              obsEffort = input$observer_effort, #Yes
              logEffort = input$logbook_effort, #Yes
              obsCatch = input$observer_catch, #Yes
              catchUnit = input$observer_catchunits, #Yes
              catchType = input$observer_catchtype, #Yes
              logNum = if (input$logbook_aggregationcolumn == "NA") NA else input$logbook_aggregationcolumn, #Yes
              sampleUnit = input$observer_sampleunit, #Yes
              factorVariables = unique(c(bldFactor$observer)), # input$observer_year)), #Yes
              numericVariables = unique(bldNumeric$observer), #Yes
              baseDir = outDir, #Yes
              runName = input$datachecks_name,  #Yes
              runDescription = input$datachecks_name, #Yes
              common = input$spp_name, #Yes
              sp = input$spp_scientificname #Yes,
            )
            print("bycatchSetup ran successfully")
            showModal(
              modalDialog(
                title = div(icon("circle-check", style = "color: #007bff;"), div("Checks completed", style = "display: inline; padding-left: 5px;")),
                tags$ul(
                  tags$li("Select step 2"),
                  tags$li("Review your results")
                ),
                easyClose = TRUE,
                footer = NULL
              )
            )
          }, error = function(e) {
            showModal(
              modalDialog(
                title = div(icon("circle-xmark", style = "color: red;"), div("Run checks failed", style = "display: inline; padding-left: 5px;")),
                tags$ul(
                  tags$li("Review fields for errors"),
                  tags$li("If this problem persists, consult user guide")
                ),
                easyClose = TRUE,
                footer = NULL
              )
            )
            showNotification(paste("Error running bycatchSetup:", e$message), type = "error")
            print(paste("bycatchSetup error:", e$message))
          })
          
          #create list of inputs to be saved in resultsDir
          bycatchsetup_inputs <- list(
            observerdata_title = observerdataInput()$title,
            logbookdata_title = logbookdataInput()$title,
            observer_catch = input$observer_catch,
            observer_catchunits = input$observer_catchunits,
            observer_catchtype = input$observer_catchtype,
            observer_year = input$observer_year,
            logbook_year = input$logbook_year,
            observer_effort = input$observer_effort,
            logbook_effort = input$logbook_effort,
            observer_sampleunit = input$observer_sampleunit,
            logbook_sampleunit = input$logbook_sampleunit,
            logbook_aggregationcolumn = input$logbook_aggregationcolumn,
            factor_variables = paste(unique(bldFactor$observer),collapse = ";"),
            numeric_variables = paste(unique(bldNumeric$observer),collapse = ";"),
            common_name = input$spp_name,
            scientific_name = input$spp_scientificname,
            run_description = input$datachecks_name
          )
          
          bycatchsetup_inputs_df <- data.frame(
            name = names(bycatchsetup_inputs),
            value = unlist(bycatchsetup_inputs, use.names = FALSE),
            stringsAsFactors = FALSE)
          write.csv(bycatchsetup_inputs_df, paste0(outDir,"/bycatch_inputs.csv"),row.names = FALSE)
          
          # save data sets in outDir
          write.csv(observerdataInput()$dt,paste0(outDir,"/observer_dataset.csv"),row.names = FALSE)
          write.csv(logbook_dt,paste0(outDir,"/logbook_dataset.csv"),row.names = FALSE)

          observe({
            print(bycatchsetup_inputs_df)
          })

          # Save path to generated output
          resultsDir$output<-outDir
          waitScreen$hide()
          #print(resultsDir())
          #print(setupObj)
          print(list.files(outDir,recursive = TRUE,full.names = TRUE))
        }
      })


      #-------------------
      #Reactives returned
      #------------------
      resultsDir
     
    }
  )
}

