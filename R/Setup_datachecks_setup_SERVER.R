#-------------------------------
#module data checks setup
#-------------------------------


############# SERVER #############
datachecksSetup_SERVER <- function(id, observerdataInput = reactive(NULL), logbookdataInput = reactive(NULL), outDir){
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
      #Retain information
      #-----------------------------
      rowKeysFactor <- reactiveVal(character(0))
      rowKeysNumeric <- reactiveVal(character(0))
      resultsDir <- reactiveVal(list(active = FALSE, setupObj = NULL,bycatchsetup_inputs = NULL))
      
      # ==============================================================================
      # Centralized State Controller (Single Source of Truth)
      # ==============================================================================
      observeEvent(list(observerdataInput(), logbookdataInput()), {
        
        shinyjs::toggle("warningPanelToggle", condition = any(!observerdataInput()$formComplete, !logbookdataInput()$formComplete))  
        shinyjs::toggle("inputPanelToggle", condition = all(observerdataInput()$formComplete, logbookdataInput()$formComplete))  
        
        # Define all inputs to control
        observer_fields <- c("observer_effort")
        logbook_fields  <- c("logbook_effort", "logbook_aggregationcolumn")
        
        # --- STEP 1: RESET SAMPLE UNITS IMMEDIATELY ---
        updatePickerInput(session = session, inputId = "observer_sampleunit", selected = character(0))
        updatePickerInput(session = session, inputId = "logbook_sampleunit", selected = character(0))
        updateTextInput(session = session, inputId = "spp_name", value = character(0))
        updateTextInput(session = session, inputId = "spp_scientificname", value = character(0))
        updateTextInput(session = session, inputId = "datachecks_name", value = character(0))
        
        # --- STEP 2: LOCK UI & FREEZE REACTIVITY ---
        lapply(c(observer_fields, logbook_fields), shinyjs::disable)
        lapply(c(observer_fields, logbook_fields), function(x) freezeReactiveValue(input, x))
        
        # --- STEP 3: OBSERVER DATA PROFILE EVALUATION ---
        if (!is.null(observerdataInput()$dt)) {
          obs_cols <- setdiff(names(observerdataInput()$dt), c(observerdataInput()$observer_year, observerdataInput()$observer_catch))
          updatePickerInput(session = session, inputId = "observer_effort", choices = obs_cols, selected = character(0))
          shinyjs::enable("observer_effort")
        } else {
          updatePickerInput(session = session, inputId = "observer_effort", choices = character(0))
        }
        
        # --- STEP 4: LOGBOOK DATA PROFILE EVALUATION (FIXED LOGIC CHECK) ---
        if (!is.null(logbookdataInput()$dt)) {
          log_cols <- setdiff(names(logbookdataInput()$dt), logbookdataInput()$logbook_year)
          updatePickerInput(session = session, inputId = "logbook_effort", choices = log_cols, selected = character(0))
          updatePickerInput(session = session, inputId = "logbook_aggregationcolumn", choices = log_cols, selected = character(0))
          
          # FIXED: Restored the correct enable targeting for logbook_effort
          shinyjs::enable("logbook_effort")
          shinyjs::enable("logbook_aggregationcolumn")
        } else {
          updatePickerInput(session = session, inputId = "logbook_effort", choices = character(0))
          updatePickerInput(session = session, inputId = "logbook_aggregationcolumn", choices = character(0))
        }
        
        # --- STEP 5: CLEANUP FACTOR PATHWAYS ---
        removeUI(
          selector = paste0("#", session$ns("factor_insertUI"), " > div"),
          multiple = TRUE
        )
        rowKeysFactor(character(0))
        
        # --- STEP 6: CLEANUP NUMERIC PATHWAYS ---
        removeUI(
          selector = paste0("#", session$ns("numeric_insertUI"), " > div"),
          multiple = TRUE
        )
        rowKeysNumeric(character(0))
        
        # --- STEP 7: Clear prior execution results on data shift ---
        # 
        unlink(dir(outDir, full.names = TRUE), recursive = TRUE, force = TRUE)
        resultsDir(list(active = FALSE, setupObj = NULL, bycatchsetup_inputs = NULL))
        
        updateTabsetPanel(
          session = session,
          inputId = "inputUIPanel", 
          selected = "dataStructureTab"
        )
        
      }, ignoreInit = TRUE, ignoreNULL = FALSE)
      
      # ==============================================================================
      # Logbook Sample Unit Conditional Toggles
      # ==============================================================================
      observeEvent(input$logbook_sampleunit, {
          
        shinyjs::toggle("logbook_aggregation", condition = !is.null(input$logbook_sampleunit) && (input$logbook_sampleunit == "Aggregated further"))
        
        # Freeze before resetting selection to stop cascading cross-observer loops
        freezeReactiveValue(input, "logbook_aggregationcolumn")
        updatePickerInput(session = session, inputId = "logbook_aggregationcolumn", selected = character(0))
      }, ignoreNULL = FALSE)
      
      
      # ==============================================================================
      # 3. Dynamic Factor Variable Allocation Layout (UI Generator)
      # ==============================================================================
      # Holds the dynamic factor configurations safely in a clean reactive list

      observeEvent(input$addFactor, {
        # Build isolated component structural IDs
        
        key <- paste0("rowFactor_", as.integer(input$addFactor))
        picker1ID <- paste0(key, "_pickerFactor1")
        picker2ID <- paste0(key, "_pickerFactor2")
        btnID    <- paste0(key, "_removeFactor")

        # Insert UI into placeholder structure safely
        insertUI(
          selector = paste0("#", session$ns("factor_insertUI")),
          where = "beforeEnd",
          session = session,
          ui = tags$div(
            id = session$ns(key), 
            div(
              style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
              
              fluidRow(
                column(6,
                   div(icon("binoculars"), HTML("&nbsp;"), tags$strong("Observer dataset"), style = "margin-bottom: 10px;"),
                   pickerInput(
                     inputId = session$ns(picker1ID), 
                     label = tagList(tags$strong("Select factor"), div("*", style = "display: inline; color: red;")),
                     choices = setdiff(names(observerdataInput()$dt), c(observerdataInput()$observer_year, observerdataInput()$observer_catch)),
                     selected = character(0),
                     width = '100%', 
                     multiple = TRUE,
                     options = pickerOptions(
                       maxOptions = 1
                     )
                   )
                ),
                column(6,
                   div(
                     style = "float: right; margin: 0px;",
                     actionBttn(inputId = session$ns(btnID), label = "Remove", size = "xs")
                   ),
                   div(icon("ship"), HTML("&nbsp;"), tags$strong("Logbook dataset"), style = "margin-bottom: 10px;"),
                   pickerInput(
                     inputId = session$ns(picker2ID), 
                     label = tagList(tags$strong("Select", tags$em("corresponding", style = "color: #0d6efd;"),  "factor"), div("*", style = "display: inline; color: red;")),
                     choices = setdiff(names(logbookdataInput()$dt), logbookdataInput()$logbook_year),
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
          )
        )
        
        # Wholesale reassignment -- this is what makes downstream reactives
        # correctly re-fire.
        rowKeysFactor(c(rowKeysFactor(), key))
      })
      
      #Placeholder for Year variable.
      output$yearPlaceholder <- renderUI({
        req(observerdataInput()$observer_year, logbookdataInput()$logbook_year)
        div(
          style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
          fluidRow(
            column(6,
                   div(icon("binoculars"), HTML("&nbsp;"), tags$strong("Observer dataset"), style = "margin-bottom: 10px;"),
                   tags$strong("Year factor"),
                   div(
                     style = "padding: 5px 15px; background-color: #F8F9FA; border-radius: 5px;",
                     observerdataInput()$observer_year
                   )
            ),
            column(6,
                   div(icon("ship"), HTML("&nbsp;"), tags$strong("Logbook dataset"), style = "margin-bottom: 10px;"),
                   div(tags$strong("Select", tags$em("corresponding", style = "color: #0d6efd;"),  "factor"), div("*", style = "display: inline; color: red;")),
                   div(
                     style = "padding: 5px 15px; background-color: #F8F9FA; border-radius: 5px;",
                     logbookdataInput()$logbook_year
                   )
            )
          )
        )
      })
      
      
      # ==============================================================================
      # 4. Global Observer tracking Dynamic Factors (Prevents Memory Leaks)
      # ==============================================================================
      # Instead of nested observers, use one global observer tracking input changes
      observe({
        keys <- rowKeysFactor()
        
        for (key in keys) {
          removeID <- paste0(key, "_removeFactor")
          btn_val <- input[[removeID]]
          
          if (!is.null(btn_val) && btn_val > 0) {
            removeUI(selector = paste0("#", session$ns(key)))
            rowKeysFactor(setdiff(rowKeysFactor(), key)) # wholesale reassignment again
          }
        }
      })
      
      compiledFactorPairs <- reactive({
        keys <- rowKeysFactor()
        
        if (length(keys) == 0) return(list())
        
        pairs_list <- lapply(keys, function(key) {
          p1 <- input[[paste0(key, "_pickerFactor1")]]
          p2 <- input[[paste0(key, "_pickerFactor2")]]
          
          # Store elements inside a clean lookup list structured by ID key
          list(observer = p1, logbook = p2)
        })
        
        names(pairs_list) <- keys
        return(pairs_list)
      })
      
      observe({
        shinyjs::toggle("warningFactorObserver", condition = !is.null(input$observer_effort) && !is.null(bind_rows(compiledFactorPairs())[['observer']]) && any(grepl(input$observer_effort, bind_rows(compiledFactorPairs())$observer))) 
      })
      
      observe({
        shinyjs::toggle("warningFactorLogbook", condition = !is.null(input$logbook_effort) && !is.null(bind_rows(compiledFactorPairs())[['logbook']]) && any(grepl(input$logbook_effort, bind_rows(compiledFactorPairs())$logbook))) 
      })
      
      # ==============================================================================
      # 5. Numeric variables
      # ==============================================================================
 
      observeEvent(input$addNumeric, {
        # Build isolated component structural IDs
        
        key <- paste0("rowNumeric_", as.integer(input$addNumeric))
        picker1ID <- paste0(key, "_pickerNumeric1")
        picker2ID <- paste0(key, "_pickerNumeric2")
        btnID    <- paste0(key, "_removeNumeric")
        
        # Insert UI into placeholder structure safely
        insertUI(
          selector = paste0("#", session$ns("numeric_insertUI")),
          where = "afterBegin",
          session = session,
          ui = tags$div(
            id = session$ns(key), 
            div(
              style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
              div(
                style = "float: right; margin: 8px;",
                actionBttn(inputId = session$ns(btnID), label = "Remove", size = "xs")
              ),
              fluidRow(
                column(6,
                   div(icon("binoculars"), HTML("&nbsp;"), tags$strong("Observer data"), style = "margin-bottom: 10px;"),
                   pickerInput(
                     inputId = session$ns(picker1ID), 
                     label = tagList(tags$strong("Select numeric variable"), div("*", style = "display: inline; color: red;")),
                     choices = names(observerdataInput()$dt), 
                     selected = character(0),
                     width = '100%', 
                     multiple = TRUE,
                     options = pickerOptions(
                       maxOptions = 1
                     )
                   )
                ),
                column(6,
                   div(icon("ship"), HTML("&nbsp;"), tags$strong("Logbook data"), style = "margin-bottom: 10px;"),
                   pickerInput(
                     inputId = session$ns(picker2ID), 
                     label = tagList(tags$strong("Select", tags$em("corresponding", style = "color: #0d6efd;"),  "numeric variable"), div("*", style = "display: inline; color: red;")),
                     choices = names(logbookdataInput()$dt), 
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
          )
        )
        
        # Wholesale reassignment -- this is what makes downstream reactives
        # correctly re-fire.
        rowKeysNumeric(c(rowKeysNumeric(), key))
      })
      
      # ==============================================================================
      # 6. Global Observer tracking Dynamic Numeric variables (Prevents Memory Leaks)
      # ==============================================================================
      # Instead of nested observers, use one global observer tracking input changes
      observe({
        keys <- rowKeysNumeric()
        
        for (key in keys) {
          removeID <- paste0(key, "_removeNumeric")
          btn_val <- input[[removeID]]
          
          if (!is.null(btn_val) && btn_val > 0) {
            removeUI(selector = paste0("#", session$ns(key)))
            rowKeysNumeric(setdiff(rowKeysNumeric(), key)) # wholesale reassignment again
          }
        }
      })
      
      compiledNumericPairs <- reactive({
        keys <- rowKeysNumeric()
        
        if (length(keys) == 0) return(list())
        
        pairs_list <- lapply(keys, function(key) {
          p1 <- input[[paste0(key, "_pickerNumeric1")]]
          p2 <- input[[paste0(key, "_pickerNumeric2")]]
          
          # Store elements inside a clean lookup list structured by ID key
          list(observer = p1, logbook = p2)
        })
        
        names(pairs_list) <- keys
        return(pairs_list)
      })
     
      # ==============================================================================
      # 7. Preview and activate
      # ==============================================================================

      output$logbook_effortLinkage <- renderReactable({
        req(input$observer_sampleunit, input$logbook_effort, input$logbook_sampleunit)
        
        #Logbook
        if(input$logbook_sampleunit == "Same aggregation as observer data") {
          logDat <- head(logbookdataInput()$dt, n = 3L) %>%
            select(any_of(input$logbook_effort))
          
          log_columns <- c(input$logbook_effort, "sample.unit")
          
          linkage <- data.frame(
            sample.unit = switch(
              input$observer_sampleunit,
              Trip = c("Trip (each row) ->", NA, NA),
              Set = c("Set (each row) ->", NA, NA)
            )
          )
        } else {
          logDat <- head(logbookdataInput()$dt, n = 3L) %>% {
            if (!is.null(input$logbook_aggregationcolumn)) {
              select(., any_of(input$logbook_aggregationcolumn), any_of(input$logbook_effort))
            } else {
              select(., any_of(input$logbook_effort))
            }
          }
          
          log_columns <- c(input$logbook_effort, "sample.unit")
          if(!is.null(input$logbook_aggregationcolumn)) log_columns <- c(input$logbook_effort, input$logbook_aggregationcolumn, "sample.unit")
          
          
          linkage <- data.frame(
            sample.unit = c("Aggregated further ->", NA, NA)
          )
        }
        
        outDat <- cbind(linkage, logDat)
        
        reactable(
          outDat,
          highlight = FALSE, compact = T, style = list(color = "#343a40"),
          pagination = FALSE,
          sortable = FALSE,
          bordered = TRUE,
          striped = TRUE,
          defaultColDef = colDef(vAlign = "center", align = "center", headerVAlign = "bottom"),
          columns = list(
            sample.unit = colDef(name = "Sample unit")
          ),
          theme = reactableTheme(
            backgroundColor = "transparent",
            cellPadding = "10px 5px"
          ),
          columnGroups = list(
            colGroup(name = "Logbook dataset", columns = log_columns, headerStyle = "background-color: pink;")
          )
        )
      })
      
      output$observer_effortLinkage <- renderReactable({
        req(observerdataInput()$dt, input$observer_effort, input$observer_sampleunit)
        
        #Observer
        obsDat <- head(observerdataInput()$dt, n = 3L) %>% 
          select(
            any_of(observerdataInput()$observer_catch), 
            any_of(input$observer_effort)
          )
        
        obs_columns <- c(input$observer_effort, observerdataInput()$observer_catch, "sample.unit")
        
        linkage <- data.frame(
          sample.unit = switch(
            input$observer_sampleunit,
            Trip = c("<- Trip (each row)", NA, NA),
            Set = c("<- Set (each row)", NA, NA)
          )
        )
        
        outDat <- cbind(obsDat, linkage)
        
        reactable(
          outDat,
          highlight = FALSE, compact = T, style = list(color = "#343a40"),
          pagination = FALSE,
          sortable = FALSE,
          bordered = TRUE,
          striped = TRUE,
          defaultColDef = colDef(vAlign = "center", align = "center", headerVAlign = "bottom"),
          columns = list(
            sample.unit = colDef(name = "Sample unit")
          ),
          theme = reactableTheme(
            backgroundColor = "transparent",
            cellPadding = "10px 5px"
          ),
          columnGroups = list(
            colGroup(name = "Observer dataset", columns = obs_columns, headerStyle = "background-color: pink;")
          )
        )
      })
      
      output$checksPreview <- renderUI({
        req(observerdataInput()$formComplete, logbookdataInput()$formComplete)
        
        # Fetch lists safely from our optimized one-way bridge expressions
        bldFactor  <- compiledFactorPairs()
        bldNumeric <- compiledNumericPairs() # Assumed to follow the same list structure
        tagList(
          tags$strong(tags$em("Activation complete")),
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
                "Activation complete",
                tags$ul(
                  tags$li("Year variable and factor variables sometimes require re-naming for consistency across datasets."),
                  tags$li("This task is done automatically, with details shown in the table below."),
                  tags$li("In cases where re-naming has occurred, note the syntax 'old name' --> 'new name'.")
                 )
              ))
          ),
          div(
            style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
            
            # Headers Layout
            fluidRow(
              column(4, div(icon("clipboard-check"), HTML("&nbsp;"), tags$strong(tags$em("Input")))),
              column(4, div(icon("binoculars"), HTML("&nbsp;"), tags$strong(tags$em("Observer dataset")))),
              column(4, div(icon("ship"), HTML("&nbsp;"), tags$strong(tags$em("Logbook dataset"))))
            ),
            
            # Title Row
            fluidRow(
              column(4, tags$strong("Title:")),
              column(4, div(style = "color: #007bff;", observerdataInput()$title)),
              column(4, div(style = "display: inline; color: #007bff;", logbookdataInput()$title))
            ),
            
            # Catch Row
            fluidRow(
              column(4, tags$strong("Catch:")),
              column(4, 
                     if("Catch" == observerdataInput()$observer_catch) {
                       div(style = "color: #007bff;", "Catch")
                     } else {
                       div(style = "color: #007bff;", observerdataInput()$observer_catch, icon("arrow-right"), "Catch")
                     }
              ),
              column(4)
            ),
            
            
            # Units of Catch Row
            fluidRow(
              column(4, tags$strong("Units of catch:")),
              column(4, div(style = "color: #007bff;", observerdataInput()$observer_catchunits)),
              column(4)
            ),
            
            # Catch Type Row
            fluidRow(
              column(4, tags$strong("Catch type:")),
              column(4, div(style = "color: #007bff;", observerdataInput()$observer_catchtype)),
              column(4)
            ),
            
            # Year Row
            fluidRow(
              column(4, tags$strong("Year:")),
              column(4, 
                     if("Year" == observerdataInput()$observer_year) {
                       div(style = "color: #007bff;", "Year")
                     } else {
                       div(style = "color: #007bff;", observerdataInput()$observer_year, icon("arrow-right"), "Year")
                     }
              ),
              column(4, 
                     if("Year" == logbookdataInput()$logbook_year) {
                       div(style = "color: #007bff;", "Year")
                     } else {
                       div(style = "color: #007bff;", logbookdataInput()$logbook_year, icon("arrow-right"), "Year")
                     }
              )
            ),
            
            # Effort Row
            fluidRow(
              column(4, tags$strong("Effort:")),
              column(4, div(style = "color: #007bff;", input$observer_effort)),
              column(4, div(style = "color: #007bff;", input$logbook_effort))
            ),
            
            # Sample Unit Row
            fluidRow(
              column(4, tags$strong("Sample unit:")),
              column(4, div(style = "color: #007bff;", input$observer_sampleunit)),
              column(4, div(style = "color: #007bff;", input$logbook_sampleunit))
            ),
            
            # --- DYNAMIC FACTOR VARIABLES SECTION (FIXED LIST LOOPING) ---
            fluidRow(
              column(4, tags$strong("Factor variables:")),
              column(4, 
                     if("Year" == observerdataInput()$observer_year) {
                       div(style = "color: #007bff;", "Year")
                     } else {
                       div(style = "color: #007bff;", observerdataInput()$observer_year, icon("arrow-right"), "Year")
                     },
                     if(length(bldFactor) > 0) {
                       lapply(bldFactor, function(item) {
                         obs_val <- item$observer
                         log_val <- item$logbook
                         if(all(!is.null(obs_val), !is.null(log_val))){
                          return(div(style = "display: block; color: #007bff;", obs_val))
                         }
                       })
                     }
              ),
              column(4, 
                     if("Year" == logbookdataInput()$logbook_year) {
                       div(style = "color: #007bff;", "Year")
                     } else {
                       div(style = "color: #007bff;", logbookdataInput()$logbook_year, icon("arrow-right"), "Year")
                     },
                     if(length(bldFactor) > 0) {
                       lapply(bldFactor, function(item) {
                         obs_val <- item$observer
                         log_val <- item$logbook
                         if(all(!is.null(obs_val), !is.null(log_val))){
                           if(obs_val == log_val) {
                             return(div(style = "color: #007bff;", log_val))
                           } else {
                             return(div(style = "color: #007bff;", log_val, icon("arrow-right"), obs_val))
                           }
                         }
                       })
                     }
              )
            )#,
            
            # --- DYNAMIC NUMERIC VARIABLES SECTION (FIXED LIST LOOPING) ---
            # fluidRow(
            #   column(4, tags$strong("Numeric variables:")),
            #   column(4, 
            #          if(length(bldNumeric) > 0) {
            #            lapply(bldNumeric, function(item) {
            #              obs_val <- item$observer
            #              log_val <- item$logbook
            #              if(all(!is.null(obs_val), !is.null(log_val))){
            #                return(div(style = "display: block; color: #007bff;", obs_val))
            #              }
            #            })
            #          }
            #   ),
            #   column(4, 
            #          if(length(bldNumeric) > 0) {
            #            lapply(bldNumeric, function(item) {
            #              obs_val <- item$observer
            #              log_val <- item$logbook
            #              if(all(!is.null(obs_val), !is.null(log_val))){
            #                if(obs_val == log_val) {
            #                  return(div(style = "color: #007bff;", log_val))
            #                } else {
            #                  return(div(style = "color: #007bff;", log_val, icon("arrow-right"), obs_val))
            #                }
            #              }
            #            })
            #          }
            #   )
            # )
            
          )
        )
      })
      
      
      ## Button for running data checks
      

      observeEvent(input$run_datachecks, {
        
        on.exit({
          shinyjs::runjs("document.getElementById('ui-shield').style.display = 'none';")
        })
        
        if(
          !observerdataInput()$formComplete || 
          !logbookdataInput()$formComplete ||
          is.null(input$observer_effort) ||
          is.null(input$logbook_effort) ||
          is.null(input$observer_sampleunit) ||
          is.null(input$logbook_sampleunit) ||
          !nzchar(input$spp_scientificname) ||
          !nzchar(input$spp_name) ||
          !nzchar(input$datachecks_name) ||
          isTRUE("Effort" %in% names(observerdataInput()$dt) && input$observer_effort != "Effort") ||
          isTRUE("Effort" %in% names(logbookdataInput()$dt) && input$logbook_effort != "Effort") ||
          isTRUE(input$logbook_sampleunit == "Aggregated further" && is.null(input$logbook_aggregationcolumn)) ||
          isTRUE(input$logbook_sampleunit == "Aggregated further" && input$logbook_aggregationcolumn == input$logbook_effort) ||
          isTRUE(input$logbook_sampleunit == "Aggregated further" && "SampleUnits" %in% names(logbookdataInput()$dt) && input$logbook_aggregationcolumn != "SampleUnits") ||
          isTRUE(length(compiledFactorPairs()) > 0 && any(duplicated(bind_rows(compiledFactorPairs())$observer))) ||
          isTRUE(length(compiledFactorPairs()) > 0 && any(duplicated(bind_rows(compiledFactorPairs())$logbook))) ||
          isTRUE(length(compiledFactorPairs()) > 0 && is.null(bind_rows(compiledFactorPairs())[['observer']])) ||
          isTRUE(length(compiledFactorPairs()) > 0 && is.null(bind_rows(compiledFactorPairs())[['logbook']])) ||     
          isTRUE(length(compiledFactorPairs()) > 0 && any(is.na(bind_rows(compiledFactorPairs())$observer))) ||
          isTRUE(length(compiledFactorPairs()) > 0 && any(is.na(bind_rows(compiledFactorPairs())$logbook))) ||
          isTRUE(length(compiledFactorPairs()) > 0 && any(grepl(input$observer_effort, bind_rows(compiledFactorPairs())$observer))) ||
          isTRUE(length(compiledFactorPairs()) > 0 && any(grepl(input$logbook_effort, bind_rows(compiledFactorPairs())$logbook)))
          #When numeric factors included, checks are need here for unique values and can be factor or numeric, not both (b/c of dataset renaming)
        ){
          showModal(
            modalDialog(
              title = div(icon("circle-xmark", style = "color: red;"), div("Activation failed", style = "display: inline; padding-left: 5px;")),
              h6("Cannot activate.", style = "color: red;"),
              div(
                style = "color: black; font-weight: normal;",
                "Check the following:",
                br(),
                tags$ul(
                  tags$li("Review form for missing entries in mandatory fields."),
                  tags$li("Review warning messages."),
                  tags$li("Each observer factor variable can only be selected once."),
                  tags$li("Each logbook factor variable can only be selected once."),
                  tags$li("Effort column cannot be selected as factor variable.")
                )
              ),
              easyClose = TRUE,
              footer = NULL
            )
          )
        } else {
          
          #Get factor variable names
          bldFactor <- data.frame()
          if(length(compiledFactorPairs()) > 0){
            bldFactor <- bind_rows(compiledFactorPairs(), .id = "id")
            bldFactor$newLogbook <- bldFactor$observer
          }
          
          #Get numeric variable names
          #When numeric factors included, checks are need here for unique values and can be factor or numeric, not both (b/c of dataset renaming)
          bldNumeric <- data.frame()
          
          #Select data and rename columns, if needed
          observer_dt <- observerdataInput()$dt %>%
            select(any_of(c(observerdataInput()$observer_year, observerdataInput()$observer_catch, input$observer_effort, bldFactor$observer))) %>% #
            dplyr::rename("Year" := !!observerdataInput()$observer_year) %>%
            dplyr::rename("Catch" := !!observerdataInput()$observer_catch) %>%
            dplyr::rename("Effort" := !!input$observer_effort)
          
          logbook_dt <- logbookdataInput()$dt %>%
            select(any_of(c(logbookdataInput()$logbook_year, input$logbook_effort,  bldFactor$logbook))) %>%
            dplyr::rename("Year" := !!logbookdataInput()$logbook_year) %>%
            dplyr::rename("Effort" := !!input$logbook_effort)
          
          if(NROW(bldFactor) > 0){
            for(i in 1:NROW(bldFactor)){
              logbook_dt <- logbook_dt %>%
                rename(!!bldFactor$observer[i] := !!bldFactor$logbook[i])
            }
          }

          # Run bycatchSetup function
          tryCatch({ # for debugging
            setupObj <- BycatchEstimator::bycatchSetup(
              obsdat = observer_dt, #Yes
              logdat = logbook_dt, #Yes
              yearVar = "Year", #Yes
              obsEffort = "Effort", #Yes
              logEffort = "Effort", #Yes
              obsCatch = "Catch", #Yes
              catchUnit = observerdataInput()$observer_catchunits, #Yes
              catchType = observerdataInput()$observer_catchtype, #Yes
              logNum = if (input$logbook_sampleunit == "Aggregated further") input$logbook_aggregationcolumn else NA, #Yes
              sampleUnit = input$observer_sampleunit, #Yes
              factorVariables = c("Year", unique(c(bldFactor$observer))), #UI will not allow Year to be selected, so added here.
              numericVariables = unique(bldNumeric$observer), #Yes
              baseDir = outDir, #Yes
              runName = input$datachecks_name,  #Yes
              runDescription = input$datachecks_name, #Yes
              common = input$spp_name, #Yes
              sp = input$spp_scientificname #Yes,
            )
            
            #create list of inputs to be saved in resultsDir
            bycatchsetup_inputs <- list(
              observerdata_title = observerdataInput()$title,
              logbookdata_title = logbookdataInput()$title,
              observer_catch = observerdataInput()$observer_catch,
              observer_catchunits = observerdataInput()$observer_catchunits,
              observer_catchtype = observerdataInput()$observer_catchtype,
              observer_year = observerdataInput()$observer_year,
              logbook_year = logbookdataInput()$logbook_year,
              observer_effort = input$observer_effort,
              logbook_effort = input$logbook_effort,
              observer_sampleunit = input$observer_sampleunit,
              logbook_sampleunit = input$logbook_sampleunit,
              logbook_aggregationcolumn = ifelse(input$logbook_sampleunit == "Aggregated further", input$logbook_aggregationcolumn, NA),
              factor_variables = ifelse(is.null(bldFactor$observer), NA, paste(unique(bldFactor$observer),collapse = ";")),
              numeric_variables = ifelse(is.null(bldNumeric$observer), NA, paste(unique(bldNumeric$observer),collapse = ";")),
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
            write.csv(observer_dt, paste0(outDir,"/observer_dataset.csv"),row.names = FALSE)
            write.csv(logbook_dt, paste0(outDir,"/logbook_dataset.csv"),row.names = FALSE)

            resultsDir(list(active = TRUE, setupObj = setupObj, bycatchsetup_inputs = bycatchsetup_inputs))

            showModal(
              modalDialog(
                title = div(icon("circle-check", style = "color: #198754;"), div("Activation completed", style = "display: inline; padding-left: 5px;")),
                tags$ul(
                  tags$li("Bycatch setup was successful"),
                  tags$li("Proceed to Data Checks and select diagnostics & summary")
                ),
                easyClose = TRUE,
                footer = NULL
              )
            )
            
            updateTabsetPanel(
              session = session,
              inputId = "inputUIPanel", 
              selected = "runChecksTab"
            )

          }, error = function(e) {
            
            resultsDir(list(active = FALSE, setupObj = NULL, bycatchsetup_inputs = NULL))
            
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
          })
        }
      })

      
      observeEvent(input$deactivate, {
        on.exit({
          shinyjs::runjs("document.getElementById('ui-shield').style.display = 'none';")
        })
        
        shinyalert(
          inputId = session$ns("deactivateAlert"),
          title = "Deactivate bycatch setup?",
          text = "Deactivation allows you to modify your bycatch setup. However, doing so will reset 'Data checks' and 'Design-based estimators'. Your current analyses will be lost. Are you sure you want to continue?",
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "Yes",
          cancelButtonText = "No",
          callbackR = function(value = input$deactivateAlert) {
            #Yes button clicked, delete project from server
            if(value){
              unlink(dir(outDir, full.names = TRUE), recursive = TRUE, force = TRUE)
              resultsDir(list(active = FALSE, setupObj = NULL, bycatchsetup_inputs = NULL))
              updateTabsetPanel(
                session = session,
                inputId = "inputUIPanel", 
                selected = "dataStructureTab"
              )
            }
          }
        )
      }, ignoreInit = TRUE)
      
      
      #-------------------
      #Reactives returned
      #------------------
      return(resultsDir)
     
    }
  )
}

