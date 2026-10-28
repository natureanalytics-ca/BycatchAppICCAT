#-------------------------------
#module data checks setup
#-------------------------------


############# SERVER #############
designSpecifications_SERVER <- function(id, bycatchSetup, outDir){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      #---------------------------
      #Input constraints on nchar
      #---------------------------
      shinyjs::runjs("$('#designSpecifications_module-design_name').attr('maxlength', 100)")


      #-----------------------------
      #Retain information
      #-----------------------------
      rowKeysStrat <- reactiveVal(character(0))
      resultsActive <- reactiveVal(FALSE)

      # ==============================================================================
      # Centralized State Controller (Single Source of Truth)
      # ==============================================================================
      observeEvent(bycatchSetup(), {

        shinyjs::toggle("warningPanelToggle", condition = !bycatchSetup()$active)  
        shinyjs::toggle("inputPanelToggle", condition = bycatchSetup()$active)
 
        # Define all inputs to control
        
    
        # --- STEP 1: RESET STATIC INPUTS IMMEDIATELY ---
        updateTextInput(session = session, inputId = "design_name", value = character(0))
        updatePickerInput(session = session, inputId = "analysis_method", selected = c("Ratio", "Delta"))
        
        # --- STEP 2: LOCK UI & FREEZE REACTIVITY ---
        var_fields <- c("strat_var", "group_var")
        lapply(var_fields, shinyjs::disable)
        lapply(var_fields, function(x) freezeReactiveValue(input, x))

        if(bycatchSetup()$active){
          col_names <- bycatchSetup()$setupObj$bycatchInputs$factorVariables
          updatePickerInput(session = session, inputId = "group_var", choices = col_names, selected = character(0))
          shinyjs::enable("group_var")
          
        } else {
          updatePickerInput(session = session, inputId = "strat_var", choices = character(0))
          updatePickerInput(session = session, inputId = "group_var", choices = character(0))
        }
        
        # --- STEP 5: CLEANUP FACTOR PATHWAYS ---
        removeUI(
          selector = paste0("#", session$ns("addStrat_var_insertUI"), " > div"),
          multiple = TRUE
        )
        rowKeysStrat(character(0))
        
        resultsActive(FALSE)
        
        updateTabsetPanel(
          session = session,
          inputId = "inputUIPanel",
          selected = "specificationsTab"
        )

      }, ignoreInit = TRUE, ignoreNULL = FALSE)
      
      output$analysisPreview <- renderUI({
        req(bycatchSetup()$active)

        tagList(
          tags$strong(tags$em("Analysis summary")),
          br(),
          HTML(paste(tags$strong("Scenario name:"), tags$em(input$design_name, style = "color: #007bff;"))),
          br(),
          HTML(paste(tags$strong("Analysis method(s):"), tags$em(paste(input$analysis_method, collapse = ", "), style = "color: #007bff;"))),
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
              column(4, div(style = "color: #007bff;", bycatchSetup()$bycatchsetup_inputs$observerdata_title)),
              column(4, div(style = "display: inline; color: #007bff;", bycatchSetup()$bycatchsetup_inputs$logbookdata_title))
            ),

            # Catch Row
            fluidRow(
              column(4, tags$strong("Catch:")),
              column(4, div(style = "color: #007bff;", "Catch")),
              column(4)
            ),

            # Units of Catch Row
            fluidRow(
              column(4, tags$strong("Units of catch:")),
              column(4, div(style = "color: #007bff;", bycatchSetup()$bycatchsetup_inputs$observer_catchunits)),
              column(4)
            ),

            # Catch Type Row
            fluidRow(
              column(4, tags$strong("Catch type:")),
              column(4, div(style = "color: #007bff;", bycatchSetup()$bycatchsetup_inputs$observer_catchtype)),
              column(4)
            ),

            # Year Row
            fluidRow(
              column(4, tags$strong("Year:")),
              column(4, div(style = "color: #007bff;", "Year")),
              column(4, div(style = "color: #007bff;", "Year"))
            ),

            # Effort Row
            fluidRow(
              column(4, tags$strong("Effort:")),
              column(4, div(style = "color: #007bff;", "Effort")),
              column(4, div(style = "color: #007bff;", "Effort"))
            ),

            # Sample Unit Row
            fluidRow(
              column(4, tags$strong("Sample unit:")),
              column(4, div(style = "color: #007bff;", bycatchSetup()$bycatchsetup_inputs$observer_sampleunit)),
              column(4, div(style = "color: #007bff;", bycatchSetup()$bycatchsetup_inputs$logbook_sampleunit))
            ),

            fluidRow(
              column(4, tags$strong("Stratification variables:")),
              column(4, div(style = "color: #007bff;", paste(input$strat_var, collapse = ", "))),
              column(4, div(style = "color: #007bff;", paste(input$strat_var, collapse = ", ")))
            ),
            
            fluidRow(
              column(4, tags$strong("Grouping variable:")),
              column(4, div(style = "color: #007bff;", paste(input$group_var, collapse = ", "))),
              column(4, div(style = "color: #007bff;", paste(input$group_var, collapse = ", ")))
            )
          )
        )
      })

      # ==============================================================================
      # 3. Dynamic design stratification variable allocation layout (UI Generator)
      # ==============================================================================
      
      observeEvent(input$addStrat_var, {
        # Build isolated component structural IDs
        
        key <- paste0("rowStrat_", as.integer(input$addStrat_var))
        picker1ID <- paste0(key, "_pickerDesignVar")
        picker2ID <- paste0(key, "_pickerPoolType")
        picker3ID <- paste0(key, "_pickerPoolVar")
        picker4ID <- paste0(key, "_pickerAdjacentNum")
        btnID    <- paste0(key, "_removeStratFactor")

        # Insert UI into placeholder structure safely
        insertUI(
          selector = paste0("#", session$ns("addStrat_var_insertUI")),
          where = "beforeEnd",
          session = session,
          ui = tags$div(
            id = session$ns(key), 
            div(
              style = "margin: 10px 0px; border: 1px solid #ADB5BD; border-radius: 5px; padding: 10px;",
              
              fluidRow(
                column(4,
                       div(tags$strong("Design stratification variable"), style = "margin-bottom: 10px;"),
                       pickerInput(
                         inputId = session$ns(picker1ID), 
                         label = tagList(tags$strong("Select factor"), div("*", style = "display: inline; color: red;")),
                         choices = bycatchSetup()$setupObj$bycatchInputs$factorVariables,
                         selected = character(0),
                         width = '100%', 
                         multiple = TRUE,
                         options = pickerOptions(
                           maxOptions = 1
                         )
                       )
                ),
                column(8,
                       div(
                         style = "float: right; margin: 0px;",
                         actionBttn(inputId = session$ns(btnID), label = "Remove", size = "xs")
                       ),
                       div(tags$strong("Pooling"), style = "margin-bottom: 10px;"),
                       pickerInput(
                         inputId = session$ns(picker2ID), 
                         label = tagList(tags$strong("Pooling type"), div("*", style = "display: inline; color: red;")),
                         choices = c("none", "all", "pooledVar", "adjacent"),
                         selected = "none",
                         width = '100%', 
                         multiple = FALSE#,
                         #options = pickerOptions(
                        #   maxOptions = 1
                        # )
                       ),
                       pickerInput(
                         inputId = session$ns(picker3ID), 
                         label = tags$strong("New variable (select if Pooling type is 'pooledVar')"),
                         choices = bycatchSetup()$setupObj$bycatchInputs$factorVariables,
                         selected = character(0),
                         width = '100%', 
                         multiple = TRUE,
                         options = pickerOptions(
                           maxOptions = 1
                         )
                       ),
                       numericInput(
                         inputId = session$ns(picker4ID), 
                         label = tags$strong("Number of adjacent years (select if Pooling type is 'adjacent')"),
                         value = 1,
                         min = 1,
                         max = 5,
                         width = '100%'
                       )
                       
                )
              )
            )
          )
        )
        
        # Wholesale reassignment -- this is what makes downstream reactives
        # correctly re-fire.
        rowKeysStrat(c(rowKeysStrat(), key))
      })
      
      
      
      # ==============================================================================
      # 4. Global Observer tracking Dynamic Factors (Prevents Memory Leaks)
      # ==============================================================================
      # Instead of nested observers, use one global observer tracking input changes
      observe({
        keys <- rowKeysStrat()
        
        for (key in keys) {
          removeID <- paste0(key, "_removeStratFactor")
          btn_val <- input[[removeID]]
          
          if (!is.null(btn_val) && btn_val > 0) {
            removeUI(selector = paste0("#", session$ns(key)))
            rowKeysStrat(setdiff(rowKeysStrat(), key)) # wholesale reassignment again
          }
        }
      })
      
      compiledStratPairs <- reactive({
        keys <- rowKeysStrat()

        if (length(keys) == 0) return(list())

        pairs_list <- lapply(keys, function(key) {
          p1 <- input[[paste0(key, "_pickerDesignVar")]]
          p2 <- input[[paste0(key, "_pickerPoolType")]]
          p3 <- input[[paste0(key, "_pickerPoolVar")]]
          p4 <- input[[paste0(key, "_pickerAdjacentNum")]]
          
          warnStrat <- FALSE
          if(is.null(p1)) warnStrat <- TRUE
          if(!is.null(p1) && p1 != "Year" && p2 == "adjacent") warnStrat <- TRUE
          if(!is.null(p1) && p1 == "Year" && p2 == "adjacent" && is.na(p4)) warnStrat <- TRUE
          if(!is.null(p1) && p1 == "Year" && p2 == "adjacent" && !is.na(p4) && p4 < 1) warnStrat <- TRUE
          if(!is.null(p1) && p2 == "pooledVar" && is.null(p3)) warnStrat <- TRUE
          
          # Store elements inside a clean lookup list structured by ID key
          list(designVar = p1,  poolType= p2, poolVar = p3, adjacentNum = p4, warnStrat = warnStrat)
        })

        names(pairs_list) <- keys
        return(pairs_list)
      })
      
      observe({
        req(compiledStratPairs())
        warnStrat <- any(bind_rows(compiledStratPairs())[['warnStrat']])
        shinyjs::toggle("warningStrat", condition = warnStrat) 
      })
      

      ## Button for running data checks
      observeEvent(input$run_design, {

        on.exit({
          shinyjs::runjs("document.getElementById('ui-shield').style.display = 'none';")
        })

        if(
          !bycatchSetup()$active ||
          is.null(input$analysis_method) ||
          !nzchar(input$design_name) ||
          is.na(input$minStrataUnit) ||
          input$minStrataUnit < 1 ||
          length(compiledStratPairs()) == 0 ||
          isTRUE(length(compiledStratPairs()) > 0 && any(bind_rows(compiledStratPairs())[['warnStrat']])) 
        ){
          showModal(
            modalDialog(
              title = div(icon("circle-xmark", style = "color: red;"), div("Analysis failed", style = "display: inline; padding-left: 5px;")),
              h6("Cannot run analysis.", style = "color: red;"),
              div(
                style = "color: black; font-weight: normal;",
                "Check the following:",
                br(),
                tags$ul(
                  tags$li("Review form for missing entries in mandatory fields."),
                  tags$li("Review warning messages.")
                )
              ),
              easyClose = TRUE,
              footer = NULL
            )
          )
        } else {

          
          # Run bycatchSetup function
          tryCatch({ # for debugging
            #create list of inputs to be saved in resultsDir
            
            
            #Design vars as table
            designTmp <- bind_rows(compiledStratPairs())
            
            designVars <- designTmp$designVar
            
            designPooling <- FALSE
            if(any(designTmp$poolType != "none")) designPooling <- TRUE
            
            poolTypes <- designTmp$poolType
            
            pooledVar <- rep(NA, length(designVars))
            if(!is.null(designTmp[['poolVar']])) pooledVar <- designTmp$poolVar
            
            adjacentNum <- rep(NA, length(designVars))
            adjacentNum[which(designTmp$poolType == "adjacent")] <- designTmp$adjacentNum[which(designTmp$poolType == "adjacent")]
            
            varTmp <- input$group_var
            if(is.null(input$group_var)) varTmp <- NA
            
            # print(designVars)
            # print(designPooling)
            # print(poolTypes)
            # print(pooledVar)
            # print(adjacentNum)
            
            BycatchEstimator::bycatchDesign(
              setupObj = bycatchSetup()$setupObj,
              designScenario = input$design_name,
              designMethods = input$analysis_method,
              groupVar = varTmp,
              designVars = designVars,
              designPooling = designPooling,
              poolTypes=poolTypes,
              pooledVar=pooledVar,
              adjacentNum=adjacentNum,
              minStrataUnit = input$minStrataUnit
            )

            resultsActive(TRUE)
            
            showModal(
              modalDialog(
                title = div(icon("circle-check", style = "color: #198754;"), div("Analysis completed", style = "display: inline; padding-left: 5px;")),
                tags$ul(
                  tags$li("Anaysis was successful"),
                  tags$li("Proceed to Analysis results")
                ),
                easyClose = TRUE,
                footer = NULL
              )
            )

            updateTabsetPanel(
              session = session,
              inputId = "inputUIPanel",
              selected = "analysisTab"
            )

          }, error = function(e) {

            resultsActive(FALSE)
            
            showModal(
              modalDialog(
                title = div(icon("circle-xmark", style = "color: red;"), div("Analysis failed", style = "display: inline; padding-left: 5px;")),
                tags$ul(
                  tags$li("Review fields for errors"),
                  tags$li("If this problem persists, consult user guide")
                ),
                easyClose = TRUE,
                footer = NULL
              )
            )
            showNotification(paste("Error running bycatchDesign:", e$message), type = "error")
          })
        }
      })
  
      observeEvent(input$deactivate, {
        on.exit({
          shinyjs::runjs("document.getElementById('ui-shield').style.display = 'none';")
        })
        
        shinyalert(
          inputId = session$ns("deactivateAlert"),
          title = "Reset design-based estimator analysis?",
          text = "Resetting will allow you to modify your specification for this analysis. However, doing so will delete your current design-based analysis. Consider downloading results before a reset. Are you sure you want to continue?",
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "Yes",
          cancelButtonText = "No",
          callbackR = function(value = input$deactivateAlert) {
            #Yes button clicked, delete project from server
            if(value){
              #Deletes design files folder
              matching_dirs <- list.dirs(path = outDir, recursive = TRUE, full.names = TRUE)
              filtered_dirs <- grep("Design files", matching_dirs, value = TRUE)
              if (length(filtered_dirs) > 0) {
                unlink(filtered_dirs, recursive = TRUE, force = TRUE)
              }
              
              matching_files <- list.files(path = outDir, recursive = TRUE, full.names = TRUE)
              #design results html
              filtered_results <- grep("DesignResults.html", matching_files, value = TRUE)
              if (length(filtered_results) > 0) {
                unlink(filtered_results, force = TRUE)
              }
              #rds file
              filtered_rds <- grep(paste0("BycatchDesign", ".*", ".rds"), matching_files, value = TRUE)
              if (length(filtered_rds) > 0) {
                unlink(filtered_rds, force = TRUE)
              }
              
              resultsActive(FALSE)
              updateTabsetPanel(
                session = session,
                inputId = "inputUIPanel",
                selected = "specificationsTab"
              )
            }
          }
        )
      }, ignoreInit = TRUE)


      
      #-------------------
      #Reactives returned
      #-------------------
      return(resultsActive)
     
    }
  )
}

