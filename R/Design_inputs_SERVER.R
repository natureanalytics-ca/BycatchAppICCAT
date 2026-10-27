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
          updatePickerInput(session = session, inputId = "strat_var", choices = col_names, selected = character(0))
          shinyjs::enable("strat_var")
          updatePickerInput(session = session, inputId = "group_var", choices = col_names, selected = character(0))
          shinyjs::enable("group_var")
          
        } else {
          updatePickerInput(session = session, inputId = "strat_var", choices = character(0))
          updatePickerInput(session = session, inputId = "group_var", choices = character(0))
        }
        
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


      
      ## Button for running data checks
      observeEvent(input$run_design, {

        on.exit({
          shinyjs::runjs("document.getElementById('ui-shield').style.display = 'none';")
        })

        if(
          !bycatchSetup()$active ||
          is.null(input$analysis_method) ||
          is.null(input$strat_var) ||
          #!("Year" %in% input$strat_var) ||
          !nzchar(input$design_name)
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
            
            varTmp <- input$group_var
            if(is.null(input$group_var)) varTmp <- NA
            
            BycatchEstimator::bycatchDesign(
              setupObj = bycatchSetup()$setupObj,
              designScenario = input$design_name,
              designMethods = input$analysis_method,
              groupVar = varTmp,
              designVars = input$strat_var
              #designPooling = TRUE,
              #poolTypes=c("adjacent","all","none"),
              #pooledVar=c(NA,NA,NA),
              #adjacentNum=c(1,NA,NA),
              #minStrataUnit = 1
            )

            resultsActive(TRUE)
            
            showModal(
              modalDialog(
                title = div(icon("circle-check", style = "color: #007bff;"), div("Analysis completed", style = "display: inline; padding-left: 5px;")),
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

