#-------------------------------
#module upload observer data
#-------------------------------

############# SERVER #############


observerupload_SERVER <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      #---------------------------
      #Input contraints on nchar
      #---------------------------
      shinyjs::runjs("$('#observerdata_upload-observerdata_title').attr('maxlength', 50)")      
      
      # Holds the file uploaded by user
      previewObserverdata <- reactiveVal(NULL)
      
      # Process file upload
      observeEvent(input$customObserverData, {
        on.exit({
          shinyjs::runjs("document.getElementById('ui-shield').style.display = 'none';")
        })
        
        req(input$customObserverData)

        # Extract file extension (converted to lower case)
        ext <- tools::file_ext(input$customObserverData$name)

        if (tolower(ext) != "csv") {
          showNotification(
            "Invalid file type. Please select a CSV file (.csv).", 
            type = "warning"
          )
          parsed_data <- NULL
        } else {
          # Read CSV safely
          parsed_data <- tryCatch(
            {
              read_csv(
                input$customObserverData$datapath,
                col_names = TRUE, 
                show_col_types = FALSE
              )
            },
            error = function(e) {
              showNotification(
                paste("Error reading file:", e$message), 
                type = "error"
              )
              return(NULL)
            }
          )
        }
        
        # Update state only if data successfully parsed
        if (!is.null(parsed_data)) {
          
          # Automatically pre-fill the title box for the user
          raw_name <- tools::file_path_sans_ext(input$customObserverData$name)
          clean_spaces <- gsub("[_-]", " ", raw_name)
          polished_title <- tools::toTitleCase(clean_spaces)
          
          updateTextInput(
            session = session, 
            inputId = "observerdata_title", 
            value = polished_title
          )
          
          previewObserverdata(parsed_data)
          
          # Reveal UI elements upon success
          shinyjs::show("preview")
          shinyjs::show("finalize")
          shinyjs::show("observerInputs")
          shinyjs::show("observerInputs2")
        } else {
          # Hide controls if file failed to load
          previewObserverdata(NULL)
          shinyjs::hide("preview")
          shinyjs::hide("finalize")
          shinyjs::hide("observerInputs")
          shinyjs::hide("observerInputs2")
        }
      }, ignoreInit = TRUE)
      
      #Clear state on Reset Button click
      observeEvent(input$resetObserverData, {
        
        previewObserverdata(NULL)
        shinyjs::reset("customObserverData")
        
        # Hide container panels
        shinyjs::hide("preview")
        shinyjs::hide("finalize")
        shinyjs::hide("observerInputs")
        shinyjs::hide("observerInputs2")
        
        showNotification("File upload cleared.", type = "message")
      })
      
      # Data table output
      output$previewObserverDT <- renderDT({
        req(previewObserverdata())
        datatable(
          previewObserverdata(),
          rownames = FALSE
        )
      })
      
      #Centralized State Controller (Single Source of Truth)
      observeEvent(previewObserverdata(), {
        
        updatePickerInput(
          session = session,
          inputId = "observer_catchunits",
          selected = character(0)
        )
        
        updatePickerInput(
          session = session,
          inputId = "observer_catchtype",
          selected = character(0),
        )

        # Define dropdown fields tied to the data structure
        target_inputs <- c("observer_catch", "observer_year")
        
        # UI Locks & Backend Freezes
        lapply(target_inputs, shinyjs::disable)
        lapply(target_inputs, function(x) freezeReactiveValue(input, x))
        
        if (!is.null(previewObserverdata())) {
          cols <- names(previewObserverdata())
          
          # --- SUCCESS STATE (File Uploaded) ---
          updatePickerInput(
            session = session, 
            inputId = "observer_catch", 
            choices = setdiff(cols, c("Year", "Effort", "SampleUnits")),
            selected = character(0)
          )
          
          updatePickerInput(
            session = session, 
            inputId = "observer_year", 
            choices = setdiff(cols, c("Catch", "Effort", "SampleUnits")),
            selected = character(0)
          )
          
          # Unlock inputs for user interaction
          lapply(target_inputs, shinyjs::enable)
          
        } else {
          
          updateTextInput(
            session = session,
            inputId = "observerdata_title",
            value = character(0)
          )
          
          updatePickerInput(
            session = session, 
            inputId = "observer_catch", 
            choices = character(0)
          )
          
          updatePickerInput(
            session = session, 
            inputId = "observer_year", 
            choices = character(0)
          )
          # Left disabled so users can't interact with empty inputs
        }
      }, ignoreNULL = FALSE)

       
      # Reactive object to pass downstream
      observerInputsInfo <- reactive({
        list(
          dt                  = previewObserverdata(),
          title               = input$observerdata_title,
          observer_year       = input$observer_year,
          observer_catch      = input$observer_catch,
          observer_catchunits = input$observer_catchunits,
          observer_catchtype  = input$observer_catchtype,
          formComplete = isFALSE(
              is.null(previewObserverdata()) ||
              !nzchar(input$observerdata_title) ||
              is.null(input$observer_year) ||
              is.null(input$observer_catch) ||
              is.null(input$observer_catchunits) ||
              is.null(input$observer_catchtype) ||
              isTRUE("Year" %in% names(previewObserverdata()) && input$observer_year != "Year") ||
              isTRUE("Catch" %in% names(previewObserverdata()) && input$observer_catch != "Catch") ||
              isTRUE(input$observer_year == input$observer_catch)
          )
        )
      })
      
      #-----------------------------
      #Reactives returned by module
      #-----------------------------
      return(observerInputsInfo)
      
    }) #close function moduleServer
} #close observerupload_SERVER


