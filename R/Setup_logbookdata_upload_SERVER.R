#-------------------------------
#module upload logbook data
#-------------------------------


############# SERVER #############

logbookupload_SERVER <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      #---------------------------
      #Input contraints on nchar
      #---------------------------
      shinyjs::runjs("$('#logbookdata_upload-logbookdata_title').attr('maxlength', 50)")
      
      #Holds the file uploaded by user
      previewLogbookdata<-reactiveVal(NULL)
      
      # Process file upload
      observeEvent(input$customLogbookData, {
        on.exit({
          shinyjs::runjs("document.getElementById('ui-shield').style.display = 'none';")
        })
        
        req(input$customLogbookData)
        
        # Extract file extension (converted to lower case)
        ext <- tools::file_ext(input$customLogbookData$name)
        
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
                input$customLogbookData$datapath,
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
          raw_name <- tools::file_path_sans_ext(input$customLogbookData$name)
          clean_spaces <- gsub("[_-]", " ", raw_name)
          polished_title <- tools::toTitleCase(clean_spaces)
          
          updateTextInput(
            session = session, 
            inputId = "logbookdata_title", 
            value = polished_title
          )
          
          previewLogbookdata(parsed_data)
          
          # Reveal UI elements upon success
          shinyjs::show("preview")
          shinyjs::show("finalize")
          shinyjs::show("logBookInputs")
        } else {
          # Hide controls if file failed to load
          previewObserverdata(NULL)
          shinyjs::hide("preview")
          shinyjs::hide("finalize")
          shinyjs::hide("logBookInputs")
        }
      }, ignoreInit = TRUE)
      
      #Clear state on Reset Button click
      observeEvent(input$resetLogbookData, {
        
        previewLogbookdata(NULL)
        shinyjs::reset("customLogbookData")
        
        #Hide the preview elements again
        shinyjs::hide("preview")
        shinyjs::hide("finalize")
        shinyjs::hide("logBookInputs")
        
        showNotification("File upload cleared.", type = "message")
      })
      
      # Data table output
      output$previewLogbookDT <- renderDT({
        req(previewLogbookdata())
        datatable(
          previewLogbookdata(),
          rownames = FALSE
        )
      })
      
      #Centralized State Controller (Single Source of Truth)
      observeEvent(previewLogbookdata(), {
        
        shinyjs::disable("logbook_year")
        freezeReactiveValue(input, "logbook_year")
        
        if (!is.null(previewLogbookdata())) {
          # Valid data: populate picker with column names
          updatePickerInput(
            session = session,
            inputId = "logbook_year",
            choices = names(previewLogbookdata()),
            selected = character(0)
          )
          shinyjs::enable("logbook_year")
          
        } else {
          
          updateTextInput(
            session = session,
            inputId = "logbookdata_title",
            value=character(0)
          )

          updatePickerInput(
            session = session,
            inputId = "logbook_year",
            choices = character(0)
          )
          # Left disabled so users can't interact with empty inputs
        }
      }, ignoreNULL = FALSE)
      
      # Reactive object to pass downstream
      logbookInputsInfo <- reactive({
        list(
          dt = previewLogbookdata(),
          title = input$logbookdata_title,
          logbook_year = input$logbook_year,
          formComplete = isFALSE(
            is.null(previewLogbookdata()) ||
            !nzchar(input$logbookdata_title) ||
            is.null(input$logbook_year) ||
            isTRUE("Year" %in% names(previewLogbookdata()) && input$logbook_year != "Year")
          )
        )
      })
      
      #-----------------------------
      #Reactives returned by module
      #-----------------------------
      logbookInputsInfo
      
    }) #close function moduleServer
} #close logbookupload_SERVER
