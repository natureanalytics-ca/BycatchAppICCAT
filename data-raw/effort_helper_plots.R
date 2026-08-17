

library(reactable)
library(htmlwidgets)
library(htmltools)
library(webshot2)

#--------------------------
#Example effort arrangement


observer_sampleunit <- c("Trips", "Sets")
logbook_sampleunit <- c("Same aggregation as observer data", "Aggregated further")

observer_sampleunit <- observer_sampleunit[2]
logbook_sampleunit <- logbook_sampleunit[2]

table_name <- "sets_furtherAgg"

#Obs  
obsDat <- data.frame(
  Catch = "e.g., 3 fish",
  Effort = switch(
    observer_sampleunit,
    Trips = "e.g., 7 sets in this trip",
    Sets = "e.g., 400 hooks in this set"
  ),
  sample.unit = switch(
    observer_sampleunit,
    Trips = "<- Trip (each row)",
    Sets = "<- Set (each row)"
  ) 
)

obs_columns <- c("Catch", "Effort", "sample.unit")

table_left <- reactable(
  obsDat,
  highlight = FALSE, compact = T, style = list(color = "#343a40"),
  pagination = FALSE,
  sortable = FALSE,
  bordered = TRUE,
  striped = TRUE,
  defaultColDef = colDef(vAlign = "center", align = "center", headerVAlign = "bottom", headerStyle = "font-family: Arial;", style = "font-family: Arial;"),
  columns = list(
    sample.unit = colDef(name = "Sample unit")
  ),
  theme = reactableTheme(
    backgroundColor = "transparent",
    cellPadding = "10px 5px"
  ),
  columnGroups = list(
    colGroup(name = "Observer effort", columns = obs_columns, headerStyle = "background-color: pink; font-family: Arial;")
  )
)

#Logbook
if(logbook_sampleunit == "Same aggregation as observer data") {
  logDat <- data.frame(
    sample.unit = switch(
      observer_sampleunit,
      Trips = "Trip (each row) ->",
      Sets = "Set (each row) ->"
    ),
    Effort = switch(
      observer_sampleunit,
      Trips = "e.g., 12 sets in this trip",
      Sets = "e.g., 650 hooks in this set"
    )
  )
  
  log_columns <- c("sample.unit", "Effort")
  
  
  table_right <- reactable(
      logDat,
      highlight = FALSE, compact = T, style = list(color = "#343a40"),
      pagination = FALSE,
      sortable = FALSE,
      bordered = TRUE,
      striped = TRUE,
      defaultColDef = colDef(vAlign = "center", align = "center", headerVAlign = "bottom", headerStyle = "font-family: Arial;", style = "font-family: Arial;"),
      columns = list(
        sample.unit = colDef(name = "Sample unit")
      ),
      theme = reactableTheme(
        backgroundColor = "transparent",
        cellPadding = "10px 5px"
      ),
      columnGroups = list(
        colGroup(name = "Logbook effort", columns = log_columns, headerStyle = "background-color: pink; font-family: Arial;")
      )
    )
  
  
} else {
  logDat <- data.frame(
    sample.unit = "Aggregated further ->",
    Effort = switch(
      observer_sampleunit,
      Trips = "e.g., 9 sets per trip",
      Sets = "e.g., 375 hooks per set"
    ),
    sample.unit.counts = switch(
      observer_sampleunit,
      Trips = "e.g., 5 trips taken",
      Sets = "e.g., 8 sets deployed"
    )
  )
  
  log_columns <- c("sample.unit", "Effort", "sample.unit.counts")
  
  
  table_right <- reactable(
      logDat,
      highlight = FALSE, compact = T, style = list(color = "#343a40"),
      pagination = FALSE,
      sortable = FALSE,
      bordered = TRUE,
      striped = TRUE,
      defaultColDef = colDef(vAlign = "center", align = "center", headerVAlign = "bottom", headerStyle = "font-family: Arial;", style = "font-family: Arial;"),
      columns = list(
        sample.unit = colDef(name = "Sample unit"),
        sample.unit.counts = colDef(name = "Sample unit counts")
      ),
      theme = reactableTheme(
        backgroundColor = "transparent",
        cellPadding = "10px 5px"
      ),
      columnGroups = list(
        colGroup(name = "Logbook effort", columns = log_columns, headerStyle = "background-color: pink; font-family: Arial;")
      )
    )
  
}

#Save
# 2. Combine them inside a CSS Flexbox container for a side-by-side layout
combined_layout <- tags$div(
  style = "display: flex; gap: 20px; justify-content: space-between; width: 100%;",
  tags$div(
    style = "flex: 1; min-width: 0;", 
    tags$strong(paste("Sample unit:", observer_sampleunit), style = "color: #343a40; font-family: Arial;"),
    table_left
  ),
  tags$div(
    style = "flex: 1; min-width: 0;", 
    tags$strong(paste("Aggregation level:", logbook_sampleunit), style = "color: #343a40;  font-family: Arial;"),
    table_right
  )
)

# 3. Apply the class trick so R can parse it as a standalone HTML page
html_widget_obj <- browsable(combined_layout)
class(html_widget_obj) <- c("htmlwidget", class(html_widget_obj))

# 4. Save to a temporary HTML file first
temp_html <- tempfile(fileext = ".html")
htmltools::save_html(html = combined_layout, file = temp_html)

# 5. Take a screenshot of the HTML page and save it as a PNG file
output_image <- paste0("www/", table_name, ".png")

webshot2::webshot(
  url = temp_html, 
  file = output_image, 
  vwidth = 1400,      # Simulated browser screen width
  vheight = 60,       # Simulated browser screen height (adjust to avoid truncation)
  zoom = 2    
)

