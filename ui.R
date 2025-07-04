

#------------------------
#Header
#------------------------

header <- dashboardHeader(
  
  # #Branding for the app - client logo, link to website, etc.
  # title = dashboardBrand(
  #   title = "BycatchEstimator",
  #   color = "primary",
  #   href = "https://github.com/ebabcock/BycatchEstimator",
  #   image = "imgfile.png",
  # ),
  # 
  #Icon to open/close sidebar
  sidebarIcon = icon("compress"),
  
  #Icon for right control bar
  controlbarIcon = shiny::icon("th")
)


#------------------------
#Sidebar
#------------------------

sidebar <- dashboardSidebar(
  
  #----------------
  #Settings
  #---------------
  minified = TRUE,
  collapsed = FALSE,

  
  #----------------
  #NA footer
  #----------------
  shiny::tags$div(
    style = "position: absolute; bottom: 4px;", 
    tags$a(href="https://natureanalytics.ca", 
           tags$img(src="NA.png",
                    height=60,
                    width=60,
           ),
           target="_blank")
  ),
  
  #-------------
  #Items
  #-------------
  sidebarMenu(
    
    menuItem(
      tabName = "home",
      text = tagList(
        div(
          style = "display: flex; justify-content: center; align-items: center;",
        tags$img(
          src = "imgfile.png",
          height = "200px"
        )
      ))
    ),
    br(),
    menuItem(
      tabName = "upload",
      text = "Data upload",
      icon = icon("paperclip"),
      menuSubItem(
        text = tags$small("Observer data"),
        tabName = "Observer_upload",
      ),
      menuSubItem(
        text = tags$small("Logbook data"),
        tabName = "Logbook_upload"
      )
    ),
   menuItem(
     tabName = "checks",
     text = "Data checks",
     icon = icon("file-lines")
   ),
   menuItem(
     tabName = "help",
     text = "Help",
     icon = icon("question")
   )
  )
)


#----------------------------------
#Body
#----------------------------------

body<-dashboardBody(
  
  #---------------
  #Call to
  #---------------
  useShinyjs(),
  
  
  #----------------------
  #Read styling items
  #----------------------
  use_theme(inputTheme),
  inputSliderSkin,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
  
  #load modules here as tabItems
  tabItems(
    tabItem(
      tabName = "home", 
      landingpage_UI("landingpage")
    ),
    tabItem(
      tabName = "Observer_upload",
      observerupload_UI("observerdata_upload")
    ),
    tabItem(
      tabName = "Logbook_upload",
      logbookupload_UI("logbookdata_upload")
    ),
    tabItem(
      tabName = "checks",
      datachecks_UI("datachecks")
    ),
    tabItem(
      tabName = "help",
      help_UI("help_module")
    )
  )
)

#-----------------
#Dashboard Page
#-----------------

bs4Dash::dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  #footer = footer,
  dark = FALSE
)



