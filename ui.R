

#------------------------
#Header
#------------------------

header <- dashboardHeader(
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
  minified = FALSE,
  collapsed = FALSE,
  status = "primary",
  
  #-------------
  #Items
  #-------------
  sidebarMenu(
    id = "sidebar",
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
    br(),
    menuItem(
      text = "Bycatch setup",
      icon = icon("table-cells"),
      menuSubItem(
        text = tagList(
          tags$small("Instructions")
        ),
        tabName = "setup_instructions",
        icon = NULL
      ),
      
      menuSubItem(
        text = tagList(
          dashboardBadge(
            id = "gather1", 
            icon("1"),
            color = "primary", 
            position = "left"
          ),
          HTML("&nbsp;"),
          tags$small("Gather observer data")
        ),
        tabName = "Observer_upload",
        icon = NULL
      ),
      menuSubItem(
        text = tagList(
          dashboardBadge(
            id = "gather2", 
            icon("2"), 
            color = "primary", 
            position = "left"
          ),
          HTML("&nbsp;"),
          tags$small("Gather logbook data")
        ),
        tabName = "Logbook_upload",
        icon = NULL
      ),
      menuSubItem(
        text = tagList(
          dashboardBadge(
            id = "gather3",
            icon("3"), 
            color = "primary", 
            position = "left"
          ),
          HTML("&nbsp;"),
          tags$small("Align data & activate")
        ),
        tabName = "setup_checks",
        icon = NULL
      )
    ),
    menuItem(
      text = "Data checks",
      icon = icon("chart-simple"),
      menuSubItem(
        text = tags$small("Diagnostics & summary"),
        tabName = "results_checks",
        icon = NULL
      )
    ),
   
   br(),
   # background: #007bff;
   div(
     style = "display: inline; color: #FFFFFF;  margin: 5px 5px; padding: 10px; border-radius: 5px;",
     icon("question"),
     HTML("&nbsp;"),
     "Support"
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
  
  tags$head(
    tags$script(src = "browserSideJS.js")
  ),
  
  # 4. The UI shield DOM element placed at the top of the body
  tags$div(id = "ui-shield"),
  
  #---------------
  #Call to
  #---------------
  useShinyjs(),
  useWaiter(),
  
  #----------------------
  #Read styling items
  #----------------------
  use_theme(inputTheme),
  inputSliderSkin,
  includeCSS("www/main.css"),
  
  #load modules here as tabItems
  tabItems(
    tabItem(
      tabName = "home", 
      landingpage_UI("landingpage")
    ),
    tabItem(
      tabName = "setup_instructions", 
      setupInstuctions_UI("setuppage")
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
      tabName = "setup_checks",
      datachecksSetup_UI("datachecks_setup")
    ),
    tabItem(
      tabName = "results_checks",
      datachecksResults_UI("datachecks_results")
    ),
    tabItem(
      tabName = "help",
      help_UI("help_module")
    )
  )
)

#-----------------------------------------------------
#Footer - For client use (turned off in UI by default)
#-----------------------------------------------------
footer = dashboardFooter(
  left =  HTML("<div style='padding-left: 10px;'>&copy ICCAT</div>"),
  
  right =
    tags$a(href="https://natureanalytics.ca",
           tags$image(src="NA.png",
                      height=32,
                      
           ),
           target="_blank")
)

#-----------------
#Dashboard Page
#-----------------

bs4Dash::dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  footer = footer,
  dark = NULL,
  help = NULL
)



