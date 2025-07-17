

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
   #  menuItem(
   #    tabName = "home",
   #    text = tagList(
   #      div(
   #        style = "display: flex; justify-content: center; align-items: center;",
   #      tags$img(
   #        src = "imgfile.png",
   #        height = "200px"
   #      )
   #    ))
   #  ),
   #  br(),
   #  menuItem(
   #    tabName = "upload",
   #    text = "Data upload",
   #    icon = icon("paperclip"),
   #    startExpanded = TRUE,
   #    menuSubItem(
   #      text = tagList(
   #        dashboardBadge("Step 1", color = "primary", position = "left"),
   #        HTML("&nbsp;"),
   #        tags$small("Observer data")
   #      ),
   #      tabName = "Observer_upload",
   #      icon = NULL
   #    ),
   #    menuSubItem(
   #      text = tagList(
   #        dashboardBadge("Step 2", color = "primary", position = "left"),
   #        HTML("&nbsp;"),
   #        tags$small("Logbook data")
   #      ),
   #      tabName = "Logbook_upload",
   #      icon = NULL
   #    )
   #  ),
   # menuItem(
   #   tabName = "checks",
   #   text = "Data checks",
   #   icon = icon("file-lines"),
   #   menuSubItem(
   #     text = tagList(
   #       dashboardBadge("Step 1", color = "primary", position = "left"),
   #       HTML("&nbsp;"),
   #       tags$small("Setup")
   #     ),
   #     tabName = "setup_checks",
   #     icon = NULL
   #   ),
   #   menuSubItem(
   #     text = tagList(
   #       dashboardBadge("Step 2", color = "primary", position = "left"),
   #       HTML("&nbsp;"),
   #       tags$small("Results")
   #     ),
   #     tabName = "results_checks",
   #     icon = NULL
   #   )
   # ),
   # menuItem(
   #   tabName = "help",
   #   text = "Help",
   #   icon = icon("question")
   # )
    
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
    #background: #007bff;
    div(
      style = "display: inline; color: #FFFFFF;  margin: 5px 5px; padding: 10px; border-radius: 5px;",
      icon("paperclip"),
      HTML("&nbsp;"),
      "Data upload"
    ),
    menuItem(
      text = tagList(
        dashboardBadge("Step 1", color = "primary", position = "left"),
        HTML("&nbsp;"),
        tags$small("Observer data")
      ),
      tabName = "Observer_upload",
      icon = NULL
    ),
    menuItem(
      text = tagList(
        dashboardBadge("Step 2", color = "primary", position = "left"),
        HTML("&nbsp;"),
        tags$small("Logbook data")
      ),
      tabName = "Logbook_upload",
      icon = NULL
    ),
    br(),
    #background: #007bff
    div(
      style = "display: inline; color: #FFFFFF; ; margin: 5px 5px; padding: 10px; border-radius: 5px;",
      icon("file-lines"),
      HTML("&nbsp;"),
      "Data checks"
    ),
    menuItem(
      text = tagList(
        dashboardBadge("Step 1", color = "primary", position = "left"),
        HTML("&nbsp;"),
        tags$small("Setup")
      ),
      tabName = "setup_checks",
      icon = NULL
    ),
    menuItem(
      text = tagList(
        dashboardBadge("Step 2", color = "primary", position = "left"),
        HTML("&nbsp;"),
        tags$small("Results")
      ),
      tabName = "results_checks",
      icon = NULL
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



