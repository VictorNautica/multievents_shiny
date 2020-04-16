navbarPage(theme = shinythemes::shinytheme("yeti"), 
  title = "Multi-events repository",
  tabPanel(
    "Calculator",
    navbarPage(
      title = "",
      theme = "yeti",
      tabPanel("Decathlon",
    sidebarLayout(sidebarPanel(width = 2,
      fluidRow(
        fluidRow(column(6, numericInput("event_one", "100m (s):", 10.87, min = 0, max = 100, step = 0.1, width = "80%")),
                 padding("value_one")
      )),
      fluidRow(
        fluidRow(column(6, numericInput("event_two", "Long jump (m):", 7.6, min = 0, max = 100, step = 0.1, width = "80%")),
                 padding("value_two")
        )),
      fluidRow(
        fluidRow(column(6, numericInput("event_three", "Shot put (m):", 14, min = 0, max = 100, step = 0.1, width = "80%")),
                 padding("value_three")
        )),
      fluidRow(
        fluidRow(column(6, numericInput("event_four", "High jump (m):", 2.00, min = 0, max = 100, step = 0.03, width = "80%")),
                 padding("value_four")
        )),
      fluidRow(
        fluidRow(column(6, numericInput("event_five", "400m (s)", 50, min = 0, max = 100, step = 0.2, width = "80%")),
                 padding("value_five")
        )),
      fluidRow(
        fluidRow(column(6, numericInput("event_six", "110m hurdles (s):", 16, min = 0, max = 100, step = 0.1, width = "80%")),
                 padding("value_six")
        )),
      fluidRow(
        fluidRow(column(6, numericInput("event_seven", "Discus throw (m):", 42, min = 0, max = 100, step = 0.5, width = "80%")),
                 padding("value_seven")
        )),
      fluidRow(
        fluidRow(column(6, numericInput("event_eight", "Pole vault (m):", 4.70, min = 0, max = 100, step = 0.1, width = "80%")),
                 padding("value_eight")
        )),
      fluidRow(
        fluidRow(column(6, numericInput("event_nine", "Javelin throw (m):", 62, min = 0, max = 100, step = 1, width = "80%")),
                 padding("value_nine")
        )),
      fluidRow(
        fluidRow(column(6, numericInput("event_ten", "1500m (s):", 290, min = 0, max = 500, step = 1, width = "80%")),
                 padding("value_ten")
        ))
    ), 
    mainPanel(
      # Output: Header + table of distribution ----
      fluidRow(column(width = 6, h4("Summary statistics"),
      tableOutput("dec_table"),
      plotOutput("dec_plot")), fluidRow(width = 6))
    )
    )
    ),
    tabPanel("Heptathlon",
             sidebarLayout(sidebarPanel(width = 2,
                                        fluidRow(
                                          fluidRow(column(6, numericInput("hept_one", "100m hurdles (s):", 13.36, min = 0, step = 0.1, width = "80%")),
                                                   padding("valueh_one")
                                          )),
                                        fluidRow(
                                          fluidRow(column(6, numericInput("hept_two", "High jump (m):", 1.77, min = 0, step = 0.3, width = "80%")),
                                                   padding("valueh_two")
                                          )),
                                        fluidRow(
                                          fluidRow(column(6, numericInput("hept_three", "Shot put (m):", 14.02, min = 0, max = 100, step = 0.1, width = "80%")),
                                                   padding("valueh_three")
                                          )),
                                        fluidRow(
                                          fluidRow(column(6, numericInput("hept_four", "200m (s):",  	24.29, min = 0, max = 100, step = 0.1, width = "80%")),
                                                   padding("valueh_four")
                                          )),
                                        fluidRow(
                                          fluidRow(column(6, numericInput("hept_five", "Long jump (m):", 6.28, min = 0, max = 100, step = 0.1, width = "80%")),
                                                   padding("valueh_five")
                                          )),
                                        fluidRow(
                                          fluidRow(column(6, numericInput("hept_six", "Javelin throw (m):", 46.74, min = 0, step = 0.1, width = "80%")),
                                                   padding("valueh_six")
                                          )),
                                        fluidRow(
                                          fluidRow(column(6, numericInput("hept_seven", "800m (s):", 126.78, min = 0, step = 0.1, width = "80%")),
                                                   padding("valueh_seven")
                                          )),
                                        ),
                           mainPanel(fluidRow(column(width = 6, h4("Summary statistics"),
                                                     tableOutput("hept_table"),                
                                     fluidRow(column(width = 6)
                                              )
                                     )
  )
  )
  )
  )
  )
  ),
  tabPanel(
    "Previous events data",
    navbarPage(
      title = "",
      theme = "yeti",
    tabPanel("Decathlon", 
    navbarPage(
      theme = "yeti",
      title = 'Select competition',
      decathlon_tabs(
        tab_label = "Olympics",
        ex_id = "ex1",
        select_year_label = "year_olympics",
        dfs_proper_call = "olympics",
        plotoutputlabel = "bar_olympics"
      ),
      decathlon_tabs(
        tab_label = "World Championships",
        ex_id = "ex2",
        select_year_label = "year_wc",
        dfs_proper_call = "world_championships",
        plotoutputlabel = "bar_wc"
      ),
      decathlon_tabs(
        tab_label = "Gotzis Hypomeeting",
        ex_id = "ex3",
        select_year_label = "year_gotzis",
        dfs_proper_call = "gotzis",
        plotoutputlabel = "bar_gotzis"
      )
    )),
    tabPanel("Heptathlon (coming soon)"))
  ),
  tabPanel("Score to points",
           navbarPage(theme = "yeti",
                      title = "",
                      tabPanel("Decathlon", navlistPanel(
                        widths = c(2, 10),
                        tabPanel(
                          "100m",
                          plotlyOutput("plotly_dec100m", height = "800px"),
                          "Note: may take a few seconds to load"
                        ),
                        tabPanel("Long jump"),
                        tabPanel("Shot put"),
                        tabPanel("High jump"),
                        tabPanel("400m"),
                        tabPanel("110m hurdles"),
                        tabPanel("Discus throw"),
                        tabPanel("Pole vault"),
                        tabPanel("Javelin throw"),
                        tabPanel("1500m")
                      ),
                      ),
                      tabPanel("Heptathlon", navlistPanel(
                        widths = c(2, 10),
                        tabPanel("100m hurdles"),
                        tabPanel("High jump"),
                        tabPanel("Shot put"),
                        tabPanel("200m"),
                        tabPanel("Long jump"),
                        tabPanel("Javelin throw"),
                        tabPanel("800m")
                      ))
                      )
           ), 
  tabPanel("Custom Data (coming soon)", 
           titlePanel(HTML(paste0("Upload completed multievents competition"))),
           
           # Sidebar layout with input and output definitions ----
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               
               # Input: Select a file ----
               fileInput("file1", "Choose CSV File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Checkbox if file has header ----
               checkboxInput("header", "Header", TRUE),
               
               # Input: Select separator ----
               radioButtons("sep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = ","),
               
               # Input: Select quotes ----
               radioButtons("quote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = '"'),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Select number of rows to display ----
               radioButtons("disp", "Display",
                            choices = c(Head = "head",
                                        All = "all"),
                            selected = "head")
               
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
               
               # Output: Data file ----
               tableOutput("contents")
               
             )
             
           )),
  tabPanel("Athlete Profile (coming soon)",
           sidebarLayout(sidebarPanel(
             selectInput(
               "athlete_select",
               "Athlete Name:",
               c("", lapply(dfs, function(x)
                 x$`Athlete`) %>% unlist(use.names = F) %>% unique() %>% sort())
             ),
             fluidRow(column(5, htmlOutput("use_this_athletename")),
                      column(7, HTML(paste(h4("Name: "), htmlOutput("athlete_specific"),
                                            h4("Date of Birth: "), htmlOutput("athlete_birth"),
                                            h4("Country: "), htmlOutput("athlete_country"),
                                            h4("IAAF Code: "), htmlOutput("iaaf_code"),
                                            h4("Height: "))))
           )),
           mainPanel(dataTableOutput("individual_athlete_profile"),
                     plotOutput("individual_athlete_plot"))
           )
           )
  , 
  tabPanel("About",
           HTML(
             paste(
               h4("About"),
               "<br/>",
               "I previously competed in the decathlon at an amateur level. I mainly created this Shiny App to showcase some of the skills I have in this feature of R for work training and my CV. If you have any queries or spot any inaccuracies in the data, please feel free to contact me through the form below:",
               "<br/>", "<br/>"
             )
           ),
           textAreaInput("form", NULL, placeholder = "Enter text", width = "400px", height = "150px"),
           actionButton("goButton", "Submit", width = "200px"),
           "Feel free give a small donation through the Paypal button below to support future features/my hosting costs.",
           HTML(rep("<br/>", 40), "Also I row 1:30 fat ergos for an infinite timeframe"))
)