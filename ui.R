navbarPage(theme = shinythemes::shinytheme("cerulean"), 
  title = "Multi-events repository",
  tabPanel(
    "Decathlon calculator",
    sidebarLayout(sidebarPanel(
      splitLayout(
        verticalLayout(
          numericInput("event_one", "100m:", 10.87, min = 0, max = 100, step = 0.1, width = "40%"),
          numericInput("event_two", "LJ:", 7.6, min = 0, max = 100, step = 0.1, width = "40%"),
          numericInput("event_three", "SP:", 14, min = 0, max = 100, step = 0.1, width = "40%"),
          numericInput("event_four", "HJ:", 2.00, min = 0, max = 100, step = 0.03, width = "40%"),
          numericInput("event_five", "400m:", 50, min = 0, max = 100, step = 0.2, width = "40%"), 
          numericInput("event_six", "110m hurdles:", 16, min = 0, max = 100, step = 0.1, width = "40%"),
          numericInput("event_seven", "Discus throw:", 42, min = 0, max = 100, step = 0.5, width = "40%"),
          numericInput("event_eight", "Pole vault:", 4.70, min = 0, max = 100, step = 0.1, width = "40%"),
          numericInput("event_nine", "Javelin throw:", 62, min = 0, max = 100, step = 1, width = "40%"),
          numericInput("event_ten", "1500m:", 290, min = 0, max = 500, step = 1, width = "40%")
          ),
        verticalLayout(
          title = "Points",
          textOutput("value_one"),
          textOutput("value_two"),
          textOutput("value_three"),
          textOutput("value_four"),
          textOutput("value_five"),
          textOutput("value_six"),
          textOutput("value_seven"),
          textOutput("value_eight"),
          textOutput("value_nine"),
          textOutput("value_ten")
        )
      )
    ),
    mainPanel(
      # Output: Header + table of distribution ----
      h4("Summary statistics"),
      tableOutput("dec_table"),
      plotOutput("dec_plot")
    ))
  ),
  tabPanel(
    "Previous decathlons",
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
  tabPanel("About",
           HTML(
             paste(
               h4("About"),
               "<br/>",
               "I previously competed in the decathlon at an amateur level. I mainly created this Shiny App to showcase some of the skills I have in this feature of R for work training and my CV. If you spot any inaccuracies in the data, please feel free to contact me through the form below:",
               "<br/>", "<br/>"
             )
           ),
           textAreaInput("form", NULL, placeholder = "Enter text", width = "400px", height = "150px"),
           actionButton("goButton", "Submit", width = "200px"),
           HTML(rep("<br/>", 40), "Also I row 1:30 fat ergos for an infinite timeframe"))
)