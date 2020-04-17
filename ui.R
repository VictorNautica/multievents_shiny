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
      plotOutput("dec_plot")), fluidRow(width = 6))))
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
                           mainPanel(
                             fluidRow(column(width = 6, h4("Summary statistics"),
                                             tableOutput("hept_table"),
                                             plotOutput("hept_plot")), fluidRow(width = 6))))
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
                        tabPanel("100m", 
                                 plotlyOutput("plotly_dec100m", height = "800px")),
                        tabPanel("Long jump",
                                 plotlyOutput("plotly_declj", height = "800px")),
                        tabPanel("Shot put",
                                 plotlyOutput("plotly_decsp", height = "800px")),
                        tabPanel("High jump",
                                 plotlyOutput("plotly_dechj", height = "800px")),
                        tabPanel("400m",
                                 plotlyOutput("plotly_dec400m", height = "800px")),
                        tabPanel("110m hurdles",
                                 plotlyOutput("plotly_dec110mh", height = "800px")),
                        tabPanel("Discus throw",
                                 plotlyOutput("plotly_decdt", height = "800px")),
                        tabPanel("Pole vault",
                                 plotlyOutput("plotly_decpv", height = "800px")),
                        tabPanel("Javelin throw",
                                 plotlyOutput("plotly_decjt", height = "800px")),
                        tabPanel("1500m",
                                 plotlyOutput("plotly_dec1500m", height = "800px")),
                        h6("Note: may take a few seconds to load")
                      ),
                      ),
                      tabPanel("Heptathlon (coming soon)", navlistPanel(
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
  tabPanel("Heat Plots",
           navbarPage(theme = "yeti",
                      title = "",
                      tabPanel("Decathlon", 
                               radioButtons("decathlon_tile_choose", 
                                            label = NULL, 
                                            inline = T,
                                            choices = c("Score" = "score",
                                              "Points" = "points")),
                               plotOutput("decathlon_heatplot_points", height = "800px")),
                      tabPanel("Heptathlon (coming soon)")
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
           sidebarLayout(sidebarPanel(width = 3,
             selectInput(
               "athlete_select",
               "Athlete Name:",
               lapply(dfs, function(x)
                 x$`Athlete`) %>% unlist(use.names = F) %>% unique() %>% sort()
             ),
             fluidRow(
               column(4, htmlOutput("use_this_athletename")),
               column(
                 8,
                 fluidRow(column(
                   6,  div(style = "text-align:right", tags$b("Date of Birth: "))
                 ), column(6, htmlOutput("athlete_birth"))),
                 fluidRow(column(
                   6,  div(style = "text-align:right", tags$b("Country: "))
                 ),
                 column(6, htmlOutput("athlete_country"))),
                 fluidRow(column(
                   6,  div(style = "text-align:right", tags$b("IAAF Code: "))
                 ),
                 column(6, htmlOutput("iaaf_code"))),
                 fluidRow(column(
                   6, div(style = "text-align:right", tags$b("Height: "))
                 ),
                 column(6, "Coming soon")),
                 
                             )),
             tags$br(),
             plotOutput("radar_athlete")
             ),
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
               "I previously competed in the decathlon at an amateur level. I mainly created this Shiny App to showcase some of the skills I have in this feature of R for work training and my CV.<br/>
              If you have any queries or spot any inaccuracies in the data, please contact me through the form below:",
               "<br/>", "<br/>"
             )
           ),
           textAreaInput(
             "form",
             NULL,
             placeholder = "Enter text",
             width = "500px",
             height = "150px",
             resize = "none"
           ), 
           actionButton("goButton", "Submit", width = "200px"),
           HTML("<br/><br/>"),
           "If you like the app than feel free to give a small donation through the Paypal button below to support future development/my hosting costs.",
           HTML(paste('<br/><br/><form action="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=DV63U2B9R7FE6&source=url" method="post" target="_top">
<input type="hidden" name="cmd" value="_s-xclick" />
<input type="hidden" name="hosted_button_id" value="DV63U2B9R7FE6" />
<input type="image" src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" border="0" name="submit" title="PayPal - The safer, easier way to pay online!" alt="Donate with PayPal button" />
<img alt="" border="0" src="https://www.paypal.com/en_GB/i/scr/pixel.gif" width="1" height="1" />
</form>
')),
           HTML("<br/><br/>"),
           h4("Credits"),
           HTML("<br/>"),
           "- decathlon2000.com for providing the scores/points data",
           HTML("<br/>"),
           "- Stack Overflow for being an amazing resource for coding Q&A",
           HTML("<br/>"),
           "- Tom Jemmett for helping with some initial bugs on the dataset view tab",
           HTML("<br/>"),
           "- Thomas Park for the gorgeous Yeti CSS theme")
)