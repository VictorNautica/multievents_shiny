navbarPage(
  theme = shinythemes::shinytheme("sandstone"),
  title = "Multi-events repository",
  tabPanel(
    ## 1) Calculator ####
    "Calculator",
    navbarPage(
      title = "",
      tabPanel("Decathlon",
    sidebarLayout(sidebarPanel(width = 2,
      fluidRow(
        fluidRow(column(6, numericInput("event_one", "100m (s):", 10.87, min = 0, max = 100, step = 0.1, width = "80%")),
                 padding("value_one")
      )),
      fluidRow(checkboxInput("handtime_100m", "Hand timed", value = F)),
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
      fluidRow(checkboxInput("handtime_400m", "Hand timed", value = F)),
      fluidRow(
        fluidRow(column(6, numericInput("event_six", "110m hurdles (s):", 16, min = 0, max = 100, step = 0.1, width = "80%")),
                 padding("value_six")
        )),
      fluidRow(checkboxInput("handtime_110mh", "Hand timed", value = F)),
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
      ## Output: Header + table of distribution ####
      fluidRow(column(width = 6,
                      h4("Summary statistics"),
                      tableOutput("dec_table")),
               column(width = 6, 
                      h4(" "),
                      plotOutput("dec_plot", height = "600px")))
      ))
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
                             fluidRow(column(width = 6,
                                             h4("Summary statistics"),
                                             tableOutput("hept_table")),
                                      column(width = 6, 
                                             h4(" "),
                                             plotOutput("hept_plot", height = "600px")))
                             ))
  )
  )
  ),
  tabPanel(
  ## 2) Previous events data ####  
    "Previous events data",
    navbarPage(
      title = "",
    tabPanel("Decathlon", 
    navbarPage(
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
  ## 3) Score to Points #####
  tabPanel("Score to points",
           navbarPage(title = "",
                      tabPanel("Decathlon", navlistPanel(
                        widths = c(2, 10),
                        s2ppanel("100m", "plotly_dec100m"),
                        s2ppanel("Long Jump", "plotly_declj"),
                        s2ppanel("Shot put", "plotly_decsp"),
                        s2ppanel("High jump", "plotly_dechj"),
                        s2ppanel("400m", "plotly_dec400m"),
                        s2ppanel("110m hurdles", "plotly_dec110mh"),
                        s2ppanel("Discus throw", "plotly_decdt"),
                        s2ppanel("Pole vault", "plotly_decpv"),
                        s2ppanel("Javelin throw", "plotly_decjt"),
                        s2ppanel("1500m", "plotly_dec1500m"))
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
  ## 4) Data Visualisations####
  tabPanel("Data Visualisations",
           navlistPanel(
             widths = c(2, 10),
             tabPanel("Heat Plots",
                      tabsetPanel(
                        tabPanel(
                          "Decathlon",
                          radioButtons(
                            "decathlon_tile_choose",
                            label = NULL,
                            inline = T,
                            choices = c("Score" = "score",
                                        "Points" = "points")
                          ),
                          plotOutput("decathlon_heatplot_points", height = "800px")
                        ),
                        tabPanel("Heptathlon (coming soon)")
                      )),
             tabPanel(
               "Final Score Ordered Bar Plots",
               tabsetPanel(tabPanel("Decathlon", 
                                    plotlyOutput("OFSP_plot_decathlon")),
                           tabPanel("Heptathlon (coming soon)"))
             )
           )),   
  ## 4) Custom Data ####
  tabPanel("Custom Data (coming soon)", 
           titlePanel(HTML(paste0("Upload completed multievents competition"))),
           
           # Sidebar layout with input and output definitions ----
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(width = 2,
               
               # Input: Select a file ----
               fileInput("file1", "Upload CSV File",
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
               tabsetPanel(
                 tabPanel("Instructions",
               "Allow user to input complete decathlon in .csv/.xslx format and allow for plot creation. Think UX.",
               tags$br(),tags$br(),
               "Step 1) Upload a .csv or .xslx file in the following format:",tags$br(),tags$br(),
               "<Screenshot of excel spreadsheet here>",tags$br(),tags$br(),
               "The primary requirement for this feature to function is that the dataset has to have points the athletes have scored in in sequential columns, in the order the decathlon/heptathlon events take place. Ranking order, points conversion, or other miscellaneous columns not required."),
               
               # Output: Data file ----
               tabPanel("Output",
               tableOutput("contents")
               )
             )
             )
             
           )),
  ## 5) Athlete Profile ####
  tabPanel("Athlete Profile (coming soon)",
           sidebarLayout(sidebarPanel(width = 3,
             selectInput(
               "athlete_select",
               "Athlete Name:",
               ultimate_df_list[["ultimate_df_points"]] %>% pull(Athlete) %>% unique() %>% sort()
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
                 column(6, "Scrape from sports-reference")),
                 
                             )),
             tags$br(),
             tabsetPanel(tabPanel("Radar Plot",
             plotOutput("radar_athlete"),
             tags$br(),
             tags$b("Relative Performance:"),
             tags$br(),
             fluidRow(column(3,"Speed"), column(9, htmlOutput("radar_speed"))),
             fluidRow(column(3,"Throws"), column(9, htmlOutput("radar_throws"))),
             fluidRow(column(3,"Jumps"), column(9, htmlOutput("radar_jumps"))),
             fluidRow(column(3,"Endurance"), column(9, htmlOutput("radar_endurance")))
             ),
             tabPanel("Scatter Plot",
                      fluidRow(column(4, selectInput("scatter3d_x", "x-axis",
                                                     choices = scatter3d_options,
                                                     selected = "Speed")),
                               column(4, selectInput("scatter3d_y", "y-axis",
                                                     choices = scatter3d_options,
                                                     selected = "Throws")),
                               column(4, selectInput("scatter3d_z", "z-axis",
                                                     choices = scatter3d_options,
                                                     selected = "Jumps"))
                               ),
                      plotlyOutput("scatterplot3d"),
                      tags$br(),
                      "Note: requires a browser with WebGL compatibility to run"),
             tabPanel("About",
                      tags$br(),
                      "The radar plot maps the relative performance of the athlete's average performance in the pre-defined categories against other athletes' average performance through standardisation. Post-transformation using 0-1 normalisation is applied to align the grid-rings.",
                      tags$br(),tags$br(),
                      "This can be notated by:",
                      withMathJax("$$\\frac{\\sum_{i}^a\\sum_{j}^b x_{ij}}{|a||b|}\\!$$'"),
             tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$']]}
            });
            </script >
            ")),
             ## tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]} remove the brackets otherwise it fucks up the formatting in the app globally
             paste0("Where $a$ = the set of the unique events in each category and $b$ = the set of the number of competitions the athlete has participated in. Thus $x_{ij}$ is simply the number of points the athlete received in the $i$th event of $j$th competition.")
             ## If we let Z represent the set of athletes featured in the dataset, and Y for one of our four categories 
             )
             )
             ),
           mainPanel(
             div(DT::dataTableOutput("individual_athlete_profile"), style = "font-size: 82.5%; width: 110%"),
                     "What to include here..",
             textOutput("athlete_df_idx"))
           )
           )
  , 
  tabPanel("About",
           fluidRow(column(width = 6,
           HTML(
             paste(
               h4("About the website"),
              "This website features data from all decathlons at the three major world events (Olympics, World Championships, and Gotzis Hypomeeting) since 2000. It allows you to view relevant visualisations, calculate common points conversion for events, view trends, see athlete profiles, and allows you to upload your own combined events dataset.<br/><br/>
              For best user experience use a browser with WebGL compatibility and a screen resolution of atleast 1920x1080.",
               h4("About me"),
               "I previously competed in the decathlon at an amateur level. I mainly created this Shiny App to showcase some of the skills I have in this feature of R for work training and my CV.<br/><br/>
              
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
           "- Thomas Park for the gorgeous Yeti CSS theme"),
           column(width = 6,
                  HTML(paste(h4("More info:"),
                             "For more general news about combined events you can visit:",
                             "<br/><br/>",
                             "-", tags$a(href = "http://decathlon2000.com", "decathlon2000"),
                             "<br/>",
                             "-", tags$a(href = "http://decathlonpedia.com", "decathlonpedia")))
           )
           )
  ),
  tabPanel("test",
           DT::dataTableOutput("foobar")),
  tabPanel("To do",
           "Update hand timed checkbox for table and plot",
           tags$br(),
           "Fix sidebar plots on datasets page",
           tags$br(),
           "Think of plot for athlete page",
           tags$br(),
           "Create customer user upload",
           tags$br(),
           "Convert 1500m to display mm:ss")
)