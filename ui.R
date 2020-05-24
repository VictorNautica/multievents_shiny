navbarPage(
  # theme = shinythemes::shinytheme("sandstone"),
  title = "Multi-events repository",
  tabPanel(
    ## 1) Calculator ####
    "Calculator",
    navbarPage(
      title = "",
      ## ++++ Dec ####
      tabPanel("Decathlon",
    sidebarLayout(sidebarPanel(width = 3,
      fluidRow(
        fluidRow(column(5, smallInput(numericInput("event_one", "100m (s):", 10.87, min = 0, max = 100, step = 0.1, width = "80%"))),
                 column(4, checkboxInput("handtime_100m", "Hand timed", value = F), style='padding-top:19px;'),
                 padding("value_one"), style="padding:-2px;margin-top:-2px;"
      )),
      fluidRow(
        fluidRow(column(9, smallInput(numericInput("event_two", "Long jump (m):", 7.6, min = 0, max = 100, step = 0.1, width = "41%"))),
                 padding("value_two")
        )),
      fluidRow(
        fluidRow(column(9, smallInput(numericInput("event_three", "Shot put (m):", 14, min = 0, max = 100, step = 0.1, width = "41%"))),
                 padding("value_three")
        )),
      fluidRow(
        fluidRow(column(9, smallInput(numericInput("event_four", "High jump (m):", 2.00, min = 0, max = 100, step = 0.03, width = "41%"))),
                 padding("value_four")
        )),
      fluidRow(
        fluidRow(column(5, smallInput(numericInput("event_five", "400m (s)", 50, min = 0, max = 100, step = 0.2, width = "80%"))),
                 column(4, checkboxInput("handtime_400m", "Hand timed", value = F), style='padding-top:19px;'),
                 padding("value_five")
        )),
      fluidRow(
        fluidRow(column(5, smallInput(numericInput("event_six", "110m hurdles (s):", 16, min = 0, max = 100, step = 0.1, width = "80%"))),
                 column(4, checkboxInput("handtime_110mh", "Hand timed", value = F), style='padding-top:19px;'),
                 padding("value_six")
        )),
      fluidRow(
        fluidRow(column(9, smallInput(numericInput("event_seven", "Discus throw (m):", 42, min = 0, max = 100, step = 0.5, width = "41%"))),
                 padding("value_seven")
        )),
      fluidRow(
        fluidRow(column(9, smallInput(numericInput("event_eight", "Pole vault (m):", 4.70, min = 0, max = 100, step = 0.1, width = "41%"))),
                 padding("value_eight")
        )),
      fluidRow(
        fluidRow(column(9, smallInput(numericInput("event_nine", "Javelin throw (m):", 62, min = 0, max = 100, step = 1, width = "41%"))),
                 padding("value_nine")
        )),
      fluidRow(fluidRow(column(3, smallInput(
        numericInput(
          "event_ten_minutes",
          "1500m (m):",
          5,
          min = 0,
          max = 99,
          step = 1
        )
      )),
      column(3, smallInput(
        numericInput(
          "event_ten_seconds",
          "(s):",
          0,
          min = 0,
          max = 59.99,
          step = 0.1
        )
      )),
      column(3),
      padding("value_ten")))
    ), 
    mainPanel(
      ## ++++ Main Panel ####
      fluidRow(width = 9,
                      column(width = 1),
               column(width = 11, tableOutput("dec_table"),
                      plotOutput("dec_plot", height = "400px", width = "860px")))
      ))
    ),
    ## ++++ Hept ####
    tabPanel("Heptathlon",
             sidebarLayout(sidebarPanel(width = 3,
                                        fluidRow(
                                          fluidRow(column(5, smallInput(numericInput("hept_one", "100m hurdles (s):", 13.36, min = 0, step = 0.1, width = "80%"))),
                                                   column(4, checkboxInput("handtime_100mh", "Hand timed", value = F), style='padding-top:19px;'),
                                                   padding("valueh_one")
                                          )),
                                        fluidRow(
                                          fluidRow(column(9, smallInput(numericInput("hept_two", "High jump (m):", 1.77, min = 0, step = 0.3, width = "41%"))),
                                                   padding("valueh_two")
                                          )),
                                        fluidRow(
                                          fluidRow(column(9, smallInput(numericInput("hept_three", "Shot put (m):", 14.02, min = 0, max = 100, step = 0.1, width = "41%"))),
                                                   padding("valueh_three")
                                          )),
                                        fluidRow(
                                          fluidRow(column(5, smallInput(numericInput("hept_four", "200m (s):",  	24.29, min = 0, max = 100, step = 0.1, width = "80%"))),
                                                   column(4, checkboxInput("handtime_200m", "Hand timed", value = F), style='padding-top:19px;'),
                                                   padding("valueh_four")
                                          )),
                                        fluidRow(
                                          fluidRow(column(9, smallInput(numericInput("hept_five", "Long jump (m):", 6.28, min = 0, max = 100, step = 0.1, width = "41%"))),
                                                   padding("valueh_five")
                                          )),
                                        fluidRow(
                                          fluidRow(column(9, smallInput(numericInput("hept_six", "Javelin throw (m):", 46.74, min = 0, step = 0.1, width = "41%"))),
                                                   padding("valueh_six")
                                          )),
                                        fluidRow(fluidRow(column(3, smallInput(
                                          numericInput(
                                            "hept_seven_minutes",
                                            "800m (m):",
                                            2,
                                            min = 0,
                                            max = 99,
                                            step = 1
                                          )
                                        )),
                                        column(3, smallInput(
                                          numericInput(
                                            "hept_seven_seconds",
                                            "(s):",
                                            0,
                                            min = 0,
                                            max = 59.99,
                                            step = 0.1
                                          )
                                        )),
                                        column(3),
                                        padding("valueh_seven"))),
                                        ),
                           ## ++++ Main Panel ####
                           mainPanel(
                             width = 9,
                             fluidRow(
                               column(1),
                               column(11,
                                      tableOutput("hept_table"),
                                      plotOutput("hept_plot", height = "400px", width = "860px")))
                           ))
  )
  )
  ),
  tabPanel(
  ## 2) Previous events data ####  
    "Previous events data",
    navbarPage(
      title = "",
      ## ++++ Dec ####
    tabPanel("Decathlon", 
    navbarPage(
      title = 'Select competition',
      decathlon_tabs(
        tab_label = "Olympics",
        ex_id = "ex1",
        select_year_label = "year_olympics",
        dfs_proper_call = "olympics",
        plotoutputlabel = "bar_olympics_dynamic"
      ),
      decathlon_tabs(
        tab_label = "World Championships",
        ex_id = "ex2",
        select_year_label = "year_wc",
        dfs_proper_call = "world_championships",
        plotoutputlabel = "bar_wc_dynamic"
      ),
      decathlon_tabs(
        tab_label = "Gotzis Hypomeeting",
        ex_id = "ex3",
        select_year_label = "year_gotzis",
        dfs_proper_call = "gotzis",
        plotoutputlabel = "bar_gotzis_dynamic"
      )
    )),
    ## ++++ Hept ####
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
                                    plotlyOutput("OFSP_plot_decathlon", height = "800px")),
                           tabPanel("Heptathlon (coming soon)"))),
             tabPanel("Average Points",
                      tabsetPanel(tabPanel("Decathlon",
                                  plotlyOutput("full_average_decathlon", height = "800px")),
                      tabPanel("Heptathlon (coming soon"))
             )
           )),   
  ## 5) Custom Data ####
  tabPanel("Custom Data (coming soon)", 
           titlePanel(
             h2("Upload completed multievents competition", 
             style = "padding-top: 0px; margin-top: -25px;margin-bottom: 20px")), 
           
           sidebarLayout(
             
             sidebarPanel(width = 2,
               
               fileInput("file1", "Upload CSV File",
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               tags$hr(),
               selectInput(
                 "custom_competition_select",
                 "Select competition:",
                 c("Decathlon", "Heptathlon")
               ),
               tags$hr(),
               tags$b("Submit data:"),
               actionButton("goButton", "Submit", width = "250px"),
               tags$hr(),
               selectInput(
                 "custom_datasetview_select",
                 "Select dataset view:",
                 c("Score", "Points", "Cumulative Points", "Rank", "Average", "Standardised")
               ),
               tags$hr(),
               selectInput(
                 "custom_athlete_select",
                 "Athlete Name:",
                 ""
               ),
               tags$hr(),
               downloadButton("download_custom_athlete",
                              label = paste0(stringi::stri_dup(intToUtf8(160), 2),"Download Indiv. Athlete Profile")
               )
               
             ),
             
             mainPanel(
               width = 10,
               tabsetPanel(
                 ## ++++ Instructions ####
                 tabPanel(
                   "Instructions",
                   tags$br(),
                   "This tab allows you to upload your own completed multi events dataset and generate useful graphics and summary statistics. At the moment, it only supports functionality for the decathlon.",
                   tags$br(),
                   tags$br(),
                   tags$b("Step 1)"),
                   "Upload a .csv file in the following format:",
                   tags$br(),
                   tags$br(),
                   div(dataTableOutput("exampledf"), style = "height:300px; width:1500px; font-size: 80%; overflow-y: scroll"),
                   tags$br(),
                   
                   "The primary requirement for this program to function is that your dataset has to have a column denoting the athlete's name, and then columns for points the athletes have scored in in sequential order of the decathlon/heptathlon. Jumps and throws should be measured in the metric system. 1500m times can be recorded either in the format mm:ss or purely as seconds. Ranking order, points conversion, or other miscellaneous columns are not required.",
                   tags$br(),
                   tags$br(),
                   "Events with no marks should be labelled as ",
                   tags$b("NMR", .noWS	= "after") ,
                   ", events where the athlete did not start with ",
                   tags$b("DNS"),
                   "and should be the final entry in the respective athlete's series. If there are blank entries before the end of the competition or before a ",
                   tags$b("DNS", .noWS	= "after"),
                   ", for example in Deo Milandu's and Thomas Grantham's High Jump entry, this will be considered as a ",
                   tags$b("NMR"),
                   " for jumps/throws events and ",
                   tags$b("DNF"),
                   " for running events.",
                   tags$br(),
                   tags$br(),
                   tags$b("Step 2"), " (optional)", tags$b(")", .noWS = "before"),
                   "In the ",
                   tags$em("Your dataset"),
                   " tab, you can make ad hoc edits to cells if there are inaccuracies.",
                   tags$br(), tags$br(),
                   tags$b("Step 3)"),
                   "When you're happy with the dataset, click the submit button to send the data to the server to generate the plots."
                 ),
                 ## ++++ User Dataset ####
                 tabPanel(
                   "Your dataset",
                   tags$br(),
                   div(dataTableOutput("users_dataset", width = "1500"), style = "font-size: 80%; width: 70%")
                 ),
                 tabPanel(
                   "Your dataset (calculated)",
                   tags$br(),
                   div(dataTableOutput("users_dataset_calculated", width = "1500"), style = "font-size: 80%; width: 70%")
                 ),
                 # Bumps Plot ####
                 tabPanel(
                   "Bumps Plot (Ranking)",
                   plotOutput(
                     "example_athlete_bumpplot",
                     width = "1525",
                     height = "725"
                   )
                 ),
                 tabPanel(
                   "Tile Plot (Ranking)",
                   plotOutput(
                     "example_athete_ranktileplot",
                     width = "1525",
                     height = "725"
                   )
                 ),
                 tabPanel(
                   "Average Points",
                   plotOutput(
                     "example_athlete_avg_points",
                     width = "1525",
                     height = "725"
                   )
                 ),
                 tabPanel("Points Plot",
                          plotlyOutput("example_user_points_boxplot",
                                       width = "1525",
                                       height = "725")),
                 tabPanel("Cumulative Points Plot",
                          plotlyOutput("example_user_cum_points_boxplot",
                                       width = "1525",
                                       height = "725")),
                 tabPanel("Individual Athlete (coming soon)",
                          fluidRow(column(width = 6, tableOutput("custom_dec_table")),
                          column(width = 6, plotOutput("custom_dec_plot"))),
                          fluidRow(column(width = 6, plotOutput("custom_rank_tile")))
                          ),
                 tabPanel("About Plots")
               )
             )
             
           )),
  ## 6) Athlete Profile ####
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
                 column(6, htmlOutput("height", inline = T), "cm")),
                 
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
                      withMathJax("$$\\frac{\\sum_{i}^a\\sum_{j}^b x_{ij}}{|a||b|}\\!$$"),
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
             ## ++++ Main Panel ####
           mainPanel(
             div(DT::dataTableOutput("individual_athlete_profile"), 
                 style = "font-size: 82.5%; width: 110%"),
             tags$br(),
             tabsetPanel(tabPanel("Avg Cumulative Plot", plotlyOutput("athlete_cumulative_plot", height = "400px")),
                         tabPanel("About", 
                                  tags$br(),
                                  "This plot shows the change in the cumulative average of the athlete's points over the selected competition(s)")
                         )
             )
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
               "I previously competed in the decathlon at an amateur level. I mainly created this Shiny App to showcase some of the skills I have in this feature of R for work training and my portfolio.<br/><br/>
              
              If you have any queries or spot any inaccuracies in the data, please contact me at victoryu@sent.com"
             )
           ),
           h4("Support"),
           "If you like the app, feel free to give a small donation through the Paypal button below to support future development/my hosting costs. You can also support the site by sharing it among those interested in combined events.",
           HTML(paste('<br/><br/><form action="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=DV63U2B9R7FE6&source=url" method="post" target="_top">
<input type="hidden" name="cmd" value="_s-xclick" />
<input type="hidden" name="hosted_button_id" value="DV63U2B9R7FE6" />
<input type="image" src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" border="0" name="submit" title="PayPal - The safer, easier way to pay online!" alt="Donate with PayPal button" />
<img alt="" border="0" src="https://www.paypal.com/en_GB/i/scr/pixel.gif" width="1" height="1" />
</form>
')),
           HTML("<br/>"),
           h4("Credits"),
           HTML("<br/>"),
           "- decathlon2000.com for providing the scores/points data",
           HTML("<br/>"),
           "- Stack Overflow for coding Q&A",
           HTML("<br/>"),
           "- Tom Jemmett for helping with some initial bugs on the dataset view tab",
           HTML("<br/>"),
           "- Thomas Park for the Yeti CSS theme"),
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
  ## 7) Testing ####
  tabPanel("test",
           "Ignore this page",
           DT::dataTableOutput("foobar"),
           textOutput("text_test"),
           textOutput("athlete_df_idx"),
           verbatimTextOutput("print"),
           verbatimTextOutput("editabletest"),
           plotOutput("first_test")),
  tabPanel("To do",
           "Update hand timed checkbox for table and plot - DONE",
           tags$br(),
           "Fix sidebar plots on datasets page - DONE",
           tags$br(),
           "Think of plot for athlete page - DONE",
           tags$br(),
           "Create customer user upload - DONE",
           tags$br(),
           "Convert 1500m to display mm:ss - DONE",
           tags$br(),
           "Add average points flow to calculator plot - DONE",
           tags$br(),
           "Finish custom user upload, points dot plot (stuff in notebook)",
           tags$br(),
           "Avg points in calculator - DONE",
           tags$br(),
           "Change previous events data plot back to ggplot - DONE",
           tags$br(),
           "Constrain width of pictures in athlete profile - DONE",
           tags$br(),
           "Remove faded lines for avg points, move to data visualisations - DONE",
           tags$br(),
           "Look at plotly options for 3d scatter plot",
           tags$br(),
           "Left align data tables in custom data - DONE",
           tags$br(),
           "Add hand times to heptathlon calculator - DONE",
           tags$br(),
           "Check plotly radar function",
           tags$br(),
           "Update limits in decathlon/hetathlon_viz"
           )
)