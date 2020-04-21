function(input, output) {
  
  ## For calculator ####


      ## 1)  Decathlon ####
        output$value_one <- renderText({ dec_100m(input$event_one) })
        output$value_two <- renderText({ dec_lj(input$event_two) })
        output$value_three <- renderText({ dec_sp(input$event_three) })
        output$value_four <- renderText({ dec_hj(input$event_four) })
        output$value_five <- renderText({ dec_400m(input$event_five) })
        output$value_six <- renderText({ dec_110mh(input$event_six) })
        output$value_seven <- renderText({ dec_dt(input$event_seven) })
        output$value_eight <- renderText({ dec_pv(input$event_eight) })
        output$value_nine <- renderText({ dec_jt(input$event_nine) })
        output$value_ten <- renderText({ dec_1500m(input$event_ten) })
        
        output$dec_table <- renderTable({
    decathlon_s2p(input$event_one,
                               input$event_two,
                               input$event_three,
                               input$event_four,
                               input$event_five,
                               input$event_six,
                               input$event_seven,
                               input$event_eight,
                               input$event_nine,
                               input$event_ten)
  })
        
        output$dec_plot <- renderPlot({
          decathlon_vis(
            input$event_one,
            input$event_two,
            input$event_three,
            input$event_four,
            input$event_five,
            input$event_six,
            input$event_seven,
            input$event_eight,
            input$event_nine,
            input$event_ten
          )
        })
        
        ## 2) Heptathlon ####
        
        output$valueh_one <- renderText({ hept_100mh(input$hept_one) })
        output$valueh_two <- renderText({ hept_hj(input$hept_two) })
        output$valueh_three <- renderText({ hept_sp(input$hept_three) })
        output$valueh_four <- renderText({ hept_200m(input$hept_four) })
        output$valueh_five <- renderText({ hept_lj(input$hept_five) })
        output$valueh_six <- renderText({ hept_jt(input$hept_six) })
        output$valueh_seven <- renderText({ hept_800m(input$hept_seven) })
        
        output$hept_table <- renderTable({
          heptathlon_s2p(input$hept_one,
                        input$hept_two,
                        input$hept_three,
                        input$hept_four,
                        input$hept_five,
                        input$hept_six,
                        input$hept_seven)
        })
        
        output$hept_plot <- renderPlot({
          heptathlon_vis(
            input$hept_one,
            input$hept_two,
            input$hept_three,
            input$hept_four,
            input$hept_five,
            input$hept_six,
            input$hept_seven
          )
        })      ## change to hept
        
        
        
        ## For tables ####
        
        display_dfs <- NULL

        datatablecreation <- function(event, inputid) {
          
## When it's not a run event I need to reverse the SliderInputId [1] and [2]          
          renderDataTable(
            DT::datatable({
              
                years <- input[[inputid]]
                
                df_removedate <- dfs_joined[[event]] %>% select(-Date)
                
                if (length(years) == 0) {
                  # reassign in the parent data frame!!!
                  display_dfs <<- df_removedate
                } else {
                  display_dfs <<- df_removedate[which(df_removedate$Year %in% years), ]
                }
                
                display_dfs
              },
              rownames = FALSE,
              container = df_grouped_container,
              class = 'cell-border stripe compact',
              options = list(fixedHeader = TRUE),
              extensions = "FixedHeader"
            ) %>% 
              formatString(~ `100m_Score` + `400m_Score` + `110mh_Score` + `1500m_Score`, suffix = "s") %>%
              formatString(~ LJ_Score + SP_Score + HJ_Score + DT_Score + PV_Score + JT_Score, suffix = "m")
          )
        }
        
        output$ex1 <- datatablecreation("olympics", "year_olympics")
        output$ex2 <- datatablecreation("world_championships", "year_wc")
        output$ex3 <- datatablecreation("gotzis", "year_gotzis")
        
        ## Visualisations ####
        
        barplotcreation <- function(event, event_text, exnum_rows_selected) {renderPlotly({
          
          hasClick <- input[[exnum_rows_selected]]
          
          if (is.null(hasClick)) return(NULL) ## shows blank plot otherwise
          
          temp_shiny <- display_dfs[hasClick, ] %>%
            gather(key = "event", value = "points", `100m`:`1500m`) %>%
            mutate_at(.vars = "event", .funs = as_factor)
          
          if (length(hasClick) == 1) {
          
            
            foobar <- temp_shiny %>% 
            ggplot(aes(event, points)) + 
            geom_bar(stat = "identity", alpha = 0.75) +
            scale_y_continuous(limits = c(0,1100),
                               breaks = seq(0, 1200, 200)) +
            labs(x = "Event",
                 y = "Points",
                 title = paste0(unique(temp_shiny$Year), " ", event_text, ", ", unique(temp_shiny$Athlete)),
                 subtitle = paste0(unique(temp_shiny$Country), "\n", "Final rank: ", unique(temp_shiny$Rank))
            )
            
            ggplotly(foobar)
            
            } else {
              temp_shiny$Athlete <- paste0(temp_shiny$Year, " | ", temp_shiny$Rank, " | ", temp_shiny$Country, " | ", temp_shiny$Athlete)
              
              foobar <- temp_shiny %>%
                ggplot(aes(event, points, fill = Athlete)) +
                geom_bar(stat = "identity", alpha = 0.66, colour = "black", size = 0.25, position = "dodge") +
                scale_y_continuous(limits = c(0, 1100),
                                   breaks = seq(0, 1200, 200),
                                   expand = c(0,0)) +
                scale_fill_brewer(name = "Year | Rank | Country | Athlete",
                                  palette = "Set1") +
                labs(
                  x = "Event",
                  y = "Points",
                  title = event_text) +
                coord_flip()
              
              ggplotly(foobar)
              
              
            } ## selecting 2+ rows
          
          
          
        })}
        
        
        output$bar_olympics <- barplotcreation("olympics", "Olympics", "ex1_rows_selected")
        output$bar_wc <- barplotcreation("world_championships", "World Championships", "ex2_rows_selected")
        output$bar_gotzis <- barplotcreation("gotzis", "Gotzis Hypomeeting", "ex3_rows_selected")
        
        output$decathlon_heatplot_points <- renderPlot({
          
          choose <- switch(input$decathlon_tile_choose,
                         score = "mean_score",
                         points = "mean_points")
          
          tile_function(choose)
          })
        
  ## Upload custom Files ####
        
        output$contents <- renderTable({
          
          # input$file1 will be NULL initially. After the user selects and uploads a 
          # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
          # columns. The 'datapath' column will contain the local filenames where the 
          # data can be found.
          
          inFile <- input$file1
          
          if (is.null(inFile)) return(NULL)
          
          read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
        })
      
        # output$wewew <- renderPrint(input$filter_100m)
        
## Athlete Profile ####
        
        for_indiv_athlete_tab <-
          bind_rows(dfs_joined,
                    .id = "Major Event")
        for_indiv_athlete_tab$`Major Event` <-
          as_factor(for_indiv_athlete_tab$`Major Event`)
        for_indiv_athlete_tab$`Major Event` <- fct_recode(
          for_indiv_athlete_tab$`Major Event`,
          "Olympics" = "olympics",
          "World Championships" = "world_championships",
          "G\u00f6tzis" = "gotzis"
        )
        
        # for_indiv_athlete_tab <- as.data.frame(for_indiv_athlete_tab)
        output$individual_athlete_profile <-
          renderDataTable(
            {
              df <- within(for_indiv_athlete_tab[which(for_indiv_athlete_tab$Athlete == input$athlete_select), ], rm(Athlete, Country, Year)) %>% arrange(Date) ## remove unneeded columns
              
              DT::datatable(df,
                            class = 'cell-border compact',
                            rownames = FALSE,
                            options = list(pageLength = 25, scrollX = TRUE),
                            container = htmltools::withTags(table(
                              thead(
                                tr(
                                  th(rowspan = 2, 'Major Event'),
                                  th(rowspan = 2, 'Date'),
                                  th(rowspan = 2, 'Age at Comp'),
                                  th(rowspan = 2, 'Rank'),
                                  th(rowspan = 2, 'Final Score'),
                                  th(colspan = 2, '100m'),
                                  th(colspan = 2, 'LJ'),
                                  th(colspan = 2, 'SP'),
                                  th(colspan = 2, 'HJ'),
                                  th(colspan = 2, '400m'),
                                  th(colspan = 2, '110mh'),
                                  th(colspan = 2, 'DT'),
                                  th(colspan = 2, 'PV'),
                                  th(colspan = 2, 'JT'),
                                  th(colspan = 2, '1500m'),
                                ),
                                tr(
                                  lapply(rep(c('Score', 'Points'), 10), th)
                                )
                              )
                            ))) %>% 
                formatString(~ `100m_Score` + `400m_Score` + `110mh_Score` + `1500m_Score`, suffix = "s") %>%
                formatString(~ LJ_Score + SP_Score + HJ_Score + DT_Score + PV_Score + JT_Score, suffix = "m")
              
              
          }
          )
        
        
        # output$individual_athlete_plot <- renderPlot({
        #   foobar <-
        #     for_indiv_athlete_tab[which(for_indiv_athlete_tab$Athlete == input$athlete_select), ]
        # 
        #   foobar %<>% pivot_longer(`Final Score`:`1500m`, "Event", values_to = "Score") %>% unite("Major Event", c(`Major Event`, Year), sep = " ") %>% arrange(Date)
        #   foobar$`Major Event` %<>% as_factor()
        # 
        #   this_plot <- foobar %>% ggplot(aes(`Major Event`, Score, group = Event, fill = `Major Event`)) +
        #     geom_bar(stat = "identity", alpha = 2 / 3) +
        #     facet_wrap("Event",
        #                nrow = 2,
        #                ncol = 6,
        #                scales = "free") +
        #     theme(axis.text.x = element_text(
        #       angle = 45,
        #       hjust = 1,
        #       vjust = 1
        #     )) +
        #     scale_fill_brewer(palette = "Set1")
        # 
        #   return(this_plot)
        # })
        
        ## Scrapping ####
        
        output$use_this_athletename <- renderText({c('<img src="',athlete_info[[which(athlete_info$Athlete == input$athlete_select),"image_url"]],'">')})
        
        ## Birth data ####
        
        output$athlete_birth <- renderText(as.character(athlete_info[[which(athlete_info$Athlete == input$athlete_select),"birth_date"]]))
        output$iaaf_code <- renderText(athlete_info[[which(athlete_info$Athlete == input$athlete_select),"iaaf_code"]])
        output$athlete_specific <- renderText(input$athlete_select)
        output$athlete_country <- renderText(athlete_info[[which(athlete_info$Athlete == input$athlete_select),"Country"]])

## S2P functions ####

output$plotly_dec100m <- renderPlotly(
  {
    eventpointsfull_func(
      plotpointnumber = "X100m",
      eventtype = "runs",
      nudgeathlete = -0.5,
      nudgescore = -150,
      alpha = 25.4347,
      beta = 18,
      delta = 1.81,
      lowerx = 9,
      upperx = 12,
      by_x = -1,
      event_and_measurement = "100m time (seconds)",
      short_measure = "Time"
    )
  }
)
        
output$plotly_declj <- renderPlotly(
  {
    eventpointsfull_func(
      plotpointnumber = "LJ",
      eventtype = "jumps",
      nudgeathlete = -2,
      nudgescore = -200,
      alpha = 0.14354,
      beta = 220,
      delta = 1.4,
      lowerx = 5,
      upperx = 9,
      by_x = 0.5,
      event_and_measurement = "Long Jump distance (m)",
      opt_minorbreaks = seq(2, 9, 0.1),
      short_measure = "Distance"
    )
  }
)

output$plotly_decsp <- renderPlotly(
  {
    eventpointsfull_func(
      plotpointnumber = "SP",
      eventtype = "throws",
      nudgeathlete = -2,
      nudgescore = -200,
      alpha = 51.39,
      beta = 1.5,
      delta = 1.05,
      lowerx = 10,
      upperx = 25,
      by_x = 2,
      event_and_measurement = "Shot put distance (m)",
      short_measure = "Distance"
    )
  }
)

output$plotly_dechj <- renderPlotly(
  {
    eventpointsfull_func(
      plotpointnumber = "HJ",
      eventtype = "jumps",
      nudgeathlete = -25,
      nudgescore = -200,
      alpha = 0.8465,
      beta = 75,
      delta = 1.42 ,
      lowerx = 1.7,
      upperx = 2.6,
      by_x = 0.1,
      event_and_measurement = "High Jump height (m)",
      short_measure = "Distance"
    )
  }
)

output$plotly_dec400m <- renderPlotly(
  {
    eventpointsfull_func(
      plotpointnumber = "X400m",
      eventtype = "runs",
      nudgeathlete = -5,
      nudgescore = -200,
      alpha = 1.53775,
      beta = 82,
      delta = 1.81,
      lowerx = 40,
      upperx = 56,
      by_x = -2,
      event_and_measurement = "400m time (seconds)",
      short_measure = "Time"
    )
  }
)

output$plotly_dec110mh <- renderPlotly(
  {
    eventpointsfull_func(
      plotpointnumber = "X110mh",
      eventtype = "runs",
      nudgeathlete = -2.5,
      nudgescore = -200,
      alpha = 5.74352,
      beta = 28.5,
      delta = 1.92,
      lowerx = 12,
      upperx = 18,
      by_x = -1,
      event_and_measurement = "110m hurdles time (m)",
      short_measure = "Time"
    )
  }
)

output$plotly_decdt <- renderPlotly(
  {
    eventpointsfull_func(
      plotpointnumber = "DT",
      eventtype = "throws",
      nudgeathlete = -10,
      nudgescore = -200,
      alpha = 12.91,
      beta = 4,
      delta = 1.1 ,
      lowerx = 25,
      upperx = 80,
      by_x = 5,
      event_and_measurement = "Discus throw distance (m)",
      short_measure = "Distance"
    )
  }
)

output$plotly_decpv <- renderPlotly(
  {
    eventpointsfull_func(
      plotpointnumber = "PV",
      eventtype = "jumps",
      nudgeathlete = -50,
      nudgescore = -200,
      alpha = 0.2797,
      beta = 100,
      delta = 1.35,
      lowerx = 3.6,
      upperx = 6.5,
      by_x = 0.2,
      event_and_measurement = "Pole vault height (m)",
      short_measure = "Height"
    )
  }
)

output$plotly_decjt <- renderPlotly(
  {
    eventpointsfull_func(
      plotpointnumber = "JT",
      eventtype = "throws",
      nudgeathlete = -25,
      nudgescore = -200,
      alpha = 10.14,
      beta = 7,
      delta = 1.08,
      lowerx = 40,
      upperx = 100,
      by_x = 10,
      event_and_measurement = "Javelin throw distance (m)",
      short_measure = "Distance"
    )
  }
)

output$plotly_dec1500m <- renderPlotly(
  {
    eventpointsfull_func(
      plotpointnumber = "X1500m",
      eventtype = "runs",
      nudgeathlete = -50,
      nudgescore = -200,
      alpha = 0.03768,
      beta = 480,
      delta = 1.85,
      lowerx = 180,
      upperx = 345,
      by_x = -15,
      event_and_measurement = "1500 time (m:ss)",
      short_measure = "Time",
      opt_label = function(x)
        seconds_to_period(x) %>% gsub(pattern = "M ", replacement = ":", .) %>% gsub(pattern = "5S", replacement = "5", .) %>% gsub("30S", "30", .) %>% gsub("0S", "00", .)
    )
  }
)

## Radar ####

output$radar_athlete <- renderPlot(radar_function(input$athlete_select))

## https://stackoverflow.com/questions/47702624/shiny-unwanted-space-added-by-plotoutput-and-or-renderplot this might be relevant to get rid of the whitespace

output$radar_speed <- renderText(radar_colourbar_func(input$athlete_select, "avg_speed__standardised__normalised"))
output$radar_throws <- renderText(radar_colourbar_func(input$athlete_select, "avg_throws__standardised__normalised"))
output$radar_jumps <- renderText(radar_colourbar_func(input$athlete_select, "avg_jumps__standardised__normalised"))
output$radar_endurance <- renderText(radar_colourbar_func(input$athlete_select, "avg_endurance__standardised__normalised"))

## 3d scatter ####

output$scatterplot3d <- renderPlotly({
  
  df_scatter <- df_radar %>% 
    mutate(select_athlete = case_when(Athlete == input$athlete_select ~ "Select", TRUE ~ "No")) %>% 
    select(Athlete, select_athlete, everything()) ## maybe make this reactive if necessary
  
  x_choose <- switch(input$scatter3d_x,
                     Speed = "avg_speed__standardised__normalised",
                     Throws = "avg_throws__standardised__normalised",
                     Jumps = "avg_jumps__standardised__normalised",
                     Endurance = "avg_endurance__standardised__normalised")
  
  y_choose <- switch(input$scatter3d_y,
                     Speed = "avg_speed__standardised__normalised",
                     Throws = "avg_throws__standardised__normalised",
                     Jumps = "avg_jumps__standardised__normalised",
                     Endurance = "avg_endurance__standardised__normalised")
  
  z_choose <- switch(input$scatter3d_z,
                     Speed = "avg_speed__standardised__normalised",
                     Throws = "avg_throws__standardised__normalised",
                     Jumps = "avg_jumps__standardised__normalised",
                     Endurance = "avg_endurance__standardised__normalised")  
  
  plot_ly(df_scatter %>% group_by(select_athlete), 
             x = ~get(x_choose), 
             y = ~get(y_choose),
             z = ~get(z_choose),
             color = ~select_athlete,
             colors = c('#757575', '#0C4B8E'),
             text = ~ paste(Athlete),
             alpha = 0.66,
             hoverinfo = 'text',
             showlegend = F) %>% 
  layout(scene = list(xaxis = list(title = input$scatter3d_x),
                      yaxis = list(title = input$scatter3d_y),
                      zaxis = list(title = input$scatter3d_z)
                      )
         )
  }
  )


## Closing server brackets ####

}