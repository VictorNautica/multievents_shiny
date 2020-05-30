function(input, output, session) {
  
  ## For calculator ####


      ## 1)  Decathlon ####
  
## ++++ Raw Calculations ####
  
  custom_dec_100m <- reactive({
    if (input$handtime_100m == T) {
    req(input$event_one) + 0.24
  } else {
    req(input$event_one)
  }
  })
  custom_dec_400m <- reactive({if (input$handtime_400m == T) {
    req(input$event_five) + 0.14
  } else {
    req(input$event_five)
  }
  })
  custom_dec_110mh <- reactive({if (input$handtime_110mh == T) {
    req(input$event_six) + 0.24
  } else {
    req(input$event_six)
  }
  })
  
  custom_dec_lj <- reactive(
    req(input$event_two)
  )
  custom_dec_sp <- reactive(
    req(input$event_three)
  )
  custom_dec_hj <- reactive(
    req(input$event_four)
  )
  custom_dec_dt <- reactive(
    req(input$event_seven)
  )
  custom_dec_pv <- reactive(
    req(input$event_eight)
  )
  custom_dec_jt <- reactive(
    req(input$event_nine)
  )
  
  custom_dec_1500m <- reactive(
  (req(input$event_ten_minutes)*60)+req(input$event_ten_seconds)
  )
  
  ## ++++ Raw Score ####
  output$value_one <-
    renderText({
      dec_100m(custom_dec_100m())
    })
  output$value_five <-
    renderText({
      dec_400m(custom_dec_400m())
    })
  output$value_six <-
    renderText({
      dec_110mh(custom_dec_110mh())
    })
  
  output$value_two <- renderText({
    dec_lj(custom_dec_lj())
  })
  output$value_three <-
    renderText({
      dec_sp(custom_dec_sp())
    })
  output$value_four <-
    renderText({
      dec_hj(custom_dec_hj())
    })
  output$value_seven <-
    renderText({
      dec_dt(custom_dec_dt())
    })
  output$value_eight <-
    renderText({
      dec_pv(custom_dec_pv())
    })
  output$value_nine <-
    renderText({
      dec_jt(custom_dec_jt())
    })
  output$value_ten <-
    renderText({
      dec_1500m(custom_dec_1500m())
    })
      
  
  ## ++++ Table ####  
        output$dec_table <- renderTable({
          decathlon_s2p(
            custom_dec_100m(),
            custom_dec_lj(),
            custom_dec_sp(),
            custom_dec_hj(),
            custom_dec_400m(),
            custom_dec_110mh(),
            custom_dec_dt(),
            custom_dec_pv(),
            custom_dec_jt(),
            custom_dec_1500m()
          )
  }, spacing = 'xs',
  bordered = TRUE)
  
  ## ++++ Plot ####
        
        output$dec_plot <- renderPlot({
          decathlon_vis(
            custom_dec_100m(),
            custom_dec_lj(),
            custom_dec_sp(),
            custom_dec_hj(),
            custom_dec_400m(),
            custom_dec_110mh(),
            custom_dec_dt(),
            custom_dec_pv(),
            custom_dec_jt(),
            custom_dec_1500m()
          )
        })
        
        ## 2) Heptathlon ####
  
  ## ++++ Raw Calculations ####
        
        custom_100mh <- reactive({if (input$handtime_100mh == T) {
            input$hept_one + 0.24
          } else {
            input$hept_one
          }
        })
        custom_200m <- reactive({if (input$handtime_200m == T) {
          input$hept_four + 0.24
        } else {
          input$hept_four
        }
        })
        
        custom_hept_hj <- reactive(
          req(input$hept_two)
        )
        custom_hept_sp <- reactive(
          req(input$hept_three)
        )
        custom_hept_lj <- reactive(
          req(input$hept_five)
        )
        custom_hept_jt <- reactive(
          req(input$hept_six)
        )
        
        custom_hept_800m <- reactive(
          (req(input$hept_seven_minutes)*60)+req(input$hept_seven_seconds)
        )
        
  ## ++++ Raw Score ####

        
        output$valueh_one <- renderText({ hept_100mh(custom_100mh()) })
        output$valueh_two <- renderText({ hept_hj(custom_hept_hj()) })
        output$valueh_three <- renderText({ hept_sp(custom_hept_sp()) })
        output$valueh_four <- renderText({ hept_200m(custom_200m()) })
        output$valueh_five <- renderText({ hept_lj(custom_hept_lj()) })
        output$valueh_six <- renderText({ hept_jt(custom_hept_jt()) })
        output$valueh_seven <- renderText({ hept_800m(custom_hept_800m()) })
        
        
      ## ++++ Table ####
        output$hept_table <- renderTable({
          heptathlon_s2p(custom_100mh(),
                         custom_hept_hj(),
                         custom_hept_sp(),
                        custom_200m(),
                        custom_hept_lj(),
                        custom_hept_jt(),
                        custom_hept_800m())
        },  spacing = 'xs',
        bordered = TRUE)
        
        ## ++++ Plot ####
        
        output$hept_plot <- renderPlot({
          heptathlon_vis(
            custom_100mh(),
            custom_hept_hj(),
            custom_hept_sp(),
            custom_200m(),
            custom_hept_lj(),
            custom_hept_jt(),
            custom_hept_800m()
          )
        })      ## change to hept
        
        
        
        ## For tables ####
        
        display_dfs <- NULL

        datatablecreation <- function(event, inputid) {
          
## When it's not a run event I need to reverse the SliderInputId [1] and [2]          
          renderDataTable(
            DT::datatable({
              
                years <- input[[inputid]]
                
                df_removedate <- dfs_joined[[event]] %>% select(-Date, -"1500m_Score")
                
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
              formatString(~ `100m_Score` + `400m_Score` + `110mh_Score` + `1500m_ScoreMMSS`, suffix = "s") %>%
              formatString(~ LJ_Score + SP_Score + HJ_Score + DT_Score + PV_Score + JT_Score, suffix = "m")
          )
        }
        
        output$ex1 <- datatablecreation("olympics", "year_olympics")
        output$ex2 <- datatablecreation("world_championships", "year_wc")
        output$ex3 <- datatablecreation("gotzis", "year_gotzis")
        
        ## Visualisations ####
        
        barplotcreation <-
          function(event_name_in_df, exnum_rows_selected) {
            renderPlot({
              hasClick <- input[[exnum_rows_selected]]
              
              if (is.null(hasClick)) {
                
                df_selected <- ultimate_df_standardised_global[[input$previous_events_radiobutton]] %>% ggplot(aes(event)) +
                  {if (input$previous_events_radiobutton == "Z-Score")
                  geom_hline(yintercept = 0, linetype = "dotdash") } +
                    {
                      if (input$previous_events_radiobutton == "Z-Score")
                        labs(y = "z-score")
                      else if (input$previous_events_radiobutton == "Points")
                        labs(y = "Points")
                    } +
                  geom_linerange(aes(ymin = min, ymax = max)) +
                    {
                      if (input$previous_events_radiobutton == "Z-Score")
                        scale_y_continuous(breaks = seq(-5, 4, 1))
                      else if (input$previous_events_radiobutton == "Points")
                        scale_y_continuous(breaks = seq(
                          plyr::round_any(
                          min(ultimate_df_standardised_global[["Points"]]$min[1:10])
                          ,
                          100,
                          f = floor
                        ),
                        round(
                          max(ultimate_df_standardised_global[["Points"]]$max[1:10]), -2
                        ),
                        100))
                    } +
                  theme(axis.title.x = element_blank())
                
                return(df_selected)
                
                
              }
              
              if (length(hasClick) >= 1 & length(hasClick) < 11) {
                
                df <- ultimate_df_standardised_global[[input$previous_events_radiobutton]] %>% filter(comp == event_name_in_df)
                
                athlete_names <- display_dfs[hasClick, "Athlete"]
                years <- display_dfs[hasClick, "Year"]
                
                df <- map2_dfr(athlete_names, years, ~ df %>% filter(Athlete == .x, Year == .y))
                
                plot <-
                  df %>%
                  filter(comp == event_name_in_df, Athlete %in% athlete_names) %>%
                  ggplot(aes(event, score, group = Athlete)) +
                  {if (input$previous_events_radiobutton == "Z-Score")
                    geom_hline(yintercept = 0, linetype = "dotdash") } +
                  geom_linerange(aes(ymin = min, ymax = max)) +
                  geom_point(aes(colour = Athlete), size = 2) +
                  {if (input$previous_events_radiobutton == "Z-Score")
                    geom_hline(yintercept = 0, linetype = "dotdash") } +
                  {
                    if (input$previous_events_radiobutton == "Z-Score")
                      labs(y = "z-score")
                    else if (input$previous_events_radiobutton == "Points")
                      labs(y = "Points")
                  } +
                  {
                    if (input$previous_events_radiobutton == "Z-Score")
                      scale_y_continuous(breaks = seq(-5, 4, 1))
                    else if (input$previous_events_radiobutton == "Points")
                      scale_y_continuous(breaks = seq(
                        plyr::round_any(
                          min(ultimate_df_standardised_global[["Points"]]$min[1:10])
                          ,
                          100,
                          f = floor
                        ),
                        round(
                          max(ultimate_df_standardised_global[["Points"]]$max[1:10]), -2
                        ),
                        100))
                  } +
                  theme(axis.title.x = element_blank(), 
                        legend.position = "bottom") +
                  guides(col = guide_legend(ncol = 2)) +
                  scale_colour_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(10))
                
                return(plot)
                
                
              }
                
                if (length(hasClick) >= 11) {
                  
                  ultimate_df_standardised_global[[input$previous_events_radiobutton]] %>% ggplot(aes(event)) +
                    {if (input$previous_events_radiobutton == "Z-Score")
                      geom_hline(yintercept = 0, linetype = "dotdash") } +
                    geom_linerange(aes(ymin = min, ymax = max)) +
                    {
                      if (input$previous_events_radiobutton == "Z-Score")
                        labs(y = "z-score")
                      else if (input$previous_events_radiobutton == "Points")
                        labs(y = "Points")
                    } +
                    {
                      if (input$previous_events_radiobutton == "Z-Score")
                        scale_y_continuous(breaks = seq(-5, 4, 1))
                      else if (input$previous_events_radiobutton == "Points")
                        scale_y_continuous(breaks = seq(
                          plyr::round_any(
                            min(ultimate_df_standardised_global[["Points"]]$min[1:10])
                            ,
                            100,
                            f = floor
                          ),
                          round(
                            max(ultimate_df_standardised_global[["Points"]]$max[1:10]), -2
                          ),
                          100))
                    } +
                    {
                      if (input$previous_events_radiobutton == "Z-Score")
                        annotate("text", x = 5.5, y = 0, size = 14, colour = "red", label = "Warning: More than\n10 athletes selected")
                      else if (input$previous_events_radiobutton == "Points")
                        annotate("text", x = 5.5, y = 800, size = 14, colour = "red", label = "Warning: More than\n10 athletes selected")
                    } +
                    theme(axis.title.x = element_blank())
                  
                }
                
            })
          }
        
          output$bar_olympics <- barplotcreation("Olympics", "ex1_rows_selected")
          output$bar_wc <- barplotcreation("Worlds", "ex2_rows_selected")
          output$bar_gotzis <- barplotcreation("Gotzis", "ex3_rows_selected")
          
          make_bar_dynamic <- function(bar_obj, exnum_rows_selected) {
            renderUI({
              hasClick <- input[[exnum_rows_selected]]
              
              plotOutput(bar_obj,
                         height = if (is.null(hasClick) | length(hasClick) > 10) {
                           "550"
                         } else if (length(hasClick) %in% 1:2) {
                           "590"
                         } else if (length(hasClick) %in% 3:4) {
                           "608"
                         } else if (length(hasClick) %in% 5:6) {
                           "622"
                         } else if (length(hasClick) %in% 7:8) {
                           "638"
                         } else if (length(hasClick) %in% 9:10) {
                           "654"
                         })
              
            })
          }
          
          output$bar_olympics_dynamic <- make_bar_dynamic("bar_olympics", "ex1_rows_selected")
          output$bar_wc_dynamic <- make_bar_dynamic("bar_wc", "ex2_rows_selected")
          output$bar_gotzis_dynamic <- make_bar_dynamic("bar_gotzis", "ex3_rows_selected")

################################  depreciated code for old plots  
     # if (length(hasClick) == 1) {
          # 
          # 
          #   foobar <- temp_shiny %>%
          #   ggplot(aes(event, points)) +
          #   geom_bar(stat = "identity", alpha = 0.75) +
          #   scale_y_continuous(limits = c(0,1100),
          #                      breaks = seq(0, 1200, 200)) +
          #   labs(x = "Event",
          #        y = "Points",
          #        title = paste0(unique(temp_shiny$Year), " ", event_text, ", ", unique(temp_shiny$Athlete)),
          #        subtitle = paste0(unique(temp_shiny$Country), "\n", "Final rank: ", unique(temp_shiny$Rank))
          #   )
          # 
          #   ggplotly(foobar)
          # 
          #   } else {
          #     temp_shiny$Athlete <- paste0(temp_shiny$Year, " | ", temp_shiny$Rank, " | ", temp_shiny$Country, " | ", temp_shiny$Athlete)
          # 
          #     foobar <- temp_shiny %>%
          #       ggplot(aes(event, points, fill = Athlete)) +
          #       geom_bar(stat = "identity", alpha = 0.66, colour = "black", size = 0.25, position = "dodge") +
          #       scale_y_continuous(limits = c(0, 1100),
          #                          breaks = seq(0, 1200, 200),
          #                          expand = c(0,0)) +
          #       scale_fill_brewer(name = "Year | Rank | Country | Athlete",
          #                         palette = "Set1") +
          #       labs(
          #         x = "Event",
          #         y = "Points",
          #         title = event_text) +
          #       coord_flip()
          # 
          #     ggplotly(foobar)
          # 
          # 
          #   } ## selecting 2+ rows



        # })}
        
        # output$bar_olympics <- barplotcreation("olympics", "Olympics", "ex1_rows_selected")
        # output$bar_wc <- barplotcreation("world_championships", "World Championships", "ex2_rows_selected")
        # output$bar_gotzis <- barplotcreation("gotzis", "Gotzis Hypomeeting", "ex3_rows_selected")
        
################################  DEPRECIATED ^^^^^^^^^^      
        
        ## Heatplot ####
        
        output$decathlon_heatplot_points <- renderPlot({
          
          choose <- switch(input$decathlon_tile_choose,
                         score = "mean_score",
                         points = "mean_points")
          
          tile_function(choose)
          })
        
        ## Final Score ####
        
        ultimate_df_list[["ultimate_df_points_with_cats"]] -> OFSP_plot
        OFSP_plot$usethis <- with(OFSP_plot, paste0(Year, " ", comp, ", ", Athlete))
        OFSP_plot$rank_cat_extended %<>% fct_recode("Non-Medallist" = "Other")
        
        OFSP_plot_p <- OFSP_plot %>% ggplot(aes(reorder(usethis, finalscore), finalscore, fill = rank_cat_extended, text = 
                                                  paste0("Competition: ", Year, " ", comp, "\n",
                                                         "Athlete: ", Athlete))) + 
          theme_bw() +
          labs(y = "Final Score") +
          geom_bar(stat = "identity") +
          theme(axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(),
                panel.grid.major.x = element_blank(), 
                axis.title.x = element_blank(), 
                legend.title = element_blank()) +
          scale_y_continuous(expand = expansion(mult = c(0, .1)),
                             limits = c(7000, 9200),
                             breaks = seq(7000, 9500, 100),
                             oob = scales::rescale_none) +
          scale_fill_manual(values = c("darkgoldenrod1", "azure3", "darkgoldenrod4", "lightpink"))
        
        OFSP_plot_p <- OFSP_plot_p %>% plotly::ggplotly(tooltip = c("text", "y"))
        
        OFSP_plot_p[["x"]][["data"]] <- map(OFSP_plot_p[["x"]][["data"]],
                                          ~ {
                                            .x$text <- str_replace_all(.x$text,"finalscore","Final Score")
                                            # .x$marker$line$width <- 1 ## bar outline width
                                            
                                            return(.x)
                                          })
        
        output$OFSP_plot_decathlon <- renderPlotly(OFSP_plot_p)
        
  ## Upload custom Files ####

      
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
        for_indiv_athlete_tab %<>% arrange(Date)
        
        # for_indiv_athlete_tab <- as.data.frame(for_indiv_athlete_tab)
        
        athlete_df <- NULL
        
        output$individual_athlete_profile <-
          renderDataTable(
            {
              athlete_df <<- within(for_indiv_athlete_tab[which(for_indiv_athlete_tab$Athlete == input$athlete_select), ], rm(Athlete, Country, Year, `1500m_Score`)) ## remove unneeded columns
              
              DT::datatable(athlete_df,
                            class = 'cell-border compact',
                            rownames = FALSE,
                            options = list(pageLength = 10, scrollX = TRUE,
                                           dom = "ltipr"),
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
                formatString(~ `100m_Score` + `400m_Score` + `110mh_Score` + `1500m_ScoreMMSS`, suffix = "s") %>%
                formatString(~ LJ_Score + SP_Score + HJ_Score + DT_Score + PV_Score + JT_Score, suffix = "m")
              
              
          }
          )
        
        ## Athlete Profile - Cum Plot ####
        
        output$athlete_cumulative_plot <- renderPlotly({
          
          if (is.null(input$individual_athlete_profile_rows_selected)) {
            voop_plot <-
              voop %>%
              ggplot(aes(event, value, group = Event_Athlete_ID)) +
              labs(y = "Average Points") +
              theme(legend.title = element_blank(), 
                    axis.title.x = element_blank())
            
            ggplotly(voop_plot)
          } else {
          dates <- athlete_df[input$individual_athlete_profile_rows_selected,] %>% pull("Date")

          voop_plot <-
            voop %>% filter(Athlete == input$athlete_select &
                              date %in% c(dates)) %>%
            ggplot(aes(event, value, group = Event_Athlete_ID)) +
            geom_line(aes(colour = EventOnly)) +
            labs(y = "Average Points") +
            scale_color_brewer(palette = "Paired") +
            theme(legend.title = element_blank(),
                  axis.title.x = element_blank())

          voop_plot <-
            ggplotly(voop_plot) %>% layout(legend = list(
              orientation = "h",
              x = 0.4,
              y = -0.2
            ))
          voop_plot[["x"]][["data"]][[1]][["line"]][["width"]] <- 1.5
          voop_plot
          }

        })
        
        output$athlete_df_idx <- renderText({
          athlete_df[input$individual_athlete_profile_rows_selected,] %>% pull("Date")
        })
        
        ## Athlete Profile - Profile pic ####
        
        output$use_this_athletename <- renderText({
          c('<img src="', athlete_info[[which(athlete_info$Athlete == input$athlete_select), "image_url"]], '" height="150" style="border:1px solid black" >')
        })
        
        ## Athlete Profile - Rest of profile ####
        
        output$athlete_birth <- renderText(as.character(athlete_info[[which(athlete_info$Athlete == input$athlete_select),"birth_date"]]))
        output$iaaf_code <- renderText(athlete_info[[which(athlete_info$Athlete == input$athlete_select),"iaaf_code"]])
        output$athlete_specific <- renderText(input$athlete_select)
        output$athlete_country <- renderText(athlete_info[[which(athlete_info$Athlete == input$athlete_select),"Country"]])
        output$height <- renderText(athlete_info[[which(athlete_info$Athlete == input$athlete_select),"Height"]])

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
          symbol =  ~select_athlete,
          symbols = c("diamond", "circle"),
          color = ~get(x_choose),
          colors = RColorBrewer::brewer.pal(11, "Spectral")[c(1:4, 8:11)],
             text = ~ paste(Athlete),
             alpha = 0.66,
             hoverinfo = 'text',
             showlegend = F) %>% 
  layout(scene = list(xaxis = list(title = input$scatter3d_x),
                      yaxis = list(title = input$scatter3d_y),
                      zaxis = list(title = input$scatter3d_z)
                      )
         # ,
         # paper_bgcolor = 'rgb(0, 0, 0)',
         # plot_bgcolor = 'rgb(0, 0, 0)',
         # xaxis = list(color = "rgb(255, 255, 255)")
         )
  }
  )

## PDF individual athlete generator ####

output$download_custom_athlete = downloadHandler(
  filename = function() {
    paste0(
      str_replace_all(input$custom_athlete_select, "[:punct:]", "") %>%
        str_replace_all("[:blank:]", " _"),
      "_profile.pdf"
    )
  },
  
  content = function(file) {
    pdf(file,
        onefile = TRUE,
        width = 33.1/2,
        height = 23.4/2)
    grid.arrange(tableGrob(
      indiv_table() %>% rename(
        `Cumulative\nPoints` = `Cumulative Points`,
        `Average\nPoints` = `Average Points`,
        `Cumulative\nProportion` = `Cumulative Proportion`
      ),
      rows = NULL
    ),
    indiv_line_plot() + theme(plot.margin = margin(1.5, 1.5, 0.5, 1, "cm"), 
                              text = element_text(size = 10),
                              legend.margin = margin(-5, unit = "pt")),
    indiv_rank_plot(),
    indiv_spider_athlete(),
    ncol = 2,
    top = textGrob(input$custom_athlete_select, x = 0.02, hjust = 0,
                   gp = gpar(fontsize = 20, font = 10)))
    dev.off()
  }
)

## Testing stuff goes here ####

output$text_test <- renderText({
  
  hasClick <- input[["ex1_rows_selected"]]
  
  if (is.null(hasClick))
    return(NULL)
  
  if (length(hasClick) >= 1) {
    athlete_names <- display_dfs[hasClick, "Athlete"]
    years <- display_dfs[hasClick, "Year"]
  }
  
  return(paste0(athlete_names, years))
  
}
)

output$example_athlete_bumpplot <- renderPlot({
  
  x_scale_expand <- yyy()[["preproc_forcustomplot"]]$Athlete %>% unique() %>% str_length() %>% max()/100 %>% rep(times = 2)
  
  line_100m <- yyy()[["preproc_forcustomplot"]] %>% filter(name == "100m")
  
  if (line_100m$value %>% duplicated() %>% any()) {
    
    line_100m_list <- list()
    
    for (duplicated_rank in line_100m$value[line_100m$value %>% duplicated()] %>% unique()) {
      temp <- line_100m[which(line_100m$value == duplicated_rank),] %>% select(Athlete, name, value)
      temp$Athlete %<>% as.character()
      temp[1, "Athlete"] <-
        str_c(temp$Athlete, collapse = " / ") %>% str_wrap(width = yyy()[["preproc_forcustomplot"]]$Athlete %>% unique() %>% str_length() %>% max()*1.5)
      temp <- temp[1,]
      line_100m_list[[paste0(duplicated_rank)]] <- temp
      line_100m <- line_100m[which(!line_100m$value == duplicated_rank),]
      rm(temp)
    }
    
  }
  
  final_line_plot <- yyy()[["preproc_forcustomplot"]] %>% ggplot(aes(
    name,
    value,
    group = Athlete
  )) +
    theme_bw() +
    theme(text = element_text(family = "Segoe UI", size = 18), 
          panel.grid.minor.y = element_blank()) +
    geom_text(data = yyy()[["preproc_forcustomplot"]][1,], aes(5.5, 10, label = "victoryu.co.uk"), size = 40, alpha = .2, family = "Segoe UI") +
    geom_text(data = yyy()[["preproc_forcustomplot"]][1,], aes(5.5, 20, label = "victoryu.co.uk"), size = 40, alpha = .2, family = "Segoe UI") +
    geom_line(colour = "black", size = 2.5) +
    geom_point(colour = "black", size = 2.5) +
    geom_point(aes(colour = Athlete), size = 2) +
    geom_line(aes(colour = Athlete), size = 2) +
    geom_text(data = yyy()[["preproc_forcustomplot"]] %>% filter(name == "1500m"), aes(label = Athlete), nudge_x = 0.1, hjust = 0, family = "Segoe UI", size = 6) +
    geom_text(data = line_100m, aes(label = Athlete), nudge_x = -0.1, hjust = 1, family = "Segoe UI", size = 6) +
    scale_y_reverse(breaks = 1:36) +
    scale_x_discrete(expand = expansion(mult = x_scale_expand)) +
    scale_color_manual(values = yyy()[["preproc_forcustomplot"]] %>% summarise(colour = unique(Colour)) %>% pull(colour),
                       guide = F) +
    labs(y = "Rank",
         x = "Event")
  
  
  for (duplicated_rank in line_100m_list) {
    final_line_plot <- final_line_plot + geom_text(
      data = duplicated_rank,
      aes(label = Athlete),
      nudge_x = -0.1,
      hjust = 1,
      family = "Segoe UI",
      size = 4
    )
  }
  
  final_line_plot
  
})

output$example_athlete_avg_points <- renderPlot({
  
  yyy()[["df_avg_pivot"]] %>% 
    ggplot(aes(event, value, group = Athlete)) +
    geom_rect(data = yyy()[["df_avg_pivot"]][1:10,], aes(ymin = -Inf, ymax = Inf), fill = "lightgrey", xmin = 1.5, xmax = 2.5, alpha = 0.2/5) + ## select 10 rows otherwise it interferes with x axis order despite being factor'ed already
    geom_rect(data = yyy()[["df_avg_pivot"]][1:10,], aes(ymin = -Inf, ymax = Inf), fill = "lightgrey", xmin = 3.5, xmax = 4.5, alpha = 0.2/5) +
    geom_rect(data = yyy()[["df_avg_pivot"]][1:10,], aes(ymin = -Inf, ymax = Inf), fill = "lightgrey", xmin = 5.5, xmax = 6.5, alpha = 0.2/5) +
    geom_rect(data = yyy()[["df_avg_pivot"]][1:10,], aes(ymin = -Inf, ymax = Inf), fill = "lightgrey", xmin = 7.5, xmax = 8.5, alpha = 0.2/5) +
    geom_rect(data = yyy()[["df_avg_pivot"]][1:10,], aes(ymin = -Inf, ymax = Inf), fill = "lightgrey", xmin = 9.5, xmax = 10.5, alpha = 0.2/5) +
    geom_text(data = function(x) x[1,], aes(5.5, 600, label = "victoryu.co.uk"), size = 48, alpha = .1, family = "Segoe UI", angle = 45) +
    # geom_tile(colour = "black", height = 2) +
    # geom_point(aes(fill = fct_reorder(Athlete, final_rank)), size = 5, shape = 22, position = myjit) +
    geom_tile(aes(fill = Athlete), height = 12, width = 7.5, position=myjit) +
    geom_text(aes(label = Athlete_abbr, colour = Athlete), position = myjit, size = 4.5, family = "Segoe UI", show.legend = F) +
    labs(y = "Average Points") +
    theme(legend.title = element_blank(), 
          axis.title.x = element_blank(),
          text = element_text(family = "Segoe UI", size = 18)) +
    guides(fill = guide_legend(ncol = 1)) +
    scale_fill_manual(values = pull(summarise(yyy()[["df_avg_pivot"]], unique_colour = unique(Colour)))) +
    scale_colour_manual(values = pull(summarise(yyy()[["df_avg_pivot"]], unique_colour = unique(BorW))))  
  
}
)

output$example_athete_ranktileplot <- renderPlot({
  yyy()[["preproc_forcustomplot"]] %>% 
    ggplot(aes(
      name,
      value,
      group = Athlete
    )) +
    # theme_bw() +
    theme(text = element_text(family = "Segoe UI", size = 18), 
          panel.grid.minor.y = element_blank()) +
    geom_text(data = yyy()[["preproc_forcustomplot"]][1,], aes(11.5, 20, label = "victoryu.co.uk"), size = 30, alpha = .2, family = "Segoe UI", angle = 285) +
    geom_line(data = yyy()[["preproc_forcustomplot_line"]], aes(x = event_as_integer), colour = "black", size = 1.5) +
    geom_line(data = yyy()[["preproc_forcustomplot_line"]], aes(x = event_as_integer, colour = Athlete), size = 1) +
    geom_tile(aes(fill = Athlete), width = 0.5, height = 0.9) +
    geom_text(data = yyy()[["preproc_forcustomplot"]] %>% filter(name == "1500m"), aes(label = Athlete), nudge_x = 0.6, hjust = 0, family = "Segoe UI", size = 6) +
    scale_y_reverse(breaks = 1:36) +
    scale_x_discrete(expand = expansion(mult = c(.05, .3))) +
    scale_fill_manual(values = pull(summarise(yyy()[["preproc_forcustomplot"]], unique_colour = unique(Colour))),
                      guide = F) +
    scale_colour_manual(values = pull(summarise(yyy()[["preproc_forcustomplot"]], unique_colour = unique(Colour))),
                        guide = F) +
    ggnewscale::new_scale_colour() +
    geom_text(aes(label = Athlete_abbr, colour = Athlete), size = 4.5, family = "Segoe UI", show.legend = F) +
    scale_colour_manual(values = pull(summarise(yyy()[["preproc_forcustomplot"]], unique_colour = unique(BorW)))) +
    labs(y = "Rank",
         x = "Event")
})

output$exampledf <- renderDataTable(datatable(exampledf,
                                              class = 'cell-border stripe compact',
                                              rownames = F,
                                              options = list(pageLength = nrow(exampledf),
                                                             dom = "tir",
                                                             columnDefs = list(list(className = 'dt-body-right', targets = 2:11))), ## index starts from 0
                                              selection = "none"))

##################################################


## test custom user modify ####

x = reactiveValues(user_df = NULL)

## user upload ####
observe({
    req(input$file1)
    x$user_df <- read.csv(input$file1$datapath, 
                          stringsAsFactors = F)
})

## initial user df displayed ####

output$users_dataset <- renderDataTable({
  datatable(
    x$user_df,
    class = 'cell-border stripe compact',
    editable = T,
    rownames = F,
    options = list(
      pageLength = 20,
      scrollX = TRUE,
      dom = "ltipr",
      columnDefs = list(list(
        className = 'dt-body-right', targets = 2:11
      ))
    )
  )
})


## initial user df displayed ####

output$users_dataset_calculated <- renderDataTable({
  
  custom_df_view <- switch(input$custom_datasetview_select,
                   Score = "base_df",
                   Points = "df_points",
                   `Cumulative Points` = "df_cum",
                   Rank = "df_rank",
                   Average = "df_avg",
                   Standardised = "df_standardised")
  
  datatable(
    yyy()[[custom_df_view]],
    class = 'cell-border stripe compact',
    rownames = F,
    options = list(
      pageLength = 20,
      scrollX = TRUE,
      dom = "ltipr",
      columnDefs = list(list(
        className = 'dt-body-right', targets = 2:11
      ))
    ),
    selection = "none"
  )
  
})

##############

observeEvent(input$users_dataset_cell_edit, {
  info = input$users_dataset_cell_edit
  str(info)
  i = info$row
  j = info$col + 1
  v = info$value
  
  # problem starts here
  x$user_df[i, j] <- isolate(DT::coerceValue(v, x$user_df[i, j]))
})

output$print <- renderPrint({
  x$user_df
})

#####################################################
####

output$full_average_decathlon <- renderPlotly({
  voop_plot <-
    voop %>%
    ggplot(aes(event, value, group = Event_Athlete_ID)) +
    geom_line(alpha = 0.05) +
    labs(y = "Average Points") +
    theme(legend.title = element_blank(), 
          axis.title.x = element_blank())
  
  voop_plot <- ggplotly(voop_plot)
  voop_plot[["x"]][["data"]][[1]][["line"]][["width"]] <- 1
  voop_plot
})

output$example_user_points_boxplot <- renderPlotly({
  
  foo <- yyy()[["df_points"]] %>% 
    pivot_longer(cols = `100m`:`1500m`, names_to = "event", values_to = "points") %>%
    mutate_at(vars("event", "Athlete"), as_factor) %>% 
    ggplot(aes(event, points)) +
    theme_dark() +
    scale_y_continuous(breaks = seq(0, 1200, 200)) +
    geom_boxplot(notch = T, alpha = 0.33) +
    geom_jitter(
      aes(colour = Athlete, text = paste0("Athlete: ", Athlete,
                                          "\nPoints: ", points)),
      height = 0,
      width = 0.2,
      shape = 18,
      size = 2.5,
      alpha = 0.75
    ) +
    scale_colour_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(nrow(yyy()[["base_df"]]))) +
    theme(axis.title.x = element_blank(),
          text = element_text(family = "Segoe UI", size = 18), 
          legend.text = element_text(size = 10), 
          legend.title = element_blank()) +
    labs(y = "Points")
  
  plotly::ggplotly(foo, tooltip = "text")
  
  
})

output$example_user_cum_points_boxplot <- renderPlotly({
  
  foo <- yyy()[["df_cum"]] %>% 
    pivot_longer(cols = `100m`:`1500m`, names_to = "event", values_to = "points") %>% 
    mutate_at(vars("event", "Athlete"), as_factor) %>% 
    ggplot(aes(event, points)) +
    theme_dark() +
    geom_boxplot(alpha = 0.33) +
    geom_jitter(
      aes(colour = Athlete, text = paste0("Athlete: ", Athlete,
                                          "\nCumulative Points: ", points)),
      height = 0, width = 0.2, shape = 18, size = 1, alpha = 0.75) +
    facet_wrap(~ event, scales = "free", ncol = 10, strip.position = "bottom") +
    scale_colour_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(nrow(yyy()[["base_df"]]))) +
    theme(
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      text = element_text(family = "Segoe UI", size = 18),
      legend.text = element_text(size = 10), 
      legend.title = element_blank(),
      axis.text.y = element_text(angle = 90),
      panel.spacing = unit(0, "points")
    ) +
    labs(y = "Points")
  plotly::ggplotly(foo, tooltip = "text")
  
})


## testing obs/reactive of custom submit button

observeEvent(input$goButton, 
             {
               make_reactive$value <- T
               make_reactive$custom_df <- x$user_df
               }
             )
             
make_reactive <- reactiveValues(value = F)


yyy <-
  reactive(
    scrape_function(
      custom_csv_activate = make_reactive$value,
      custom_df = make_reactive$custom_df
    )
  )

## custom summary table

custom_athlete_data <<- list()
custom_athlete_data$score <<- reactive(
  yyy()[["base_df"]] %>% filter(Athlete == input$custom_athlete_select) %>% select("100m":"1500m") %>% as_vector())
custom_athlete_data$score_num <<- reactive(as.numeric(custom_athlete_data$score()[1:9]))

## Indiv Athlete - Summary Table ####  

indiv_table <- reactive(
  
  decathlon_s2p(
  custom_athlete_data$score_num()[1],
  custom_athlete_data$score_num()[2],
  custom_athlete_data$score_num()[3],
  custom_athlete_data$score_num()[4],
  custom_athlete_data$score_num()[5],
  custom_athlete_data$score_num()[6],
  custom_athlete_data$score_num()[7],
  custom_athlete_data$score_num()[8],
  custom_athlete_data$score_num()[9],
  if (is.na(custom_athlete_data$score()[10]))
    NA
  else if (str_detect(custom_athlete_data$score()[10], ":"))
    custom_athlete_data$score()[10]
  else if (str_detect(custom_athlete_data$score()[10], "DNS|DNF|UNKNOWN"))
    NA
  else
    as.integer(custom_athlete_data$score()[10])
  ) %>% mutate_at("Score", ~ imap_chr(., ~ {
    if (is.na(.x)) custom_athlete_data$score()[.y] else .x
  }))
  

)

output$custom_dec_table <- renderTable({
indiv_table()
}, spacing = 'xs',
bordered = TRUE)

## Indiv Athlete - Points Plot #### 

indiv_line_plot <- reactive(
  decathlon_vis(
    custom_athlete_data$score_num()[1],
    custom_athlete_data$score_num()[2],
    custom_athlete_data$score_num()[3],
    custom_athlete_data$score_num()[4],
    custom_athlete_data$score_num()[5],
    custom_athlete_data$score_num()[6],
    custom_athlete_data$score_num()[7],
    custom_athlete_data$score_num()[8],
    custom_athlete_data$score_num()[9],
    if (is.na(custom_athlete_data$score()[10]))
      NA
    else if (str_detect(custom_athlete_data$score()[10], ":"))
      custom_athlete_data$score()[10]
    else if (str_detect(custom_athlete_data$score()[10], "DNS|DNF|UNKNOWN"))
      NA
    else
      as.integer(custom_athlete_data$score()[10])
  ) + theme(axis.text.x = element_text(size = 11),
            plot.margin = margin(1.5, 1.5, 0.5, 1, "cm"), 
            text = element_text(size = 10),
            legend.margin = margin(-5, unit = "pt"))
)

output$custom_dec_plot <- renderPlot({
  indiv_line_plot()
}
)

## Indiv Athlete - Rank Plot ####

custom_indiv_athlete_rank <- reactive(
  yyy()[["preproc_forcustomplot"]] %>%
  mutate(select_athlete = case_when(Athlete == input$custom_athlete_select ~ "Select",
                                    TRUE ~ "Other")) %>%
  ungroup() %>%
  mutate_at("select_athlete", as_factor) %>%
  mutate_at("select_athlete", fct_relevel, "Select", "Other")
)

filtered_athlete <-
  reactive(
  custom_indiv_athlete_rank() %>% filter(Athlete == input$custom_athlete_select)
  )

indiv_rank_plot <- reactive(
  custom_indiv_athlete_rank() %>%
  ggplot(aes(name,
             value,
             group = Athlete)) +
  theme_bw() +
  theme(
    text = element_text(size = 18),
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(0, 1.5, 0.5, 1, "cm"),
    legend.margin = margin(-5, unit = "pt")
  ) +
  geom_text(
    data = custom_indiv_athlete_rank()[1,],
    aes(5.5, 10, label = "victoryu.co.uk"),
    size = 20,
    alpha = .05
  ) +
  geom_text(
    data = custom_indiv_athlete_rank()[1,],
    aes(5.5, 20, label = "victoryu.co.uk"),
    size = 20,
    alpha = .05
  ) +
  geom_line(data = filtered_athlete(),
            colour = "black",
            size = 2.5) +
  geom_point(data = filtered_athlete(),
             colour = "black",
             size = 7.5) +
  geom_point(data = filtered_athlete(),
             colour = "#347deb",
             size = 7) +
  geom_line(aes(colour = select_athlete, alpha = select_athlete), size = 2) +
  geom_text(data = filtered_athlete(),
            aes(label = value),
            colour = "white",
            size = 5) +
  scale_y_reverse(breaks = 1:nrow(custom_indiv_athlete_rank())) +
  scale_x_discrete() +
  scale_alpha_manual(values = c(1, 0.2)) +
  scale_colour_manual(values = c("#347deb", "#b3b3b3")) +
  labs(y = "Rank",
       x = "Event")
)

output$custom_rank_tile <- renderPlot({
  indiv_rank_plot()
})

# Indiv Athlete - Spider Plot ####

indiv_spider_athlete <- reactive(
  ggradar::ggradar(
    yyy()[["df_standardisednormalised"]] %>% filter(Athlete == input$custom_athlete_select) %>% select("Athlete", "100m":"1500m") %>% select_if(~!all(is.na(.))),
  grid.min = 0,
  grid.mid = 0.5,
  grid.max = 1,
  values.radar	= NA,
  
  axis.label.offset = 1.1,
  axis.label.size = 5,
  background.circle.colour	= "#ac68e3",
  group.colours = "#118dc2",
  axis.line.colour	= "black",
  gridline.min.colour = "black",
  gridline.mid.colour = "black", 
  gridline.max.colour = "black",
  font.radar = "Segoe UI",
  group.point.size	= 3,
  group.line.width = 1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA))
)

output$custom_spider_plot <- renderPlot(
  indiv_spider_athlete()
)

##

outVar <- reactive(yyy()[["base_df"]]$Athlete %>% sort())

observe({
  updateSelectInput(session, "custom_athlete_select",
                    choices = outVar()
  )})

## Test ####

output$first_test <- renderPlot(indiv_line_plot())
output$first_test_text <- renderPrint(mooooo[["data"]][["score"]])


## Closing server brackets ####

}