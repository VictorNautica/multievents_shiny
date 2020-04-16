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
        
        # output$dec_plot <- renderPlot({
        #   decathlon_vis(
        #     input$event_one,
        #     input$event_two,
        #     input$event_three,
        #     input$event_four,
        #     input$event_five,
        #     input$event_six,
        #     input$event_seven,
        #     input$event_eight,
        #     input$event_nine,
        #     input$event_ten
        #   )
        # })      ## change to hept  
        
        
        
        ## For tables ####
        
        display_dfs <- NULL

        datatablecreation <- function(event, inputid) {
          
## When it's not a run event I need to reverse the SliderInputId [1] and [2]          
          renderDataTable(
            datatable({
                years <- input[[inputid]]
                
                if (length(years) == 0) {
                  # reassign in the parent data frame!!!
                  display_dfs <<- dfs[[event]][dfs[[event]][["100m"]] > dec_100m(input$filter_100m[2]) &
                                                dfs[[event]][["100m"]] < dec_100m(input$filter_100m[1]),]
                } else {
                  display_dfs <<- dfs[[event]][which(dfs[[event]]$Year %in% years), ]
                }
                
                display_dfs
              },
              options = list(pageLength = 25),
              rownames = FALSE,
              class = 'cell-border stripe'
            )
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
        
  ## Score to points line graph ####
        
        output$trdhrdthdrth <- renderPlotly({
          
          eventpoints_func <-
            function(plotpointnumber,
                     eventtype,
                     alpha,
                     beta,
                     delta,
                     nudgeathlete,
                     nudgescore,
                     event_and_measurement,
                     upperx = NULL,
                     lowerx = NULL,
                     by_x,
                     opt_label = waiver(),
                     opt_minorbreaks = waiver()) {
              ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
                stat_function(
                  fun = function(x)
                    floor(alpha * ((if (eventtype == "runs") {
                      beta - x
                    }
                    else {
                      x - beta
                    }) ^ delta))
                ) +
                geom_point(data = plotpoints[[plotpointnumber]], aes(x = Score, y = Points, colour =
                                                                       Record)) +
                geom_label_repel(
                  data = plotpoints[[plotpointnumber]],
                  aes(x = Score, y = Points, label = Athletename),
                  box.padding   = 0.35,
                  point.padding = 0.5,
                  segment.color = 'grey50',
                  direction = "x",
                  nudge_x = nudgeathlete
                ) +
                geom_label_repel(
                  data = plotpoints[[plotpointnumber]],
                  aes(x = Score, y = Points, label = score_and_points),
                  box.padding   = 0.35,
                  point.padding = 0.5,
                  segment.color = 'grey50',
                  direction = "y",
                  nudge_y = nudgescore
                ) +
                geom_label_repel(
                  data = plotpoints[[plotpointnumber]],
                  aes(x = Score, y = Points, label = score_and_points),
                  box.padding   = 0.35,
                  point.padding = 0.5,
                  segment.color = 'grey50',
                  direction = "y",
                  segment.alpha = 0,
                  nudge_y = nudgescore
                ) +
                scale_y_continuous(name = "Points",
                                   breaks = seq(0, 1500, 100)) +
                theme(
                  text = element_text(size = 18, family = "Segoe UI"),
                  legend.title = element_blank(),
                  legend.position = "top"
                ) +
                
                if (eventtype == "jumps") {
                  scale_x_continuous(
                    name = event_and_measurement,
                    breaks = seq(lowerx, upperx, by_x),
                    limits = c(lowerx, upperx),
                    minor_breaks = opt_minorbreaks,
                    labels = function(x)
                      x / 100
                  )
                } else if (eventtype == "runs") {
                  scale_x_reverse(
                    name = event_and_measurement,
                    breaks = seq(upperx, lowerx, by_x),
                    limits = c(upperx, lowerx),
                    labels = opt_label
                  )
                } else {
                  scale_x_continuous(
                    name = event_and_measurement,
                    breaks = seq(lowerx, upperx, by_x),
                    limits = c(lowerx, upperx)
                  )
                }
            }
          
          eventpoints_func(
            plotpointnumber = 1,
            eventtype = "runs",
            nudgeathlete = -2,
            nudgescore = -200,
            alpha = 25.4347,
            beta = 18,
            delta = 1.81,
            lowerx = 9,
            upperx = 18,
            by_x = -1,
            event_and_measurement = "100m time (seconds)"
          ) %>% plotly()
          
          
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


        
        
        for_indiv_athlete_tab <- readRDS("dfs.Rds")
        for_indiv_athlete_tab <-
          bind_rows(for_indiv_athlete_tab,
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
          renderDataTable({
            within(for_indiv_athlete_tab[which(for_indiv_athlete_tab$Athlete == input$athlete_select),],
                   rm(Athlete, Country, Year)) ## remove unneeded columns
          })
        
        
        output$individual_athlete_plot <- renderPlot({
          foobar <-
            for_indiv_athlete_tab[which(for_indiv_athlete_tab$Athlete == input$athlete_select), ]

          foobar %<>% pivot_longer(`Final Score`:`1500m`, "Event", values_to = "Score") %>% unite("Major Event", c(`Major Event`, Year), sep = " ") %>% arrange(Date)
          foobar$`Major Event` %<>% as_factor()

          this_plot <- foobar %>% ggplot(aes(`Major Event`, Score, group = Event, fill = `Major Event`)) +
            geom_bar(stat = "identity", alpha = 2 / 3) +
            facet_wrap("Event",
                       nrow = 2,
                       ncol = 6,
                       scales = "free") +
            theme(axis.text.x = element_text(
              angle = 45,
              hjust = 1,
              vjust = 1
            )) +
            scale_fill_brewer(palette = "Set1")

          return(this_plot)
        })
        
        ## Scrapping ####
        
        output$use_this_athletename <- renderText({c('<img src="',athlete_info[[input$athlete_select]][["image_url"]],'">')})
        
        ## Birth data ####
        
        output$athlete_birth <- renderText(as.character(athlete_info[[input$athlete_select]][["birth_date"]]))
        output$iaaf_code <- renderText(athlete_info[[input$athlete_select]][["iaaf_code"]])
        output$athlete_specific <- renderText(input$athlete_select)
        output$athlete_country <- renderText(athlete_info_tbl[[which(athlete_info_tbl$Athlete == input$athlete_select), "Country"]])

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

}