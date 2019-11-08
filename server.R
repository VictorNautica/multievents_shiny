function(input, output) {
  
  ## For calculator ####


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
        
        ## For tables ####
        

        datatablecreation <- function(event, inputid) {
## When it's not a run event I need to reverse the SliderInputId [1] and [2]          
          DT::renderDataTable(
            DT::datatable(
              if (is.na(input[[inputid]])) {
                dfs[[event]][dfs[[event]][["100m"]] > dec_100m(input$filter_100m[2]) &
                               dfs[[event]][["100m"]] < dec_100m(input$filter_100m[1]),]
              }
              else if (input[[inputid]] == "select_all") {
                dfs[[event]][dfs[[event]][["100m"]] > dec_100m(input$filter_100m[2]) &
                               dfs[[event]][["100m"]] < dec_100m(input$filter_100m[1]),]
              } else {
                dfs[[event]][which(dfs[[event]]$Year %in% input[[inputid]]), ]
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
        
        barplotcreation <- function(event, event_text, exnum_rows_selected) {renderPlot({
          
          hasClick <- input[[exnum_rows_selected]]
          if (is.null(hasClick)) return(NULL) ## shows blank plot otherwise
          
          temp_shiny <- dfs[[event]][input[[exnum_rows_selected]], ] %>% gather(key = "event", value = "points", `100m`:`1500m`) %>% mutate_at(.vars = "event", .funs = as_factor)
          
          if (length(hasClick) == 1) {
          temp_shiny %>% 
            ggplot(aes(event, points)) + 
            geom_bar(stat = "identity", alpha = 0.75) +
            scale_y_continuous(limits = c(0,1100),
                               breaks = seq(0, 1200, 200)) +
            labs(x = "Event",
                 y = "Points",
                 title = paste0(unique(temp_shiny$Year), " ", event_text, ", ", unique(temp_shiny$Athlete)),
                 subtitle = paste0(unique(temp_shiny$Country), "\n", "Final rank: ", unique(temp_shiny$Rank))
            ) } else {
              temp_shiny$Athlete <- paste0(temp_shiny$Year, " | ", temp_shiny$Rank, " | ", temp_shiny$Country, " | ", temp_shiny$Athlete)
              
              temp_shiny %>%
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
              
              
            } ## selecting 2+ rows
          
          
          
        })}
        
        
        output$bar_olympics <- barplotcreation("olympics", "Olympics", "ex1_rows_selected")
        output$bar_wc <- barplotcreation("world_championships", "World Championships", "ex2_rows_selected")
        output$bar_gotzis <- barplotcreation("gotzis", "Gotzis Hypomeeting", "ex3_rows_selected")
        
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
        
}