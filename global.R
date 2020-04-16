library(shiny)
library(multievents)
library(tidyverse)
library(magrittr)
# devtools::install_github("victornautica/multievents")
library(multievents)
library(DT)
library(plotly)

dfs <- readRDS("dfs.Rds")
dfs_score <- readRDS("dfs_score.Rds")
ultimate_df_list <- readRDS("E:/clean_analysis/ultimate_df_list.Rds")

## UI - tab df modules ####

decathlon_tabs <-
  function(tab_label,
           ex_id,
           select_year_label,
           dfs_proper_call,
           plotoutputlabel) {
      tabPanel(
        tab_label,
        sidebarLayout(
          mainPanel(dataTableOutput(ex_id)),
          sidebarPanel(
            style = "position:fixed;width:inherit;",
            selectInput(
              inputId = select_year_label,
              label = "Select year of competition",
              choices = c(unique(dfs[[dfs_proper_call]]$Year)),
              multiple = TRUE,
              selectize = TRUE
            ),
            sliderInput(
                     inputId = "filter_100m",
                     label = "Filter 100m time",
                     min = min(dfs_score[[dfs_proper_call]]$`100m`),
                     max = max(dfs_score[[dfs_proper_call]]$`100m`),
                     value = c(min(dfs_score[[dfs_proper_call]]$`100m`), max(dfs_score[[dfs_proper_call]]$`100m`)),
                     post = "s",
                     dragRange = F
                   ),
            plotlyOutput(plotoutputlabel, height = "550")
          )
        )
      )
  }

## UI - tags padding test ####

padding <- function(calc_output) {
  column(6, tags$label("Score:"), tags$div(textOutput(calc_output), style = "padding-top:10px"))
}

## SERVER - shiny function for score to points function ####

eventpointsfull_func <-
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
           short_measure,
           opt_label = waiver(),
           opt_minorbreaks = waiver()) {
    
    ultimate_df_score <- ultimate_df_list[["ultimate_df_score"]] %>% arrange(date, Rank)
    ultimate_df_points <- ultimate_df_list[["ultimate_df_points"]] %>% arrange(date, Rank)
    
    
    foob <-
      data.frame(
        "Athlete" = ultimate_df_score$Athlete,
        "Event" = paste(ultimate_df_score$Year, ultimate_df_score$comp),
        "x" = ultimate_df_score[,which(colnames(ultimate_df_score) == plotpointnumber)] %>% pull(),
        "Points" = ultimate_df_points[,which(colnames(ultimate_df_points) == plotpointnumber)]
      )
    
    
    p <- ggplot(foob, mapping = aes(x, Points, text = paste("Athlete: ", Athlete,
                                                            "\nEvent: ", Event)
    )
    ) +
      # geom_jitter(shape = 4, alpha = 0.5, width = 0, height = 50) +
      geom_point(shape = 1, alpha = 0.5) +
      stat_function(
        fun = function(x)
          alpha * ((if (eventtype == "runs") {
            beta - x
          }
          else if (eventtype == "jumps"){
            (x*100) - beta ## *100 as jumps need to be in cm
          } else {
            x - beta
          }) ^ delta)
      ) +
      geom_rug(alpha = 1 / 10) +
    scale_y_continuous(breaks = seq(0, 1500, 100)) +
      theme(
        text = element_text(size = 18, family = "Segoe UI"),
        legend.title = element_blank(),
        legend.position = "none"
      ) +
      
      if (eventtype == "jumps") {
        scale_x_continuous(
          name = event_and_measurement,
          breaks = seq(lowerx, upperx, by_x),
          limits = c(lowerx, upperx),
          minor_breaks = opt_minorbreaks)
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
    # p <- ggMarginal(p)
    p <- plotly::ggplotly(p) %>% 
      plotly::layout(hovermode = "x")
    p$x$data[[2]]$hoverinfo <- "none"
    
    p[["x"]][["data"]][[1]][["text"]] <- str_replace_all(p[["x"]][["data"]][[1]][["text"]], "x:", paste0(short_measure, ":")) ## points
    p[["x"]][["data"]][[3]][["text"]] <- str_replace_all(p[["x"]][["data"]][[1]][["text"]], "x:", paste0(short_measure, ":")) ## rug
    p[["x"]][["data"]][[4]][["text"]] <- str_replace_all(p[["x"]][["data"]][[1]][["text"]], "x:", paste0(short_measure, ":")) ## rug
    
    return(p)
  }