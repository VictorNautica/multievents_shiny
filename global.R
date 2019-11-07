library(shiny)
library(multievents)
library(tidyverse)
# devtools::install_github("victornautica/multievents")
library(multievents)

dfs <- readRDS("dfs.Rds")
dfs <- lapply(dfs, function(x) {x$date <- NULL
              return(x)}
              )
dfs_score <- readRDS("dfs_score.Rds")

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
          mainPanel(DT::dataTableOutput(ex_id)),
          sidebarPanel(
            style = "position:fixed;width:inherit;",
            selectInput(
              inputId = select_year_label,
              label = "Select year of competition",
              choices = c(All = "select_all", unique(dfs[[dfs_proper_call]]$Year)),
              multiple = TRUE,
              selected = "select_all",
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
            plotOutput(plotoutputlabel, height = "600")
          )
        )
      )
  }

