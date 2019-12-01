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

