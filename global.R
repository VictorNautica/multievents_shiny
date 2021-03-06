library(shiny)
library(multievents)
library(tidyverse)
library(magrittr)
library(multievents)
library(DT)
library(plotly)
library(lubridate)
library(BBmisc)
library(shinycssloaders)
library(ggrepel)
library(extrafont)
library(RColorBrewer)
library(gridExtra)
library(grid)

dfs <- readRDS("dfs.Rds")
dfs_score <- readRDS("dfs_score.Rds")
ultimate_df_list <- readRDS("ultimate_df_list.Rds")
athlete_info <- readRDS("athlete_info_tbl.Rds")
source("custom_data_plots.R")

## DF - Joined Cols ####

dfs <- map(dfs, ~ {
  
  .x$Athlete <- stringi::stri_enc_toutf8(.x$Athlete)
  
  if (any(.x$Athlete %in% "Edgars Erinš")){
    .x[which(.x$Athlete == "Edgars Erinš"),"Athlete"] <- paste0("Edgars Eri\U0146š")}
  if (any(.x$Athlete %in% "Janis Karlivans")){
    .x[which(.x$Athlete == "Janis Karlivans"),"Athlete"] <- "J\U0101nis Karliv\U0101ns"}
  if (any(.x$Athlete %in% "Jirí Ryba")){
    .x[which(.x$Athlete == "Jirí Ryba"),"Athlete"] <- "Ji\U0159í Ryba"}
  if (any(.x$Athlete %in% "Pawel Wiesiolek")){
    .x[which(.x$Athlete == "Pawel Wiesiolek"),"Athlete"] <- paste0("Pawe\U0142 Wiesio\U0142", "ek")}
  if (any(.x$Athlete %in% "Slaven Dizdarevic")){
    .x[which(.x$Athlete == "Slaven Dizdarevic"),"Athlete"] <- "Slaven Dizdarevi\U010D"}
  if (any(.x$Athlete %in% "Tomáš Dvorák")){
    .x[which(.x$Athlete == "Tomáš Dvorák"),"Athlete"] <- "Tomáš Dvo\U0159ák"
  }
  return(.x)
}
)

dfs_score <- map(dfs_score, ~ {
  
  .x$Athlete <- stringi::stri_enc_toutf8(.x$Athlete)
  
  if (any(.x$Athlete %in% "Edgars Erinš")){
    .x[which(.x$Athlete == "Edgars Erinš"),"Athlete"] <- paste0("Edgars Eri\U0146š")}
  if (any(.x$Athlete %in% "Janis Karlivans")){
    .x[which(.x$Athlete == "Janis Karlivans"),"Athlete"] <- "J\U0101nis Karliv\U0101ns"}
  if (any(.x$Athlete %in% "Jirí Ryba")){
    .x[which(.x$Athlete == "Jirí Ryba"),"Athlete"] <- "Ji\U0159í Ryba"}
  if (any(.x$Athlete %in% "Pawel Wiesiolek")){
    .x[which(.x$Athlete == "Pawel Wiesiolek"),"Athlete"] <- paste0("Pawe\U0142 Wiesio\U0142", "ek")}
  if (any(.x$Athlete %in% "Slaven Dizdarevic")){
    .x[which(.x$Athlete == "Slaven Dizdarevic"),"Athlete"] <- "Slaven Dizdarevi\U010D"}
  if (any(.x$Athlete %in% "Tomáš Dvorák")){
    .x[which(.x$Athlete == "Tomáš Dvorák"),"Athlete"] <- "Tomáš Dvo\U0159ák"
  }
  return(.x)
}
) ## ultimately need to inc this in the dfs lists in the pkg, this code doesn't work when I source it from ugly_fix_dfs.R for whatever reason


dfs_joined <- list(olympics = NA,
                   world_championships = NA,
                   gotzis = NA)

source("merge_df.R")

## CRITICAL ####

athlete_names <- ultimate_df_list[["ultimate_df_points"]] %>% pull(Athlete) %>% unique() %>% sort()

smallInput <- function(tag) {
  tag$children[[2]] <- htmltools::tagAppendAttributes(tag$children[[2]], class = "input-sm")
  tag
} ## https://www.w3schools.com/bootstrap/bootstrap_forms_sizing.asp

## UI - For tab df plot ####

source("FF.R")

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
          mainPanel(
            div(DT::dataTableOutput(ex_id), style = "font-size: 74.5%; width: 70%"),
            width = 9
            ),
          sidebarPanel(
          # style = "position:fixed;width:inherit;",
          selectInput(
            inputId = select_year_label,
            label = "Select year of competition",
            choices = c(unique(dfs_joined[[dfs_proper_call]]$Year)),
            multiple = TRUE,
            selectize = TRUE
          ),
          radioButtons(
            "previous_events_radiobutton",
            label = NULL,
            inline = T,
            choices = c("Points", "Z-Score")
          ),
          uiOutput(plotoutputlabel),
          tags$br(),
          "Maximum of 10 athletes allowed for plot",
          width = 3
          )
        )
      )
  }

## UI - tags padding test ####

padding <- function(calc_output) {
  column(3, tags$label("Points:"), tags$div(textOutput(calc_output), style = "padding-top:4px"))
}

## UI - custom hr for calculator tab ####

hr_calculator <- function(colour) {
  tags$hr(style=paste0("border-color: ", colour, ";margin-top: 0px;margin-bottom: 0px"))
}

## UI - S2P panels ####

s2ppanel <- function(event, plot_reference) {
  tabPanel(event, 
         plotlyOutput(plot_reference, height = "800px") %>% withSpinner())
}

## SERVER - grouped container for dfs ####

df_grouped_container <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Year'),
      th(rowspan = 2, 'Athlete'),
      th(rowspan = 2, 'Age at Comp'),
      th(rowspan = 2, 'Country'),
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
))

## UI - 3d scatter options ####

## for selectinput

scatter3d_options <- c("Speed", "Throws", "Jumps", "Endurance")

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
    
    
    p <- ggplot(
      foob, mapping = aes(x, Points, colour = Event, text = paste("Athlete: ", Athlete))
    ) +
      # geom_jitter(shape = 3, alpha = 0.5, width = 0, height = 50) +
      geom_point(shape = 3, alpha = 0.5) +
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
      scale_colour_manual(values = rep("black", nrow(ultimate_df_points))) +
      
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
      plotly::layout(hovermode = "x", hoverdistance = 1)
    
    
    p$x$data <- map(p$x$data, ~ {
      
      if (.x$mode == "lines") .x$hoverinfo <- "none" ## eliminate hover for rugs
      
      if (.x$mode == "markers") .x$text <- .x$text %>% str_replace_all("x:", paste0(short_measure, ":"))
      
      return(.x)
      
    })
    
    return(p)
  }

## SERVER - HEAT MAP ####

ultimate_df_points_long <- ultimate_df_list[["ultimate_df_points"]] %>% gather(key = "event", value = "points", X100m:X1500m)
ultimate_df_points_long %<>% arrange(date)
ultimate_df_points_long$event_fct_order <- ultimate_df_points_long %$% paste0(Year, " ", comp) %>% as_factor()

ultimate_df_scores_long <- ultimate_df_list[["ultimate_df_score"]] %>% gather(key = "event", value = "score", X100m:X1500m)
ultimate_df_scores_long %<>% arrange(date)
ultimate_df_scores_long$event_fct_order <- ultimate_df_points_long %$% paste0(Year, " ", comp) %>% as_factor()

test <- ultimate_df_scores_long %>% group_by(event_fct_order, event) %>% summarise(mean_score = round(mean(score), digits = 2))
test %<>% mutate(mean_score = case_when(event == "X1500m" ~ mean_score %>% 
                                          seconds_to_period() %>% 
                                          round(digits=2) %>% 
                                          gsub(pattern = "M ", replacement = ":", .) %>% 
                                          gsub(pattern = "S", replacement = "", .) %>% 
                                          str_to_lower(),
                                        event %in% c("X100m", "X400m", "X110mh") ~ paste(mean_score, "s", sep = ""),
                                        event %in% c("PV", "JT", "SP", "HJ", "LJ", "DT") ~ paste (mean_score, "m", sep = "")))

test1 <- ultimate_df_points_long %>% group_by(event_fct_order, event) %>% summarise(mean_points = round(mean(points), digits = 0))
test_combined <- inner_join(test, test1, by = c("event_fct_order", "event"))

rm(test, test1, ultimate_df_points_long, ultimate_df_scores_long)

tile_function <- function(which_metric){
  
  test_combined %>% ggplot() +
    geom_raster(aes(
      fill = mean_points,
      x = event_fct_order,
      y = reorder(event, mean_points)
    ), alpha = 0.75) +
    geom_text(
      aes(label = !!sym(which_metric), x = event_fct_order, y = event),
      family = "Segoe UI",
      size = 3.75
    ) +
    scale_y_discrete(
      labels = c(
        "1500m",
        "Javelin Throw",
        "Discus Throw",
        "Shotput",
        "High Jump",
        "Pole Vault",
        "400m",
        "100m",
        "Long Jump",
        "110m hurdles"
      )
    ) +
    scale_fill_gradient(low = "#FCFBFD",
                        high = "#6A51A3",
                        guide = guide_colourbar(title.vjust = 1)) +
    theme(
      text = element_text(size = 18),
      legend.position = "top",
      legend.key.width = unit(90, "pt"),
      legend.key.height = unit(7.5, "pt"),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 12
      )
    ) +
    labs(fill = "Mean Points  ",
         x = "Competition (chronological order)",
         y = "Event")
}

## SERVER - RADAR INITIAL DF ####

df_radar <- ultimate_df_list[["ultimate_df_points"]] %>% select(Athlete, X100m:X1500m) %>% 
  group_by(Athlete) %>% 
  summarise(avg_speed = sum(X100m+X400m+X110mh)/(n()*3),
            avg_throws = sum(SP+DT+JT)/(n()*3),
            avg_jumps = sum(LJ+HJ+PV)/(n()*3),
            avg_endurance = sum(X1500m)/n())
df_radar %<>% mutate_if(is.numeric, list(`_standardised` = ~scale(.)))
df_radar <- lapply(df_radar, function(x) { attributes(x) <- NULL; x }) %>% as_tibble() ## strip attributes otherwise normalize crashes rstudio
df_radar %<>% mutate_at(vars(contains("standardised")), list(`_normalised` = ~normalize(., method = "range")))

radar_function <- function(athletename) {
  
  df_radar %>% filter(Athlete == athletename)

ggradar::ggradar(df_radar %>% filter(Athlete == athletename) %>% select("Athlete", contains("normalised")), 
                 grid.min = 0,
                 grid.mid = 0.5,
                 grid.max = 1,
                 values.radar	= NA,
                 axis.labels = c("Speed",
                                 "Throws",
                                 "Jumps",
                                 "Endurance"),
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
                 group.line.width = 1,
                 plot.extent.x.sf	= 1.1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA))
}

## SERVER - COLOUR INTERPOLATE ####

hex_colours <- colorRampPalette(c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"))(100)

hexfillfunction <- function(hexfill) paste0('<svg width="50" height="10">
                                            <rect width="50" height="10" style="fill:', hexfill, ';stroke-width:1;stroke:rgb(0,0,0)" />
                                          </svg>')

radar_colourbar_func <- function(athletename, grouping) {
  
  percentile <- df_radar %>% filter(Athlete == athletename) %>% pull(grouping)
  percentile <- round(percentile*100, 0)
  
  if (percentile == 0) {
    percentile <- 1
  }
  
  usethishex <- hex_colours[percentile]
  
  return(hexfillfunction(usethishex))
  
}

## SERVER - Athlete Cum DF ####

voop <- ultimate_df_list[["ultimate_df_cum"]] %>%
  mutate(
    LJ = LJ / 2,
    SP = SP / 3,
    HJ = HJ / 4,
    X400m = X400m / 5,
    X110mh = X110mh / 6,
    DT = DT / 7,
    PV = PV / 8,
    JT = JT / 9,
    X1500m = X1500m / 10,
    Event_Athlete_ID = paste(Year, comp, Athlete),
    EventOnly = paste(Year, comp)
  ) %>%
  pivot_longer(cols = c(X100m:X1500m), names_to = "event") %>%
  mutate_at("event", str_replace_all, "X", "") %>%
  mutate_at("event", as_factor)

## SERVER - Example DF ####

exampledf <- read_csv("scrapenglandathletics_withoutpoints.csv")

## SERVER - Facet Wrap Size Normalise ####

# https://stackoverflow.com/questions/61580973/first-and-last-facets-using-facet-wrap-with-ggplotly-are-larger-than-middle-face

# fixfacets <- function(figure, facets, domain_offset){
#   
#   fig <- figure
#   
#   n_facets <- length(facets)
#   
#   
#   # split x ranges from 0 to 1 into
#   # intervals corresponding to number of facets
#   # xHi = highest x for shape
#   xHi <- seq(0, 1, len = n_facets+1)
#   xHi <- xHi[2:length(xHi)]
#   
#   xOs <- domain_offset
#   
#   # Shape manipulations, identified by dark grey backround: "rgba(217,217,217,1)"
#   # structure: p$x$layout$shapes[[2]]$
#   shp <- fig$x$layout$shapes
#   j <- 1
#   for (i in seq_along(shp)){
#     if (shp[[i]]$fillcolor=="rgba(217,217,217,1)" & (!is.na(shp[[i]]$fillcolor))){
#       #$x$layout$shapes[[i]]$fillcolor <- 'rgba(0,0,255,0.5)' # optionally change color for each label shape
#       fig$x$layout$shapes[[i]]$x1 <- xHi[j]
#       fig$x$layout$shapes[[i]]$x0 <- (xHi[j] - xOs)
#       #fig$x$layout$shapes[[i]]$y <- -0.05
#       j<-j+1
#     }
#   }
#   
#   # annotation manipulations, identified by label name
#   # structure: p$x$layout$annotations[[2]]
#   ann <- fig$x$layout$annotations
#   annos <- facets
#   j <- 1
#   for (i in seq_along(ann)){
#     if (ann[[i]]$text %in% annos){
#       # but each annotation between high and low x,
#       # and set adjustment to center
#       fig$x$layout$annotations[[i]]$x <- (((xHi[j]-xOs)+xHi[j])/2)
#       fig$x$layout$annotations[[i]]$xanchor <- 'center'
#       #print(fig$x$layout$annotations[[i]]$y)
#       #fig$x$layout$annotations[[i]]$y <- -0.05
#       j<-j+1
#     }
#   }
#   
#   # domain manipulations
#   # set high and low x for each facet domain
#   xax <- names(fig$x$layout)
#   j <- 1
#   for (i in seq_along(xax)){
#     if (!is.na(pmatch('xaxis', xax[i]))){
#       #print(p[['x']][['layout']][[lot[i]]][['domain']][2])
#       fig[['x']][['layout']][[xax[i]]][['domain']][2] <- xHi[j]
#       fig[['x']][['layout']][[xax[i]]][['domain']][1] <- xHi[j] - xOs
#       j<-j+1
#     }
#   }
#   
#   return(fig)
# }