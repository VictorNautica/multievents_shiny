library(tidyverse)
library(magrittr)

## Vectorise hex black/white function ####

hex_bw <- Vectorize(function(hex_code) {
  
  myrgb <- as.integer(col2rgb(hex_code))
  
  rgb_conv <- lapply(myrgb, function(x) {
    i <- x / 255
    if (i <= 0.03928) {
      i <- i / 12.92
    } else {
      i <- ((i + 0.055) / 1.055) ^ 2.4
    }
    return(i)
  })
  
  rgb_calc <- (0.2126*rgb_conv[[1]]) + (0.7152*rgb_conv[[2]]) + (0.0722*rgb_conv[[3]])
  
  if (rgb_calc > 0.179) return("#000000") else return("#ffffff")
  
})

## Initial scrape ####


list_tidy <- list()
list_tidy[["base_df"]] <- read_csv("scrapenglandaltheits_withoutpoints.csv") %>% select(!contains("Points"))

# which(list_tidy[["base_df"]]$Athlete == "George HEPPINSTALL [U23]") ## sorted this manually, I will need to automate it next time

idx_100m <- str_which(names(list_tidy[["base_df"]]), pattern = "100m")

last_entry <- list_tidy[["base_df"]][,idx_100m:(idx_100m+9)] %>% t %>% as_tibble() %>% map_int( ~ max(which(!is.na(.x)))) ## return last valid entry index before NA or complete series

list_tidy[["base_df"]] <- list_tidy[["base_df"]][order(last_entry, decreasing = T), ] ## rearrange

list_tidy[["base_df"]] <-
  list_tidy[["base_df"]] %>% select(idx_100m:(idx_100m+9)) %>% t() %>% array_branch(2) %>%
  map( ~ {
    has_na <- which(is.na(.x))
    
    if (length(has_na > 0)) {
      if (max(which(!is.na(.x))) > min(has_na)) {
        .x[min(has_na)] <- "UNKNOWN"
        
      }
    }
    return(.x)
  }) %>% map_dfr(as.list) %>% 
  bind_cols(list_tidy[["base_df"]][, 1:(idx_100m-1)], .)
colnames(list_tidy[["base_df"]]) %<>% str_replace_all("_Score", "")

## Calculate Points ####

list_tidy[["df_points"]] <- list_tidy[["base_df"]] %>% select("100m":"1500m") %>% imap_dfc(~ {
  
  use_this_func <- switch(.y,
                             "100m" = multievents::dec_100m,
                             "LJ" = multievents::dec_lj,
                             "SP" = multievents::dec_sp,
                             "HJ" = multievents::dec_hj,
                             "400m" = multievents::dec_400m,
                             "110mh" = multievents::dec_110mh,
                             "DT" = multievents::dec_dt,
                             "PV" = multievents::dec_pv,
                             "JT" = multievents::dec_jt,
                             "1500m" = multievents::dec_1500m)

  mod_function <- possibly(use_this_func, otherwise = NA_integer_)
  
  if (.y == "1500m") {
    if (str_detect(.x, ":")) .x %>% map_dbl(~ mod_function(.x))
    } else {
    mod_function(as.numeric(.x))
    }
  
  }
  ) %>% bind_cols(list_tidy[["base_df"]][, 1:(idx_100m-1)], .)

last_fullentry_idx <- max(which(list_tidy[["base_df"]]$`1500m` %>% str_detect("[:digit:]")))

list_tidy[["df_points"]][1:last_fullentry_idx,] %<>% mutate_at(vars("100m":"1500m"),
                                                                                    replace_na, 0) ## replace missing points in anyone with a rank with 0

unranked_with_letter <- list_tidy[["base_df"]][(last_fullentry_idx+1):nrow(list_tidy[["base_df"]]),] %>% select("100m":"1500m") %>% map(~ {
  
  .x %>% str_which("NMR|UNKNOWN|DNF")
  
})

list_tidy[["df_points"]][(last_fullentry_idx+1):nrow(list_tidy[["base_df"]]),idx_100m:(idx_100m+9)] %<>% imap_dfc( ~ {
 
  if (length(unranked_with_letter[[.y]]) > 0) {
    .x[unranked_with_letter[[.y]]] <- 0
  }
  
  return(.x)
  
}
)

## Calculate Cumulative Points ####

list_tidy[["df_cum"]] <- list_tidy[["df_points"]][,idx_100m:(idx_100m+9)] %>%
  as.matrix() %>%
  t() %>%
  array_branch(2) %>%
  map(~ cumsum(.x)) %>%
  bind_cols() %>%
  t() %>%
  as_tibble() %>% bind_cols(list_tidy[["base_df"]][,1:(idx_100m-1)], .)

colnames(list_tidy[["df_cum"]])[idx_100m:(idx_100m+9)] <- colnames(list_tidy[["df_points"]])[idx_100m:(idx_100m+9)]

## Calculate Rank ####

list_tidy[["df_rank"]] <- list_tidy[["df_cum"]] %>% mutate_at(idx_100m:(idx_100m+9),
                                                              function(x) rank(-x, ties.method = "average"))

## Calculate Average Points ####

list_tidy[["df_avg"]] <- list_tidy[["df_cum"]] %>%
  mutate(
    LJ = LJ / 2,
    SP = SP / 3,
    HJ = HJ / 4,
    `400m` = `400m` / 5,
    `110mh` = `110mh` / 6,
    DT = DT / 7,
    PV = PV / 8,
    JT = JT / 9,
    `1500m` = `1500m` / 10
  )

## Average Points Pivot for Plot ####

list_tidy[["df_avg_pivot"]] <- list_tidy[["df_avg"]] %>%
  mutate_at("Athlete", as_factor) %>%
  mutate(
    "Colour" = c(
      colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(last_fullentry_idx),
      rep("black", nrow(.) - last_fullentry_idx)
    ),
    "BorW" = hex_bw(Colour) %>% unname()
  ) %>%
  group_by(Athlete) %>%
  mutate(
    final_rank = `1500m`,
    Athlete_abbr = Athlete %>% map_chr(
      ~ .x %>% str_split("[:blank:](?=[:alpha:])") %>% map_chr( ~ paste0(str_sub(
        .x , start = 1, end = 1
      ), collapse = "")),
    )
  ) %>% pivot_longer(cols = c(`100m`:`1500m`), names_to = "event") %>%
  mutate_at("event", as_factor)


## Pre-processing for custom line plot ####

preproc_forcustomplot <-
  list_tidy[["df_rank"]] %>%
  mutate(
    "Colour" = c(
      colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(last_fullentry_idx),
      rep("black", nrow(.) - last_fullentry_idx)
    ),
    "BorW" = hex_bw(Colour) %>% unname(),
    Athlete_abbr = Athlete %>% map_chr(
      ~ .x %>% str_split("[:blank:](?=[:alpha:])") %>% map_chr(~ paste0(str_sub(
        .x , start = 1, end = 1
      ), collapse = ""))
    )
  ) %>% 
  pivot_longer(c("100m":"1500m")) %>% 
  mutate_at(c("name", "Athlete"), as_factor) %>% 
  group_by(Athlete) %>% 
  mutate(final_rank = value[name == "1500m"])

## Line modification in the rank tile plot ####

preproc_forcustomplot_line <- preproc_forcustomplot %>% mutate(event_as_integer = as.integer(name),
                                         duplicate = case_when(as.integer(name) %in% c(1,10) ~ 1,
                                                               TRUE ~ 2)) %>% 
  uncount(duplicate) %>%
  group_by(Athlete, event_as_integer) %>% mutate(test = 1:n()) %>%
  ungroup() %>% 
  mutate_at("event_as_integer",
            ~ case_when(. == 1 ~ . + 0.25,
                        . == 10 ~ . - 0.25,
                        . %in% 2:9 & test == 1 ~ . - 0.4,
                        . %in% 2:9 & test == 2 ~ . + 0.4))

## Function for aligned jitters in diff geoms ####

myjit <- ggproto("fixJitter", PositionDodge,
                 width = 0.5,
                 dodge.width = 0.15,
                 jit = NULL,
                 compute_panel =  function (self, data, params, scales) 
                 {
                   
                   #Generate Jitter if not yet
                   if(is.null(self$jit) ) {
                     self$jit <-jitter(rep(0, nrow(data)), amount=self$dodge.width)
                   }
                   
                   data <- ggproto_parent(PositionDodge, self)$compute_panel(data, params, scales)
                   
                   data$x <- data$x + self$jit
                   #For proper error extensions
                   if("xmin" %in% colnames(data)) data$xmin <- data$xmin + self$jit
                   if("xmax" %in% colnames(data)) data$xmax <- data$xmax + self$jit
                   data
                 } )

## OLD CODE ####

## Average Points Plot ####

# list_tidy[["df_avg_pivot"]] %>% 
#   ggplot(aes(event, value, group = Athlete)) +
#   geom_rect(data = list_tidy[["df_avg_pivot"]][1:10,], aes(ymin = -Inf, ymax = Inf), fill = "lightgrey", xmin = 1.5, xmax = 2.5, alpha = 0.2/5) + ## select 10 rows otherwise it interferes with x axis order despite being factor'ed already
#   geom_rect(data = list_tidy[["df_avg_pivot"]][1:10,], aes(ymin = -Inf, ymax = Inf), fill = "lightgrey", xmin = 3.5, xmax = 4.5, alpha = 0.2/5) +
#   geom_rect(data = list_tidy[["df_avg_pivot"]][1:10,], aes(ymin = -Inf, ymax = Inf), fill = "lightgrey", xmin = 5.5, xmax = 6.5, alpha = 0.2/5) +
#   geom_rect(data = list_tidy[["df_avg_pivot"]][1:10,], aes(ymin = -Inf, ymax = Inf), fill = "lightgrey", xmin = 7.5, xmax = 8.5, alpha = 0.2/5) +
#   geom_rect(data = list_tidy[["df_avg_pivot"]][1:10,], aes(ymin = -Inf, ymax = Inf), fill = "lightgrey", xmin = 9.5, xmax = 10.5, alpha = 0.2/5) +
#   geom_text(data = function(x) x[1,], aes(5.5, 600, label = "victoryu.co.uk"), size = 48, alpha = .1, family = "Segoe UI", angle = 45) +
#   # geom_tile(colour = "black", height = 2) +
#   # geom_point(aes(fill = fct_reorder(Athlete, final_rank)), size = 5, shape = 22, position = myjit) +
#   geom_tile(aes(fill = Athlete), height = 12, width = 7.5, position=myjit) +
#   geom_text(aes(label = Athlete_abbr, colour = Athlete), position = myjit, size = 4.5, family = "Segoe UI", show.legend = F) +
#   labs(y = "Average Points") +
#   theme(legend.title = element_blank(), 
#         axis.title.x = element_blank(),
#         text = element_text(family = "Segoe UI", size = 18)) +
#   scale_fill_manual(values = pull(summarise(list_tidy[["df_avg_pivot"]], unique_colour = unique(Colour)))) +
#   scale_colour_manual(values = pull(summarise(list_tidy[["df_avg_pivot"]], unique_colour = unique(BorW))))

# Cairo::CairoWin()
# preproc_forcustomplot %>% ggplot(aes(
#   name,
#   value,
#   group = Athlete
# )) +
#   theme_bw() +
#   theme(text = element_text(family = "Segoe UI", size = 18)) +
#   # geom_text(aes(5.5, 18, label = "victoryu.co.uk"), size = 24, alpha = .002, family = "Segoe UI") +
#   # geom_point(colour = "black", size = 1.5) +
#   # geom_point(aes(colour = fct_reorder(Athlete, final_rank)), size = 1) +
#   # geom_line(colour = "black", size = 1.5) +
#   # geom_line(aes(colour = fct_reorder(Athlete, final_rank)), size = 1) +
#   geom_smooth((method='ggpchip', colour = "black", se = F, size = 1.5) +
#   geom_smooth((method='ggpchip', aes(colour = fct_reorder(Athlete, final_rank)), se = F, size = 1) +
#   geom_text(data = preproc_forcustomplot %>% filter(name == "1500m"), aes(label = Athlete), nudge_x = 0.2, hjust = 0, family = "Segoe UI", size = 6) +
#   scale_y_reverse() +
#   scale_x_discrete(expand = expansion(mult = c(.05, .2))) +
#   scale_color_manual(values = c(rev(colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(max(list_tidy[["df_rank"]]$X, na.rm = T))),
#                                 rep("white", nrow(list_tidy[["df_rank"]])-max(list_tidy[["df_rank"]]$X, na.rm = T))
#                                 ),
#                      guide = F) +
#   labs(y = "Rank",
#        x = "Event")

## The black ones are determined by initial row order (or factor after converting)