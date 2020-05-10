## For Previous Events Data tab

ultimate_df_list[["ultimate_df_points"]] %>% mutate_at(vars(X100m:X1500m), scale) -> ultimate_df_standardised_global

ultimate_df_standardised_global <- ultimate_df_standardised_global %>% pivot_longer(cols = X100m:X1500m, names_to = "event", values_to = "score") %>% 
  mutate(min = case_when(event == "X100m" ~ min(ultimate_df_standardised_global$X100m),
                         event == "LJ" ~ min(ultimate_df_standardised_global$LJ),
                         event == "SP" ~ min(ultimate_df_standardised_global$SP),
                         event == "HJ" ~ min(ultimate_df_standardised_global$HJ),
                         event == "X400m" ~ min(ultimate_df_standardised_global$X400m),
                         event == "X110mh" ~ min(ultimate_df_standardised_global$X110mh),
                         event == "DT" ~ min(ultimate_df_standardised_global$DT),
                         event == "PV" ~ min(ultimate_df_standardised_global$PV),
                         event == "JT" ~ min(ultimate_df_standardised_global$JT),
                         event == "X1500m" ~ min(ultimate_df_standardised_global$X1500m)),
         max = case_when(event == "X100m" ~ max(ultimate_df_standardised_global$X100m),
                         event == "LJ" ~ max(ultimate_df_standardised_global$LJ),
                         event == "SP" ~ max(ultimate_df_standardised_global$SP),
                         event == "HJ" ~ max(ultimate_df_standardised_global$HJ),
                         event == "X400m" ~ max(ultimate_df_standardised_global$X400m),
                         event == "X110mh" ~ max(ultimate_df_standardised_global$X110mh),
                         event == "DT" ~ max(ultimate_df_standardised_global$DT),
                         event == "PV" ~ max(ultimate_df_standardised_global$PV),
                         event == "JT" ~ max(ultimate_df_standardised_global$JT),
                         event == "X1500m" ~ max(ultimate_df_standardised_global$X1500m))
  ) %>% 
  mutate_at(vars("event"), str_replace_all, "X", "") %>% 
  mutate_at(vars("event"), as_factor)


  df_selected <- ultimate_df_standardised_global %>% ggplot(aes(event)) +
  geom_hline(yintercept = 0, linetype = "dotdash") +
  geom_linerange(aes(ymin = min, ymax = max)) +
  labs(y = "z-score",
       x = "Event") +
  scale_y_continuous(breaks = seq(-5, 4, 1)) +
  scale_x_discrete(limits = rev(levels(ultimate_df_standardised_global$event))) +
  coord_flip()

  k <- df_selected %>% plotly::ggplotly() %>%
    layout(title = list(text = "Warning: More than 10 athletes selected",
                 font = list(color = "red",
                             size = 12)))
