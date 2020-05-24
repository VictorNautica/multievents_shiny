ultimate_df_standardised_global <- list()

## For Previous Events Data tab, Z-Score 

ultimate_df_list[["ultimate_df_points"]] %>% mutate_at(vars(X100m:X1500m), scale) -> ultimate_df_standardised_global[["Z-Score"]]

ultimate_df_standardised_global[["Z-Score"]] <- ultimate_df_standardised_global[["Z-Score"]] %>% pivot_longer(cols = X100m:X1500m, names_to = "event", values_to = "score") %>% 
  mutate(min = case_when(event == "X100m" ~ min(ultimate_df_standardised_global[["Z-Score"]]$X100m),
                         event == "LJ" ~ min(ultimate_df_standardised_global[["Z-Score"]]$LJ),
                         event == "SP" ~ min(ultimate_df_standardised_global[["Z-Score"]]$SP),
                         event == "HJ" ~ min(ultimate_df_standardised_global[["Z-Score"]]$HJ),
                         event == "X400m" ~ min(ultimate_df_standardised_global[["Z-Score"]]$X400m),
                         event == "X110mh" ~ min(ultimate_df_standardised_global[["Z-Score"]]$X110mh),
                         event == "DT" ~ min(ultimate_df_standardised_global[["Z-Score"]]$DT),
                         event == "PV" ~ min(ultimate_df_standardised_global[["Z-Score"]]$PV),
                         event == "JT" ~ min(ultimate_df_standardised_global[["Z-Score"]]$JT),
                         event == "X1500m" ~ min(ultimate_df_standardised_global[["Z-Score"]]$X1500m)),
         max = case_when(event == "X100m" ~ max(ultimate_df_standardised_global[["Z-Score"]]$X100m),
                         event == "LJ" ~ max(ultimate_df_standardised_global[["Z-Score"]]$LJ),
                         event == "SP" ~ max(ultimate_df_standardised_global[["Z-Score"]]$SP),
                         event == "HJ" ~ max(ultimate_df_standardised_global[["Z-Score"]]$HJ),
                         event == "X400m" ~ max(ultimate_df_standardised_global[["Z-Score"]]$X400m),
                         event == "X110mh" ~ max(ultimate_df_standardised_global[["Z-Score"]]$X110mh),
                         event == "DT" ~ max(ultimate_df_standardised_global[["Z-Score"]]$DT),
                         event == "PV" ~ max(ultimate_df_standardised_global[["Z-Score"]]$PV),
                         event == "JT" ~ max(ultimate_df_standardised_global[["Z-Score"]]$JT),
                         event == "X1500m" ~ max(ultimate_df_standardised_global[["Z-Score"]]$X1500m))
  ) %>% 
  mutate_at(vars("event"), str_replace_all, "X", "") %>% 
  mutate_at(vars("event"), as_factor)

## Regular Points ####

ultimate_df_standardised_global[["Points"]] <- ultimate_df_list[["ultimate_df_points"]] %>% pivot_longer(cols = X100m:X1500m, names_to = "event", values_to = "score") %>% 
  mutate(min = case_when(event == "X100m" ~ min(ultimate_df_list[["ultimate_df_points"]]$X100m),
                         event == "LJ" ~ min(ultimate_df_list[["ultimate_df_points"]]$LJ),
                         event == "SP" ~ min(ultimate_df_list[["ultimate_df_points"]]$SP),
                         event == "HJ" ~ min(ultimate_df_list[["ultimate_df_points"]]$HJ),
                         event == "X400m" ~ min(ultimate_df_list[["ultimate_df_points"]]$X400m),
                         event == "X110mh" ~ min(ultimate_df_list[["ultimate_df_points"]]$X110mh),
                         event == "DT" ~ min(ultimate_df_list[["ultimate_df_points"]]$DT),
                         event == "PV" ~ min(ultimate_df_list[["ultimate_df_points"]]$PV),
                         event == "JT" ~ min(ultimate_df_list[["ultimate_df_points"]]$JT),
                         event == "X1500m" ~ min(ultimate_df_list[["ultimate_df_points"]]$X1500m)),
         max = case_when(event == "X100m" ~ max(ultimate_df_list[["ultimate_df_points"]]$X100m),
                         event == "LJ" ~ max(ultimate_df_list[["ultimate_df_points"]]$LJ),
                         event == "SP" ~ max(ultimate_df_list[["ultimate_df_points"]]$SP),
                         event == "HJ" ~ max(ultimate_df_list[["ultimate_df_points"]]$HJ),
                         event == "X400m" ~ max(ultimate_df_list[["ultimate_df_points"]]$X400m),
                         event == "X110mh" ~ max(ultimate_df_list[["ultimate_df_points"]]$X110mh),
                         event == "DT" ~ max(ultimate_df_list[["ultimate_df_points"]]$DT),
                         event == "PV" ~ max(ultimate_df_list[["ultimate_df_points"]]$PV),
                         event == "JT" ~ max(ultimate_df_list[["ultimate_df_points"]]$JT),
                         event == "X1500m" ~ max(ultimate_df_list[["ultimate_df_points"]]$X1500m))
  ) %>% 
  mutate_at(vars("event"), str_replace_all, "X", "") %>% 
  mutate_at(vars("event"), as_factor)
