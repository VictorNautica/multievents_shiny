dfs_joined <- imap(dfs_joined, ~ { 
    
  
  merged_df <- left_join(
  dfs[[.y]] %>% bind_rows(),
  dfs_score[[.y]] %>% 
    bind_rows() %>% 
    rename("Date" = "date") %>% 
    select(Year, Date, Athlete, `100m`:`1500m`),
  by = c("Year", "Date", "Athlete"),
  suffix = c("_Points", "_Score")
) %>% select(
  Year,
  Date,
  Athlete,
  `Age at Comp`,
  Country,
  Rank,
  `Final Score`,
  `100m_Score`,
  `100m_Points`,
  `LJ_Score`,
  `LJ_Points`,
  `SP_Score`,
  `SP_Points`,
  `HJ_Score`,
  `HJ_Points`,
  `400m_Score`,
  `400m_Points`,
  `110mh_Score`,
  `110mh_Points`,
  `DT_Score`,
  `DT_Points`,
  `PV_Score`,
  `PV_Points`,
  `JT_Score`,
  `JT_Points`,
  `1500m_Score`,
  `1500m_Points`) %>% as.data.frame()
  
  return(merged_df)
  }
)

dfs_joined %<>% map( ~ {
  df <- .x
  df %<>% mutate(`1500m_ScoreMMSS` = {
    foo <- seconds_to_period(`1500m_Score`)
    use <- sprintf('%02d:%.2f', minute(foo), second(foo))
    
    use <-
      use %>% map_chr(~ if (.x %>% str_detect(":[:digit:]{1}\\.")) {
        str_replace(.x, ":", ":0")
      } else {
        .x
      })
    
    use <-
      use %>% map_chr( ~ if (.x %>% str_detect("^0[:digit:]{1}:")) {
        str_replace(.x, "^0", "")
      } else {
        .x
      })
    
    use
  })
  df %>% select(-"1500m_Points", everything())
})
