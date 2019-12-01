dfs_score <- lapply(dfs_score, function(x) {
  x <- dplyr::ungroup(x)
  x <- x %>% left_join(athlete_info_tbl %>% select(Athlete, birth_date), by = "Athlete")
  x <- x %>% mutate(`Age at Comp` = time_length(difftime(x$date, x$birth_date), unit = "years") %>% round(1))
  x$birth_date <- NULL
  x <- x %>% select(Year, comp, date, Athlete, `Age at Comp`, everything())
  return(x)
})

dfs_score <- lapply(dfs_score, function(x) {
  x <- rename(x, "Country" = "countrycode")
  return(x)
})

dfs <- imap(dfs, function(original_df, y) {
  moo <- original_df %>% left_join((dfs_score[[y]] %>% select(Year, Athlete, Country, `Age at Comp`)), by = c("Year", "Athlete", "Country")) %>% select(Year, Athlete, `Age at Comp`, everything())
  return(moo)
}) ## bind year at comp from dfs_score, also helped to notice Ashton Eaton's + Frank Busemann's incorrect country code in dfs_score

# dfs %>% lapply(function(x) x %>% select(Athlete, Country)) %>% bind_rows() %>% unique() ## athlete and countrycode

dfs <- imap(dfs, function(original_df, y) {
  moo <- original_df %>% left_join((dfs_score[[y]] %>% select(Year, date) %>% unique()), by = "Year") %>% select(Year, date, Athlete, `Age at Comp`, everything())
  return(moo)
})

dfs <- lapply(dfs, function(x) {
  moo <- rename(x, "Date" = "date")
  return(moo)
})

dfs[["olympics"]][[which(dfs[["olympics"]]$Year == 2016 & dfs[["olympics"]]$Athlete == "Ashton Eaton"),"Age at Comp"]] <- 28.6
dfs[["gotzis"]][[which(dfs[["gotzis"]]$Year == 2000 & dfs[["gotzis"]]$Athlete == "Frank Busemann"),"Age at Comp"]] <- 25.3

write_rds(dfs, "./dfs.Rds")
write_rds(dfs_score, "./dfs_score.Rds")