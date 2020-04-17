library(xml2)

athlete_info <- list()

scrap_function <- function(athlete_name){
  
  starting_name <- stringi::stri_trans_general(athlete_name, "latin-ascii")
  
  initial_url <- paste0("https://www.iaaf.org/athletes/search?query=", starting_name, "&disciplineCode=DEC")
  initial_search_page <- try(xml2::read_html(initial_url))
  if(class(initial_search_page)[1] == "try-error") {
    cat(athlete_name, "name failed, reattempting with + in whitespace\n")
    initial_search_page <- try(xml2::read_html(str_replace_all(initial_url, " ", "+")))
  }
  
  rawnodes <- initial_search_page %>% rvest::html_nodes("table td")
  
  rawnodes_children <- rawnodes %>% rvest::html_children()
  rawnodes_text <- rawnodes %>% rvest::html_text(trim = T) %>% stringi::stri_trans_general("latin-ascii")
  
  name_split <- as_vector(strsplit(starting_name, " ", fixed = T))
  number <- which(sapply(rawnodes_text, function(x)
    grepl(name_split[1], x, ignore.case = T) &
      grepl(name_split[length(name_split)], x, ignore.case = T)))
  
  if (length(number) != 0){
    athlete_info[[athlete_name]] <<- list()
    athlete_info[[athlete_name]][["birth_date"]] <<- rawnodes_text[(number+4)] %>% as.Date("%d %B %Y")
    partial_link <- xml_attrs(rawnodes_children[(number%/%5)*2+1][[1]])
    full_link <- paste0("https://www.iaaf.org", partial_link)
  }
  
  athlete_page <- xml2::read_html(full_link)
  image_rip <- athlete_page %>% rvest::html_nodes(".profiles") %>% rvest::html_attrs()
  
  ## IAAF Code ####
  athlete_info[[athlete_name]][["iaaf_code"]] <<- str_extract(athlete_page %>% rvest::html_nodes("._label--athlete-id") %>% rvest::html_text(), "[:digit:]{6,8}")
  #################
  
  athlete_info[[athlete_name]][["image_url"]] <<- (if (isTRUE(RCurl::url.exists(
    image_rip[[1]][["data-bind"]] %>% str_extract("https://(.*?)\\.jpg")
  ))) {
    image_rip[[1]][["data-bind"]] %>% str_extract("https://(.*?)\\.jpg")
  }
  else {
    "https://www.worldathletics.org/Areas/Competitions/Assets/Images/layout/profile-not-found.png"
  })

}

list_of_athletes <- lapply(dfs, function(x) x$`Athlete`) %>% unlist(use.names = F) %>% unique() %>% sort()

error_list <- list()

for (athlete in list_of_athletes) {
  tryCatch({scrap_function(athlete)},
          error = function(x){
            cat("ERROR:", athlete, "\n")
              error_list[[athlete]] <<- TRUE
            }
  )
}

athlete_info %>% keep(~ .x$birth_date %>% rlang::is_na())
athlete_info[["Nils Büker"]][["birth_date"]] <- as.Date("1986-12-12")
athlete_info[["Thomas Walser"]][["birth_date"]] <- as.Date("1978-02-09")
athlete_info %>% keep(~ length(.x$birth_date) != 1)
athlete_info[["Felipe dos Santos"]][["birth_date"]] <- as.Date("1994-07-30")
athlete_info[["Óscar González"]][["birth_date"]] <- as.Date("1976-08-08")
athlete_info[["Philipp Huber"]][["birth_date"]] <- as.Date("1974-02-18")
athlete_info %>% keep(~ length(.x$iaaf_code) != 1)
athlete_info %>% keep(~ .x$iaaf_code %>% rlang::is_na())
athlete_info %>% keep(~ length(.x$image_url) != 1)
athlete_info %>% keep(~ .x$image_url %>% rlang::is_na())
athlete_info[["Thomas Walser"]][["iaaf_code"]] <- NA

error_fix <- function(name, dob, code, profile_url) {
  athlete_info[[name]] <<- list()
  athlete_info[[name]][["birth_date"]] <<- as.Date(dob)
  athlete_info[[name]][["iaaf_code"]] <<- code
  athlete_info[[name]][["image_url"]] <<- profile_url
}

error_fix("Jake Arnold",
          "1984-01-03",
          "14231563",
          "https://media.aws.iaaf.org/athletes/205596.jpg")
error_fix("Yordanis García",
          "1988-11-21",
          "14169554",
          "https://media.aws.iaaf.org/athletes/208433.jpg")
error_fix("Xaver Weibel",
          "1980-01-21",
          "14226662",
          "https://www.worldathletics.org/Areas/Competitions/Assets/Images/layout/profile-not-found.png")
error_fix("Volodymyr Mykhailenko",
          "1973-08-27",
          "14230299",
          "https://www.worldathletics.org/Areas/Competitions/Assets/Images/layout/profile-not-found.png")
error_fix("Prodromos Korkizoglou",
          "1975-02-27",
          "14195248",
          "https://www.worldathletics.org/Areas/Competitions/Assets/Images/layout/profile-not-found.png")
error_fix("Nikolai Tishchenko",
          "1977-02-04",
          "14224271",
          "https://www.worldathletics.org/Areas/Competitions/Assets/Images/layout/profile-not-found.png")
error_fix("Mikalai Shubianok",
          "1985-05-04",
          "14176563",
          "https://www.worldathletics.org/Areas/Competitions/Assets/Images/layout/profile-not-found.png")
error_fix("Luiggy Llanos",
          "1978-08-22",
          "14218821",
          "https://www.worldathletics.org/Areas/Competitions/Assets/Images/layout/profile-not-found.png")
error_fix("Junior Diaz",
          "1987-04-28",
          "14169609",
          "https://www.worldathletics.org/Areas/Competitions/Assets/Images/layout/profile-not-found.png")
error_fix("Andrei Krauchanka",
          "1986-01-04",
          "14176795",
          "https://media.aws.iaaf.org/athletes/196475.jpg")
error_fix("Aliaksandr Parkhomenka",
          "1981-03-21",
          "14176667",
          "https://media.aws.iaaf.org/athletes/189824.jpg")
error_fix("Aleksandr Yurkov",
          "1975-07-21",
          "14230808",
          "https://www.worldathletics.org/Areas/Competitions/Assets/Images/layout/profile-not-found.png")

athlete_info_second <- athlete_info

athlete_info_tbl <- athlete_info %>% lapply(as_tibble) %>% bind_rows(.id = "Athlete")
athlete_info_tbl <- left_join(athlete_info_tbl, (dfs %>% lapply(function(x) x %>% select(Athlete, Country)) %>% bind_rows() %>% unique()), by = "Athlete") %>% select(Athlete, Country, everything())

athlete_info_tbl$Country %<>% countrycode::countrycode("ioc", "country.name")
write_rds(athlete_info_tbl, "athlete_info_tbl.Rds")


## athlete_info[["Bryan Clay"]][["birth_date"]] %>% as.Date("%d %B %Y")

## for 30 athletes..
## rawnodes 150 == raw html [1] Name [2] Discipline [3] Sex [4] Country [5] DOB
## rawnodes_atts_all 150  == attributes ( e.g. "Sex", "Name", "Disciplines")
## rawnodes_children 60 == [1] url and <span class="name-uppercase">EATON</span>, Ashton</a>, [2] Flag Indicator
## rawnodes_text 150 == rawnodes information