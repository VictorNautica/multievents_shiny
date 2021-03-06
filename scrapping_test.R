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

athlete_info_tbl$Athlete <- athlete_info_tbl$Athlete %>% str_replace_all("Zachery Ziemek", "Zach Ziemek")

athlete_info_tbl <- athlete_info_tbl %>% arrange(Athlete)

fix_athlete_idx <- which(
  ultimate_df_list[["ultimate_df_points"]] %>% pull(Athlete) %>% unique() %>% sort() !=
    athlete_info_tbl$Athlete) ## compare
# 1 Edgars Erinš     
# 2 Janis Karlivans  
# 3 Jirí Ryba        
# 4 Pawel Wiesiolek  
# 5 Slaven Dizdarevic
# 6 Tomáš Dvorák   


athlete_info_tbl[fix_athlete_idx,"Athlete"] <- ultimate_df_list[["ultimate_df_points"]] %>% pull(Athlete) %>% unique() %>% sort() %>% .[fix_athlete_idx]

athlete_info_tbl$Height <- NA

# readr::write_rds(athlete_info_tbl, "athlete_info_tbl.Rds")
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Adam Helcelet"), "Height"] <- 190
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Ashton Eaton"), "Height"] <- 185
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Bryan Clay"), "Height"] <- 178
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Ashley Bryant"), "Height"] <- 178
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Kévin Mayer"), "Height"] <- 186
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Keisuke Ushiro"), "Height"] <- 196
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Attila Zsivoczky"), "Height"] <- 193
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Tom Pappas"), "Height"] <- 196
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Trey Hardee"), "Height"] <- 196
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Cedric Dubler"), "Height"] <- 191
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Dmitriy Karpov"), "Height"] <- 198
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Aleksandr Pogorelov"), "Height"] <- 201
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Arthur Abele"), "Height"] <- 184
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Karl Robert Saluri"), "Height"] <- 178
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Maicel Uibo"), "Height"] <- 194
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Qi Haifeng"), "Height"] <- 191
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Oleksiy Kasyanov"), "Height"] <- 191
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Akihiko Nakamura"), "Height"] <- 180
athlete_info_tbl[which(athlete_info_tbl$Athlete == "André Niklaus"), "Height"] <- 189
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Dean Macay"), "Height"] <- 196
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Tomáš Dvořák"), "Height"] <- 186
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Chris Huffins"), "Height"] <- 189
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Erki Nool"), "Height"] <- 184
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Massimo Bertocchi"), "Height"] <- 196
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Hans Van Alphen"), "Height"] <- 191
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Jaakko Ojaniemi"), "Height"] <- 193
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Jānis Karlivāns"), "Height"] <- 193
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Zsolt Kürtösi"), "Height"] <- 188
athlete_info_tbl[which(athlete_info_tbl$Athlete == "Hamdi Dhouibi"),"Country"] <- "Tunisia"

## Finally functionalise this ####

fix_athlete_profile <- function(Athlete, Column, Value) athlete_info_tbl[which(athlete_info_tbl$Athlete == Athlete),Column] <<- Value

fix_athlete_profile("Agustín Félix", "image_url", "https://1.bp.blogspot.com/_WfKuaRVqTzo/TCM_n21MYYI/AAAAAAAAjP8/6Xo2-aDHdsM/s1600/Agust%C3%ADn+F%C3%A9lix+representar%C3%A1+al+equipo+espa%C3%B1ol..jpg")
fix_athlete_profile("Aki Heikkinen", "image_url", "https://media.gettyimages.com/photos/finlands-aki-heikkinen-picture-id650922848?s=612x612")
fix_athlete_profile("Alberto Juantorena", "image_url", "https://www3.pictures.gi.zimbio.com/IAAF+World+Athletics+Championship+Day+7+9qNfBzUFPS-x.jpg")
fix_athlete_profile("Aleksandr Pogorelov", "image_url", "https://media.gettyimages.com/photos/aleksandr-pogorelov-of-russia-competes-during-the-1500-metres-in-the-picture-id71631698?s=612x612")
fix_athlete_profile("Aleksandr Shtepa", "image_url", "https://media.gettyimages.com/photos/aleksandr-pogorelov-of-russia-competes-during-the-1500-metres-in-the-picture-id71631698?s=612x612")
fix_athlete_profile("Aleksandr Yurkov", "image_url", "https://media.gettyimages.com/photos/aug-2001-oleksandr-yurkov-of-ukraine-throwing-the-discus-during-the-picture-id604368?s=2048x2048")
fix_athlete_profile("Aleksey Drozdov", "image_url", "https://c8.alamy.com/comp/GCKKKM/athletics-iaaf-european-championships-2010-day-two-olympic-stadium-GCKKKM.jpg")
fix_athlete_profile("Aleksey Sysoyev", "image_url", "https://lh3.googleusercontent.com/proxy/ng7QNUWCRnvu56NZnvvqROkw2XThGtnJnrzsKpCZ22TLnbjeKHM8cugP00BS5glaasRlQibCgNchI85jtQ0oszlGdIusERZdM9nBWkUP-YXRGuvaqx_JKhOxw_M")

## Write ####

readr::write_rds(athlete_info_tbl, "athlete_info_tbl.Rds")


## athlete_info[["Bryan Clay"]][["birth_date"]] %>% as.Date("%d %B %Y")

## for 30 athletes..
## rawnodes 150 == raw html [1] Name [2] Discipline [3] Sex [4] Country [5] DOB
## rawnodes_atts_all 150  == attributes ( e.g. "Sex", "Name", "Disciplines")
## rawnodes_children 60 == [1] url and <span class="name-uppercase">EATON</span>, Ashton</a>, [2] Flag Indicator
## rawnodes_text 150 == rawnodes information