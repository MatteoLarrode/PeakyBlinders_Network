# ---- SCRAPING OF PEAKY BLINDERS TRANSCRIPTS FROM ALL SEASONS ----
# Main website souce: https://scriptmochi.com/tv-series/peaky-blinders

library(rvest)
library(tidyverse)

# ---- CREATE DATAFRAME OF LINKS ----
url <- "https://scriptmochi.com/tv-series/peaky-blinders"
webpage <- read_html(url)

# Initialize an empty data frame to store the results
peaky_blinders_txt <- data.frame(
  Season = character(), 
  Episode = numeric(), 
  Link = character(),
  stringsAsFactors = FALSE)

# Iterate over season blocks
seasons <- webpage  |> 
  html_elements(".session__block")

for (season_elem in seasons) {
  season_name <- gsub("session-", 
                      "Season ", 
                      season_elem |>  
                        html_attr("id"))
  
  episodes <- season_elem  |> 
    html_elements(".chapter__number")  |> 
    html_text() |> 
    as.numeric()
  
  links <- paste0("https://scriptmochi.com",
                  season_elem  |> 
                    html_elements(".chapter__link")  |> 
                    html_attr("href"))
  
  season_data <- data.frame(
    Season = season_name,
    Episode = episodes,
    Link = links,
    stringsAsFactors = FALSE
  )
  
  peaky_blinders_txt <- rbind(peaky_blinders_txt, season_data)
}

# ---- SCRAPE TRANSCRIPTS FROM LINKS ----
transcripts <- list()

for (link in peaky_blinders_txt$Link){
  url <- link
  episode_page <- read_html(url)
  
  transcript <- episode_page |>
    html_element(".content") |> 
    html_text()
  
  transcripts <- append(transcripts, transcript)
}

for (i in 1:36){
  peaky_blinders_txt$Transcripts[i] <- transcripts[[i]]
}

# ---- SAVE DATAFRAME AS A .RDA OBJECT ---
saveRDS(peaky_blinders_txt, "peaky_blinders_transcripts.rds")