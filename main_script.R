require(rvest)
require(jsonlite)

steamappid <- "294100"

url <- paste('https://store.steampowered.com/appreviews/',steamappid,'?json=1', sep = '')
reviewoffset <- "&start_offset="
reviewoffsetvalue <- 0
reviewloopbool = FALSE

  while(reviewloopbool = FALSE)
  {
    newurl <- paste(url,)
    
    
  }




reviewdata_raw <- read_html(url)

reviewdata_raw <- reviewdata_raw %>% html_nodes('body') %>% html_nodes('p') %>% html_text()
reviewdata_df <- fromJSON(reviewdata_raw)
reviewdata_df
reviewdata_summary <- reviewdata_df[["query_summary"]]
reviewdata_df <- reviewdata_df[["reviews"]]
reviewdata_summary
reviewdata_df
