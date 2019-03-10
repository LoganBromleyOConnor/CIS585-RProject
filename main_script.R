require(rvest)
require(jsonlite)

steamappid <- "294100"
url <- paste0('https://store.steampowered.com/appreviews/',steamappid,'?json=1&num_per_page=100&day_range=9223372036854775807&filter=all&language=english&review_type=all&purchase_type=all&start_offset=')
pages <- list()
i <- 0

while(i < 9223372036854775807){
  
  rawjson <- fromJSON(paste0(url,i))
  
  message("Retrieving review ", i)
  
  if (rawjson[["query_summary"]][["num_reviews"]] == 0)
  {
    message("Done")
    break
  }
  
  pages[[i+1]] <- rawjson$reviews
  i <- i+100
  
}
reviewdata_df <- rbind_pages(pages)
reviewdata_df<- unique(reviewdata_df)

negreview_df <- reviewdata_df[reviewdata_df$voted_up == FALSE,]
posreview_df <- reviewdata_df[reviewdata_df$voted_up == TRUE,]


