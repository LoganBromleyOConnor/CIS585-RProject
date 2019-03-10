#Required packages

require(rvest)
require(jsonlite)

#-----------------------------

#Getting data

steamappid <- "294100" #ID of the game/app in Steam
url <- paste0('https://store.steampowered.com/appreviews/',steamappid,'?json=1&num_per_page=100&day_range=9223372036854775807&filter=all&language=english&review_type=all&purchase_type=all&start_offset=')
pages <- list() #The list that the raw JSON will be stored in
i <- 0 #The starting offset value for the API request

while(i < 9223372036854775807){
  
  rawjson <- fromJSON(paste0(url,i)) #Getting JSON
  
  message("Retrieving review ", i)
  
  if (rawjson[["query_summary"]][["num_reviews"]] == 0) #Stopping the loop when there are no reviews left
  {
    message("Done")
    break
  }
  
  pages[[i+1]] <- rawjson$reviews #Storing JSON in list
  i <- i+100 #Moving to next set of reviews
  
}
#-----------------------------

#Organizing data
reviewdata_df <- rbind_pages(pages) #Turning list into a data frame
reviewdata_df<- unique(reviewdata_df)#Removing any duplicate entries that might have snuck in

negreview_df <- reviewdata_df[reviewdata_df$voted_up == FALSE,] #Data frame of negative reviews
posreview_df <- reviewdata_df[reviewdata_df$voted_up == TRUE,] #Data frame of positive reviews
#-----------------------------
