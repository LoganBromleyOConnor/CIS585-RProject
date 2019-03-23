#Required packages
require(rvest)
require(jsonlite)
require(tm)
require(ggwordcloud)
require(dplyr)
#-----------------------------

#Getting data

steamappid <- "730" #ID of the game/app in Steam
url <- paste0('https://store.steampowered.com/appreviews/',steamappid,'?json=1&filter=all&language=english&num_per_page=100&start_offset=')
pages <- list() #The list that the raw JSON will be stored in
i <- 0 #The starting offset value for the API request
amount <- 1000 #amount of reviews
numofentries <- 0

while(numofentries < amount){
  
  rawjson <- fromJSON(paste0(url,i)) #Getting JSON
  
  message("Retrieving review ", numofentries, " out of ", amount, ".")
  
  if (rawjson[["query_summary"]][["num_reviews"]] == 0) #Stopping the loop when there are no reviews left
  {
    message("Done")
    break
  }
  
  pages[[i+1]] <- rawjson$reviews #Storing JSON in list
  reviewdata_df <- rbind_pages(pages) #Turning list into a data frame
  reviewdata_df <- reviewdata_df %>% distinct(recommendationid, .keep_all = TRUE)#Removing any duplicate entries that might have snuck in
  numofentries <- length(reviewdata_df$recommendationid)
  
  i <- i+100 #Moving to next set of reviews
  
}

#-----------------------------
#Organizing data

negreview_df <- reviewdata_df[reviewdata_df$voted_up == FALSE,] #Data frame of negative reviews
posreview_df <- reviewdata_df[reviewdata_df$voted_up == TRUE,] #Data frame of positive reviews
#-----------------------------
rm(pages, rawjson, reviewdata_df) #Removing unused data to clear up memory

#Text mining reviews

posreviews <- posreview_df$review #Getting only the review part of the data frame
negreviews <- negreview_df$review
posreviews <- iconv(posreviews, 'UTF-8', 'ASCII') #Removing emojis or other non-text characters
negreviews <- iconv(negreviews, 'UTF-8', 'ASCII')

rm(posreview_df, negreview_df) #Removing unused data to clear up memory

posreviews <- VCorpus(VectorSource(posreviews)) #Creating Corpus of the reviews
negreviews <- VCorpus(VectorSource(negreviews))

clean_corpus <- function(corpus){  #Function to clean the corpus 
  
  corpus <- tm_map(corpus, stripWhitespace) #Getting rid of unwanted white space
  corpus <- tm_map(corpus, removePunctuation) #Getting rid of punctuation
  corpus <- tm_map(corpus, content_transformer(tolower)) #making all characters lowercase
  corpus <- tm_map(corpus, removeWords, c("game", "review", "like", "can", "theres", 
                                          "just", "even", "games", "really", "1010", "dont",
                                          "will", "ive", "one", "along", "doesnt", "well", 
                                          "much", "many", "also", "lets", "isnt", "good", "bad", 
                                          "not", "get", "great", "play", "playing", "played")) #List of unwanted words
  corpus <- tm_map(corpus, removeWords, stopwords("en")) #Removing stopwords
  return(corpus) #Returning the cleaned Corpus
}


posreviews <- clean_corpus(posreviews) #Applying the corpus function to the review corpuses
negreviews <- clean_corpus(negreviews)

find_freq_words <- function(corpus){ #Funcion to find the frequency of each word
  
  dtm <- TermDocumentMatrix(corpus) #Turning the corpus into a TermDocumentMatrix
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v) #Creating a data frame of the terms and their frequencies
  return(d) #Returning data frame
  
}

posreviewfreq <- find_freq_words(posreviews) #Applying frequency function 
negreviewfreq <- find_freq_words(negreviews)

wc_min_freq <- 5 #Variable to adjust the minimum frequency required to be displayed on the word cloud
wc_max_words <- 30 #Variable to adjust the maxium amount of words to be displayed on the word cloud

create_wordcloud <- function(wordfreq, wc_min_freq, wc_max_words){ #Function to create word cloud
  
  set.seed(78)
  ggwordcloud(wordfreq$word, wordfreq$freq, min.freq = wc_min_freq, max.words = wc_max_words)
  
}

poswordcloud <- create_wordcloud(posreviewfreq, wc_min_freq, wc_max_words) #Creating word cloud
poswordcloud #Displaying word cloud

negwordcloud <- create_wordcloud(negreviewfreq, wc_min_freq, wc_max_words)
negwordcloud
