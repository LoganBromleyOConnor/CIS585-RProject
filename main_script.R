#Required packages
require(rvest)
require(jsonlite)
require(tm)
require(ggwordcloud)
require(dplyr)
require(anytime)
require(tidytext)
require(hunspell)
#-----------------------------

#Getting data

steamappid <- "294100" #ID of the game/app in Steam
url <- paste0('https://store.steampowered.com/appreviews/',steamappid,'?json=1&filter=all&language=english&day_range=9223372036854775807&num_per_page=100&purchase_type=all&start_offset=')
storeurl <- read_html(paste0("https://store.steampowered.com/app/",steamappid))
gamename <- tolower(storeurl %>% html_nodes("div.apphub_AppName") %>% html_text())
gamename <- strsplit(gamename, " ")[[1]]
pages <- list() #The list that the raw JSON will be stored in
i <- 0 #The starting offset value for the API request
amount <- 3000 #amount of reviews
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
message("Done")
message(numofentries, " reviews with the oldest being from ", anytime(tail(reviewdata_df$timestamp_created, 1)) )

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

badpos <- hunspell(posreview_df$review) #Getting list of misspelled words to remove
badneg <- hunspell(negreview_df$review)
badpos <- unlist(badpos)
badneg <- unlist(badneg)

badpos <- iconv(badpos, 'UTF-8', 'ASCII')
badneg <- iconv(badneg, 'UTF-8', 'ASCII')

rm(posreview_df, negreview_df) #Removing unused data to clear up memory

posreviews <- VCorpus(VectorSource(posreviews)) #Creating Corpus of the reviews
negreviews <- VCorpus(VectorSource(negreviews))

# Gathering list of misspelled words that will be cleaned and combined to be used later
badneg <- removeNumbers(badneg)
badpos <- removeNumbers(badpos)

badpos <- removePunctuation(badpos)
badneg <- removePunctuation(badneg)

badpos <- removeWords(badpos, "")
badneg <- removeWords(badneg, "")

badpos <- unique(badpos)
badneg <- unique(badneg)

badwords <- c(badpos, badneg)

badwords <- unique(badwords)
# End of misspelled word cleaning

clean_corpus <- function(corpus, badwords){  #Function to clean the corpus 
  corpus <- tm_map(corpus, stripWhitespace) #Getting rid of unwanted white space
  corpus <- tm_map(corpus, removePunctuation) #Getting rid of punctuation
  corpus <- tm_map(corpus, content_transformer(tolower)) #making all characters lowercase
  corpus <- tm_map(corpus, removeWords, c("game", "review", "like", "can", "theres", 
                                          "just", "even", "games", "really", "1010", "dont",
                                          "will", "ive", "one", "along", "doesnt", "well", 
                                          "much", "many", "also", "lets", "isnt", "good", "bad", 
                                          "not", "get", "great", "play", "playing", "played", "early",
                                          "access", "b", "u")) #List of unwanted words
  corpus <- tm_map(corpus, removeWords, stopwords("en")) #Removing stopwords
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, gamename)
  
  #Removing words that are misspelled
  #Lists larger than 2000 can break the removewords function
  #To fix the problem we do the removal in chunks of 2000 if the list is larger than 2000
  
  if (length(badwords) > 2000){
    s <- 1
    e <- 2000
    
    while (anyNA(badwords[s:e] == FALSE)){ 
      
      corpus <- tm_map(corpus, removeWords, tolower(badwords[s:e]))
      
      if (anyNA(badwords[s:e]) == TRUE) break #If the function is calling values in the list that don't exist, break the loop
      
      s <- s+2000 #Moving to next set of words
      e <- e+2000
      
    }
    
  }
  
  else{
    
    corpus <- tm_map(corpus, removeWords, tolower(badwords)) #If there is less than 2000 words remove them normally
    
  }
  

  return(corpus) #Returning the cleaned Corpus
  
}

posreviews <- clean_corpus(posreviews, badwords) #Applying the corpus function to the review corpuses
negreviews <- clean_corpus(negreviews, badwords)


find_freq_words <- function(corpus){ #Funcion to find the frequency of each word
  
  BigramTokenizer <-
    function(x)
      unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  
  dtm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
  
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
  ggwordcloud(wordfreq$word, wordfreq$freq, min.freq = wc_min_freq, max.words = wc_max_words, shape = "pentagon")
  
}

poswordcloud <- create_wordcloud(posreviewfreq, wc_min_freq, wc_max_words) #Creating word cloud
poswordcloud #Displaying word cloud

negwordcloud <- create_wordcloud(negreviewfreq, wc_min_freq, wc_max_words)
negwordcloud

