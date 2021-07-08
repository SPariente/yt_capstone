##########DATA GATHERING - FOR INFORMATION##########

#Initialisation
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(httr)) install.packages("httr")
if(!require(jsonlite)) install.packages("jsonlite")
if(!require(rvest)) install.packages("rvest")
if(!require(lubridate)) install.packages("lubridate")

library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(lubridate)

API_key <- #removed for privacy purposes
  
API_url <- "https://youtube.googleapis.com/youtube/v3/"

#Create a list containing random strings for searching
B <- 120
rwords_set <- replicate(B, {
  n <- sample(1:5, 1)
  OpenRepGrid::randomWords(n)
})

strings_set <- lapply(rwords_set, function(rwords) {
  paste(rwords, collapse = "%20")
})

strings_set <- unique(strings_set)
length(strings_set)


#Recursively go through the created strings list
api_extract <- sapply(strings_set, function(rstring) {
  
  #Get a tibble of relevant data based on the video ID, for videos published after 2021-01-01  
  get_url <- paste0(API_url,
                    "search?part=snippet&maxResults=50&publishedAfter=2020-01-01T00%3A00%3A00Z&q=",
                    rstring,
                    "&regionCode=US&relevanceLanguage=en&key=",
                    API_key)
  res_url <- GET(get_url)
  res_url_JSON <- fromJSON(rawToChar(res_url$content))
  
  #Get a tibble of video IDs and relevant information (publication time, channel ID, age in days)
  video_ids <- tibble(video_id = as.character(res_url_JSON$items$id[,2]), 
                      time = as.Date(res_url_JSON$items$snippet$publishTime), 
                      channel = as.character(res_url_JSON$items$snippet$channelId),
                      age = as.integer(today() - time))
  
  #Convert the video IDs into a string with the required separator
  video_ids_str <- paste(video_ids$video_id, collapse = "&id=")
  
  #Convert the channel IDs into a string with the required separator
  video_ch_str <- paste(video_ids$channel, collapse = "&id=")
  
  #Get a tibble of video elements (name, views, likes, dislikes, number of comments, topics, description)
  get_video <- paste0(API_url,
                      "videos?part=snippet&part=statistics&part=topicDetails&id=",
                      video_ids_str,
                      "&key=",
                      API_key)
  res_video <- GET(get_video)
  res_video_JSON <- fromJSON(rawToChar(res_video$content))
  video_stats <- tibble(
    video_id = as.character(res_video_JSON$items$id),
    title = as.character(res_video_JSON$items$snippet$title),
    views = as.integer(res_video_JSON$items$statistics$viewCount),
    likes = as.integer(res_video_JSON$items$statistics$likeCount),
    dislikes = as.integer(res_video_JSON$items$statistics$dislikeCount),
    comments = as.integer(res_video_JSON$items$statistics$commentCount),
    topics_vid = as.list(res_video_JSON$items$topicDetails$topicCategories),
    description = as.character(res_video_JSON$items$snippet$description),
    category_id = as.integer(res_video_JSON$items$snippet$categoryId)
  )
  
  #Get a tibble of channel stats (subscribers, number of videos, number of views, topics)
  get_channel <- paste0(API_url,
                        "channels?part=statistics&part=status&part=topicDetails&id=",
                        video_ch_str,
                        "&key=",
                        API_key)
  res_channel <- GET(get_channel)
  res_channel_JSON <- fromJSON(rawToChar(res_channel$content))
  channel_stats <- tibble(
    channel = as.character(res_channel_JSON$items$id),
    subscribers = as.integer(res_channel_JSON$items$statistics$subscriberCount),
    nb_videos = as.integer(res_channel_JSON$items$statistics$videoCount),
    topics_ch = as.list(res_channel_JSON$items$topicDetails$topicCategories)
  )
  
  #Join the data
  left_join(video_ids, video_stats, by = "video_id") %>%
    left_join(., channel_stats, by = "channel")
})


#Create a combined tibble of results
extraction_data <- tibble()

for (i in 1:ncol(api_extract)) {
  extraction_data <- bind_rows(extraction_data, api_extract[,i])
}
extraction_data

#Tidy data, by excluding NA results and removing duplicates
extraction_data <- extraction_data %>% 
  filter(!is.na(video_id) & 
           !is.na(channel) & 
           !is.na(title) & 
           !is.na(views) & 
           !is.na(likes) & 
           !is.na(dislikes) & 
           !is.na(comments) & 
           !is.na(subscribers)) %>% 
  distinct(video_id, .keep_all = TRUE)

#Combine with previously saved data
ifelse(file.exists("combined_data.RData"), {
  load(file = "combined_data.RData")
  save(combined_data, 
       file = paste0("raw_data\\combined_data", 
                     as.numeric(Sys.time()), 
                     ".RData"))
  combined_data <- anti_join(combined_data, extraction_data, by = "video_id") %>% 
    mutate(time = as.Date(time))
},
combined_data <- tibble())


combined_data <- bind_rows(combined_data, extraction_data)

#Save data
save(combined_data, file = "combined_data.RData")
rm(api_extract, extraction_data)

