library(janitor)
library(dplyr)
library(tidyr)
library(stringr)

#480 Project Data cleaning
tags<-read.csv("Tags.csv")
links<-read.csv("Links.csv")
movies<-read.csv("Movies.csv")
ratings<-read.csv("Ratings.csv")

sum(is.na(movies))
sum(is.na(tags))
sum(is.na(links))
sum(is.na(ratings))

genre_list <- strsplit(as.character(movies$genres), split = "\\|")
unique_genres <- unique(unlist(genre_list, use.names = FALSE))
binary_genres <- t(sapply(genre_list, function(e) unique_genres %in% e))
mode(binary_genres) <- "integer"
colnames(binary_genres) <- unique_genres
movies <- cbind(movies[1:3], binary_genres)

movies$title<-removeNumbers(as.character(movies$title))
movies$title<-removePunctuation(as.character(movies$title))
write.csv(movies,"~/movie_cleaned.csv",row.names = FALSE)

