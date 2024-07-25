# loading packages

library(EGAnet)
library(zoo)
# library(TSEntropies)

# helper functions 

populism <- function(gps, i){
# Function to extract populism data from GPS dataset
# Args:
#   gps: GPS dataset
#   i: index of the party in urls dataframe
# Returns:
#   Populism score of the given party
  p <- urls$party[i]
  return(gps$Type_Populism[gps$CPARTYABB==p][1])
}



clean.csv <- function(df, k, face = 0, tmax = 240, transformation = 'none', neutral = T){
#' Clean a dataframe of facial expression data extracted from videos
#'
#' This function cleans a dataframe of facial expression data extracted from videos, by selecting a subset of columns, applying a transformation to the data, and adding a column with a populism score.
#'
#' @param df A dataframe of facial expression data extracted from videos
#' @param k An integer representing the index of the party in the urls dataframe
#' @param face An integer representing the index of the face in the video
#' @param tmax An integer representing the maximum number of time points to include in the cleaned dataframe
#' @param transformation A string representing the type of transformation to apply to the data, either 'none', 'additive', or 'center'
#' @param neutral A boolean indicating whether to include the neutral expression column in the cleaned dataframe
#' @return A cleaned dataframe of facial expression data extracted from videos
#' @export
  df <- df[,-1]
  temp <-  df[1:tmax,(face*8+2):(face*8+8)]
  temp[,8] <- rep(k, nrow(temp))
  colnames(temp) <- c('anger', 'disgust','fear','happiness','sadness','surprise','neutral','id')
  if(transformation == 'additive'){
    for(i in 1:nrow(temp))
    {
      if(sum(is.na(temp))==0)
      {
      temp[i,1:6] <- addlogratio(row = temp[i,])
      }
    }
    temp <- temp[,-7]
    # temp$populism <- rep(populism(gps,k), nrow(temp))
  }
  else if(transformation == 'center'){
    for(i in 1:nrow(temp))
    {
      if(sum(is.na(temp))==0){
      temp[i, 1:7] <- centlogratio(row = temp[i,])
      }
    }
    if (!neutral) {
      temp <- temp[-7]
    }
  }
  temp$populism <- rep(populism(gps,k), nrow(temp))
  return(temp)
}


get_max_id <- function(path){
#' Get the maximum ID from a directory of files
#'
#' This function takes a directory path as input and searches for files with a specific pattern.
#' It extracts the numeric values from the filenames and returns the maximum value.
#'
#' @param path The path to the directory containing the files
#' @return The maximum numeric value extracted from the filenames
#' @examples
#' get_max_id("/path/to/files")
  files <- list.files(path = path, pattern = "video-\\d+-300\\.csv$", full.names = TRUE)

  x_values <- as.numeric(gsub("video-(\\d+)-300\\.csv$", "\\1", basename(files)))

  max_x <- max(x_values, na.rm = TRUE)

  return(max_x)
}

get_empty_rows <- function(df){
  # get fraction of rows which have at least one NA value
  return(sum(rowSums(is.na(df)) > 0) / nrow(df))
}
