## Fn inputs fifa world rankings into usable data frame
library(plyr)

rankings_inputer <- function(rankings_file){
  
  ## Read in rankings file
  rankings <- read.csv(rankings_file, sep = ",")
  
  ## Remove unnecessary columns (ie. previous points and rank change)
  rankings <- rankings[, -c(6,7)]
  
  ## Convert points and rankings columns to integers
  rankings$rank <- as.integer(rankings$rank)
  rankings$total_points <- as.integer(rankings$total_points)
  
  ## Create table for sum of each set of rankings & merge with rankings table
  rankings_sums <- ddply(rankings, .(rank_date), summarise, Sum_Points = sum(total_points))
  rankings <- merge(rankings, rankings_sums, by = "rank_date")
  
  ## Insert column that normalizes to sum of rankings
  rankings$points_norm <- (rankings$total_points * 1000) / rankings$Sum_Points
  
  return(rankings)
}