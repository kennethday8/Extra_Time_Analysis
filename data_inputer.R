## Fn creates usable dataframe from match source data from website
data_inputer <- function(file_name){
  
  ## Read in file with the table of info for the matches
  matches <- read.csv(file_name, sep = "\t")
  
  ## Remove unnecessary columns (ie. time and "-" columns)
  matches <- matches[, -c(2,4)]
  
  ## Fill in missing dates so they match the previous entry's date
  matches_count <- nrow(matches)
  for (i in 2:matches_count){
    if (matches[i,1] == ""){
      matches[i,1] <- matches[(i - 1), 1]
    }
  }
  
  ## Fix dates so they have the same format
  matches$Date <- as.Date(matches$Date, "%d/%m/%Y")
  
  return(matches)
}