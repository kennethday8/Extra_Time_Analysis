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
  
  ## Insert column that shows whether match is group stage or knockout
  matches$Knockout <- 0
  for (i in 1:matches_count){
    group_check <- str_locate_all(matches[i,5], pattern = "Group")
    if (nrow(group_check[[1]]) == 0){
      matches[i,8] <- "KO"
    } else {
      matches[i,8] <- "GS"
    }
  }
  
  ## Insert column that shows whether the match when to a penalty shoot-out
  matches$pso <- 0
  for (i in 1:matches_count){
    pso_check <- str_locate_all(matches[i,4], pattern = "pso")
    if (nrow(pso_check[[1]]) == 0){
      matches[i,9] <- "no pso"
    } else {
      matches[i,9] <- "pso"
    }
  }
  
  ## Insert column that shows whether the match when to a extra time
  matches$extra_time <- 0
  for (i in 1:matches_count){
    aet_check <- str_locate_all(matches[i,4], pattern = "aet")
    pso_check <- str_locate_all(matches[i,4], pattern = "pso")
    if ((nrow(aet_check[[1]]) + nrow(pso_check[[1]])) == 0){
      matches[i,10] <- "regulation"
    } else {
      matches[i,10] <- "extra time"
    }
    
    ## Fix for Copa America matches where the is a pso but no extra time
    et_years <- 2011
    et_final_years <- c(2011, 2015, 2016)
    
    ## Case 1: non-final matches at Copa America played in years without extra time
    if (matches[i,7] == "Copa America" & matches[i,5] != "Final" & year(matches[i,1]) != et_years){
      matches[i,10] <- "regulation"
    }
    ## Case 2: final matches at Copa America played in years without extra time
    else if (matches[i,7] == "Copa America" & matches[i,5] == "Final" & !is.element(year(matches[i,1]), et_final_years)){
      matches[i,10] <- "regulation"
    }
  }
  
  ## Insert column that shows which team won; change score column to just show final score
  matches$winner <- 0
  for (i in 1:matches_count){
    score_i <- matches[i,4]
    score1_text <- str_locate_all(score_i, pattern = "[0-9]:")
    score1_start_n <- 1
    score1_end_n <- score1_text[[1]][1,2] - 1
    score1 <- as.integer(substr(score_i, score1_start_n, score1_end_n))
    
    score2_text <-str_locate_all(score_i, pattern = ":[0-9]{1,2} ")
    score2_start_n <- score2_text[[1]][1,1] + 1
    score2_end_n <- score2_text[[1]][1,2] - 1
    score2 <- as.integer(substr(score_i, score2_start_n, score2_end_n))
    
    if (score1 > score2){
      matches[i,11] <- 1
    }
    else if (score2 > score1){
      matches[i,11] <- 2
    }
    
    matches[i,4] <- paste(score1, ":", score2, sep ="")
  }
  
  ## Change comp column to include year as well
  matches$Comp <- paste(matches$Comp, year(matches$Date), sep = " ")
  
  return(matches)
}