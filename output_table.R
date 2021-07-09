source("match_reader.R")

## Fn creates final output table (ot)
output_table <- function(matches, rankings, country_ids, comp_rankings){
  time_start <- Sys.time()
  ot <- matches
  match_count <- nrow(ot)
  
  ## Use match link to read info related to last goal and subs
  ot$last_goal <- 0
  ot$t1_subs <- 0
  ot$t1_sub_mins <- 0
  ot$t2_subs <- 0
  ot$t2_sub_mins <- 0
  
  for (i in 1:5){
    match_link <- ot[i,6]
    match_input <- gettxt(match_link)
    match_output <- match_reader(match_input, ot[i,3], ot[i,4])
    
    ot[i,12] <- match_output[1]
    ot[i,13] <- match_output[2]
    ot[i,14] <- match_output[4]
    ot[i,15] <- match_output[3]
    ot[i,16] <- match_output[5]
  }
  
  ## Insert columns for country id and FIFA rankings
  
  ## Remove unnecessary FIFA-country column from country_ids
  country_ids <- country_ids[,-2]
  
  ot <- merge(ot, country_ids, by.x = "Team1", by.y = "country", all.x = TRUE, sort = FALSE)
  colnames(ot)[17] <- "Team1_ID"
  
  ot <- merge(ot, country_ids, by.x = "Team2", by.y = "country", all.x = TRUE, sort = FALSE)
  colnames(ot)[18] <- "Team2_ID"
  
  ## Add column for date of FIFA rankings to be used
  ot <- merge(ot, comp_rankings, by.x = "Comp", by.y = "Comp", all.x = TRUE, sort = FALSE)
  
  ## Rearange columns
  ot <- ot[, c(4,3,2,1,5:19)]
  
  ## Add in ranking and normalized points for Team1; rename columns
  ot <- merge(ot, rankings, by.x = c("Team1_ID", "FIFA.Rankings"), by.y = c("id", "rank_date"), all.x = TRUE)[,c(1:20,26)]
  colnames(ot)[20:21] <- c("Team1_rank", "Team1_pts_norm")
  
  ## Add in ranking and normalized points for Team2; rename columns
  ot <- merge(ot, rankings, by.x = c("Team2_ID", "FIFA.Rankings"), by.y = c("id", "rank_date"), all.x = TRUE)[,c(1:22,28)]
  colnames(ot)[22:23] <- c("Team2_rank", "Team2_pts_norm")
  
  ## Reorder by match date and reset row index
  ot <- ot[order(ot$Date),]
  rownames(ot) <- NULL
  
  ## Insert column that shows number of days since last match for team 1
  ot$Team1_last_match <- NA
  for (i in 2:match_count){
    matches_i_t1 <- filter(ot[1:(i-1),], Team1_ID == ot[i,3] & Comp == ot[i,7])[,4]
    matches_i_t2 <- filter(ot[1:(i-1),], Team2_ID == ot[i,3] & Comp == ot[i,7])[,4]
    matches_i <- c(matches_i_t1, matches_i_t2)
    ## Case 1: not team's first match of the competition
    if (length(matches_i) != 0){
      ot[i,24] <- sort(matches_i)[length(matches_i)]
    }
  }
  ot$Team1_last_match <- as_date(ot$Team1_last_match)
  
  ## Insert column that shows number of days since last match for team 2
  ot$Team2_last_match <- NA
  for (i in 2:match_count){
    matches_i_t1 <- filter(ot[1:(i-1),], Team1_ID == ot[i,1] & Comp == ot[i,7])[,4]
    matches_i_t2 <- filter(ot[1:(i-1),], Team2_ID == ot[i,1] & Comp == ot[i,7])[,4]
    matches_i <- c(matches_i_t1, matches_i_t2)
    ## Case 1: not team's first match of the competition
    if (length(matches_i) != 0){
      ot[i,25] <- sort(matches_i)[length(matches_i)]
    }
  }
  ot$Team2_last_match <- as_date(ot$Team2_last_match)
  
  time_end <- Sys.time()
  print((time_end - time_start))
  return(ot)
}