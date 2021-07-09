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
  
  for (i in 1:match_count){
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
  
  ## Insert columns that show stats for team1's previous matches
  
  ## Add columns to be filled in
  ot$Team1_last_date <- NA
  ot$Team1_previous_matches <- 0
  ot$Team1_subs_per_game <- 0
  ot$Team1_sub_min_per_game <- 0
  ot$Team1_subs_previous <- 0
  ot$Team1_sub_mins_previous <- 0
  ot$Team1_subs_previous_3 <- 0
  ot$Team1_sub_mins_previous_3 <- 0
  
  for (i in 2:match_count){
    
    ## Create table of previous matches and sort by date
    columns_use <- c(4,8,16,17)
    matches_i_t1 <- filter(ot[1:(i-1),], Team1_ID == ot[i,3] & Comp == ot[i,7])[,columns_use]
    matches_i_t2 <- filter(ot[1:(i-1),], Team2_ID == ot[i,3] & Comp == ot[i,7])[,columns_use]
    matches_i <- rbind(matches_i_t1, matches_i_t2)
    matches_i <- matches_i[order(matches_i$Date),]
    
    ## Count number of matches played
    previous_matches_i <- nrow(matches_i)
    ot[i,25] <- previous_matches_i
    
    ## Compute subs and sub mins per game
    subs_total <- sum(matches_i$t1_subs)
    sub_mins_total <- sum(matches_i$t1_sub_mins)
    if (previous_matches_i != 0){
      ot[i,26] <- subs_total / previous_matches_i
      ot[i,27] <- sub_mins_total / previous_matches_i
    }
    
    ## Compute subs and sub mins in previous match
    if (previous_matches_i != 0){
      ot[i,28] <- matches_i[previous_matches_i,3]
      ot[i,29] <- matches_i[previous_matches_i,4]
    }
    
    ## Compute subs and sub mins in previous 3 matches
    if (previous_matches_i >= 3){
      ot[i,30] <- sum(matches_i[((previous_matches_i-2):previous_matches_i),3])
      ot[i,31] <- sum(matches_i[((previous_matches_i-2):previous_matches_i),4])
    }
    
    ## Determine last match played
    matches_dates_i <- matches_i[,1]
    ## Case 1: not team's first match of the competition
    if (previous_matches_i != 0){
      ot[i,24] <- matches_dates_i[previous_matches_i]
    }
  }
  ot$Team1_last_date <- as_date(ot$Team1_last_date)
  
  ## Insert columns that show stats for team2's previous matches
  
  ## Add columns to be filled in
  ot$Team2_last_date <- NA
  ot$Team2_previous_matches <- 0
  ot$Team2_subs_per_game <- 0
  ot$Team2_sub_min_per_game <- 0
  ot$Team2_subs_previous <- 0
  ot$Team2_sub_mins_previous <- 0
  ot$Team2_subs_previous_3 <- 0
  ot$Team2_sub_mins_previous_3 <- 0
  
  for (i in 2:match_count){
    
    ## Create table of previous matches and sort by date
    columns_use <- c(4,8,18,19)
    matches_i_t1 <- filter(ot[1:(i-1),], Team1_ID == ot[i,1] & Comp == ot[i,7])[,columns_use]
    matches_i_t2 <- filter(ot[1:(i-1),], Team2_ID == ot[i,1] & Comp == ot[i,7])[,columns_use]
    matches_i <- rbind(matches_i_t1, matches_i_t2)
    matches_i <- matches_i[order(matches_i$Date),]
    
    ## Count number of matches played
    previous_matches_i <- nrow(matches_i)
    ot[i,33] <- previous_matches_i
    
    ## Compute subs and sub mins per game
    subs_total <- sum(matches_i$t2_subs)
    sub_mins_total <- sum(matches_i$t2_sub_mins)
    if (previous_matches_i != 0){
      ot[i,34] <- subs_total / previous_matches_i
      ot[i,35] <- sub_mins_total / previous_matches_i
    }
    
    ## Compute subs and sub mins in previous match
    if (previous_matches_i != 0){
      ot[i,36] <- matches_i[previous_matches_i,3]
      ot[i,37] <- matches_i[previous_matches_i,4]
    }
    
    ## Compute subs and sub mins in previous 3 matches
    if (previous_matches_i >= 3){
      ot[i,38] <- sum(matches_i[((previous_matches_i-2):previous_matches_i),3])
      ot[i,39] <- sum(matches_i[((previous_matches_i-2):previous_matches_i),4])
    }
    
    ## Determine last match played
    matches_dates_i <- matches_i[,1]
    ## Case 1: not team's first match of the competition
    if (previous_matches_i != 0){
      ot[i,32] <- matches_dates_i[previous_matches_i]
    }
  }
  ot$Team2_last_date <- as_date(ot$Team2_last_date)
  
  ## Clean up table by removing unnecessary columns
  columns_remove <- c(1,2,3,9,10)
  ot <- ot[,-columns_remove]
  
  time_end <- Sys.time()
  print((time_end - time_start))
  return(ot)
}