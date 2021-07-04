match_reader <- function(match, team2, score){
  ## Isolate match info text
  match_start <- str_locate_all(match, pattern = paste(team2, "\n\n", score, sep = ""))
  match_start_n <- match_start[[1]][1,1]
  match_end <- str_locate_all(match, pattern = "\n\nManager: ")
  match_end_n <- match_end[[1]][1,1]
  match <- substr(match, match_start_n, match_end_n)
  
  match_start <- str_locate_all(match, pattern = "\n\ngoals\n\n")
  match_start_n <- match_start[[1]][1,2] + 1
  match_end_n <- str_length(match) - 1
  match <- substr(match, match_start_n, match_end_n)
  
  ## Determine who scored last in regulation
  pso_check <- str_locate_all(match, pattern = "\n\nPenalty shootout\n\n")
  ## Case #1: Match does go to a penalty shoot-out (PSO)
  if (length(pso_check[[1]]) != 0){
    goal_list_start_n <- 1
    goal_list_end_n <- pso_check[[1]][1,1]
    goal_list <- substr(match, goal_list_start_n, goal_list_end_n)
    
    goal_count <- str_locate_all(goal_list, pattern = " [0-9]{1,3}\\. / ")
    goal_minute_ends <- str_locate_all(goal_list, pattern = "[0-9]\\. / ")
    goal_count_n <- nrow(goal_count[[1]])
    if (goal_count_n == 0){
      last_goal_team <- 0
    } else {
      last_goal_number <- 0
      for (i in 1:goal_count_n){
        goal_i_min_start_n <- goal_count[[1]][i,1] + 1
        goal_i_min_end_n <- goal_minute_ends[[1]][i,1]
        goal_i_min <- as.integer(substr(goal_list, goal_i_min_start_n, goal_i_min_end_n))
        if (goal_i_min <= 90){
          last_goal_number <- i
        }
      }
    }
  } 
  ## Case #2: Match does not go to a penalty shoot-out (PSO)
  else {
    ## Calculate number of goals scored in match
    score_sep = str_locate_all(score, pattern = ":")
    score_sep_n <- score_sep[[1]][1,1]
    team1_goals <- as.integer(substr(score, 1, (score_sep_n - 1)))
    team2_goals <- as.integer(substr(score, (score_sep_n + 1), str_length(score)))
    goal_count_n <- team1_goals + team2_goals
    
    ## Isolate text with goals
    goal_list_start_n <- 1
    goal_list_end <- str_locate_all(match, pattern = "\n\n")
    goal_list_end_n <- goal_list_end[[1]][goal_count_n, 1]
    goal_list <- substr(match, goal_list_start_n, goal_list_end_n)
    
    goal_count <- str_locate_all(goal_list, pattern = " [0-9]{1,3}\\. / ")
    goal_minute_ends <- str_locate_all(goal_list, pattern = "[0-9]\\. / ")
    goal_count_n <- nrow(goal_count[[1]])
    if (goal_count_n == 0){
      last_goal_team <- 0
    } else {
      last_goal_number <- 0
      for (i in 1:goal_count_n){
        goal_i_min_start_n <- goal_count[[1]][i,1] + 1
        goal_i_min_end_n <- goal_minute_ends[[1]][i,1]
        goal_i_min <- as.integer(substr(goal_list, goal_i_min_start_n, goal_i_min_end_n))
        if (goal_i_min <= 90){
          last_goal_number <- i
        }
      }
    }
  }
  
  return(last_goal_number)
}