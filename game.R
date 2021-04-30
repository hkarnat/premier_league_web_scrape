game <- function (seasonstart, seasonend, hteam, ateam) {
  title <- paste('https://www.worldfootball.net/report/premier-league-', seasonstart,'-', seasonend, '-', hteam, '-', ateam, '/', sep = "")
  
  if (hteam == "southampton-fc" & ateam == "west-ham-united" & seasonstart == "2017") {
    title <- 'https://www.worldfootball.net/report/premier-league-2017-2018-southampton-fc-west-ham-united_2/'
  }
  a <- read_html(title)
  date <- html_text2(html_nodes(a, "th:nth-child(2)"))
  score <- html_text(html_nodes(a, ".resultat"))
  home_team <- html_text(html_nodes(a, "th:nth-child(1) a"))
  away_team <- html_text(html_nodes(a, "th+ th a"))
  score_table <- html_text(html_nodes(a, '.e4~ .standard_tabelle td > b'))
  scorer <- html_text(html_nodes(a, '.e4~ .standard_tabelle td:nth-child(2) > a:nth-child(1)'))
  minute_assist <- html_text(html_nodes(a, '.standard_tabelle:nth-child(4) td:nth-child(2)'))
  stadium <- html_text(html_nodes(a, ".standard_tabelle tr:nth-child(1) .dunkel a"))
  attendance <- html_text(html_nodes(a, "br+ .standard_tabelle tr:nth-child(2) .dunkel~ .dunkel+ .dunkel"))
  referee <- html_text(html_nodes(a, "tr:nth-child(3) .dunkel~ .dunkel+ .dunkel a"))
  assistant1 <- html_text(html_nodes(a, "tr:nth-child(4) .dunkel~ .dunkel+ .dunkel a"))
  assistant2 <- html_text(html_nodes(a, "tr:nth-child(5) .dunkel~ .dunkel+ .dunkel a"))
  
  date <- str_split(date, "\n")
  date1 <- date[[1]][1]
  date2 <- date[[1]][2]
  date1 <- str_split(date1, ', ')
  date_full <- str_c(date1[[1]][2], " ", date2)
  date_full <- str_remove(date_full, " Clock")
  date11 <- as.POSIXct(date_full, format = "%d. %B %Y %H:%M")

  home_score <- as.numeric(gsub("([0-9]+):.*$", "\\1", score))
  away_score <- as.numeric(gsub(".*:([0-9]+).*$", "\\1", score))
  
  score_table1 <- as.data.frame(score_table)
  
  goaltime <- as.numeric(gsub("\\D", "", minute_assist)) 
  owngoal <- grepl("own goal", minute_assist)
  penalty <- grepl("penalty", minute_assist)
  assist <- unlist(str_replace_na(str_match_all(minute_assist, "(?<=\\().+?(?=\\))")))
  if(score_table == "none"){
    t1 <- NA
    t2 <- NA
    goaltime <- NA
    scorer <- NA
    assist <- NA
    owngoal <- NA
    goalinfo <- data.frame(t1, t2, goaltime, scorer, assist, owngoal)
  } else {
    sequence <- str_extract_all(score_table1$score_table, "\\d")
    sequence <- data.frame(matrix(unlist(sequence), nrow=length(sequence), byrow=TRUE))
    sequence <- sequence %>% rename(team1 = X1, team2 = X2)
    
    goalinfo <- data.frame(sequence, goaltime, scorer, assist, owngoal, penalty)
    
    goalinfo$team1 <- as.numeric(as.character(goalinfo$team1))
    goalinfo$team2 <- as.numeric(as.character(goalinfo$team2))
    goalinfo$goaltime <- as.integer(goalinfo$goaltime)
    goalinfo$scorer <- as.character(goalinfo$scorer)
    goalinfo$assist <- as.character(goalinfo$assist)
    
    goalinfo$assist[goalinfo$assist == "character(0)"] <- NA
  }
  
  attendance <- as.numeric(gsub("\\D", "", attendance))
  
  matchinfo <- list(date = date11, home_score = home_score, away_score = away_score, 
                    goalinfo = goalinfo, stadium = stadium, 
                    attendance = attendance, referee = referee, 
                    assistant1 = assistant1, assistant2 = assistant2)
  return(matchinfo)
}