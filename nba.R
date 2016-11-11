# list of addresses for raw data.
addressList <- list(pullup_address = "http://stats.nba.com/js/data/sportvu/pullUpShootData.js", 
                    drives_address = "http://stats.nba.com/js/data/sportvu/drivesData.js", 
                    defense_address = "http://stats.nba.com/js/data/sportvu/defenseData.js", 
                    passing_address = "http://stats.nba.com/js/data/sportvu/passingData.js", 
                    touches_address = "http://stats.nba.com/js/data/sportvu/touchesData.js", 
                    speed_address = "http://stats.nba.com/js/data/sportvu/speedData.js", 
                    rebounding_address = "http://stats.nba.com/js/data/sportvu/reboundingData.js", 
                    catchshoot_address = "http://stats.nba.com/js/data/sportvu/catchShootData.js", 
                    shooting_address = "http://stats.nba.com/js/data/sportvu/shootingData.js")

addressList <- list(
  # pullup_address = "http://stats.nba.com/js/data/sportvu/pullUpShootData.js"
                    # drives_address = "http://stats.nba.com/js/data/sportvu/drivesData.js"
                    # defense_address = "http://stats.nba.com/js/data/sportvu/defenseData.js" 
                    # passing_address = "http://stats.nba.com/js/data/sportvu/passingData.js" 
                    # touches_address = "http://stats.nba.com/js/data/sportvu/touchesData.js" 
                    # speed_address = "http://stats.nba.com/js/data/sportvu/speedData.js"
                    # rebounding_address = "http://stats.nba.com/js/data/sportvu/reboundingData.js" 
                    catchshoot_address = "http://stats.nba.com/js/data/sportvu/2014-15/catchShootData.js" 
                    # shooting_address = "http://stats.nba.com/js/data/sportvu/shootingData.js"
)

readLines("http://stats.nba.com/js/data/sportvu/2016/catchShootData.json" )

#sportsvu player endpoints
# http://stats.nba.com/js/data/sportvu/{season}/drivesData.json
# http://stats.nba.com/js/data/sportvu/{season}/defenseData.json
# 
# http://stats.nba.com/js/data/sportvu/{season}/speedData.json
# http://stats.nba.com/js/data/sportvu/{season}/shootingData.json
# http://stats.nba.com/js/data/sportvu/{season}/reboundingData.json
# http://stats.nba.com/js/data/sportvu/{season}/pullUpShootData.json
# http://stats.nba.com/js/data/sportvu/{season}/touchesData.json
# http://stats.nba.com/js/data/sportvu/{season}/passingData.json



readIt <- function(address) {
  web_page <- readLines(address)
  
  ## regex to strip javascript bits and convert raw to csv format
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl = TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl = TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl = TRUE)
  x4 <- gsub(";", ",", x3, perl = TRUE)
  
  # read the resulting csv with read.table()
  nba <- read.table(textConnection(x4), header = T, sep = ",", skip = 2, stringsAsFactors = FALSE)
  return(nba)
}

df_list <- lapply(addressList, readIt)
# 
# x1 <- gsub("var \\w+Data = (.*);", "\\1", web_page, perl = TRUE)
# x2 <- fromJSON(x1)  #from rjson package


head(df_list$pullup_address)
head(df_list$drives_address)
head(df_list$defense_address)
head(df_list$passing_address)
head(df_list$touches_address)
head(df_list$speed_address)
head(df_list$rebounding_address)
head(df_list$catchshoot_address)
head(df_list$shooting_address)



install.packages("devtools")
devtools::install_github("stattleship/stattleship-r")

## Load the stattleshipR package
library(httr)
library(stattleshipR)

## Get your free token from www.stattleship.com
set_token("9ffe91d47c6b7c159965a8340a4d9106")
"18005568172"


sport <- 'basketball'
league <- 'nba'
ep <- 'game_logs'
q_body <- list(team_id='nba-bos', status='ended', interval_type='regularseason')
gls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)  
game_logs<-do.call('rbind', lapply(gls, function(x) x$game_logs))  
# colnames(game_logs)        


"http://analyticsplaybook.org/api/stattleship.html"

sport <- 'basketball'
league <- 'nba'
ep <- 'game_logs'  
q_body <- list(team_id='nba-bos')

pls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)  
players<-do.call('rbind', lapply(pls, function(x) x$players))

colnames(players)[1] <- 'player_id'
game_logs <- merge(players, game_logs, by='player_id')

game_logs$birth_date <- as.Date(game_logs$birth_date)
hist(game_logs$birth_date, breaks = "year", format = "%b-%Y")


league <- "nba"
sport <- "basketball"
ep <- "players"
q_body <- list()
players <- ss_get_result(sport = sport, league = league, ep = ep,
                         query = q_body, version = 1, walk = TRUE)
players_df <- do.call("rbind", lapply(players, function(x) x$players))


pls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)  
players<-do.call('rbind', lapply(pls, function(x) x$players))

colnames(players)[1] <- 'player_id'
game_logs <- merge(players, game_logs, by='player_id')

# TODO bring the calculations for points


