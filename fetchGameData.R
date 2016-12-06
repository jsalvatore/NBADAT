library("stattleshipR")
library("dplyr")
# library("RMySQL")
# 
# dbCon <- dbConnect(
#   MySQL(), host = Credentials$host,
#   port = 3306, user = Credentials$user, password = Credentials$password, dbname = "nba")

source("Credentials.R")

## Get your free token from www.stattleship.com
set_token(Credentials$token)



sport <- 'basketball'
league <- 'nba'
ep <- 'game_logs'
q_body <- list(since =  "2016-11-15")
gls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)
game_logs<-do.call('rbind', lapply(gls, function(x) x$game_logs))  

# dbWriteTable(dbCon, value = game_logs, name = "gamelogs", append = TRUE, row.names = FALSE) 



players<-do.call('rbind', lapply(gls, function(x) x$players))
colnames(players)[1] <- 'player_id'
players <- players[which(!duplicated(players$player_id)), ]

# dbWriteTable(dbCon, value = players, name = "players", append = TRUE, row.names = FALSE) 




teams<-do.call('rbind', lapply(gls, function(x) x$teams))
colnames(teams)[1] <- 'team_id'
teams$team <- paste(teams$name, teams$nickname)
teams <- teams[c("team_id", "team")]
teams <- teams[which(!duplicated(teams$team_id)), ]

# dbWriteTable(dbCon, value = teams, name = "teams", append = TRUE, row.names = FALSE) 





games<-do.call('rbind', lapply(gls, function(x) x$games))
colnames(games)[1] <- 'game_id'
games <- games[which(!duplicated(games$game_id)), ]
games$gameDate <- gsub(pattern = "on ", replacement = "", x = games$on)
games$gameDate <- as.Date(games$gameDate, format = "%B %d, %Y")
games$official_ids <- NULL

# dbWriteTable(dbCon, value = games, name = "games", append = TRUE, row.names = FALSE) 

# end





