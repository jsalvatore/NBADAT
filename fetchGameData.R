library(stattleshipR)
library(dplyr)
## Get your free token from www.stattleship.com
set_token("9ffe91d47c6b7c159965a8340a4d9106")



sport <- 'basketball'
league <- 'nba'
ep <- 'game_logs'
q_body <- list(status='ended', interval_type='regularseason')
gls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)  
game_logs<-do.call('rbind', lapply(gls, function(x) x$game_logs))  

players<-do.call('rbind', lapply(gls, function(x) x$players))
colnames(players)[1] <- 'player_id'
length(unique(players$player_id))
players <- players[which(!duplicated(players$player_id)), ]
players$team_id <- NULL

game_logs <- merge(players, game_logs, by='player_id')


teams<-do.call('rbind', lapply(gls, function(x) x$teams))
colnames(teams)[1] <- 'team_id'
teams$team_id
teams$team <- paste(teams$name, teams$nickname)
teams <- teams[c("team_id", "team")]
teams <- teams[which(!duplicated(teams$team_id)), ]


game_logs <- merge(teams, game_logs, by = c("team_id"))



games<-do.call('rbind', lapply(gls, function(x) x$games))
colnames(games)[1] <- 'game_id'
games$game_id
names(games)
games <- games[which(!duplicated(games$game_id)), ]
games$on
games$gameDate <- gsub(pattern = "on ", replacement = "", x = games$on)
games$gameDate <- as.Date(games$gameDate, format = "%B %d, %Y")
hist(games$gameDate, breaks = "days", format = "%m/%d/%Y")
table(games$gameDate)



game_logs <- merge(games, game_logs, by = c("game_id"))



game_logs <- game_logs %>% arrange(team_id, gameDate, player_id)

game_logs$fantasyPoints <- game_logs$points * 1 + 
game_logs$three_pointers_made * .5 + 
game_logs$rebounds_total * 1.25 + 
game_logs$assists * 1.5 + 
game_logs$steals * 2 + 
game_logs$blocks * 2 + 
game_logs$turnovers * -.5 + 
game_logs$double_double * 1.5 +
game_logs$triple_double * 3
describe(game_logs$fantasyPoints)

derozan <- game_logs[grep(pattern = "derozan", x = game_logs$last_name, ignore.case = T), ]

westbrook <- game_logs[grep(pattern = "westbrook", x = game_logs$last_name, ignore.case = T), ]
mean(westbrook$points)


game_logs <- game_logs %>% group_by(player_id) %>% arrange(gameDate) %>% mutate(gameNum = 1, 
                                                                   gameNum = cumsum(gameNum)
                                                                   )
game_logs$gameNum
hist(game_logs$gameNum)

hist(game_logs$fantasyPoints)

library(lme4)

names(game_logs)
game_logs$gameNum
model <- lmer(formula = fantasyPoints ~ points * assists * three_pointers_made
 *                gameNum + (1 | player_id), data = game_logs)
summary(model)
game_logs$predictedPoints <- predict(object = model, game_logs)

hist(game_logs$fantasyPoints - 
game_logs$predictedPoints)

game_logs$error <- game_logs$fantasyPoints - 
  game_logs$predictedPoints
options(scipen =999)
sqrt(mean(game_logs$error) ^ 2)


# project minutes
names(game_logs)
# build time lag model
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$timePlayPrevious[i] <- game_logs$time_played_total[i - 1]
  }
}
# build time lag model
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$timePlayPrevious[i] <- game_logs$time_played_total[i - 1]
  }
}


hist(game_logs$timePlayPrevious)


game_logs$time_played_total
MinutesModel <- lmer(formula = time_played_total ~ timePlayPrevious 
              +                gameNum + (1 | player_id), data = game_logs)
summary(MinutesModel)
game_logs$predictedMinutes <- predict(object = MinutesModel, game_logs)
hist(game_logs$predictedMinutes)
hist(game_logs$time_played_total)
hist(game_logs$time_played_total - game_logs$predictedMinutes)

game_logs$timePlayed <- game_logs$time_played_total / 60
game_logs$predictedMinutes <- game_logs$predictedMinutes / 60
hist(game_logs$timePlayed)
hist(game_logs$predictedMinutes)

game_logs$predictedMinutes


model <- lmer(formula = fantasyPoints ~ predictedMinutes + 
                (1 | player_id), data = game_logs)
summary(model)
game_logs$predictedPoints <- predict(object = model, game_logs)

hist(game_logs$fantasyPoints - 
       game_logs$predictedPoints)

game_logs$error <- game_logs$fantasyPoints - 
  game_logs$predictedPoints
options(scipen =999)
sqrt(mean(game_logs$error) ^ 2)


library("randomForest")
library("gbm")
set.seed(1237)
dat <- game_logs
# ## bagged regression
#
# drop things with lots of leveles
l <- lapply(dat, function(x) length(table(x)))
drop <- which(colnames(dat) %in% names(l[l > 1024 ]))
names(dat[(drop)])
bagged <- dat[,-drop]
#
#
l <- lapply(bagged, function(x) sum(is.na(x)) / NROW(x))
drop <- which(colnames(bagged) %in% names(l[l == 1]))
names(bagged[(drop)])
bagged <- bagged[,-drop]
#
l <- lapply(bagged, function(x) length(table(x)))
drop <- which(colnames(bagged) %in% names(l[l == 1]))
names(bagged[(drop)])

bagged <- bagged[,-drop]


names(bagged)


bagged$game_id <- NULL
bagged$created_at <- NULL
bagged$updated_at <- NULL
bagged$away_team_outcome.x <- NULL
bagged$broadcast <- NULL
bagged$daytime <- NULL
bagged$ended_at <- NULL
bagged$home_team_outcome.x <- NULL
bagged$label <- NULL
bagged$name.x <- NULL


bagged$game_id <- NULL
response_column <- which(colnames(bagged) == "fantasyPoints")
trainy <- bagged$fantasyPoints
gbm_formula <- as.formula(paste0("fantasyPoints ~ 1 +", paste(colnames(bagged[, -response_column]),
                                                    collapse = " + ")))
bagged <- bagged[which(!is.na(bagged$fantasyPoints)),]
set.seed(1234)
library("gbm")
gbm_model <- gbm(gbm_formula, data = bagged, distribution = "bernoulli", n.trees = 500,
                 shrinkage=.03,interaction.depth = 2,
                 bag.fraction = 0.5, train.fraction = .8, cv.folds = 0,
                 keep.data=TRUE, verbose=TRUE)

best.iter.01 <- gbm.perf(gbm_model,method="test");
best.iter.01;
model <- gbm_model;
iter <- best.iter.01
# #summarize the model
summary(gbm_model, n.trees=(best.iter.01+0))[1:40,]
#
#
# #function to create partial dependence plot
plot.gbm.2 <- function(mod=model.2,var="VarName",iter=100)
{
  a <- plot.gbm(mod,var,iter,return.grid=T)
  a[2] <- 1/(1+exp(-a[2]))
  plot(a,type="l")
  mtext(bquote(paste(delta," = ",.(round(max(a[2]) - min(a[2]),2)))) , 3)
}
plot.gbm.2(model,"number.of.historical.accounts",iter)



