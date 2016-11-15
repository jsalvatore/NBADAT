library("Hmisc")
library("RMySQL")

dbCon <- dbConnect(
  MySQL(), host = Credentials$host,
  port = 3306, user = Credentials$user, password = Credentials$password, dbname = "nba")

game_logs <- dbGetQuery(dbCon, "SELECT * FROM gamelogs;")
players <- dbGetQuery(dbCon, "SELECT * FROM players;")
teams <- dbGetQuery(dbCon, "SELECT * FROM teams;")
games <- dbGetQuery(dbCon, "SELECT * FROM games;")


dat <- merge(players, game_logs, by='player_id')
dat$team_id <- dat$team_id.x
dat <- merge(teams, dat, by = c("team_id"))
dat <- merge(games, dat, by = c("game_id"))

dat$gameDate <- as.Date(dat$gameDate)
hist(dat$gameDate, breaks = "days", format = "%m/%d/%Y")
table(dat$gameDate)





dat <- dat %>% arrange(team_id, gameDate, player_id)

dat$fantasyPoints <- dat$points * 1 + 
  dat$three_pointers_made * .5 + 
  dat$rebounds_total * 1.25 + 
  dat$assists * 1.5 + 
  dat$steals * 2 + 
  dat$blocks * 2 + 
  dat$turnovers * -.5 + 
  dat$double_double * 1.5 +
  dat$triple_double * 3
describe(dat$fantasyPoints)



dat <- dat %>% group_by(player_id) %>% arrange(gameDate) %>% mutate(gameNum = 1, 
                                                                                gameNum = cumsum(gameNum)
)
dat$gameNum
hist(dat$gameNum)

hist(dat$fantasyPoints)

table(players$position_abbreviation)

dat %>% filter(gameDate == Sys.Date() - 1) %>% arrange(-fantasyPoints) %>%
  select(first_name, last_name, position_abbreviation, fantasyPoints)







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