library("Hmisc")
library("RMySQL")
library(dplyr)

# dbCon <- dbConnect(
#   MySQL(), host = Credentials$host,
#   port = 3306, user = Credentials$user, password = Credentials$password, dbname = "nba")
# 
# game_logs <- dbGetQuery(dbCon, "SELECT * FROM gamelogs;")
# players <- dbGetQuery(dbCon, "SELECT * FROM players;")
# teams <- dbGetQuery(dbCon, "SELECT * FROM teams;")
# games <- dbGetQuery(dbCon, "SELECT * FROM games;")


dat <- merge(players, game_logs, by='player_id')
dat$team_id <- dat$team_id.x
dat <- merge(teams, dat, by = c("team_id"))
dat <- merge(games, dat, by = c("game_id"))

dat$gameDate <- as.Date(dat$gameDate)





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



dat <- dat %>% group_by(player_id) %>% mutate(lastGameNum = max(gameNum, na.rm = T))



dat$Name <- paste(dat$first_name, dat$last_name)
dat$Name <- ifelse(dat$Name == "Moe Harkless", "Maurice Harkless", dat$Name)
dat$Name <- ifelse(dat$Name == "Larry Nance", "Larry Nance Jr.", dat$Name)
dat$minutesPlayed <- dat$time_played_total / 60
dat$pointsPerMinute <- dat$fantasyPoints / dat$minutesPlayed


# lag
# pull out the last game 
lastGame <- dat %>% group_by(player_id) %>% filter(gameNum == lastGameNum)

dat <- dat %>% group_by(player_id) %>% filter(gameNum < lastGameNum) %>%
  mutate(
  difference = minutesPlayedPrevious - minutesPlayed,
  avgVar = mean(difference, na.rm = T),
  fpPerMinute = mean(sum(fantasyPoints) / sum(minutesPlayed))
  )
avgStats <- dat %>% group_by(player_id) %>% select(avgVar, fpPerMinute, player_id)
avgStats <- avgStats[which(!duplicated(avgStats$player_id)), ]
lastGame <- merge(lastGame, avgStats, by = "player_id")


hist(dat$fpPerMinute[dat$fpPerMinute > 0])



# hold out
set.seed(123)
smp_size <- floor(0.65 * nrow(dat))
train_cases <- sample(seq_len(nrow(dat)), size = smp_size)
train <- dat[train_cases, ]
test <- dat[-train_cases, ]


library(lme4)

model <- lmer(formula = minutesPlayed ~ minutesPlayedPrevious + I(minutesPlayedPrevious^2) + 
                game_started + game_started*minutesPlayedPrevious +  avgVar  + I(avgVar^2) 
              +  (1 | player_id)
              # + (1 | gameNum)  
              , data = train)
summary(model)

test$predictedMinutes <- predict(object = model, test, allow.new.levels = TRUE)
# test$predictedMinutes - test$minutesPlayed
cor(test$predictedMinutes, test$minutesPlayed, use = "complete.obs")

plot(test$predictedMinutes, test$minutesPlayed)

test$PredictedPoints <- test$predictedMinutes * test$fpPerMinute
plot(test$PredictedPoints, test$fantasyPoints)
cor(test$PredictedPoints, test$fantasyPoints, use = "complete.obs")
cor(test$PredictedPoints, test$fantasyPoints, use = "complete.obs") ^ 2
sqrt(mean(test$predictedMinutes - test$minutesPlayed, na.rm = T)^2)
sqrt(mean(test$PredictedPoints - test$fantasyPoints, na.rm = T)^2)
# > sqrt(mean(test$predictedMinutes - test$minutesPlayed, na.rm = T)^2)
# [1] 1.052033
# > sqrt(mean(test$PredictedPoints - test$fantasyPoints, na.rm = T)^2)
# [1] 2.397914

# now test on the hold out


lastGame$predictedMinutes <- predict(object = model, lastGame, allow.new.levels = TRUE)

lastGame$PredictedPoints <- lastGame$predictedMinutes * lastGame$fpPerMinute

plot(lastGame$PredictedPoints, lastGame$fantasyPoints)
cor(lastGame$PredictedPoints, lastGame$fantasyPoints, use = "complete.obs")

plot(lastGame$PredictedPoints, lastGame$PredictedPoints - lastGame$fantasyPoints)




# 
# model <- lmer(formula = minutesPlayed ~ minutesPlayedPrevious + I(minutesPlayedPrevious^2) + 
#                 game_started + avgVar  + I(avgVar^2) + salary 
#               +  (1 | player_id)
#               # + (1 | gameNum)  
#               , data = test)
# summary(model)


salaries <- read.csv("~/Downloads/DKSalaries (4).csv")
dat1 <- merge(lastGame, salaries, by = c("Name"))

lastGame %>% filter(Name %in% playersChosen$Name) %>% select(Name, minutesPlayed, predictedMinutes, 
                                                    fantasyPoints, PredictedPoints, 
                                                    gameNum, 
                                                    gameDate
                                                    )














dat1 %>% filter(C == 1) %>% arrange(-PredictedPoints) %>% mutate(efficency =  Salary / PredictedPoints) %>%
  select(Name, minutesPlayedPrevious, fantasyPoints, predictedMinutes, PredictedPoints, 
         avgPointsPerMinute, Salary, efficency)
dat1 %>% filter(PF == 1) %>% arrange(-PredictedPoints) %>% mutate(efficency =  Salary / PredictedPoints) %>%
  select(Name, minutesPlayedPrevious, fantasyPoints, predictedMinutes, PredictedPoints, 
         avgPointsPerMinute, Salary, efficency )
dat1 %>% filter(SF == 1) %>% arrange(-PredictedPoints) %>% mutate(efficency =  Salary / PredictedPoints) %>%
  select(Name, minutesPlayedPrevious, fantasyPoints, predictedMinutes, PredictedPoints, 
         avgPointsPerMinute, Salary, efficency )
dat1 %>% filter(SG == 1) %>% arrange(-PredictedPoints)%>% mutate(efficency =  Salary / PredictedPoints) %>%
  select(Name, minutesPlayedPrevious, fantasyPoints, predictedMinutes, PredictedPoints, 
         avgPointsPerMinute, Salary, efficency )
dat1 %>% filter(PG == 1) %>% arrange(-PredictedPoints)%>% mutate(efficency =  Salary / PredictedPoints) %>%
  select(Name, minutesPlayedPrevious, fantasyPoints, predictedMinutes, PredictedPoints, 
         avgPointsPerMinute, Salary, efficency )


dat1 %>% filter(F == 1) %>% arrange(-PredictedPoints)%>% mutate(efficency =  Salary / PredictedPoints) %>%
  select(Name, team, minutesPlayedPrevious, fantasyPoints, predictedMinutes, PredictedPoints, 
         avgPointsPerMinute, Salary, efficency )


dat1 %>% filter(G == 1) %>% arrange(-PredictedPoints)%>% mutate(efficency =  Salary / PredictedPoints) %>%
  select(Name, minutesPlayedPrevious, fantasyPoints, predictedMinutes, PredictedPoints, 
         avgPointsPerMinute, Salary, efficency )



dat1 %>% filter(Salary <= 6000) %>% arrange(-PredictedPoints)%>% 
  select(Name, minutesPlayedPrevious, fantasyPoints, predictedMinutes, PredictedPoints, 
         avgPointsPerMinute, Salary )

library("lpSolve") 
library(data.table) 
#setup data
df <- fread("basketball_data.csv")
mm <- cbind(model.matrix(as.formula("FP~Pos"), df)[,2:5], ifelse(df$Pos == "C", 1, 0), df$Salary, df$FP)
colnames(mm) <- c("pf", "pg", "sf", "sg", "c", "salary", 'fp')


dat1$Position

#setup solver
mm <- t(mm)
obj <- df$FP
dir <- c('=', '=', '=', '=', '=', '<=', '<=')

x <- 20000
vals <- c()
ptm <- proc.time()
for(i in 1:1000){
  rhs <- c(2, 2, 2, 2, 1, 60000, x)
  lp <- lp(direction = 'max',
           objective.in = obj,
           all.bin = T,
           const.rhs = rhs,
           const.dir = dir,
           const.mat = mm)
  vals <- c(vals, lp$objval)
  x <- lp$objval - 0.00001
}
proc.time() - ptm


players$Name <- paste(players$first_name, players$last_name)
players$Name <- ifelse(players$Name == "Moe Harkless", "Maurice Harkless", players$Name)
players$Name <- ifelse(players$Name == "Larry Nance", "Larry Nance Jr.", players$Name)


salaries <- read.csv("C:/Users/joe/Downloads/DKSalaries.csv")
head(salaries)


test <- merge(players, salaries, by = c("Name"))
salaries$check <- salaries$Name %in% test$Name 


predictions <- merge(lastGameTest, salaries, by = c("Name"))
predictions$C <- ifelse(grepl(pattern = "C", x = predictions$Position), 1, 0)
predictions$PF <- ifelse(grepl(pattern = "PF", x = predictions$Position), 1, 0)
predictions$PG <- ifelse(grepl(pattern = "PG", x = predictions$Position), 1, 0)
predictions$SG <- ifelse(grepl(pattern = "SG", x = predictions$Position), 1, 0)
predictions$SF <- ifelse(grepl(pattern = "SF", x = predictions$Position), 1, 0)
predictions$G <- ifelse(grepl(pattern = "G", x = predictions$Position), 1, 0)
predictions$F <- ifelse(grepl(pattern = "F", x = predictions$Position), 1, 0)
predictions$U <- 1



predictions %>% filter(C == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(PF == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(SF == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(SG == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(SF == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(PG == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(Salary <= 6000) %>% arrange(-PredictedPoints)



dat1 <- merge(dat, salaries, by = c("Name"))


dat1$gameNum
lastGame <- dat1 %>% group_by(Name) %>% summarise(
  lastGameNum = max(gameNum),
  lastMinutesPlayed = minutesPlayedPrevious[which(gameNum == lastGameNum)],
  lastFantasyPoints = fantasyPoints[which(gameNum == lastGame)]
)

dat1 <- merge(dat1, lastGame, by = c("Name"))

dat1$gameNum
dat1$lastGameNum
hist(dat1$pointsPerMinute)
dat2 <- dat1[which(dat1$gameNum != dat1$lastGameNum), ]

library(lme4)

str(dat2$player_id)
dat2$player_id <- as.factor(dat2$player_id)

dat2$minutesPlayedPrevious

model <- lmer(formula = minutesPlayed ~ minutesPlayedPrevious +   (1 | player_id), 
              data = dat2)

summary(model)

lastGameTest <- dat1 %>% group_by(Name) %>% summarise(
  lastGameNum = max(gameNum),
  player_id = player_id[which(gameNum == lastGameNum)],
  minutesPlayedPrevious = minutesPlayedPrevious[which(gameNum == lastGameNum)],
  lastFantasyPoints = fantasyPoints[which(gameNum == lastGameNum)],
  lastFantasyPointsPerMinute = pointsPerMinute[which(gameNum == lastGameNum)]
)

lastGameTest$predictedMinutes <- predict(object = model, lastGameTest)

plot(lastGameTest$predictedMinutes, lastGameTest$minutesPlayedPrevious)
lastGameTest$PredictedPoints <- lastGameTest$predictedMinutes * lastGameTest$lastFantasyPointsPerMinute
plot(lastGameTest$PredictedPoints, lastGameTest$lastFantasyPoints)


predictions <- merge(lastGameTest, salaries, by = c("Name"))
predictions$C <- ifelse(grepl(pattern = "C", x = predictions$Position), 1, 0)
predictions$PF <- ifelse(grepl(pattern = "PF", x = predictions$Position), 1, 0)
predictions$PG <- ifelse(grepl(pattern = "PG", x = predictions$Position), 1, 0)
predictions$SG <- ifelse(grepl(pattern = "SG", x = predictions$Position), 1, 0)
predictions$SF <- ifelse(grepl(pattern = "SF", x = predictions$Position), 1, 0)
predictions$G <- ifelse(grepl(pattern = "G", x = predictions$Position), 1, 0)
predictions$F <- ifelse(grepl(pattern = "F", x = predictions$Position), 1, 0)
predictions$U <- 1



predictions %>% filter(C == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(PF == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(SF == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(SG == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(SF == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(PG == 1) %>% arrange(-PredictedPoints)
predictions %>% filter(Salary <= 6000) %>% arrange(-PredictedPoints)



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