dat$fantasyPoints
library(ggplot2)

library("directlabels")
names(dat)
dat$gameDate

highPoints <- dat %>% group_by(player_id) %>%
mutate(minPoints = min(fantasyPoints, na.rm = T), 
maxPoints = max(fantasyPoints, na.rm = T)) %>%  
filter(maxPoints > 45)

ggplot(data = highPoints, aes(x = gameDate, y = fantasyPoints, color = Name )) + 
  geom_line() + 
  geom_dl(aes(label = Name), method =list('first.bumpup', cex = 1.3, hjust = 1)) +
facet_grid(position_abbreviation ~ .) + theme(legend.position="none")

dat$position_abbreviation




plot(dat$fantasyPoints, dat$fantasyPointsPrevious)
cor(dat$fantasyPoints, dat$fantasyPointsPrevious, use = "complete.obs")
dat$avgVarFP
names(dat)
model <- lmer(formula = fantasyPoints ~ fantasyPointsPrevious + I(fantasyPoints^2) +
                avgVarFP
              +  (1 | player_id)
              # + (1 | gameNum)  
              , data = dat)
dat$predictedFantasyPoints <- predict(model, dat, allow.new.levels = TRUE)
summary(model)

describe(dat$avgVar)


check <- dat %>% group_by(Name) %>% summarise(
  n = n(),
  avgFP = mean(fantasyPoints),
  avgVar = sd(fantasyPoints), 
  minFP = min(fantasyPoints), 
  maxFP = max(fantasyPoints)
) %>% arrange(-avgVar)

plot(dat$predictedFantasyPoints, dat$fantasyPointsPrevious)
plot(dat$predictedFantasyPoints, dat$fantasyPoints)
plot(dat$predictedFantasyPoints, dat$predictedFantasyPoints - dat$fantasyPoints)


fpmodel <-  lmer(formula = fantasyPoints ~ minutesPlayed + I(minutesPlayed^2)
                 + avgVarFP
               +  (1 | player_id)
               # + (1 | gameNum)  
               , data = dat)
summary(fpmodel)
str(fpmodel)

dat$predictedFantasyPointsNew <- predict(fpmodel, dat,  allow.new.levels = TRUE)
plot(dat$predictedFantasyPointsNew, dat$fantasyPoints)
plot(dat$predictedFantasyPoints, dat$fantasyPoints)


plot(dat$fantasyPoints, dat$predictedFantasyPointsNew - dat$fantasyPoints)





