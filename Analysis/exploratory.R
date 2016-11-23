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

