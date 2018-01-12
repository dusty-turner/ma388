library(tidyverse)
managers = read_csv("managersupdated.csv")
teams = read_csv("Teams.csv")
names(managers)
names(teams)
## for analysis below

managersjoined = 
managers %>%
left_join(teams, by = c("yearID", "teamID"))

# write.csv(managersjoined, "managersjoined.csv")
managersjoined = read_csv("managersjoined.csv")


survivaldata = 
managersjoined %>%
  group_by(playerID,teamID) %>%
  filter(row_number()==n()) %>%
  mutate(Fired = ifelse(G.y - G.x > 10,TRUE,FALSE)) %>%
  mutate(startyear = yearID - totalseasons + 1) 
# %>%
  # select(playerID, yearID,teamID,G.x,G.y,Fired, startyear)
survivaldata = 
survivaldata %>%
  mutate(labeller = paste0(playerID, " ", teamID)) %>%
  arrange(desc(yearID)) 

names(survivaldata)
library(ggalt)

survivaldata$labeller = factor(survivaldata$labeller, levels = as.character(survivaldata$labeller))
ggplot(data = survivaldata, aes(x = startyear, xend = yearID, y = labeller, group = labeller)) +
  geom_dumbbell(color="#a3c4dc", 
                size=0.75) +
  scale_y_discrete(breaks = levels(survivaldata$labeller)[c(T, rep(F, 25))]) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  xlim(min(survivaldata$startyear),max(survivaldata$yearID)) + 
  ggtitle("Coaching Tenure Since The Dawn Of Baseball") + xlab("Year") + ylab("Coach") 

survivaldata = 
survivaldata %>%
  mutate(censor = ifelse(yearID==2016, 0,1))


library(survival)
library(OIsurv) # Aumatically loads KMsurv
survobj = Surv(survivaldata$totalseasons,survivaldata$censor)

fit = survfit(survobj~1, data = survivaldata)
summary(fit)
confBands(survobj, type = "hall")

names(survivaldata)
mod = coxph(survobj ~ totalwins + PercentLast2 + PercentLast3,data = survivaldata)
summary(mod)
library(survminer)
library(ggplot2)

plot(fit)
plot(survfit(mod))
ggsurvplot(fit)

max(survivaldata$totalseasons)
survivaldata[which.max(survivaldata$totalseasons),]

write.csv(survivaldata, "managerssurvival.csv")


## test ph

test.ph = cox.zph(mod)
test.ph
library(survminer)
ggcoxzph(test.ph, df = 2)
ggcoxdiagnostics(mod, type = "dfbeta", linear.predictions = FALSE)
ggcoxdiagnostics(mod, type = "deviance", linear.predictions = FALSE)
