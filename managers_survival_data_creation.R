library(tidyverse)
library(zoo)
managers = read_csv("Managers.csv")
teams = read_csv("Teams.csv")

managers = 
managers %>%
  arrange(playerID,teamID)

# managers = managers[1:200,]
nrow(managers)

uniquemanagers =
managers %>%
  group_by(playerID, teamID) %>%
  nest %>%
  select(playerID)

nrow(uniquemanagers)



### did this to get last two percentage

winvec = NULL
lossvec = NULL
gamevec = NULL
i=1
for (i in 1:nrow(uniquemanagers)) {
  # for (i in 1:5) {
  temp =
    managers %>%
    group_by(playerID, teamID) %>%
    nest %>% slice(i) %>% unnest(data)
  
  if (nrow(temp) == 1) {
    gamevec = append(gamevec, NA)
    winvec = append(winvec, NA)
    lossvec = append(lossvec, NA)
    print(i)
  }  else if (nrow(temp) != 1) {
    rollgamevec = rollsumr(temp$G, k = 2, fill = NA)
    rollwinvec = rollsumr(temp$W, k = 2, fill = NA)
    rolllossvec = rollsumr(temp$L, k = 2, fill = NA)
    print(i)
    gamevec = append(gamevec, rollgamevec)
    winvec = append(winvec, rollwinvec)
    lossvec = append(lossvec, rolllossvec)
  }
}

managers$Glast2 = gamevec
managers$Wlast2 = winvec
managers$Llast2 = lossvec

managers = 
managers %>%
  mutate(PercentLast2 = ifelse(is.na(Glast2),W/G,Wlast2/Glast2)) 

### did this to get the last three percentage

winvec = NULL
lossvec = NULL
gamevec = NULL

for (i in 1:nrow(uniquemanagers)) {
  # for (i in 1:5) {
  temp =
    managers %>%
    group_by(playerID, teamID) %>%
    nest %>% slice(i) %>% unnest(data)
  
  if (nrow(temp) == 1) {
    gamevec = append(gamevec, c(NA))
    winvec = append(winvec, c(NA))
    lossvec = append(lossvec, c(NA))
    print(i) 
    
  }  else if (nrow(temp) == 2) {
    gamevec = append(gamevec, c(NA,NA))
    winvec = append(winvec, c(NA,NA))
    lossvec = append(lossvec, c(NA,NA))
    print(i)
    
  }  else if (nrow(temp) > 2) {
    rollgamevec = rollsumr(temp$G, k = 3, fill = NA)
    rollwinvec = rollsumr(temp$W, k = 3, fill = NA)
    rolllossvec = rollsumr(temp$L, k = 3, fill = NA)
    print(i)
    gamevec = append(gamevec, rollgamevec)
    winvec = append(winvec, rollwinvec)
    lossvec = append(lossvec, rolllossvec)
  }
}

managers$Glast3 = gamevec
managers$Wlast3 = winvec
managers$Llast3 = lossvec

managers = managers %>%
  mutate(PercentLast3 = ifelse(is.na(Wlast3/Glast3), PercentLast2, Wlast3/Glast3))


## did this to get total games/wins/losses/and seasons

games = NULL
wins = NULL
loss = NULL
rank = NULL
seasons = NULL
i=1

for (i in 1:nrow(uniquemanagers)) {
  # for (i in 1:5) {
  temp =
    managers %>%
    group_by(playerID, teamID) %>%
    nest %>% slice(i) %>% unnest(data)
  
  games = append(games,c(rep(sum(temp$G),nrow(temp))) ) 
  wins = append(wins,rep(sum(temp$W),nrow(temp)))
  loss = append(loss,rep(sum(temp$L),nrow(temp)))
  rank = append(rank,rep(mean(temp$rank),nrow(temp)))
  seasons = append(seasons,rep(nrow(temp),nrow(temp)))
  print(i) 
}


managers$totalgames = games
managers$totalwins = wins
managers$totalloss = loss
managers$totalavgrank = rank
managers$totalseasons = seasons

write.csv(managers, "managersupdated.csv")
