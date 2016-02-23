
library(data.table)
library(dplyr)
library(Hmisc)
library(reshape2)
library(bnlearn)

TourneySeeds <- fread("TourneySeeds.csv")
SampleSubmission <- fread("SampleSubmission.csv")
Seasons <- fread("Seasons.csv")
Teams <- fread("Teams.csv")
TourneySlots <- fread("TourneySlots.csv")
TourneyDetailedResults <- fread("TourneyDetailedResults.csv")
TourneyCompactResults <- fread("TourneyCompactResults.csv")

TourneySeeds <- TourneySeeds %>% 
    mutate(SeedNum = gsub("[A-Z+a-z]", "", Seed)) %>% select(Season, Team, SeedNum)
	
games.to.predict <- cbind(SampleSubmission$Id, colsplit(SampleSubmission$Id, "_", names = c('season', 'team1', 'team2')))   

temp <- left_join(games.to.predict, TourneySeeds, by=c("season"="Season", "team1"="Team"))
games.to.predict <- left_join(temp, TourneySeeds, by=c("season"="Season", "team2"="Team"))
colnames(games.to.predict)[c(1,5:6)] <- c("Id", "team1seed", "team2seed")
games.to.predict <- games.to.predict %>% mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed))	

temp <- left_join(as.data.frame(TourneyCompactResults), TourneySeeds, by=c("Season", "Wteam"="Team"))
compact.results <- left_join(temp, TourneySeeds, by=c("Season", "Lteam"="Team"))

set1 <- compact.results %>% select(SeedNum.x, SeedNum.y) %>% mutate(result=1)
set2 <- compact.results %>% select(SeedNum.y, SeedNum.x) %>% mutate(result=0)
colnames(set1) <- c("team1seed", "team2seed", "team1win")
colnames(set2) <- c("team1seed", "team2seed", "team1win")
full.set <- rbind(set1, set2)
full.set <- full.set %>% mutate(team1seed = as.numeric(team1seed), team2seed = as.numeric(team2seed))



%Learning a graph structure

cc <- compact.results
cc$Season <- as.factor(cc$Season)
cc$Daynum <- as.factor(cc$Daynum)
cc$Wteam <- as.factor(cc$Wteam)
cc$Lteam <- as.factor(cc$Lteam)
cc$Wloc <- as.factor(cc$Wloc)
cc$Numot <- as.factor(cc$Numot)
cc$SeedNum.x <- as.factor(cc$SeedNum.x)
cc$SeedNum.y <- as.factor(cc$SeedNum.y)
cc$Wscore <- as.factor(cc$Wscore)
cc$Lscore <- as.factor(cc$Lscore)

graph_tb <- tree.bayes(cc)
 
% Learning the model

model_b <- bn.fit(graph_tb, data = cc)
cpquery(model_nb, event = (C=="1229" ), evidence = ( E=="1234" ) & (F=="70"))


%--------------------------------

data_set <- cc
names(data_set)=c("A","B","C","D","E","F","G","H","I")
graph <- naive.bayes(data_set,"C")
model_nb <- bn.fit(graph, data = data_set)

cpquery(model_nb, event = (C=="1229" ), evidence = ( E=="1234" ) & (F=="70"))

% -------------------------------

net <- model2network("[E][I|E][F|E][A|E][B|E][G|E][D|I:F][H|F:I][C|D:H:A:B:G]")
model_nb <- bn.fit(graph, data = data_set)

cpquery(model_nb, event = (C=="1229" ), evidence = ( E=="1234" ) & (F=="70"))

%--------------------------------
%new data

net <- model2network("[E][I|E][F|E][D|I:F][H|I:F][C|D:H]")



