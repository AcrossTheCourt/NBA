library(XML)

#Load file to translate team names (CHI to Chicago Bulls)
dn <- read.csv('teamnames.csv',stringsAsFactors = F)

#Pick a player (b-ref ID). Supply your own, if so desired.
player <- 'p/piercpa01'
player <- 'c/cartevi01'
player <- 'm/mcgratr01'
player <- 'd/drexlcl01'
player <- 'i/iversal01'
player <- 'w/wadedw01'
player <- 'j/jamesle01'
player <- 'b/bryanko01'
player <- 'j/jordami01'

#Pick a stretch of seasons. 
#The [-8] deletes the 8th element of the array, since he had no games in 1994. Delete if it's not needed.
Seasons <- c(1987:1998)[-8]

#Scrape team ratings and player gamelogs from b-ref. 
#Sys.sleep pauses the scraper for a moment because we are being nice to b-ref.
dt <- as.data.frame(NULL)
dp <- as.data.frame(NULL)
for(i in c(1:length(Seasons))){
  dt <- rbind(dt,as.data.frame(cbind(readHTMLTable(paste0("http://www.basketball-reference.com/leagues/NBA_",Seasons[i],"_ratings.html"),stringsAsFactors = F)[[1]],Seasons[i]) ))
  dp <- rbind(dp,as.data.frame(cbind(readHTMLTable(paste0("http://www.basketball-reference.com/players/",player,"/gamelog-advanced/",Seasons[i]),stringsAsFactors = F)[[1]],Seasons[i]) ))
  Sys.sleep(abs(rnorm(1,.3,.05)))
}

#Subset empty rows out
dp <- subset(dp,Rk!='Rk'&dp$ORtg!="")

#Rename columns
names(dp)[names(dp) == 'Seasons[i]'] <- 'Season'
names(dt)[names(dt) == 'Seasons[i]'] <- 'Season'
names(dt)[names(dt) == 'ORtg/A'] <- 'ORtgAdj'
names(dt)[names(dt) == 'DRtg/A'] <- 'DRtgAdj'
names(dt)[names(dt) == 'NRtg/A'] <- 'NetRtgAdj'

#Add short-form team names to team ratings data-frame
dt <- merge(dt,dn,by.x='Team',by.y='Long.team.name',all.x = T,all.y = F)
dt$Short.team.name <- ifelse(dt$Season>2014&dt$Team=='Charlotte Hornets','CHO',dt$Short.team.name)

#Calculate adjusted defensive efficiency relative to league average
dt$DRtgAdj <- as.numeric(dt$DRtgAdj)
dt <- merge(dt,aggregate(dt$DRtgAdj,list(Yrs = dt$Season),mean),by.x = 'Season',by.y = 'Yrs',all.x = T,all.y = F)
dt$RelTmDRtg <- dt$DRtgAdj - dt$x

#Create new variables for next steps
dt$SeasonTeam <- paste(dt$Season,dt$Short.team.name)
dp$SeasonTm <- paste(dp$Season,dp$Tm)
dp$SeasonOpp <- paste(dp$Season,dp$Opp)
dp$Mins <- as.numeric(matrix(unlist(strsplit(dp$MP,":")),nrow=2)[1,])+as.numeric(matrix(unlist(strsplit(dp$MP,":")),nrow=2)[2,])/60

#Convert to numeric columns
cols <- c('ORtg','USG%')
dp[cols] <- sapply(dp[cols],as.numeric)

#Calculate relative offensive efficiency (ORtg - player's season avg)
dp$MPUSG <- dp$Mins*dp$`USG%`
dp <- merge(dp,aggregate(cbind(dp$ORtg,dp$MPUSG),list(Yrs = dp$Season),weighted.mean),by.x = 'Season',by.y = 'Yrs',all.x = T,all.y = F)
dp$RelORtg <- dp$ORtg - dp$V1

#Merge in team stats to player logs data-frame
dp <- merge(dp,subset(dt,select=c('SeasonTeam','RelTmDRtg','NetRtgAdj')),by.x='SeasonOpp',by.y='SeasonTeam',all.x=T,all.y=F)

#Regression to calculate v-stats
summary(lm(dp$RelORtg~dp$RelTmDRtg+0,weights=dp$MPUSG))
summary(lm(dp$RelORtg~dp$RelTmDRtg+0,weights=dp$MPUSG,offset=dp$RelTmDRtg))
summary(lm(dp$`USG%`~dp$RelTmDRtg,weights=dp$Mins))

#Quick plot
plot(dp$RelTmDRtg,dp$RelORtg,main='Efficiency: Michael Jordan, 1987 to 1998',
     xlab='Opponent team defensive efficiency for the season',ylab='Offensive efficiency for a game')

#Focus on one season for the v-stats
sn <- 1998
summary(lm(dp$RelORtg[which(dp$Season==sn)]~dp$RelTmDRtg[which(dp$Season==sn)]+0,weights=dp$MPUSG[which(dp$Season==sn)]))
summary(lm(dp$`USG%`[which(dp$Season==sn)]~dp$RelTmDRtg[which(dp$Season==sn)],weights=dp$Mins[which(dp$Season==sn)]))
