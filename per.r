#Instal the XML package to scrape data
library(XML)
#Choose your season of interest
Season <- 2016

url <- paste0("http://www.basketball-reference.com/leagues/NBA_",Season,"_totals.html")
#Takes the data table from the player totals page
db <- readHTMLTable(url[1])[[1]]

#Convert data's structure so it's usable
db$Player <- as.character(db$Player)
db <- subset(db,Player!='Player')
db$Pos <- as.character(db$Pos)
db$Age <- as.numeric(levels(db$Age)[db$Age])
db$Tm <- as.character(db$Tm)
for(i in c(1:25)){
  db[,5+i] <- as.numeric(levels(db[,5+i])[db[,5+i]])
}
colnames(db)[12] <- 'X3P'

#Sometimes a player with 0 MP is listed, and he'll screw up the entire season's calculation. Delete these guys.
db <- subset(db,MP>0)

#Load team data
#IMPORTANT: you have to create these files first (or use the example ones for 2016)
#Go to the season of interest on: http://www.basketball-reference.com/leagues/NBA_2017.html
#Then copy and paste the team totals (not per poss. or min.) and save as a .csv with the name teamstats
#Do the same for the miscellaneous section. Don't include league averages for either
#Put them in the same directory and set the working directory so R can find the files
#B-ref changed its structure, so scraping is much more difficult. Apologies.
dt <- read.csv(file="teamstats.csv", stringsAsFactors=FALSE)
dtm <- read.csv(file='teamstatsmisc.csv',sttringsAsFactors=FALSE)

#Team names in the player file do not match with the team names displayed in dt or dtm
#The lines below correct this. It's wise to double-check these with your selected season.
long.team.names <- c('Boston Celtics',	'Fort Wayne Pistons',	'Milwaukee Hawks',	'Minneapolis Lakers',	'New York Knicks',	'Philadelphia Warriors',	'Rochester Royals',	'Syracuse Nationals',	'League Average',	'St. Louis Hawks',	'Cincinnati Royals',	'Detroit Pistons',	'Los Angeles Lakers',	'Chicago Packers',	'Chicago Zephyrs',	'San Francisco Warriors',	'Baltimore Bullets',	'Philadelphia 76ers',	'Chicago Bulls',	'San Diego Rockets',	'Seattle SuperSonics',	'Atlanta Hawks',	'Milwaukee Bucks',	'Phoenix Suns',	'Buffalo Braves',	'Cleveland Cavaliers',	'Portland Trail Blazers',	'Golden State Warriors',	'Houston Rockets',	'Kansas City-Omaha Kings',	'Capital Bullets',	'New Orleans Jazz',	'Washington Bullets',	'Kansas City Kings',	'Denver Nuggets',	'Indiana Pacers',	'New York Nets',	'San Antonio Spurs',	'New Jersey Nets',	'San Diego Clippers',	'Utah Jazz',	'Dallas Mavericks',	'Los Angeles Clippers',	'Sacramento Kings',	'Charlotte Hornets',	'Miami Heat',	'Minnesota Timberwolves',	'Orlando Magic',	'Toronto Raptors',	'Vancouver Grizzlies',	'Washington Wizards',	'Memphis Grizzlies',	'New Orleans Hornets',	'Charlotte Bobcats',	'New Orleans/Oklahoma City Hornets',	'Oklahoma City Thunder',	'Brooklyn Nets',	'New Orleans Pelicans')
short.team.names <- c('BOS',	'FTW',	'MLH',	'MNL',	'NYK',	'PHW',	'ROC',	'SYR',	'LAVG',	'STL',	'CIN',	'DET',	'LAL',	'CHP',	'CHZ',	'SFW',	'BAL',	'PHI',	'CHI',	'SDR',	'SEA',	'ATL',	'MIL',	'PHO',	'BUF',	'CLE',	'POR',	'GSW',	'HOU',	'KCO',	'CAP',	'NOJ',	'WSB',	'KCK',	'DEN',	'IND',	'NYN',	'SAS',	'NJN',	'SDC',	'UTA',	'DAL',	'LAC',	'SAC',	'CHH',	'MIA',	'MIN',	'ORL',	'TOR',	'VAN',	'WAS',	'MEM',	'NOH',	'CHA',	'NOK',	'OKC',	'BRK',	'NOP')
short.team.names[which(short.team.names=='CHH')] <- ifelse(Season>2014,'CHO','CHH')
team.names <- data.frame(long.team.names,short.team.names,stringsAsFactors = FALSE)
dtm <- merge(dtm,team.names,by.x='Team',by.y = 'long.team.names',all.x = T)

#League-wide terms for Hollinger's PER
factor <- (2 / 3) - (0.5 * ( sum(dt$AST) / sum(dt$FG) )) / (2 * (sum(dt$FG) / sum(dt$FT)))
VOP <- sum(dt$PTS) / (sum(dt$FGA) - sum(dt$ORB) + sum(dt$TOV) + 0.44 * sum(dt$FTA))
DRB. <- (sum(dt$TRB) - sum(dt$ORB)) / sum(dt$TRB)

#Set PER and its intermediary cousins to 0
#uPER is raw per minute PER
#aPER is a pace adjustment
#PER is aPER where the league average is set to 15
db$uPER <- 0
db$aPER <- 0
db$PER <- 0
#This is the PER calculation, looping through for every player
#Ignores rows where the player has totals from multiple teams
for(i in c(1:length(db$Player))){
  if(db$Tm[i]=='TOT'){
    db$uPER[i] <- 0
  }else{
    db$uPER[i] <- (1 / db$MP[i]) *
      ( db$X3P[i] +(2/3)*db$AST[i]+
          (2 - factor * ( sum(db$AST[which(db$Tm==db$Tm[i])]) / sum(db$FG[which(db$Tm==db$Tm[i])]) )) * db$FG[i]+ 
          (db$FT[i] *0.5 * (1 + (1 - ( sum(db$AST[which(db$Tm==db$Tm[i])]) / sum(db$FG[which(db$Tm==db$Tm[i])]) )) + (2/3) * (sum(db$AST[which(db$Tm==db$Tm[i])]) / sum(db$FG[which(db$Tm==db$Tm[i])])))) - 
          VOP * db$TOV[i] -VOP*DRB.*(db$FGA[i] - db$FG[i]) - 
          VOP * 0.44 * (0.44 + (0.56 * DRB.)) * (db$FTA[i] - db$FT[i])+
          VOP * (1 - DRB.) * (db$TRB[i] - db$ORB[i]) + VOP * DRB. * db$ORB[i] +
          VOP * db$STL[i] + VOP * DRB. * db$BLK[i] -
          db$PF[i] * ((sum(dt$FT) / sum(dt$PF) ) - 0.44 * (sum(dt$FTA) / sum(dt$PF)) * VOP)  )
  }
}

#Pace adjustment using the team's pace from b-ref
for(i in c(1:length(db$Player))){
  if(db$Tm[i]=='TOT'){
    db$uPER[i] <- 0
  }else{
    pace_adjustment <- mean(dtm$Pace)/dtm$Pace[which(dtm$short.team.names==db$Tm[i])]
    db$aPER[i] <- pace_adjustment*db$uPER[i]
  }
}

#Sets league-average to 15
db$PER <- db$aPER * 15/(sum(db$MP[which(db$Tm!='TOT')]*db$aPER[which(db$Tm!='TOT')])/sum(db$MP[which(db$Tm!='TOT')]))

#Calculates PER for rows with players whose totals are from multiple teams. Minute weighted.
for(i in c(1:length(db$Player))){
  if(db$Tm[i]=='TOT'){
    db$PER[i] <- sum(db$PER[which(db$Player==db$Player[i]&db$Age==db$Age[i])]*db$MP[which(db$Player==db$Player[i]&db$Age==db$Age[i])])/sum(db$MP[which((db$Player==db$Player[i]&db$Age==db$Age[i])&db$Tm!='TOT')])
  }
}

#And that's it: you have calculated PER. Errors should be within ~0.1.
