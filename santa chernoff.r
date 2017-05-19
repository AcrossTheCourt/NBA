################
### Chernoff ###
################

library(aplpack)

#Read data from b-ref url. Can change this to other search urls.
d <- as.data.frame(readHTMLTable("http://www.basketball-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=1984&year_max=2017&is_playoffs=N&age_min=0&age_max=99&game_month=12&game_day=25&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&order_by=game_score",stringsAsFactors = FALSE))
#Delete the column name rows
d <- subset(d,stats.Player!='Player')
#Change the columns 10 through 33 to numeric columns
d[,10:33] <- sapply(d[,10:33],as.numeric)
#Sort by date
d <- d[order(d$stats.Date),]
#Create a data frame for the plot. Only use games with a GmSc over 29 so the plot is smaller.
dch <- subset(d,stats.GmSc>29)
#Create label vector
ply.names <- dch$stats.Player
#Replace NAs with 0s for 3PT%
dch$stats.3P. <- ifelse(is.na(dch$stats.3P.),0,dch$stats.3P.)
#Select only the variables of interest. The order is critical.
dch <- subset(dch,select=c(stats.GmSc,stats.PTS,stats.PF,stats.3P.,stats.2P.,stats.FT.,stats.ORB,
                stats.DRB,stats.2PA,stats.3PA,stats.FTA,stats.AST,stats.TOV,stats.BLK,stats.STL))

#1-height of face Game Score
#2-width of face PTS
#3-shape of face PF
#4-height of mouth 3PT%
#5-width of mouth 2PT%
#6-curve of smile FT%
#7-height of eyes ORB
#8-width of eyes DRB
#9-height of hair 2PA
#10-width of hair 3PA
#11-styling of hair FTA
#12-height of nose AST
#13-width of nose TOV
#14-width of ears BLK
#15-height of ears STL

#Plot. The legend text may need to be tweaked with different inputs. Change face.type to 1 for non-Santa faces and 0 for colorless.
faces(dch,labels=ply.names,face.type=2,
      main='Best Christmas Day Games by Game Score Since 1984 (source: b-ref)')
text(-110,-86,'Height of face: Game Score',adj=0,cex=.8)
text(-110,-94,'Width of face: Points',adj=0,cex=.8)
text(-110,-102,'Face shape: Fouls',adj=0,cex=.8)
text(-110,-110,'Height of mouth: 3PT%',adj=0,cex=.8)
text(-110,-118,'Width of mouth: 2PT%',adj=0,cex=.8)
text(-30,-86,'Smile: FT%',adj=0,cex=.8)
text(-30,-94,'Height of eyes: ORB',adj=0,cex=.8)
text(-30,-102,'Width of eyes: DRB',adj=0,cex=.8)
text(-30,-110,'Height of hair: 2PA',adj=0,cex=.8)
text(-30,-118,'Width of hair: 3PA',adj=0,cex=.8)
text(50,-86,'Hair style: FTA',adj=0,cex=.8)
text(50,-94,'Height of nose: AST',adj=0,cex=.8)
text(50,-102,'Width of nose: TOV',adj=0,cex=.8)
text(50,-110,'Width of ear: BLK',adj=0,cex=.8)
text(50,-118,'Height of ear: STL',adj=0,cex=.8)
text(-30,-78,'Ordered by season',adj=0,cex=.8)
