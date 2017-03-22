library(caret)
library(glmnet)
library(gbm)

#Load data
d <- read.csv(file='playerstatsyoungplayerprojection.csv',stringsAsFactors = F)
dp <- read.csv(file='youngplayerprojection.csv',stringsAsFactors = F)

#Calculate a player's peak BPM and minutes
dp <- merge(dp,aggregate(d$MP[d$Age>25&d$Age<30],list(PlyrList = d$PlayerBorn[d$Age>25&d$Age<30]),sum),by.x = 'PlayerBorn',by.y = 'PlyrList',all.x = T,all.y = F)
names(dp)[names(dp) == 'x'] <- 'PeakMP'
dp <- merge(dp,aggregate(cbind(d$MP[d$Age>25&d$Age<30], d$BPM[d$Age>25&d$Age<30]), list(PlyrList = d$PlayerBorn[d$Age>25&d$Age<30]), weighted.mean),by.x = 'PlayerBorn',by.y = 'PlyrList',all.x = T,all.y = F)
dp$V1 <- NULL
names(dp)[names(dp) == 'V2'] <- 'PeakBPM'

#Create extra variables for the models
dp$MPG <- dp$MP/dp$G
dp$X3P. <- ifelse(is.na(dp$X3P.),0,dp$X3P.)
dp$AgeSq <- dp$Age^2
dp$TSUSG <- dp$RelTS.*dp$USG.
dp$USGAST <- dp$USG.*dp$AST.
dp$TRBAST <- sqrt(dp$TRB.*dp$AST.)
dp$BLK100 <- dp$BLK/dp$Poss*100
dp$G. <- dp$G/(ifelse(dp$Season==1999,-32,0) +ifelse(dp$Season==2012,-16,0) +82)
dp$BPMAge <- dp$BPM*dp$Age
dp$BPMAgeSq <- dp$BPM*dp$AgeSq

#Data frame for players too young to have peak age data
dp2 <- subset(dp,Season -Age+28>=2018)
#Filter out players from dp2
dp <- subset(dp,Season -Age+28<2018)

#Adjust BPM and MP for survivor's bias and regress
dp$PeakMP <- ifelse(is.na(dp$PeakMP),0,dp$PeakMP)
dp$PeakBPM <- ifelse(is.na(dp$PeakBPM),0,dp$PeakBPM)
dp$PeakBPM <- ifelse(dp$PeakMP<=1500,(dp$PeakBPM*dp$PeakMP-2*750)/(dp$PeakMP+750) ,dp$PeakBPM)
dp$PeakMP <- ifelse(dp$PeakMP<=1500,750+dp$PeakMP/2,dp$PeakMP)

#Set variables
y <- dp$PeakBPM
wgts <- (dp$PeakMP*dp$MP)^.5
x.var <- subset(dp,select=c('Age','AgeSq','OBPM','DBPM','USG.','TSUSG','USGAST','TRBAST','TOV.','STL.','FT.','X3P.','FTr','X3PAr','MPG','G.','RelTS.','X3PA100adj','ORB.adj','DRB.adj','BLK100'))
x.var <- data.matrix(x.var)
al <- .15 #alpha coefficient: controls how variables are dropped from regression. 0 to 1

#Find optimal lambda coefficient: controls how variable coefficients are penalilzed. 1se is usually preferred
glmnet.model.cv <- cv.glmnet(x.var,y,weights=wgts,alpha=al)
lamb <- glmnet.model.cv$lambda.min
lamb <- glmnet.model.cv$lambda.1se

#Create the model
glmnet.model <- glmnet(x.var,y,weights=wgts,alpha=al,lambda=lamb)
coef(glmnet.model)

#Predicted peak BPM
dp$glmnet.predict <- predict(glmnet.model, s=lamb, x.var, type="response")

#Predict peak BPM for current young players
x.var2 <- subset(dp2,select=c('Age','AgeSq','OBPM','DBPM','USG.','TSUSG','USGAST','TRBAST','TOV.','STL.','FT.','X3P.','FTr','X3PAr','MPG','G.','RelTS.','X3PA100adj','ORB.adj','DRB.adj','BLK100'))
x.var2 <- data.matrix(x.var2)
dp2$glmnet <- predict(glmnet.model, s=lamb, x.var2, type="response")


#Select variables for GBM
cols <- c('PeakBPM','Age','AgeSq','OBPM','DBPM','USG.','TSUSG','USGAST','TRBAST','TOV.','STL.','FT.','X3P.','FTr','X3PAr','MPG','G.','RelTS.','X3PA100adj','ORB.adj','DRB.adj','BLK100')

#Separate data into training and testing partitions. p sets proportion of training to testing
inTrain <- createDataPartition(dp$PeakBPM,p=0.7,list=FALSE)
training <- dp[inTrain,cols]
testing <- dp[-inTrain,cols]
testing2 <- dp[-inTrain,append(cols,'Player')]
newperiod <- dp2[,cols] #Current young players

#Train data using grid. May choose multiple values of n.trees, interaction.depth, shrinkage, and n.minosinnode if desired
fitControl <- trainControl(method = 'repeatedcv', number = 5, repeats = 10, summaryFunction=defaultSummary)
grid <- expand.grid(n.trees = c(50,500), interaction.depth = 2, shrinkage = c(.1,.01), n.minobsinnode = 10)
fit.gbm <- train(PeakBPM ~ . , data=training, method = 'gbm', trControl=fitControl, tuneGrid=grid)
summary(fit.gbm) 
plot(fit.gbm) 
predicted <- predict(fit.gbm,newdata=testing) #Predicted Peak BPM on test data partition
sqrt(sum((predicted - testing$PeakBPM)^2/length(predicted))) #RMSE out of sample

#Train over full data-set
grid <- expand.grid(n.trees = 50, interaction.depth = 2, shrinkage = .1, n.minobsinnode = 10)
fit.gbm <- train(PeakBPM ~ . , data=dp[,cols], method = 'gbm', trControl=fitControl, tuneGrid=grid)
summary(fit.gbm)

#Apply to data-set with current young players
dp2$GBM <- predict(fit.gbm,newdata=newperiod)
