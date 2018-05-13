# credit
Analysis of credit data set
rm(list=ls())

library(caTools)

dd <- read.csv(file.choose())
dd<- read.csv(file.choose())

########## Split the segment
dd <- dd[which(dd$Account.Balance == "unknown" | dd$Account.Balance == "> 200 DM"),]

#### Variable transformation
#### WOE Binning of Variables
head(dd)
library(woeBinning)

binning <- woe.binning(dd,'default','Account.Balance',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Duration.of.Credit..month.',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Payment.Status.of.Previous.Credit',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Purpose',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Credit.Amount',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Value.Savings.Stocks',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Length.of.current.employment',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Instalment.per.cent',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Sex...Marital.Status',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Guarantors',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Duration.in.Current.address',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Most.valuable.available.asset',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Age..years.',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Concurrent.Credits',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Type.of.apartment',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','No.of.Credits.at.this.Bank',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','No.of.dependents',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Telephone',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Foreign.Worker',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


binning <- woe.binning(dd,'default','Job',min.perc.total = 0.05)
woe.binning.table(binning)
dd <- woe.binning.deploy(dd,binning,add.woe.or.dum.var = "woe")


#### Take out only woe variables for the regression

gg <- grep("woe",colnames(dd))
dd1 <- dd[,c(1,gg)]
colnames(dd1)


library(caTools)
set.seed(100)
intrain <- sample.split(dd1$default, SplitRatio = 0.7)
training <- subset(dd1, intrain == TRUE)
testing <- subset(dd1, intrain == FALSE)

dim(training)
dim(testing)


logitmodel = glm(default ~ . , data = training, family = binomial)
summary(logitmodel)
library(car)
vif(logitmodel)

library(dplyr)
demo_train <- training
demo_train$prob <- predict(logitmodel, newdata = demo_train, type = "response")
demo_train <- arrange(demo_train,desc(prob))
demo_train$rank <- seq(1,nrow(demo_train))
demo_train$decile <- ntile(demo_train$rank, 10)
table(demo_train$decile, demo_train$default)


demo_train <- testing
demo_train$prob <- predict(logitmodel, newdata = demo_train, type = "response")
demo_train <- arrange(demo_train,desc(prob))
demo_train$rank <- seq(1,nrow(demo_train))
demo_train$decile <- ntile(demo_train$rank, 10)
table(demo_train$decile, demo_train$default)


########## Convert logits/probs to scores ##################


dd$prob <- predict(logitmodel, newdata = dd, type = "response")
dd <- arrange(dd,desc(prob))
dd$logit <- log(dd$prob / (1-dd$prob))

nrow(dd)
dd$logit[100]

factor = -1 * 20/log(2)
offset = 600 - factor * dd$logit[100]

dd$score <- offset + factor * dd$logit

head(dd)
tail(dd)
































############ Low default probability 

install.packages("ROSE")
library(ROSE)
library(MASS)

training_ROSE <- ROSE(default ~ ., data = dd1, seed = 123)$data
head(training_ROSE)
logitmodel1 <- glm(default ~ ., data = training_ROSE, family = "binomial")
summary(logitmodel1)

step <- stepAIC(logitmodel1, direction="both")
step$anova # display results 
head(training_ROSE)
ncol(training_ROSE)
training_ROSE_v1 <- subset(training_ROSE, select = c(-21))

logitmodel1 = glm(default ~ . , data = training_ROSE_v1, family = binomial)
summary(logitmodel1)

##install.packages("dplyr")
library(dplyr)

demo_train <- training
demo_train$prob <- predict(logitmodel1, newdata = demo_train, type = "response")
demo_train <- arrange(demo_train,desc(prob))
demo_train$rank <- seq(1,nrow(demo_train))
demo_train$decile <- ntile(demo_train$rank, 10)
table(demo_train$decile, demo_train$default)


demo_train <- testing
demo_train$prob <- predict(logitmodel1, newdata = demo_train, type = "response")
demo_train <- arrange(demo_train,desc(prob))
demo_train$rank <- seq(1,nrow(demo_train))
demo_train$decile <- ntile(demo_train$rank, 10)
table(demo_train$decile, demo_train$default)

#############Intercept correction

a <- logitmodel1$coefficients[1]

dd1$default_1 <- dd1$default
dd1$default_1  <- as.integer(as.character(dd1$default_1))

training_ROSE$default_1 <- training_ROSE$default
training_ROSE$default_1 <- as.integer(as.character(training_ROSE$default_1))

table(training_ROSE$default_1)
table((dd$default_1))

t <- sum(dd1$default_1)/nrow(dd1)
a1 <- (1-t)/t
t2 <- sum(training_ROSE$default_1)/nrow(training_ROSE)
a2<- t2/(1-t2)
ans <- a1*a2
correct <- a - log(ans)
logitmodel1$coefficients[1] <- correct
correct

### Summary check after "Intercept Correction" implementation
summary(logitmodel1)

