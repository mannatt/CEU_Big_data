
#merge the two datasets

setkey(datatable_cleaned_deletek, companynames)
setkey(final_data_cleaned44, companynames)
datatable44k <- merge(datatable_cleaned_deletek, final_data_cleaned44, by.x= 'companynames', by.y='companynames')
#852 companies merged


#working on getting benefit comments: to get benefit links loop 
datatable4 <- NULL
datatable4 <- data.table(datatable4)

for (i in 1:852) {
  k <- read_html(datatable44k[[2]][[i]])
  my_companynames4 <-
    trimws(k%>%
             html_nodes('.tightAll') %>%
             html_text())
  my_companynames4
  
  header_links <- 
    k%>%
    html_node('.row')%>%
    html_nodes('a')%>%
    html_attr('href')
  
  header_links
  
  benefit_links <-
    paste0('https://www.glassdoor.com',header_links[grepl('Benefits', header_links)])
  
  benefit_links
  
  my_table4=
    data.table('companynames'= my_companynames4,'benefit_links'= benefit_links)
  
  datatable4 <-rbind(datatable4,my_table4)
  message(i)
  
}  

########cleaning 
datatable4 <- unique(datatable4, by="companynames")

#merge the two datasets

setkey(datatable44k, companynames)
setkey(datatable4, companynames)
datatable4k <- merge(datatable44k, datatable4, by.x= 'companynames', by.y='companynames')
#852 companies merged with benefit links



#benefits without comments 
datatable9 <- NULL
datatable9 <- data.table(datatable9)

for (i in 1:852) {
  l <- read_html(datatable4k[[9]] [[i]])
  
  my_companynames3 <- trimws(l %>%
                               html_nodes('.tightAll') %>%
                               html_text())
  
  my_list <- l %>%
    html_nodes('.benefitsList') %>%
    html_text() 
  
  mratings <- trimws(l %>%
                       html_node('.rating') %>%
                       html_text())
  
  my_table9=
    data.table('companynames'= my_companynames3,'benefit_list' = my_list, 'benefit_ratings'= mratings)
  
  datatable9 <-rbind(datatable9, my_table9)
  message(i)
}

########cleaning + merging 9 variables to 10 
datatable9 <- unique(datatable9, by="companynames")

#merge the two datasets

setkey(datatable4k, companynames)
setkey(datatable9, companynames)
datatable9k <- merge(datatable4k, datatable9, by.x= 'companynames', by.y='companynames')
#852 companies merged with benefit list and benefit rating

saveRDS(datatable9k, 'datatable9k.rds')
write.csv(datatable9k, 'datatable9k', row.names = F)


######## to get the first page of comments //It needed to be rescraped due to dataloss - details of rescraping later line 920//

datatable8 <- NULL
datatable8 <- data.table(datatable8)

for (i in 1:852) {
  url <- paste0(datatable9k[[9]] [[i]])
  l <- read_html(url)
  
  my_companynames3 <-
    trimws(l%>%
             html_nodes('.tightAll') %>%
             html_text())
  my_companynames3
  
  my_comments <- l%>%
    html_nodes('.wrapToggleStr') %>%
    html_text()
  
  my_table8=
    data.table('companynames'= my_companynames3, 'comments'=my_comments)
  
  datatable8 <-rbind(datatable8,my_table8)
  message(i)
}
##to delete duplicates##
datatable8k <- unique(datatable8[,c("companynames", "comments")])

saveRDS(datatable8k, 'datatable8k.rds')
write.csv(datatable8k, 'datatable8k', row.names = F)

datatable8k$companynames

##to get the rest of the comments## //It need to be rescraped due to dataloss - details of rescraping later line 920//

# https://www.glassdoor.com/Benefits/3M-US-Benefits-EI_IE446.0,2_IL.3,5_IN1_IP2.htm
# https://www.glassdoor.com/Benefits/3M-US-Benefits-EI_IE446.0,2_IL.3,5_IN1.htm

datatable10 <- NULL
datatable10 <- data.table(datatable10)

for (i in 1:852) {
  delayedAssign("do.next", {next})
  for (j in 2:394) {
    tryCatch({
      url <- paste0(datatable9k [[9]] [[i]],'_IP',j,'.htm')
      l <- read_html(url)
      
      my_companynames3 <-
        trimws(l%>%
                 html_nodes('.tightAll') %>%
                 html_text())
      my_companynames3
      
      my_comments <- l%>%
        html_nodes('.wrapToggleStr') %>%
        html_text()
      
      my_table10=
        data.table('companynames'= my_companynames3, 'comments'=my_comments)
      
      datatable10 <-rbind(datatable10,my_table10)
      message(i,'/',j)}, error=function(e) force(do.next))}
}

###########Cleaning
install.packages('qdap')
install.packages('ggplot2')
install.packages('quanteda')
install.packages('readtext')
install.packages("rjson")
install.packages('devtools')
install.packages('countrycode')
library(countrycode)
install.packages('tidyverse')
install.packages('ggmap')
install.packages('forcats')


datatable9k$rating <- as.numeric(datatable9k$rating)
datatable10k <- datatable9k

unique(datatable10k$size)
size_code
datatable10k$size_code <-
  ifelse(datatable10k$size == "Size1 to 50 employees", 1, ifelse(datatable10k$size == "Size201 to 500 employees",2,
                                                                 ifelse(datatable10k$size== "Size51 to 200 employees",3,ifelse(datatable10k$size== "Size501 to 1000 employees",4,
                                                                                                                               ifelse(datatable10k$size== "Size1001 to 5000 employees",5, ifelse(datatable10k$size== "Size5001 to 10000 employees",6,
                                                                                                                                                                                                 ifelse(datatable10k$size== "Size10000+ employees",7,0)))))))

unique(Mondaydata$industry)

##rid of mistakes
unique(datatable10k$revenue)

##tobreak up location into second part to find region in US headquarters and non-US?
Mondaydata$location_state <- lapply(strsplit(Mondaydata$location, ","), `[`, 2)
unique(Mondaydata$location_state)

Mondaydata$US <- NULL
Mondaydata$location_state <- trimws(Mondaydata$location_state)
Mondaydata$US <- ifelse(grepl("^[[:upper:]]+$",Mondaydata$location_state),1,0)


##try to drop companies with mistakes
library(dplyr)
library(data.table)

#drop the mistakes in webscraping industry
drop_mistakes <- Mondaydata [industry  %like% "Industry"]
unique(drop_mistakes$industry)

#drop the unverified benefit companies
drop_mistakes1 <- drop_mistakes[sum !=0]
drop_mistakes1 <- NULL
Mondaydata$US <- ifelse(grepl("^[[:upper:]]+$",Mondaydata$location_state),1,0)

saveRDS(drop_mistakes1, 'drop_mistakes1.rds')
write.csv(drop_mistakes1, 'drop_mistakes1.csv', row.names = F)

#merge all the datasets 
datatable11 <- merge (final_data, datatable5)
datatable12 <- merge(final_data, datatable11, by.x=company_names)
setkey(datatable11, companynames)
setkey(final_data, company_name)
datatable12 <- merge(datatable11, final_data, by.x= 'companynames', by.y='company_name')


#benefits without comments scrape employee verified this time 
datatable11 <- NULL
datatable11 <- data.table(datatable11)

for (i in 1:852) {
  l <- read_html(datatable9k[[9]] [[i]])
  
  my_companynames3 <- trimws(l %>%
                               html_nodes('.tightAll') %>%
                               html_text())
  
  my_list <- l %>%
    html_nodes('.employerVerified') %>%
    html_text() 
  
  my_table11=
    data.table('companynames'= my_companynames3,'benefit_list' = my_list)
  
  datatable11 <-rbind(datatable11, my_table11)
  message(i)
}

# CLEANING
#how to make the benefit vars binary
datatable11$Health_Insurance<- ifelse(grepl("Health Insurance",datatable11$benefit_list),1,0)

datatable11$Dental_Insurance<- ifelse(grepl("Dental Insurance",datatable11$benefit_list),1,0)

datatable11$FSA<- ifelse(grepl("Flexible Spending Account",datatable11$benefit_list),1,0)

datatable11$Vision_iInsurance<- ifelse(grepl("Vision Insurance",datatable11$benefit_list),1,0)

datatable11$HSA<- ifelse(grepl("Health Savings Account",datatable11$benefit_list),1,0)

datatable11$Life_Insurance <- ifelse(grepl("Life Insurance",datatable11$benefit_list),1,0)

datatable11$SLI <- ifelse(grepl("Supplemental Life Insurance",datatable11$benefit_list),1,0)

datatable11$ Disability_Insurance<- ifelse(grepl("Disability Insurance",datatable11$benefit_list),1,0)

datatable11$Accident_Insurance <- ifelse(grepl("Accident Insurance",datatable11$benefit_list),1,0)

datatable11$Health_Care_On_Site<- ifelse(grepl("Health Care On-Site",datatable11$benefit_list),1,0)

datatable11$Mental_Health_Care <- ifelse(grepl("Mental Health Care",datatable11$benefit_list),1,0)

datatable11$Retiree_Health<- ifelse(grepl("Retiree Health",datatable11$benefit_list),1,0)

datatable11$Dismemberment_Insurance <- ifelse(grepl("Dismemberment Insurance",datatable11$benefit_list),1,0)

datatable11$Pension_Plan <- ifelse(grepl("Pension Plan",datatable11$benefit_list),1,0)

datatable11$retire_401K<- ifelse(grepl("401K",datatable11$benefit_list),1,0)

datatable11$Retirement_Plan <-ifelse(grepl("Retirement_Plan",datatable11$benefit_list),1,0)

datatable11$Employee_Stock <- ifelse(grepl("Employee Stock",datatable11$benefit_list),1,0)

datatable11$Performance_Bonus <- ifelse(grepl("Performance Bonus",datatable11$benefit_list),1,0)

datatable11$Stock_Options <- ifelse(grepl("Stock Options",datatable11$benefit_list),1,0)

datatable11$Equity_Incentive <- ifelse(grepl("Equity Incentive Plan",datatable11$benefit_list),1,0)

datatable11$Supplemental_Workers <- ifelse(grepl("Supplemental Workers",datatable11$benefit_list),1,0)

datatable11$Charitable_Gift <- ifelse(grepl("Charitable Gift",datatable11$benefit_list),1,0)

datatable11$Maternity_Leave <- ifelse(grepl("Maternity & Paternity Leave",datatable11$benefit_list),1,0)

datatable11$Work_From_Home <- ifelse(grepl("Work From Home",datatable11$benefit_list),1,0)

datatable11$Fertility_Assistance<- ifelse(grepl("Fertility Assistance",datatable11$benefit_list),1,0)

datatable11$Dependent_Care <- ifelse(grepl("Dependent Care",datatable11$benefit_list),1,0)

datatable11$Adoption_Assistance<- ifelse(grepl("Adoption Assistance",datatable11$benefit_list),1,0)

datatable11$Childcare<- ifelse(grepl("Childcare",datatable11$benefit_list),1,0)

datatable11$Flexible_Hours<- ifelse(grepl("Flexible Hours ",datatable11$benefit_list),1,0)

datatable11$Military_Leave <- ifelse(grepl("Military Leave",datatable11$benefit_list),1,0)

datatable11$Family_Medical_Leave <- ifelse(grepl("Family Medical Leave",datatable11$benefit_list),1,0)

datatable11$Unpaid_Extended_Leave <- ifelse(grepl("Unpaid Extended Leave ",datatable11$benefit_list),1,0)

datatable11$Paid_Holiday<- ifelse(grepl("Paid Holiday",datatable11$benefit_list),1,0)

datatable11$Paid_time_off<- ifelse(grepl("Vacation",datatable11$benefit_list),1,0)

datatable11$Sick_Days<- ifelse(grepl("Sick Days",datatable11$benefit_list),1,0)

datatable11$Volunteer_Time_Off <- ifelse(grepl("Volunteer Time Off",datatable11$benefit_list),1,0)

datatable11$Sabbatical<- ifelse(grepl("Sabbatical",datatable11$benefit_list),1,0)

datatable11$Bereavement_Leave <- ifelse(grepl("Bereavement Leave",datatable11$benefit_list),1,0)

datatable11$Employee_Discount <- ifelse(grepl("Employee Discount ",datatable11$benefit_list),1,0)

datatable11$Free_Lunch_Snacks <- ifelse(grepl("Free Lunch or Snacks ",datatable11$benefit_list),1,0)

datatable11$Employee_Assistance<- ifelse(grepl("Employee Assistance Program",datatable11$benefit_list),1,0)

datatable11$Gym_Membership <- ifelse(grepl("Gym Membership",datatable11$benefit_list),1,0)

datatable11$Commuter_Checks<- ifelse(grepl("Commuter Checks",datatable11$benefit_list),1,0)

datatable11$Pet_Friendly <- ifelse(grepl("Pet Friendly",datatable11$benefit_list),1,0)

datatable11$Mobile_Discount <- ifelse(grepl("Mobile Phone Discount",datatable11$benefit_list),1,0)

datatable11$Company_Car<- ifelse(grepl("Company Car",datatable11$benefit_list),1,0)

datatable11$Company_Social_Events <- ifelse(grepl("Company Social Events",datatable11$benefit_list),1,0)

datatable11$Travel_Concierge <- ifelse(grepl("Travel Concierge",datatable11$benefit_list),1,0)

datatable11$Legal_Assistance <- ifelse(grepl("Legal Assistance ",datatable11$benefit_list),1,0)

datatable11$Diversity_Program <- ifelse(grepl("Diversity Program",datatable11$benefit_list),1,0)

datatable11$Job_Training <- ifelse(grepl("Job Training",datatable11$benefit_list),1,0)

datatable11$Professional_Development <- ifelse(grepl("Professional Development",datatable11$benefit_list),1,0)

datatable11$Apprenticeship<- ifelse(grepl("Apprenticeship",datatable11$benefit_list),1,0)

datatable11$Tuition_Assistance <- ifelse(grepl("Tuition Assistance",datatable11$benefit_list),1,0)

datatable11k <- datatable11

saveRDS(datatable11k, 'datatable11k.rds')
write.csv(datatable11k, 'datatable11k.csv', row.names = F)

##how i split up the table and made one line per co 
datatable11k <- datatable11 %>% group_by(companynames) %>% summarise_all(funs(max(as.character(.)))) 
datatable11k <- unique(datatable11k, by="companynames")

##how to make the binary vars numeric
datatable11kk <- datatable11k
sapply(datatable11kk[[3]][[1]], class) 
stringsAsFactors=FALSE
datatable11kk[3:56] <- lapply(datatable11kk[3:56], function(x) as.numeric(as.character(x)))

###make ratings numeric
drop_mistakes2 <-drop_mistakes1
drop_mistakes2$rating <- trimws(drop_mistakes2$rating)
drop_mistakes2$rating <- as.numeric(drop_mistakes2$rating)
drop_mistakes2$benefit_ratings <- trimws(drop_mistakes2$benefit_ratings)
drop_mistakes2$benefit_ratings <- as.numeric(drop_mistakes2$benefit_ratings)

sapply(drop_mistakes2[[3]][[1]], class) 
drop_mistakes2 <- lapply(drop_mistakes2[11], function(x) as.numeric(as.character(x)))
sapply(drop_mistakes2[[3]][[1]], class) 

##how to sum the binary rows
datatable11kk$sum <- rowSums(datatable11kk[,3:56], na.rm=TRUE)

#how to see how many were 0
sort(table(drop_mistakes2$sum),decreasing=TRUE)[1:10]

#see average verified benefits
mean(drop_mistakes2$sum)

#save finally
saveRDS(datatable11kk, 'datatable11kk.rds')
write.csv(datatable11kk, 'datatable11kk.csv', row.names = F)

#merge
setkey(datatable9k, companynames)
setkey(datatable11kk, companynames)
Mondaydata <- merge(datatable9k, datatable11kk, by.x= 'companynames', by.y='companynames')

#save finally
saveRDS(Mondaydata, 'Mondaydata.rds')
write.csv(Mondaydata, 'Mondaydata.csv', row.names = F)

#look at data
hist(drop_mistakes2$sum)
hist(drop_mistakes2$US)
hist(drop_mistakes2$rating)
hist(drop_mistakes2$benefit_ratings)
hist(drop_mistakes2$Vision_iInsurance)
describe(drop_mistakes2)


barplot(table(drop_mistakes2$industry))
table(drop_mistakes2$industry)

barplot(table(drop_mistakes2$revenue))
table(drop_mistakes2$revenue)

barplot(table(drop_mistakes2$size))
table(drop_mistakes2$size)

ggplot(drop_mistakes3, aes(x = benefit_ratings, y = rating, color = Retiree_Health)) +
  geom_point(size=2)+
  geom_smooth()  

#save data
saveRDS(drop_mistakes2, 'drop_mistakes2.rds')
write.csv(drop_mistakes2, 'drop_mistakes2.csv', row.names = F)

#drop the columns we don't need for ouor models
drop_mistakes3 <- subset(drop_mistakes2, select = -c(1,2,5,9,10,12,68))

##################################################

#Data cleaning II. - for analysis ########## 
#removing benefit ratings following Misi's advice

#install.packages("randomForest")
library(randomForest)
#install.packages("ggplot2")
#install.packages("cowplot")
library(ggplot2)
library(cowplot)
library(data.table)
#install.packages("randomForestExplainer")
library(randomForestExplainer)

#cleaning, factorizing
drop_mistakes3$size_code <- ifelse(drop_mistakes3$size== "Size1 to 50 employees",1,
                            ifelse(drop_mistakes3$size== "Size51 to 200 employees",2,
                            ifelse(drop_mistakes3$size== "Size201 to 500 employees",3,
                            ifelse(drop_mistakes3$size== "Size501 to 1000 employees",4,
                            ifelse(drop_mistakes3$size== "Size1001 to 5000 employees",5, 
                            ifelse(drop_mistakes3$size== "Size5001 to 10000 employees",6,
                           ifelse(drop_mistakes3$size== "Size10000+ employees",7,0)))))))  

unique(drop_mistakes3$revenue)
drop_mistakes3$revenue_code <- ifelse(drop_mistakes3$revenue=="Revenue Less than $1 million (USD) per year",1,
                               ifelse(drop_mistakes3$revenue=="Revenue $5 to $10 million (USD) per year",2,              
                               ifelse(drop_mistakes3$revenue=="Revenue $25 to $50 million (USD) per year",3,        
                               ifelse(drop_mistakes3$revenue=="Revenue $50 to $100 million (USD) per year",4,
                               ifelse(drop_mistakes3$revenue=="Revenue $100 to $500 million (USD) per year",5,    
                              ifelse(drop_mistakes3$revenue=="Revenue $500 million to $1 billion (USD) per year",6,  
                              ifelse(drop_mistakes3$revenue=="Revenue $1 to $2 billion (USD) per year",7,          
                              ifelse(drop_mistakes3$revenue=="Revenue $2 to $5 billion (USD) per year",8,          
                              ifelse(drop_mistakes3$revenue=="Revenue $5 to $10 billion (USD) per year",9,        
                              ifelse(drop_mistakes3$revenue=="Revenue $10+ billion (USD) per year",10,0))))))))))              

### removing benefit ratings following Misi's suggestion 
drop_mistakes4 <- subset(drop_mistakes3, select = -c(2:5))
drop_mistakes4 <- subset(drop_mistakes4, select = -c(2))
#no company has observation for Retirement_Plan
drop_mistakes4 <- subset(drop_mistakes4, select = -c(14))

str(drop_mistakes4)
saveRDS(drop_mistakes4, 'drop_mistakes4.rds')

##############################################################################################################################

# Install packages for analysis ####
#install.packages("tree")
#install.packages("ISLR")
#install.packages("MASS")
#install.packages("randomForest")
#install.packages("gbm")
#install.packages('reprtree')
#install.packages('rpart.plot')
#install.packages('Ecdat')
#install.packages('psych')
#install.packages('Hmisc')
#install.packages('summarytools')
#install.packages('outreg')

library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)
library(reprtree)
library(rpart.plot)
library(Ecdat)
library(psych)
library(Hmisc)
library(summarytools)
library(outreg)
######

#Look at data ####
hist(drop_mistakes2$sum)
hist(drop_mistakes2$US)
hist(drop_mistakes2$rating)
hist(drop_mistakes2$Vision_iInsurance)
summary(Boston)
describe(drop_mistakes2)

barplot(table(drop_mistakes2$industry))
table(drop_mistakes2$industry)

barplot(table(drop_mistakes2$revenue))
table(drop_mistakes2$revenue)

barplot(table(drop_mistakes2$size))
table(drop_mistakes2$size)

#######

# LINEAR REGRESSION ####
#linear model with everything
linearmodel.everything=lm(rating ~., data=drop_mistakes4)
summary(linearmodel.everything)
print(linearmodel.everything)

# rating and sum
linearmodel.sum=lm(rating ~sum, data=drop_mistakes4)
summary(linearmodel.sum)
plot(drop_mistakes4$rating~ drop_mistakes4$sum)
abline(lm(drop_mistakes4$rating ~drop_mistakes4$sum))

#linear model with everything except for sum (there was collinearity between sum and the other x variables)
drop_mistakes5 <- subset(drop_mistakes4, select = -c(55))
linearmodel.exc_sum=lm(rating ~., data=drop_mistakes5)
summary(linearmodel.exc_sum)
print(linearmodel.exc_sum)

## model with random subsample1
set.seed(1)
train = sample(1:nrow(drop_mistakes4), nrow(drop_mistakes4)/2)
linearmodel.drop_mistakes4=lm(rating~.,drop_mistakes3,subset=train)
summary(linearmodel.drop_mistakes4)
outreg(linearmodel.drop_mistakes4)

##subsample2
set.seed(5)
train = sample(5:nrow(drop_mistakes4), nrow(drop_mistakes4)/3)
linearmodel.drop_mistakes4=lm(rating~.,drop_mistakes4,subset=train)
summary(linearmodel.drop_mistakes4)
outreg(linearmodel.drop_mistakes4)

#####

# DECISION TREE #####
tree.drop_mistakes5 <-NULL
cv.drop_mistakes5 <- NULL
train <- NULL
stringsAsFactors=FALSE

set.seed(1)
train = sample(1:nrow(drop_mistakes5), nrow(drop_mistakes5)/2)
tree.drop_mistakes5=tree(rating~.,drop_mistakes5,subset=train)
summary(tree.drop_mistakes5)
plot(tree.drop_mistakes5)
text(tree.drop_mistakes5,pretty=0)

set.seed(1)
train = sample(1:nrow(drop_mistakes5), nrow(drop_mistakes5)/2)
tree.1=tree(rating~.,drop_mistakes5,subset=train)
summary(tree.1)
plot(tree.1)
text(tree.1,pretty=0)

#works
prune.drop_mistakes5=prune.tree(tree.1,best=10)
plot(prune.drop_mistakes5)
text(prune.drop_mistakes5,pretty=0)

saveRDS(drop_mistakes5, 'drop_mistakes5.rds')

saveRDS(drop_mistakes4, 'drop_mistakes4.rds')
write.csv(drop_mistakes4, 'drop_mistakes4.csv', row.names = F)
#######

## RANDOM FOREST  ##########
set.seed(100)
train <- sample(nrow(drop_mistakes5), 0.7*nrow(drop_mistakes5), replace = FALSE)
TrainSet <- drop_mistakes5[train,]
ValidSet <- drop_mistakes5[-train,]
summary(TrainSet)
summary(ValidSet)

# Model1
model1 <- randomForest(rating ~ ., data = TrainSet, proximity = TRUE, ntree=500)
model1
model1$mse
randomForest::getTree
plot(model1)
#since based on the graph the error term will not change after 300 we cutted the number of trees 300

# Model2 (mean of squared residuals: 0.178, %var explained 8.07) mtry=7
model2 <- randomForest(rating ~ ., data = TrainSet, proximity = TRUE,mtry=7, ntree=300, importance=TRUE, localImp = TRUE, na.action=na.omit, oob.prox=FALSE)
model2

# Model3 (mean of squared residuals: 0.178, %var explained 8.14) mtry=8
model3 <- randomForest(rating ~ ., data = TrainSet, proximity = TRUE,mtry=8, ntree=300, importance=TRUE, localImp = TRUE, na.action=na.omit, oob.prox=FALSE)
model3

# Model4 (mean of squared residuals: 0.178, %var explained 8.23) mtry=6
model4 <- randomForest(rating ~ ., data = TrainSet, proximity = TRUE,mtry=6, ntree=300, importance=TRUE, localImp = TRUE, na.action=na.omit, oob.prox=FALSE)
model4 

# Model5 (mean of squared residuals: 0.179, %var explained 7.88) mtry=9
model5 <- randomForest(rating ~ ., data = TrainSet, proximity = TRUE,mtry=9, ntree=300, importance=TRUE, localImp = TRUE, na.action=na.omit, oob.prox=FALSE)
model5

# Model6 (mean of squared residuals: 0.18, %var explained 7.17) mtry=10
model6 <- randomForest(rating ~ ., data = TrainSet, proximity = TRUE,mtry=10, ntree=300, importance=TRUE, localImp = TRUE, na.action=na.omit, oob.prox=FALSE)
model6

# Model7 (mean of squared residuals: 0.183, %var explained 5.55) mtry=30
model7 <- randomForest(rating ~ ., data = TrainSet, proximity = TRUE,mtry=30, ntree=300, importance=TRUE, localImp = TRUE, na.action=na.omit, oob.prox=FALSE)
model7

### Model4 explained the higher variance (8.23) (while the difference is not great between them) we choose it to our best model

varImpPlot(model4) #shows which variables are important
model4$rsq #pseudo rsquared
model4$test

## Show "importance" of variables: higher value mean more important:
print(model4)
round(importance(model4), 2)

importance(model4)
Importance <- data.frame(variable = names(model4$importance[,1]), importance = model4$importance[,1])
Importance <- Importance[ order(-Importance[,2]),]
Importance
Importance <- as.data.table(Importance) #https://medium.com/usf-msds/intuitive-interpretation-of-random-forest-2238687cae45
## test proximity in regression
str(model2$proximity)

#### Random forest explainer - creates a html file from our model
explain_forest(model4, interactions = TRUE, data = drop_mistakes5)
explain_forest(model8, interactions = TRUE, data = drop_mistakes5)
#########################

# Cross validation for random forest ####
set.seed(1)
#fit model 1
k.model <- train(rating~.,drop_mistakes5,
                 method="rf",
                 trControl =trainControl(method="cv", number=10,
                                         verboseIter=TRUE
                 ))
print(k.model)

## Cross validation showed sample size is 386, mtry is 2 or 29 (it also showed 59, but we have 57 variables, so it would risk the danger of overfitting)

# Model8 (mean of squared residuals: 0.182, %var explained 6.45) mtry=2
model8 <- randomForest(rating ~ ., data = TrainSet, proximity = TRUE,mtry=2, ntree=386, importance=TRUE, localImp = TRUE, na.action=na.omit, oob.prox=FALSE)
model8
varImpPlot(model8) #shows which variables are important
model8$rsq #pseudo rsquared
model8$test
## Show "importance" of variables: higher value mean more important:
print(model8)
round(importance(model8), 2)
importance(model8)
Importance <- data.frame(variable = names(model8$importance[,1]), importance = model8$importance[,1])
Importance <- Importance[ order(-Importance[,2]),]
Importance
Importance <- as.data.table(Importance) #https://medium.com/usf-msds/intuitive-interpretation-of-random-forest-2238687cae45
## test proximity in regression
str(model8$proximity)

# Model9 (mean of squared residuals: 0.179, %var explained 7.55) mtry=2
model9 <- randomForest(rating ~ ., data = TrainSet, proximity = TRUE,mtry=29, ntree=386, importance=TRUE, localImp = TRUE, na.action=na.omit, oob.prox=FALSE)
model9

#######

###Comparing analytical techniques ########

#linear regression 
set.seed(1)
train = sample(1:nrow(drop_mistakes5), nrow(drop_mistakes5)/2)
linearmodel.drop_mistakes5_m=lm(rating~.,drop_mistakes5,subset=train)
summary(linearmodel.drop_mistakes5_m)
yhat=predict(linearmodel.drop_mistakes5_m,newdata=drop_mistakes5[-train,])
drop_mistakes5_m.test=drop_mistakes5[-train,"rating"]
#linearmodel.drop_mistakes4.test=linearmodel.drop_mistakes4[-train,"rating"]
plot(yhat,drop_mistakes5_m.test$rating)
abline(0,1)
mean((yhat-drop_mistakes5_m.test$rating)^2)
#MSE=0.2550

#Decision Tree
set.seed(1)
train = sample(1:nrow(drop_mistakes5), nrow(drop_mistakes5)/2)
tree.DT=tree(rating~.,drop_mistakes5,subset=train)
summary(tree.DT)
plot(tree.DT)
text(tree.DT,pretty=0)
cv.DT=cv.tree(tree.DT)
plot(cv.DT$size,cv.DT$dev,type='b')
prune.DT=prune.tree(tree.DT,best=7)
plot(prune.DT)
text(prune.DT,pretty=0)
#using the full tree
yhat=predict(prune.DT,newdata=drop_mistakes5[-train,])
DT.test=drop_mistakes5[-train,"rating"]
plot(yhat,DT.test$rating)
abline(0,1)
mean((yhat-DT.test$rating)^2)
#MSE=0.239

# Random Forest
#Model4 (mtry=6, ntree=300)
set.seed(1)
rf.1=randomForest(rating~.,data=drop_mistakes5,subset=train,mtry=6,importance=TRUE,ntree=300)
yhat.rf = predict(rf.1,newdata=drop_mistakes5[-train,])
RF.test=drop_mistakes5[-train,"rating"]
mean((yhat.rf-RF.test$rating)^2)
#MSE= 0.1967

#Model8 (mtry=2, ntree=386)
set.seed(1)
rf.2=randomForest(rating~.,data=drop_mistakes5,subset=train,mtry=2,importance=TRUE,ntree=386)
yhat.rf = predict(rf.2,newdata=drop_mistakes5[-train,])
RF.test=drop_mistakes5[-train,"rating"]
mean((yhat.rf-RF.test$rating)^2)
#MSE= 0.1959

#Model9 (mtry=29, ntree=386)
set.seed(1)
rf.2=randomForest(rating~.,data=drop_mistakes5,subset=train,mtry=29,importance=TRUE,ntree=386)
yhat.rf = predict(rf.2,newdata=drop_mistakes5[-train,])
RF.test=drop_mistakes5[-train,"rating"]
mean((yhat.rf-RF.test$rating)^2)
#MSE= 0.202


#######


##############################################################################################################################
##############################################################################################################################
##############################################################################################################################





###### getting the comments vol2 (after unexpected data loss, due to computer motherboard problems and replacement/dataloss) ######

merge <- merge(drop_mistakes2, datatable9k, all=TRUE, sort=FALSE)
######## to get the first page of comments 

datatable18 <- NULL
datatable18 <- data.table(datatable18)

for (i in 1:429) {
  url <- paste0(drop_mistakes2[[9]] [[i]])
  l <- read_html(url)
  
  my_companynames3 <-
    trimws(l%>%
             html_nodes('.tightAll') %>%
             html_text())
  my_companynames3
  
  my_comments <- l%>%
    html_nodes('.wrapToggleStr') %>%
    html_text()
  
  my_table18=
    data.table('companynames'= my_companynames3, 'comments'=my_comments)
  
  datatable18 <-rbind(datatable18,my_table18)
  message(i)
}

##to delete duplicates##
datatable18k <- unique(datatable18[,c("companynames", "comments")])

saveRDS(datatable18k, 'datatable8k.rds')
write.csv(datatable18k, 'datatable8k', row.names = F)

datatable18k$companynames

##to get the rest of the comments##

# https://www.glassdoor.com/Benefits/3M-US-Benefits-EI_IE446.0,2_IL.3,5_IN1_IP2.htm
# https://www.glassdoor.com/Benefits/3M-US-Benefits-EI_IE446.0,2_IL.3,5_IN1.htm

datatable106 <- NULL
datatable106 <- data.table(datatable106)

for (i in 1:429) {
  delayedAssign("do.next", {next})
  for (j in 2:10000) {
    tryCatch({
      url <- paste0(drop_mistakes2 [[9]] [[i]],'_IP',j,'.htm')
      l <- read_html(url)
      
      my_companynames3 <-
        trimws(l%>%
                 html_nodes('.tightAll') %>%
                 html_text())
      my_companynames3
      
      my_comments <- l%>%
        html_nodes('.wrapToggleStr') %>%
        html_text()
      
      my_table106=
        data.table('companynames'= my_companynames3, 'comments'=my_comments)
      
      datatable106 <-rbind(datatable106,my_table106)
      message(i,'/',j)}, error=function(e) force(do.next))}
}

#matching
saveRDS(datatable18, 'datatable18.rds')
saveRDS(datatable106, 'datatable106.rds')
merge <- merge(datatable18, datatable106, all=TRUE, sort=TRUE)
saveRDS(datatable106, 'datatable_comments.rds')
saveRDS(merge, 'all_f_comments.rds')
saveRDS(merge, 'all_f_comments.json')
saveRDS(merge, 'merge.rds')


############################################################################################################

#sentiment analysis - trying ####

install.packages("SnowballC")
install.packages("streamR")

library(SnowballC)
library(streamR)
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)


# loading lexicon of positive and negative words (from Neal Caren)
library(readxl)
lexicon <- read_excel("~/Desktop/lexicon.xlsx")
View(lexicon)
pos.words <- lexicon$word[lexicon$polarity=="positive"]
neg.words <- lexicon$word[lexicon$polarity=="negative"]

#sentiments
# a look at a random sample of positive and negative words
sample(pos.words, 10)
sample(neg.words, 10)

text <- clean_tweets(merge$comments)
# but we want to aggregate over many tweets...
classifier <- function(text, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(text, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}

clean_tweets <- function(text){
  # loading required packages
  lapply(c("tm", "SnowballC", "stringr"), require, c=T, q=T)
  # avoid encoding issues by dropping non-unicode characters
  utf8text <- iconv(text, to='UTF-8', sub = "byte")
  # remove punctuation and convert to lower case
  words <- removePunctuation(utf8text)
  words <- tolower(words)
  # spliting in words
  words <- str_split(words, " ")
  return(words)
}


# applying classifier function
classifier(text, pos.words, neg.words)
classify(text, pos.words, neg.words)

############


