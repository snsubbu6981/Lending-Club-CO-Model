##Step1 - Importing dataset
lc.base <- read.csv("E:/Lending Club Modeling/LoanStats_2014.csv", stringsAsFactors = FALSE)
mode(lc.base$revol_util)
str(lc.base) ##using str function to make sure all the variables are of appropriate type
paste(lc.base$term) ##used this command on the original table to detect white space - trailing space found

## We're using stringsAsFactors function because we want not want R to read characters as factors
lc.base <- read.csv("E:/Lending Club Modeling/LoanStats_2014.csv",strip.white = TRUE,stringsAsFactors = FALSE)
str(lc.base) ##using str function to make sure all the variables are of appropriate type
paste(lc.base$term)
summary(lc.base$term)
##subset lc.base for loan term = 36 months
lc.base1 <- subset(lc.base,term == "36 months")
str(lc.base1, list.len=ncol(lc.base1))
summary(lc.base1$open_rv_24m)

##step2 - Removing variables
#creating a new dataset myvars 
# variables removed based on (i) No coapplicant found (ii) all values missing (iii) id variables (iv) redundant variables
myvars = names(lc.base1) %in% c("id","member_id","desc","last_pymnt_d","policy_code","application_type","annual_inc_joint",
"dti_joint","verification_status_joint","next_pymnt_d","last_credit_pull_d","term","open_acc_6m","open_il_6m",
"open_il_12m","open_il_24m","mths_since_rcnt_il","total_bal_il","il_util","open_rv_12m","open_rv_24m",
"max_bal_bc","all_util","inq_fi","total_cu_tl","inq_last_12m","fico_range_high","fico_range_low","issue_d")
lc.base2 = lc.base1[!myvars] #this code excludes variables in myvars dataset and creates a new lc.base2 dataset

##step3 creating dependent variable
# levels(lc.base2$loan_status)
lc.base3 = data.frame(lc.base2[,],ifelse(lc.base2$loan_status == "Charged Off",1,0))
names(lc.base3)
names(lc.base3)[83] = "Target"

str(lc.base3) ##using str function to make sure all the variables are of appropriate type

## Target is still a numeric variable
##converting target to a factor
lc.base3$Target = factor(lc.base3$Target)
levels(lc.base3$Target)
str(lc.base3) ##using str function to make sure all the variables are of appropriate type
#converting "int_rate" into a continuous variable
#converting "revol_util" into a continuous variable

lc.base3$int_rate = as.numeric(sub("%","",lc.base3$int_rate))/100
str(lc.base3$int_rate)
paste(lc.base3$int_rate)

lc.base3$revol_util = as.numeric(sub("%","",lc.base3$revol_util))/100
str(lc.base3$revol_util)
paste(lc.base3$revol_util)


table(lc.base3$Target)
## 0 = 151,772 & 1 = 10,798
## CO Rate =  10,798 / 151,772 = 7.1%
## CO rate in both training and test samples should be close to 7.1%

##step4 Creating training and test samples 
subset <- sample(nrow(lc.base3), nrow(lc.base3) * 0.6) # 60%-40% split
lc.train = lc.base3[subset, ]
lc.test = lc.base3[-subset, ]

table(lc.train$Target)
## 0 = 91,199 & 1 = 6,343
## CO Rate = 6,343/911,99 = 6.9%

table(lc.test$Target)
## 0 = 60,573 & 1 = 4,455
## CO Rate = 4,455/60,573 = 7.3%

#step4 Exploratory analysis
##We started with 84 variables at the start of exploratory analysis
str(lc.base3)

## Frequency table analysis - Done only for categorical variables
mytable=table(lc.train$Target,lc.train$addr_state)
mytable
prop.table(mytable, 2)

## univariate analysis - Done for continuous variables
hist(lc.train$loan_amnt) ## histogram helps you understand the distribtuion of a variable
boxplot(lc.train$loan_amnt) ## Helps understand distribution + any outliers

library(Hmisc) ## you need to install this package before running this command 
nums = sapply(lc.train,is.numeric) ##identifying numerical only variables for univariate
colnames(lc.train)

onlynum_train = lc.train[ ,nums] ##subsetting numerical variables from lc.train
str(onlynum_train)
describe(onlynum_train)

test1 = lc.train[,c("revol_util","Target")]

##bivariate analysis - Done for continuous variables
test1$revolutil_deciles <- cut(test1$revol_util,
                         breaks=c(0,quantile(test1$revol_util, prob=seq(0.1,1,0.1))))

table(test1$revolutil_deciles)

install.packages(sqldf)## you need to install this package before running this command 
library(sqldf)
sum.bad.rate <- sqldf('select revolutil_deciles ,
                              avg(revol_util) as mean_revolutil,
                     count(*) as TotalRecords,
                     sum(case when Target=="1" then 1 else 0 end) as CO,
                     sum(case when Target=="1" then 0 else 1 end) as Not_CO,
                     100*sum(case when Target=="1" then 1 else 0 end)/ sum(case when Target=="1" then 0 else 1 end) as CO_Rate
                     from test1
                     group by revolutil_deciles
                     ')

# Plot Charge Off
plot(x=sum.bad.rate$mean_revolutil,
     y=sum.bad.rate$CO_Rate,
     type = "b",
     xlab = "Avg Revolutil",
     ylab = "% Co",
     main="% CO by Revolve Utilization",
     col="red")



#step5 Excluding variables based on univariate analysis and frequency tables
myvars1 = names(lc.train) %in% c("issue_d" ) 
lc.train_univariate = lc.train[!myvars1] 


