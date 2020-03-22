#Clean the environment
rm(list = ls())

#Set working directory
setwd("C:/Users/Divyanshu/Desktop/Data Science_Edvisor")

#Load the librarires
libraries = c("plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Read the csv file
D = read.csv(file = "day.csv", header = T, sep = ",", na.strings = c(" ", "", "NA"))

########################################EXPLORE THE DATA########################################
#First few rows
head(D)

#Dimensions of data
dim(D)

#Column names
names(D)

#Structure of variables
str(D)

########################################FEATURE ENGINEERING########################################
#Create columns
D$cal_temp <- D$temp*39
D$cal_atemp <- D$atemp*50
D$cal_windspeed <- D$windspeed*67
D$cal_hum = D$hum * 100

D$new_season = factor(x = D$season, levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
D$new_yr = factor(x = D$yr, levels = c(0,1), labels = c("2011","2012"))
D$new_holiday = factor(x = D$holiday, levels = c(0,1), labels = c("Working day","Holiday"))
D$new_weathersit = factor(x = D$weathersit, levels = c(1,2,3,4), 
                               labels = c("Clear","Cloudy/Mist","Rain/Snow/Fog","Heavy Rain/Snow/Fog"))

D$weathersit = as.factor(D$weathersit)
D$season = as.factor(D$season)
D$dteday = as.character(D$dteday)
D$mnth = as.factor(D$mnth)
D$weekday = as.factor(as.character(D$weekday))
D$workingday = as.factor(as.character(D$workingday))
D$yr = as.factor(D$yr)
D$holiday = as.factor(D$holiday)

########################################MISSING VALUES########################################
missing_values = sapply(D, function(x){sum(is.na(x))})


########################################EXPLORE USING GRAPHS########################################
#Check the distribution of categorical Data using bar graph
bar1 = ggplot(data = D, aes(x = new_season)) + geom_bar() + ggtitle("Count of Season")
bar2 = ggplot(data = D, aes(x = new_weathersit)) + geom_bar() + ggtitle("Count of Weather")
bar3 = ggplot(data = D, aes(x = new_holiday)) + geom_bar() + ggtitle("Count of Holiday")
bar4 = ggplot(data = D, aes(x = workingday)) + geom_bar() + ggtitle("Count of Working day")
# ## Plotting plots together
gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)

#Check the distribution of numerical data using histogram
hist1 = ggplot(data = D, aes(x =cal_temp)) + ggtitle("Distribution of Temperature") + geom_histogram(bins = 25)
hist2 = ggplot(data = D, aes(x =cal_hum)) + ggtitle("Distribution of Humidity") + geom_histogram(bins = 25)
hist3 = ggplot(data = D, aes(x =cal_atemp)) + ggtitle("Distribution of Feel Temperature") + geom_histogram(bins = 25)
hist4 = ggplot(data = D, aes(x =cal_windspeed)) + ggtitle("Distribution of Windspeed") + geom_histogram(bins = 25)
gridExtra::grid.arrange(hist1,hist2,hist3,hist4,ncol=2)

#Check the distribution of numerical data using scatterplot
scat1 = ggplot(data = D, aes(x =cal_temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike COunt")
scat2 = ggplot(data = D, aes(x =cal_hum, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike COunt")
scat3 = ggplot(data = D, aes(x =cal_atemp, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point() + xlab("Feel Temperature") + ylab("Bike COunt")
scat4 = ggplot(data = D, aes(x =cal_windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike COunt")
gridExtra::grid.arrange(scat1,scat2,scat3,scat4,ncol=2)

#Check for outliers in data using boxplot
cnames = colnames(D[,c("cal_temp","cal_atemp","cal_windspeed","cal_hum")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = D)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "green" ,outlier.shape=20,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn3,gn2,gn4,ncol=2)

#Remove outliers in Windspeed
val = D[,19][D[,19] %in% boxplot.stats(D[,19])$out]
D = D[which(!D[,19] %in% val),]

#Check for multicollinearity using VIF
df = D[,c("instant","temp","atemp","hum","windspeed")]
vifcor(df)

#Check for collinearity using corelation graph
corrgram(D, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Remove the unwanted variables

D <- subset(D, select = -c(holiday,instant,dteday,atemp,casual,registered,cal_temp,cal_atemp,cal_windspeed,
                               cal_hum,new_season,new_yr,new_holiday,new_weathersit))

rmExcept(keepers = "D")
########################################DECISION TREE########################################
#MAPE: 24.63%
#MAE: 688
#RMSE: 936.3
#Accuracy: 75.37%

#Divide the data into train and test
set.seed(123)
train_index = sample(1:nrow(D), 0.7 * nrow(D))
train = D[train_index,]
test = D[-train_index,]

#rpart for regression
d_model = rpart(cnt ~ ., data = train, method = "anova")

#Predict the test cases
d_predictions = predict(d_model, test[,-10])

#Create dataframe for actual and predicted values
df = data.frame("actual"=test[,10], "pred"=d_predictions)
head(df)

#calculate MAPE
regr.eval(trues = test[,10], preds = d_predictions, stats = c("mae","mse","rmse","mape"))

#calculate MAPE
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test[,10], d_predictions)

########################################RANDOM FOREST########################################
#MAPE: 19.14%
#MAE: 508
#RMSE: 718
#Accuracy: 80.86%

#Train the data using random forest
r_model = randomForest(cnt~., data = train, ntree = 500)

#Predict the test cases
r_predictions = predict(r_model, test[,-10])

#Create dataframe for actual and predicted values
df = cbind(df,r_predictions)
head(df)

#Calculate MAPE
regr.eval(trues = test[,10], preds = r_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], r_predictions)

########################################LINEAR REGRESSION########################################
#MAPE: 19.87%
#RMSE: 825
#Accuracy: 80.13%
#MAE: 597
#Adjusted R squared: 0.8398
#F-statistic: 100

#Train the data using linear regression
l_model = lm(formula = cnt~., data = train)

#Check the summary of the model
summary(l_model)

#Predict the test cases
l_predictions = predict(l_model, test[,-10])

#Create dataframe for actual and predicted values
df = cbind(df,l_predictions)
head(df)

#Calculate MAPE
regr.eval(trues = test[,10], preds = l_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], lr_predictions)

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="green")
lines(l_predictions,col="red")

#Predict a sample data
predict(l_model,test[2,])
