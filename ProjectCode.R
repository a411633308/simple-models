load("G:/Unterricht/04-2019-in TUC/10-2019-Vorlesungen/maschienllenLernen/Project/WeatherAustralia2020.RData")
colnames(WeatherAustralia)
stations = unique(WeatherAustralia$Location)

#------------------------ plot the columns for pre-processing --------------------#
library(ggplot2)
library(ggpubr)
# ==>background knowledge:  higher temprature and humidity, raise pressure.
#                           low air pressure, raise the chance to rain.
# --> in DARWIN: small change in temperature at 9 am in 3 years,
#                no big humidity difference with others,
#                but much lower average pressure as others.
p1 <- ggplot(data = WeatherAustralia, aes(y=Location, x=Temp9am))+  
        geom_point(color="pink")
p2 <- ggplot(data = WeatherAustralia, aes(y=Location, x=Humidity9am))+  
  geom_point(color="red")
p3 <- ggplot(data = WeatherAustralia, aes(y=Location, x=Pressure9am))+  
  geom_point(color="orange")
ggarrange(p1,p2,p3, ncol= 1, nrow= 3, labels=c("A","B","C"))

#draw the average WindGustSpeed of a station at maps
##get each stations longitude and latitude
lo <- c(
  WeatherAustralia$longitude[which(WeatherAustralia$Location=="AliceSprings")][1],
  WeatherAustralia$longitude[which(WeatherAustralia$Location=="Brisbane")][1],
  WeatherAustralia$longitude[which(WeatherAustralia$Location=="Darwin")][1],
  WeatherAustralia$longitude[which(WeatherAustralia$Location=="Melbourne")][1],
  WeatherAustralia$longitude[which(WeatherAustralia$Location=="Perth")][1],
  WeatherAustralia$longitude[which(WeatherAustralia$Location=="Portland")][1],
  WeatherAustralia$longitude[which(WeatherAustralia$Location=="Sydney")][1],
  WeatherAustralia$longitude[which(WeatherAustralia$Location=="WaggaWagga")][1]
  )
la <- c(
WeatherAustralia$latitude[which(WeatherAustralia$Location=="AliceSprings")][1],
WeatherAustralia$latitude[which(WeatherAustralia$Location=="Brisbane")][1],
WeatherAustralia$latitude[which(WeatherAustralia$Location=="Darwin")][1],
WeatherAustralia$latitude[which(WeatherAustralia$Location=="Melbourne")][1],
WeatherAustralia$latitude[which(WeatherAustralia$Location=="Perth")][1],
WeatherAustralia$latitude[which(WeatherAustralia$Location=="Portland")][1],
WeatherAustralia$latitude[which(WeatherAustralia$Location=="Sydney")][1],
WeatherAustralia$latitude[which(WeatherAustralia$Location=="WaggaWagga")][1])
length(WeatherAustralia$WindGustSpeed)
length(WeatherAustralia$longitude)
##count the average by stations
a <- aggregate(WeatherAustralia$WindGustSpeed,list(WeatherAustralia$Location), FUN = mean)
a <- cbind(a,lo)
a <- cbind(a,la)
#fre <- table(WeatherAustralia$Location,WeatherAustralia$RainTomorrow)
library(ggplot2)
mp<-ggplot()+geom_point(aes(x=a$la, y=a$lo,size= a$x),color="darkorange")+
  ggtitle("Australian WindGustSpeed")+theme(plot.title=element_text(size=25,color="red",face="italic"))+theme_grey(base_size = 32)  
mp

# the relationships between time and wind gust speed with label RainTomorrow
qplot(x = WindGustSpeed, y = Date ,colour = WeatherAustralia$RainTomorrow, data = WeatherAustralia)
#  distribution of humidity at 3 pm with two classes of RainTomorrow
ggplot(data = WeatherAustralia, aes(y=Humidity3pm, x=RainTomorrow))+  
  geom_boxplot(color="orange")

#correlations are in python script
#write.csv(df1,"d://data//dummmyData.csv",row.names = FALSE)
#write.csv(WeatherAustralia,"g://WeatherAustralia.csv",row.names = FALSE)
library(caTools)
sum(is.na(WeatherAustralia))
colnames(WeatherAustralia)
rainTomorrow <- WeatherAustralia[,c(-2,-19,-21,-22)]
set.seed(135)

split = sample.split(rainTomorrow,SplitRatio=.8)
train_data <- subset(rainTomorrow, split == TRUE)
test_data <- subset(rainTomorrow, split == FALSE)
logistic <- glm(RainTomorrow~., data = train_data, family = binomial(link="cloglog"))
summary(logistic)

rainTomorrow <- WeatherAustralia[,c(-2,-3,-4,-5,-11,-15,-17,-18,-19,-21,-22)]
table(splitdata$Location)

set.seed(135)
plot(R)
split = sample.split(rainTomorrow,SplitRatio=.8)
train_data2 <- subset(rainTomorrow, split == TRUE)
test_data2 <- subset(rainTomorrow, split == FALSE)
logistic2 <- glm(RainTomorrow~., data = train_data2, family = binomial(link="logit"))

library(pROC)

predict <- predict(logistic,test_data, type = "response")
pred.class <- predict > 0.3
roc.c <- roc(response = test_data$RainTomorrow, predictor = predict)
plot.roc(roc.c)

predict2 <- predict(logistic2,test_data2, type = "response")
pred.class2 <- predict2 >0.3
roc2.c <- roc(response = test_data2$RainTomorrow, predictor = predict2)
plot.roc(roc2.c)

library(caret)
test_data2$RainTomorrow <- test_data2$RainTomorrow=="Yes"
length(test_data2$RainTomorrow)
length(pred.class2)
confusionMatrix(pred.class2, test_data2$RainTomorrow)
confusionMatrix(pred.class, test_data$RainTomorrow)
(conf.mat1 <- as.matrix(table(true.class, class.pred1)))
(conf.mat2 <- as.matrix(table(true.class, class.pred2)))
fitted(logistic)
summary(roc2.c)
summary(logistic2)
anova(logistic, logistic2, test="Chisq")
