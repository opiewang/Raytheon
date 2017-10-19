library("readxl")
library("dplyr")
library("reshape")
library("caret")
setwd("~/Raytheon")
raw_data<-read_excel("sheet1.xlsx",sheet=1,col_names = TRUE, na="")
raw_data <- raw_data %>%
  rename(c('D/I'="di"))%>%
  rename(c('SalaryLevel'="salary"))%>%
  rename(c('Regular / Temporary'="jobtype"))%>%
  rename(c('Mgr Level'="mgrlevel"))%>%
  rename(c('Reporting Level'="replevel"))%>%
  rename(c('Overtime Hours - Aug 2016 - Jul 2017'="othrs"))
raw_data$di<-factor(raw_data$di)
raw_data$jobtype<-factor(raw_data$jobtype)
raw_data$salary<-factor(raw_data$salary)
raw_data$mgrlevel<-factor(raw_data$mgrlevel)
raw_data$replevel<-factor(raw_data$replevel)
sapply(raw_data,function(x) sum(is.na(x)))
data<-raw_data %>%
  subset(select=c(6:30,38,41,44:48,52))
inTrain<-createDataPartition(data$emst,
                             p=0.67,
                             list=FALSE,
                             times = 1)
train<-data[inTrain,]
test<-data[-inTrain,]
model <- glm(emst ~.,family=binomial(link='logit'),data=train)
summary(model)
fitted.results <- predict(model,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
prate <-fitted.results != test$emst
misClasificError<-length(prate[prate==TRUE])/length(prate)
print(paste('Accuracy',1-misClasificError))
cm<-confusionMatrix(data=fitted.results, reference=test$emst)
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('ACCURACY CHART', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#27AE60')
  text(195, 435, 'Quit', cex=1.2)
  rect(250, 430, 340, 370, col='#E74C3C')
  text(295, 435, 'Active', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#E74C3C')
  rect(250, 305, 340, 365, col='#27AE60')
  text(140, 400, 'Quit', cex=1.2, srt=90)
  text(140, 335, 'Active', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
} 
draw_confusion_matrix(cm)
