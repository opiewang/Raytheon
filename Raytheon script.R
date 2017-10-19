library(readxl)
library(dplyr)
library(reshape)
setwd("~/Raytheon")
df<-read_excel("raytheon.xlsx",sheet=3, col_names = TRUE, na="") #load excel file into R
state<-c(state.abb) #extracted the abbreviation of 50 states
df1<-df%>% 
  rename(c('Emp Status 7/28/17'="emst"))%>% #renamed employee status to emst
  filter(emst %in% c("A","T")) #filtered emst by A(active) and T(Quit)
df1$Site<-substr(df1$Site,1,2) #extract state name using variable Site
#df1$emst<-factor(as.numeric(df1$emst=="A")) #Create binary variable based on A=1, T=0
df1$emst<-factor(df1$emst)
#head(df1$emst)
#str(df1$Site)
df2<-df1%>%
  filter(Site %in% state)%>% #filter data using the states name
  rename(c(Site="state"))%>%
  rename(c('D/I'="di"))
df2$di<-factor(df1$di)
levels(factor(df1$Grade))
row_number(df1$Grade=="G01")
#class(df2$di)
#write.csv(df2,"rdata.csv") #export data into csv file
levels(factor(df1$Site))
