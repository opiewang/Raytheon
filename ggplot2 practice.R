install.packages(c("plyr", "dplyr", "knitr", "ggplot2", "stringr", "tidyr", "readxl", "reshape2","scales"))
ggplot(mpg,aes(drv,displ))+
  geom_violin()
ggplot(mpg,aes(hwy,cty))+
  geom_()
ggplot(mpg,aes(class,displ))+
  scale_y_continuous("Displacement (l")+
  scale_x_discrete("Car Type")+
  scale_x_discrete("Type of car")+
  scale_color_discrete()+
  geom_point(aes(color=drv))+
  scale_color_discrete("Driver")
ggplot(mpg,aes(class,displ,color=drv))+
  geom_point()+
  scale_x_discrete("Type of Car")+
  scale_y_continuous("Displacement(l)")+
   scale_color_discrete("Driver")
ggplot(mpg,aes(displ,hwy))+
  geom_point()+
  scale_x_continuous("Displacement",breaks=c(2:7),labels=c("2L","3L","4L","5L","6L","7L"))+
  scale_y_continuous("Highway(miles/gallon)")
ggplot(mpg,aes(displ,hwy,color=drv))+
  geom_point()+
  scale_color_discrete(labels=c("4wd","fwd","nwd"))

  