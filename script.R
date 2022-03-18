library(ggplot2)
library(corrplot)

#loading data
fpath<-"bodyPerformance.csv"
body_perf<-read.csv(fpath,header=TRUE,stringsAsFactors = FALSE)

#basics
dim(body_perf)
colnames(body_perf)
head(body_perf)
summary(body_perf)


#cleaning data
colSums(is.na(body_perf))

#plotting data

#fat
ggplot(body_perf, aes(x=fat,fill=gender, color=gender)) +
geom_histogram(breaks=seq(0, 80, by=1),alpha = 0.5,position="identity") + scale_color_manual(values=c("#D999C7", "#7EC8C7"))+
  scale_fill_manual(values=c("#D999C7", "#7EC8C7"))
ggplot(body_perf, aes(x=fat,fill=gender, color=gender)) +
  geom_boxplot(alpha = 0.5,position="identity") + scale_color_manual(values=c("#D999C7", "#7EC8C7"))+
  scale_fill_manual(values=c("#D999C7", "#7EC8C7"))

#height
ggplot(body_perf, aes(x=height,fill=gender, color=gender)) +
  geom_histogram(breaks=seq(140, 200, by=1),alpha = 0.5,position="identity") + scale_color_manual(values=c("#D999C7", "#7EC8C7"))+
  scale_fill_manual(values=c("#D999C7", "#7EC8C7"))
ggplot(body_perf, aes(x=height,fill=gender, color=gender)) +
  geom_boxplot(alpha = 0.5,position="identity") + scale_color_manual(values=c("#D999C7", "#7EC8C7"))+
  scale_fill_manual(values=c("#D999C7", "#7EC8C7"))

#weight
ggplot(body_perf,aes(x=weight,fill=gender, color=gender)) +
  geom_histogram(breaks=seq(0, 200, by=1),alpha = 0.5,position="identity") + scale_color_manual(values=c("#D999C7", "#7EC8C7"))+
  scale_fill_manual(values=c("#D999C7", "#7EC8C7"))
ggplot(body_perf, aes(x=weight,fill=gender, color=gender)) +
  geom_boxplot(alpha = 0.5,position="identity") + scale_color_manual(values=c("#D999C7", "#7EC8C7"))+
  scale_fill_manual(values=c("#D999C7", "#7EC8C7"))

#diastolic
ggplot(body_perf,aes(x=diastolic,fill=gender, color=gender)) +
  geom_histogram(breaks=seq(0, 150, by=1),alpha = 0.5,position="identity") + scale_color_manual(values=c("#D999C7", "#7EC8C7"))+
  scale_fill_manual(values=c("#D999C7", "#7EC8C7"))
ggplot(body_perf, aes(x=diastolic,fill=gender, color=gender)) +
  geom_boxplot(alpha = 0.5,position="identity") + scale_color_manual(values=c("#D999C7", "#7EC8C7"))+
  scale_fill_manual(values=c("#D999C7", "#7EC8C7"))

#systolic
ggplot(body_perf,aes(x=systolic,fill=gender, color=gender)) +
  geom_histogram(breaks=seq(50, 200, by=1),alpha = 0.5,position="identity") + scale_color_manual(values=c("#D999C7", "#7EC8C7"))+
  scale_fill_manual(values=c("#D999C7", "#7EC8C7"))
ggplot(body_perf, aes(x=systolic,fill=gender, color=gender)) +
  geom_boxplot(alpha = 0.5,position="identity") + scale_color_manual(values=c("#D999C7", "#7EC8C7"))+
  scale_fill_manual(values=c("#D999C7", "#7EC8C7"))

#wariancje
var(body_perf$height)

#interpretacja rozkladu badanej cechy

#symetria
#wspolczynnik wyostrzenia


#test czy rozklad normalny
a <- body_perf$height[1:5000]
shapiro.test(a)

#zaleznosci miedzy zmiennymi

#macierz korelacji
#corrplot(cor(iris[,1:4]), method = 'number')
corrplot(cor(body_perf[,c(1,3,4,5,6,7,8,9,10,11)]), method = 'number')

#zaleznosc height i weight
ggplot(body_perf,aes(x = height,y = weight)) + geom_point(color = "#C0D999") + geom_smooth(formula = y ~ x, method = "lm",color = "#999999")
#zaleznosc weight i grip force
ggplot(body_perf,aes(x = weight,y = grip.force)) + geom_point(color = "#C0D999") + geom_smooth(formula = y ~ x, method = "lm",color = "#999999")
#fat i situps
ggplot(body_perf,aes(x = fat,y = sit.ups)) + geom_point(color = "#C0D999") + geom_smooth(formula = y ~ x, method = "lm",color = "#999999")
#fat i broad jump
ggplot(body_perf,aes(x = fat,y = broad.jump)) + geom_point(color = "#C0D999") + geom_smooth(formula = y ~ x, method = "lm",color = "#999999")
#diastolic i systolic
ggplot(body_perf,aes(x = diastolic,y = systolic)) + geom_point(color = "#C0D999") + geom_smooth(formula = y ~ x, method = "lm",color = "#999999")
#grip force i broad jump
ggplot(body_perf,aes(x = grip.force,y = broad.jump)) + geom_point(color = "#C0D999") + geom_smooth(formula = y ~ x, method = "lm",color = "#999999")
#sit ups i broad jump
ggplot(body_perf,aes(x = sit.ups,y = broad.jump)) + geom_point(color = "#C0D999") + geom_smooth(formula = y ~ x, method = "lm",color = "#999999")





