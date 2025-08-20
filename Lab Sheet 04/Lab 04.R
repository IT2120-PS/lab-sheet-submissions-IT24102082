setwd("C:\\Users\\it24102082\\Desktop\\Lab 04")
data<-read.table("DATA 4.txt",header=TRUE,sep =" ")
fix(data)
attach(data)

boxplot(X1,main="Box plot for Team Attendence",outline=TRUE,outpch=8,horizontal=TRUE)
boxplot(X2,main="Box plot for Team Salary",outline=TRUE,outpch=8,horizontal=TRUE)
boxplot(X3,main="Box plot for Years",outline=TRUE,outpch=8,horizontal=TRUE)


hist(X1,main="Frequency",xlab="Team Attendence",main="Historgram for Team Attendence")
hist(X1,main="Frequency",xlab="Team Salary",main="Historgram for Team Salary")
hist(X1,main="Frequency",xlab="Team Years",main="Historgram for Years")

stem(X1)
stem(X2)
stem(X3)

mean(X1)
mean(X2)
mean(X3)

median(X1)
median(X2)
median(X3)

sd(X1)
sd(X2)
sd(X3)

summary(X1)
summary(X2)
summary(X3)

quantile(X1)
quantile(X2)[2]
quantile(X3)[4]

IQR(X1)
IQR(X2)
IQR(X3)

get.mode<-function(y){
  counts<-table(X3)
  names(counts[counts == max(counts)])
}

get.mode(X3)
table(X3)
max(counts)
counts==max(counts)
counts[counts==max(counts)]
name(counts[counts==max(counts)])

get.outliers<-function(z){
  q1<-quantile(z)[2]
  q3<-quantile(z)[4]
  iqr<-q3-q1
  
  ub<-q3+1.5*iqr
  lb<-q1-1.5*iqr
  
  print(paste("Upper Bound = ", ub))
  print(paste("Lower Bound = ", lb))
  print(paste("Outliners:", paste(sort(z[z<lb|z>ub]), collapse = ",")))
}

get.outliers(X1)
get.outliers(X2)
get.outliers(X3)

get.outliers<-function(z){
  q1<-quantile(z)[2]
  q3<-quantile(z)[4]
  iqr<-q3-q1
  
  ub<-q3+1.5*iqr
  lb<-q1-1.5*iqr
  
  print(paste("Upper Bound = ", ub))
  print(paste("Lower Bound = ", lb))

  print(paste("Outliners:", paste(sort(z[z<lb|z>ub]), collapse = ",")))
}
