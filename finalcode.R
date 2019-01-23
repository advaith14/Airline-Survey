#Reading the dataset csv file
setwd("C:/Users/advai/Documents")
x<- read.csv('Satisfaction Survey.csv',stringsAsFactors =TRUE) 
Airlines <- data.frame(x)
View(Airlines)
str(Airlines)
summary(Airlines)
dim(Airlines)
unique(Airlines$Satisfaction)

#Part 1: Cleaning the Data

#Removing NA's
Airlines$Departure.Delay.in.Minutes[is.na(Airlines$Departure.Delay.in.Minutes)]<-0
View(Airlines)
Airlines$Arrival.Delay.in.Minutes[is.na(Airlines$Arrival.Delay.in.Minutes)]<-0
View(Airlines)
Airlines$Flight.time.in.minutes[is.na(Airlines$Flight.time.in.minutes)]<-0
View(Airlines)

#Garbage Value
length(which(Airlines$Satisfaction == '4.00.5') )
length(which(Airlines$Satisfaction == '4.00.2.00') )

#Removing garbage values
Airlines <- Airlines[!Airlines$Satisfaction == "4.00.5", ]
Airlines <- Airlines[!Airlines$Satisfaction == "4.00.2.00", ]
unique(x$Airline.Name) #Garbage Value removed

#Removing whitespaces from Airlines Name column
trimws(Airlines$Airline.Name, "r") #To remove white space
View(Airlines)
unique(Airlines$Airline.Name) #White space removed

#Part 2. Descriptive Stats
install.packages("ggplot2")
library(ggplot2)

#Average Customer Satisfaction for Airlines
Airlines$Satisfaction <- as.numeric(as.character(Airlines$Satisfaction))
A = tapply(Airlines$Satisfaction, Airlines$Airline.Name, mean)
avgsat <- data.frame(Customer_satisfaction = A, Airline_name = row.names(A)) #Creating a data frame to make a barplot

barp <- ggplot(avgsat, aes(x=reorder(Airline_name,Customer_satisfaction), y=Customer_satisfaction))
barp <- barp + geom_col(color = "Black", fill = "Grey")
barp <- barp + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp <- barp + ggtitle("Satisfaction for each airline")
barp <- barp + labs(x = "Airlines")
barp <- barp + labs(y = "Satisfaction")
barp
#Average customer satisfaction for all airlines is almost the same with not much difference, hence we cannot decide on a particular airline based on just average customer satisfaction value

#### No. of Entries for each Airline
B = tapply(Airlines$Airline.Name, Airlines$Airline.Name, length)
noofentries <- data.frame(No_of_Entries = B, Airline_name = row.names(B))

barp1 <- ggplot(noofentries, aes(x=reorder(Airline_name,No_of_Entries), y=No_of_Entries))
barp1 <- barp1 + geom_col(color = "Black", fill = "Blue")
barp1 <- barp1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp1 <- barp1 + ggtitle("Number of people for each airline")
barp1 <- barp1 + labs(x = "Airlines")
barp1 <- barp1 + labs(y = "Number of people")
barp1

#Creating a dataframe that contains all entries related to only Sigma Airlines
View(Airlines)
which(Airlines$Airline.Name =='Sigma Airlines Inc. ')
Sigma <- Airlines[ Airlines$Airline.Name =='Sigma Airlines Inc. ', ]
View(Sigma)
unique(Sigma$Airline.Name)
unique(Sigma$Satisfaction) #Does not contain any garbage values

Sigma$departure.Delay.greater.5.Minsyes[Sigma$Departure.Delay.in.Minutes>5]<-"yes"
Sigma$departure.Delay.greater.5.Minsyes[Sigma$Departure.Delay.in.Minutes<=5]<-"no"
View(Sigma$departure.Delay.greater.5.Minsyes)

Sigma$day <- as.factor(weekdays(as.Date(Sigma$Flight.date,format="%m/%d/%y")))
Sigma$month <- as.factor(months(as.Date(Sigma$Flight.date,format="%m/%d/%y")))


#Plotting Gender to Satisfaction for Sigma
C<-tapply(Sigma$Satisfaction, Sigma$Gender, mean)
D<-data.frame(customersatisfaction=C,gender=row.names(C))
D

barp2 <- ggplot(D, aes(x=reorder(gender,customersatisfaction), y=customersatisfaction))
barp2 <- barp2 + geom_col(color = "Black", fill = "Grey")
barp2 <- barp2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp2 <- barp2 + ggtitle(" Gender to Satisfaction for Sigma")
barp2 <- barp2 + labs(x = "Gender")
barp2 <- barp2 + labs(y = "Satisfaction")
barp2

#Plotting Status to Satisfaction for Sigma
C<-tapply(Sigma$Satisfaction, Sigma$Airline.Status, mean)
E<-data.frame(customersatisfaction=C,status=row.names(C))
E

barp3 <- ggplot(E, aes(x=reorder(status,customersatisfaction), y=customersatisfaction))
barp3 <- barp3 + geom_col(color = "Black", fill = "Blue")
barp3 <- barp3 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp3 <- barp3 + ggtitle("Status to Satisfaction for Sigma")
barp3 <- barp3 + labs(x = "Airlines Status")
barp3 <- barp3 + labs(y = "Satisfaction")
barp3

#Plotting Age to Satisfaction for Sigma

#Creating categories of age
sig <- data.frame(Sigma) #Copying values Sigma to sig 
dum<-replicate(length(Sigma$Age), "40 to 60")
dum[Sigma$Age < 40]<-"0 to 40"
dum[Sigma$Age > 60]<-"Above 60"
sig$Age<- dum

C<-tapply(sig$Satisfaction, sig$Age, mean)
G<-data.frame(customersatisfaction=C,age=row.names(C))
G

barp4 <- ggplot(G, aes(x=reorder(age,customersatisfaction), y=customersatisfaction))
barp4 <- barp4 + geom_col(color = "Black", fill = "Grey")
barp4 <- barp4 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp4 <- barp4 + ggtitle("Age to Satisfaction for Sigma")
barp4 <- barp4 + labs(x = "Age")
barp4 <- barp4 + labs(y = "Satisfaction")
barp4

#Plotting Price Sensitivity to Satisfaction for Sigma

C<-tapply(sig$Satisfaction, sig$Price.Sensitivity, mean)
H<-data.frame(customersatisfaction=C,ps=row.names(C))
H

barp5 <- ggplot(H, aes(x=reorder(ps,customersatisfaction), y=customersatisfaction))
barp5 <- barp5 + geom_col(color = "Black", fill = "Blue")
barp5 <- barp5 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp5 <- barp5 + ggtitle("Price Sensitivity to Satisfaction for Sigma")
barp5 <- barp5 + labs(x = "Price Sensitivity")
barp5 <- barp5 + labs(y = "Satisfaction")
barp5

#Plotting No. of Flights per annum to Satisfaction for Sigma

#Creating categories of flights per annum
a<-quantile(Sigma$No.of.Flights.p.a.,c(0.4,0.6))
dum<-replicate(length(Sigma$No.of.Flights.p.a.), "Average")
dum[Sigma$No.of.Flights.p.a. < 14]<-"Low"
dum[Sigma$No.of.Flights.p.a. > 21]<-"High"
sig$No.of.Flights.p.a.<- dum


C<-tapply(sig$Satisfaction, sig$No.of.Flights.p.a., mean)
I<-data.frame(customersatisfaction=C,nf=row.names(C))
I

barp6 <- ggplot(I, aes(x=reorder(nf,customersatisfaction), y=customersatisfaction))
barp6 <- barp6 + geom_col(color = "Black", fill = "Blue")
barp6 <- barp6 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp6 <- barp6 + ggtitle("No. of Flights per annum to Satisfaction for Sigma")
barp6 <- barp6 + labs(x = "No. of flights per annum")
barp6 <- barp6 + labs(y = "Satisfaction")
barp6

#Plotting Type Of Travel to Satisfaction for Sigma
C<-tapply(sig$Satisfaction, sig$Type.of.Travel, mean)
J<-data.frame(customersatisfaction=C,tt=row.names(C))
J

barp7 <- ggplot(J, aes(x=reorder(tt,customersatisfaction), y=customersatisfaction))
barp7 <- barp7 + geom_col(color = "Black", fill = "Blue")
barp7 <- barp7 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp7 <- barp7 + ggtitle("Type Of Travel to Satisfaction for Sigma")
barp7 <- barp7 + labs(x = "Type of Travel")
barp7 <- barp7 + labs(y = "Satisfaction")
barp7

#Plotting Class to Satisfaction for Sigma
C<-tapply(sig$Satisfaction, sig$Class, mean)
K<-data.frame(customersatisfaction=C,cl=row.names(C))
K

barp8 <- ggplot(K, aes(x=reorder(cl,customersatisfaction), y=customersatisfaction))
barp8 <- barp8 + geom_col(color = "Black", fill = "Grey")
barp8 <- barp8 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp8 <- barp8 + ggtitle("Class to Satisfaction for Sigma")
barp8 <- barp8 + labs(x = "Class")
barp8 <- barp8 + labs(y = "Satisfaction")
barp8

#Plotting Loyalty Cards to Satisfaction for Sigma
C<-tapply(Sigma$Satisfaction, Sigma$No..of.other.Loyalty.Cards, mean)
L<-data.frame(customersatisfaction=C,cl=row.names(C))
L

barp9 <- ggplot(L, aes(x=reorder(cl,customersatisfaction), y=customersatisfaction))
barp9 <- barp9 + geom_col(color = "Black", fill = "Blue")
barp9 <- barp9 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp9 <- barp9 + ggtitle("Loyalty Cards to Satisfaction for Sigma")
barp9 <- barp9 + labs(x = "Loyalty Cards")
barp9 <- barp9 + labs(y = "Satisfaction")
barp9

#Plotting Arrival Greater than 5 mins to Satisfaction for Sigma
C<-tapply(Sigma$Satisfaction, Sigma$Arrival.Delay.greater.5.Mins, mean)
M<-data.frame(customersatisfaction=C,a5=row.names(C))
M

barp10 <- ggplot(M, aes(x=reorder(a5,customersatisfaction), y=customersatisfaction))
barp10 <- barp10 + geom_col(color = "Black", fill = "Grey")
barp10 <- barp10 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp10 <- barp10 + ggtitle("Arrival Greater than 5 mins to Satisfaction for Sigma")
barp10 <- barp10 + labs(x = "Arrival Greater than 5 minutes")
barp10 <- barp10 + labs(y = "Satisfaction")
barp10

#Plotting Departure Greater than 5 mins to Satisfaction for Sigma
C<-tapply(Sigma$Satisfaction, Sigma$departure.Delay.greater.5.Minsyes, mean)
N<-data.frame(customersatisfaction=C,d5=row.names(C))
N

barp11 <- ggplot(N, aes(x=reorder(d5,customersatisfaction), y=customersatisfaction))
barp11 <- barp11 + geom_col(color = "Black", fill = "Blue")
barp11 <- barp11 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
barp11 <- barp11 + ggtitle("Departure Greater than 5 mins to Satisfaction for Sigma")
barp11 <- barp11 + labs(x = "Departure Greater than 5 minutes")
barp11 <- barp11 + labs(y = "Satisfaction")
barp11

#Creating maps

install.packages("ggmap")

library(ggmap)



us<-map_data("state")



State_area <- state.area

State_x<-state.center$x

State_y<-state.center$y

maps<-data.frame(State_area,State_x,State_y)

maps$Origin.State <- state.name

maps$state_center_x<-state.center$x

maps$state_center_y<-state.center$y

merge1<-merge(Airlines,maps,by="Origin.State")

merge1$Destination.State<-tolower(merge1$Destination.State)

merge1$Origin.State<-tolower(merge1$Origin.State)



#Crating map to plot satisfaction based on origin state

map.sat <- ggplot(merge1,aes(map_id=Origin.State))

map.area<-map.sat+geom_map(map=us,aes(fill=Satisfaction))+expand_limits(x=us$long, y=us$lat)+coord_map()+ggtitle("Satisfaction Based on Origin State")+geom_text(data=merge1, hjust=0.5, vjust=0.5, aes(x=State_x ,y=State_y, label=toupper(Origin.State)),colour="white",size=2.5)

map.area

#Crating map to plot satisfaction based on arrival delay

map.sat <- ggplot(merge1,aes(map_id=Origin.State))

map.area<-map.sat+geom_map(map=us,aes(fill=Arrival.Delay.greater.5.Mins))+expand_limits(x=us$long, y=us$lat)+coord_map()+ggtitle("Satisfaction Based on Arrival Delay")+geom_text(data=merge1, hjust=0.5, vjust=0.5, aes(x=State_x ,y=State_y, label=toupper(Origin.State)),colour="white",size=2.5)

map.area

#Crating map to plot satisfaction based on destination state

map.sat <- ggplot(merge1,aes(map_id=Destination.State))

map.area<-map.sat+geom_map(map=us,aes(fill=Satisfaction))+expand_limits(x=us$long, y=us$lat)+coord_map()+ggtitle("Satisfaction Based on Destination State")+geom_text(data=merge1, hjust=0.5, vjust=0.5, aes(x=State_x ,y=State_y, label=toupper(Origin.State)),colour="white",size=2.5)

map.area

#Linear Modelling

#Building linear model 
linear_model<-lm(formula=Satisfaction~Airline.Status+Age+Gender+Price.Sensitivity+Class+No.of.Flights.p.a.+Type.of.Travel+Class+Scheduled.Departure.Hour+Arrival.Delay.greater.5.Mins+Flight.cancelled, data=Sigma)
summary(linear_model)

#Taking 1000 Samples
x<-Sigma[sample(nrow(Sigma), 1000), ]
str(x)

#Prediction
v<-predict(linear_model,x,type="response")
View(v)
pred<-data.frame(x$Satisfaction,v)
plot(x$Satisfaction,v,ylim=c(0,5),xlab="Actual Satisfaction",ylab="Predicted Satisfaction", main="Actual vs Predicted Customer Satisfaction for Linear Model")

View(pred)
summary(v)

#Association Rule Mining
Sigma$day <- as.factor(weekdays(as.Date(Sigma$Flight.date,format="%m/%d/%y")))
Sigma$month <- as.factor(months(as.Date(Sigma$Flight.date,format="%m/%d/%y")))

dum<-replicate(length(Sigma$Price.Sensitivity), "Average")
dum[Sigma$Price.Sensitivity < 2]<-"Low"
dum[Sigma$Price.Sensitivity > 2]<-"High"
sig$Price.Sensitivity<- dum

dum<-replicate(length(Sigma$Satisfaction), "Average")
dum[Sigma$Satisfaction < 3]<-"Low"
dum[Sigma$Satisfaction > 3]<-"High"
sig$Satisfaction<- dum

dum<-replicate(length(Sigma$Scheduled.Departure.Hour), "Morning")
dum[Sigma$Scheduled.Departure.Hour >= 12 & Sigma$Scheduled.Departure.Hour <=16 ]<-"Afternoon"
dum[Sigma$Scheduled.Departure.Hour > 16]<-"Night"
sig$Scheduled.Departure.Hour<- dum

install.packages("arules")
library(arules)

install.packages("arulesViz")
library(arulesViz) 

arm <- data.frame(sig$Satisfaction,sig$Airline.Status,sig$Gender,sig$Age,sig$Price.Sensitivity,sig$Class,sig$Scheduled.Departure.Hour,sig$Arrival.Delay.greater.5.Mins,sig$Flight.cancelled,sig$Type.of.Travel,sig$No.of.Flights.p.a.,sig$departure.Delay.greater.5.Minsyes,sig$month, sig$day)
armx<- as(arm,"transactions")
rules<-apriori(armx,parameter = list(support=0.23,confidence=0.75,minlen=2),appearance = list(rhs=c("sig.Satisfaction=High")))

inspect(rules) 
goodrules<-rules[quality(rules)$lift>1.3]
inspect(goodrules)
plot(goodrules,method="graph",engine="htmlwidget")
arm1 <- data.frame(sig$Satisfaction,sig$Gender,sig$Airline.Status,sig$Age,sig$Price.Sensitivity,sig$Class,sig$Scheduled.Departure.Hour,sig$Arrival.Delay.greater.5.Mins,sig$Flight.cancelled,sig$Type.of.Travel,sig$No.of.Flights.p.a.,sig$departure.Delay.greater.5.Minsyes, sig$month, sig$day)
armx1<- as(arm1,"transactions")
rules1<-apriori(armx1,parameter = list(support=0.1,confidence=0.4,minlen=2),appearance = list(rhs=c("sig.Satisfaction=Low")))

inspect(rules1) 
goodrules1<-rules1[quality(rules1)$lift>1]
inspect(goodrules1)
plot(goodrules1,method="graph",engine="htmlwidget")

arm2 <- data.frame(sig$Satisfaction,sig$Gender,sig$Airline.Status,sig$Age,sig$Price.Sensitivity,sig$Class,sig$Scheduled.Departure.Hour,sig$Arrival.Delay.greater.5.Mins,sig$Flight.cancelled,sig$Type.of.Travel,sig$No.of.Flights.p.a.,sig$departure.Delay.greater.5.Minsyes, sig$month, sig$day)
armx2<- as(arm1,"transactions")
rules2<-apriori(armx2,parameter = list(support=0.1,confidence=0.4,minlen=2),appearance = list(rhs=c("sig.Satisfaction=Average")))

inspect(rules2) 
goodrules2<-rules2[quality(rules2)$lift>1]
inspect(goodrules2)
plot(goodrules2,method="graph",engine="htmlwidget")

#SVM
Sigma$speed<-(Sigma$Flight.Distance/Sigma$Flight.time.in.minutes)
Sigma$speed[Sigma$speed=="Inf"]<-0
View(Sigma)
str(Sigma)
dim(Sigma)
na.omit(Sigma)
dim(Sigma)
summary(Sigma)
install.packages("kernlab")
library(kernlab)
dim(Sigma)
#table(Sigma$Satisfaction_Bucket)
# Happy Unhappy 
# 13699    3338
randindex<-sample(1:dim(Sigma)[1])
cut_point2_3<-floor(2*dim(Sigma)[1]/3)
cut_point2_3
traindata<-Sigma[randindex[1:cut_point2_3],]
testdata<-Sigma[randindex[(cut_point2_3+1):dim(Sigma)[1]],]
dim(traindata)
View(traindata)
str(testdata)
svmoutput<-ksvm(Arrival.Delay.greater.5.Mins~Scheduled.Departure.Hour+Flight.time.in.minutes+Departure.Delay.in.Minutes+Flight.Distance+speed,data=traindata,kernal="rbfdot",kpar="automatic",C=10,cross=5,prob.model=TRUE)
svmoutput
str(testdata)
svmpred<-predict(svmoutput,testdata,type="response")
dim(svmpred)
View(svmpred)
dim(testdata)
comptable<-data.frame(testdata[,28],svmpred[])
table(comptable)
plot(svmoutput,traindata)



