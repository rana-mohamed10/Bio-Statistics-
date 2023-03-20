data <- read.csv("D:/level 4/Biostat/Assignment_One/data.csv")
#data
#1)Show the first 10 rows and the last 10 rows.

print("======priting the first 10 rows=========")
head(data,10)
print("======priting the last 10 rows=========")
tail(data,10)


#2)gender and ancestry data and average commuting time for the oldest three
dob2<- as.Date(data$dob)
#class(dob2)
order(dob2)[1:3]
#981  218  89
dob2<- as.Date(data$dob)
#class(dob2)
oldest<-order(dob2)[1:3]
#981  218  89
dob2[981]
#1923-10-10
dob2[218]
#"1924-03-28"
dob2[89]
#"1924-09-16"

#data$gender[data$dob=="1923-10-10"]
print("data for #1923-10-10")
data$gender[981]
data$ancestry[981]
data$avg_commute[981]
print("data for #1924-03-28")
data$gender[218]
data$ancestry[218]
data$avg_commute[218]

print("data for #1924-09-16")
data$gender[89]
data$ancestry[89]
data$avg_commute[89]
#another way
data$gender[oldest[1:3]]
data$ancestry[oldest[1:3]]
data$avg_commute[oldest[1:3]]



#3)the gender, daily internet use, average commute time, ancestry,and diseases among those with more than two children.

data[(data$children)>2 , c("gender","daily_internet_use","avg_commute","ancestry","disease")]


#4)indicate the number of rows that have any missing values and the number that do not.

table(complete.cases(data))

#5) summary of numerical data and number of categories in categorical data

summary(data$zipcode)
summary(data$children)
summary(data$avg_commute)
summary(data$daily_internet_use)
#we also can use filter
sapply(Filter(is.numeric,data), summary) 
#another way
s<-data[,(select=c("zipcode","children","avg_commute","daily_internet_use"))]
sapply(s, summary)
#filter funcion
sapply(Filter(is.character,data), table) 

#table(data$gender)
#table(data$employment_status)
#table(data$marital_status)
#table (data$ancestry)
#table (data$disease)
#table(data$education)
#t1<-data[,(select=c("id","dob"))]
#sapply(t1, table)

table(data$id)
table(data$dob)
t2<-data[,(select=c("gender","employment_status","marital_status","ancestry","disease","education"))]
sapply(t2, table)



#6)columns that are having any missing values, and then remove any rows where all of the columns have missing values.

NaColumns<-data[,is.na(data)]
NaColumns

na.omit(data)
table(complete.cases(data))

#a function to test that the rows of the data is all with "NA" To remove it

colSums(is.na(data))
#adding a row with NA's values to test the function
data2<-rbind(data,c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
#to see the row has been added 
tail(data2)
table(complete.cases(data2))

names(sapply(data2, anyNA))

removeNa <- function(df, nc=0) { 
  df[rowSums(is.na(df)) <= nc,]
}
data_compM<-removeNa(data2,ncol(data2)-1)
table(complete.cases(data_compM))
tail(data_compM)

#7)Show the average daily usage of the internet for each level of education.

avg<-tapply(data$daily_internet_use , data$education, mean)
avg

library(ggplot2)
edu<-data$education
net<-data$daily_internet_use

df<-data.frame(net,edu)
df
p<-ggplot(df,aes(x=data$education))
p+geom_bar(fill="violet")


#8)Show the distribution of the children count using a histogram.
children_count<-data$children
hist(children_count ,col=2, main = "freq of children")

#9)compare how men and women's avg commute distributions differ
df_males<-data[data$gender=="male","avg_commute"]
df_females<-data[data$gender=="female","avg_commute"]
#table(df_males)
#table(df_females)
par(mfrow=c(1,2))

plot(df_males,type = "l",col=4, main = "male by avg_commute")
plot(df_females,type = "l",col=2, main = "female by avg_commute")


#10)Make a histogram to show the gender distribution

df_males<-data$gender=="male"
df_females<-data$gender=="female"
df_gender<-data.frame(df_females,df_males)

par(mfrow=c(1,1))
g<-table(df_gender)
barplot(g,main="Gender",names.arg=c("female","male"),ylab="freq",col =c("lightblue","pink"))
#11)Use a histogram to show gender distribution for each disease

gender_diseases<-table(data$gender,data$disease)
barplot(gender_diseases,main="diseases by Gender",cex.names = 0.3,beside = TRUE,ylab="freq",col =c("pink","lightblue"))
#legend("topleft",c("male","female"),fill=c("lightblue","pink"))
#12)demonstrate whether there is a relationship between age and the type of disease
dob2<- as.Date(data$dob)
date_of_today <- as.Date("2022-12-3")                            
#date
age<-floor((date_of_today-dob2)/365.25)
#age
par(mfrow=c(1,1))
barplot(table(data$disease,age),col = "orange",main = "Age distribution")

#13)Make a chart to show the total number of children per disease.
child<-tapply(data$children,data$disease,sum)
child
par(mfrow=c(1,1))

barplot(child,col=rainbow(13),main="total number of children per disease.", cex.names = 0.5)


#14)Make a chart to show the ancestry distribution
ancestry<-table(data$ancestry)
ancestry
barplot(ancestry,col = rainbow(20), main = "ancestry distribution" ,cex.names = 0.3)