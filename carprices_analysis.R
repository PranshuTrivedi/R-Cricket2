library(readr)
carprice <- read_csv("C:/Users/pranshu .LAPTOP-ICT56Q7O/Downloads/carprice.csv")
View(carprice)

#Pranshu Trivedi
#1840218
#3CMS

#Title: Analysing car prices: What car is best suited for whom?

#Introdcution: The following dataset is a sample about the various cars available in the US
#It has information regarding what type of a car it is, the price of is base, mid-ranged and top
#models,the mileage the particular car has in the city and on highways, the range of the prices,
#From this analysis we can try to figure out what type of car is best suited for what 
#type of usage. A particular car maybe suited for travel in the city and some maybe best
#for travelling on highways. The price and type of the car could be combined and used to pick
#the perfect type of car for anyone.

library(ggplot2)
library(dplyr)
p<-ggplot(data=carprice,aes(carprice$Price,carprice$MPG.city,colour=carprice$Type))
p+geom_point(size=4)
#For this plot, we have considered the price of the mid-ranged models of the cars.
#This plot shows what type of car is more expensive and what type of car has a better 
#mileage to drive around in the city. This plot is very useful when you would want to decide
#what kind of a car you would want to buy based on your particular needs. The type of cars
#that are small have the best MPG and the least price, making it suitable for working class
#or lower middle class people with small families as it is money efficient in all ways.
#The same trend is followed by the type Compact, but at a slightly lesser MPG, around 20 to 25.
#Large cars tend to have a higher price, and a lower mileage. Vans also show a decent MPG
#and sporty cars tend to be with the least MPG and the highest price.

p<-ggplot(data=carprice,aes(carprice$Price,carprice$MPG.highway,colour=carprice$Type))
p+geom_point(size=4)
#A similar plot can be obtained considering the MPG for highways instead of cities.It is
#seen that midsized and large cars show a better MPG here, thus making them more efficient
#for long distance travels. Compact and small sized cars still tend to have the highest mileage.
#Vans and sporty cars also show a slight increase in MPG for highway usage.
#From the two plots we can conclude that small and compact cars are che cheapest and 
#tend to have the best mileage in both scenarios, hence are suitable for a lot of classes of people.
#Midsized and large cars tend to have a better mileage for highways and are suited for
#people who travel a lot. Sporty cars are basically best for people who want luxury and can
#afford to spend a little extra.

#We can also the compare the averages for MPGs and prices for the same
compact<-filter(carprice,carprice$Type=='Compact')
a<-mean(compact$MPG.city)
a1<-mean(compact$MPG.highway)
a2<-mean(compact$Price)
#The mean MPGs for cities and highways for compact cars is 23.42, 30.57 and the price is 12.82

large<-filter(carprice,carprice$Type=='Large')
b<-mean(large$MPG.city)
b1<-mean(large$MPG.highway)
b2<-mean(large$Price)
#As observed in the plots, the mean MPG for large cars in the city is only 18.36 whereas
#mean MPG for highways for large cars is 26.72, which is quite a difference, thus making
#large cars suitable for long distance travels.

midsize<-filter(carprice,carprice$Type=='Midsize')
c<-mean(large$MPG.city)
c1<-mean(midsize$MPG.highway)
c2<-mean(midsize$Price)

small<-filter(carprice,carprice$Type=='Small')
d<-mean(small$MPG.city)
d1<-mean(small$MPG.highway)
d2<-mean(small$Price)

sporty<-filter(carprice,carprice$Type=='Sporty')
e<-mean(sporty$MPG.city)
e1<-mean(sporty$MPG.highway)
e2<-mean(sporty$Price)

van<-filter(carprice,carprice$Type=='Van')
f<-mean(van$MPG.city)
f1<-mean(van$MPG.highway)
f2<-mean(van$Price)

cartype=c("Compact","Large","Midsize","Small","Sporty","Van")
cityMPG=c(a,b,c,d,e,f)
highwayMPG=c(a1,b1,c1,d1,e1,f1)
price=c(a2,b2,c2,d2,e2,f2)
comparing<-data.frame(cartype,cityMPG,highwayMPG,price)
comparing
p<-ggplot(comparing,aes(comparing$cartype,comparing$price))
p+geom_col()
#We tabulate and plot the mean prices, mean MPF for city and higway for all the types of cars
#to get an overview of all the factors at once.
#Comparing the prices
p<-ggplot(carprice,aes(carprice$Type,carprice$Price))
p+geom_col()
#This shows that buying small or compact cars is the cheapest, followed by vans and sporty cars.
#The most expensive cars are midsize and large types.

p<-ggplot(carprice,aes(carprice$Type,carprice$Range.Price))
p+geom_col()
#This barchart shows us how the range of price varies from the base model to the top model.
#Almost all except sporty have the range similar to each others'. Sporty cars seem to have
#a lot of variations from the base model to the top model.

p<-ggplot(carprice,aes(carprice$Type,carprice$RoughRange))
p+geom_col()
#This shows that sporty cars make the ost amount of noise, followed by midsized and large cars
#The least noise is made by compact and small cars.

#Conclusion: From the analysis we can conclude that, small cars and compact cars tend to
#be the cheapest and offer the highest mileage in cities and highways. Large and midsized
#cars tend to be expensive and provide a better mileage for highways and are suited for
#long travels. We also see that sporty type of cars tend to make a lot of noise, on the
#account of their huge engines. On careful scrutinisation of the data, one can easily 
#pick the right car from him/her based on his/her particular needs.

