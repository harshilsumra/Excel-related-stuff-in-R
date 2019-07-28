#Excel in R
install.packages("rpivotTable")
filename=read.csv(file.choose())
View(filename)
#sorting and order fn----
sort(filename$varcol)
order(filename$varcol)
filename[order(filename$varcol),]
rev(sort(filename$varcol))
unique(filename$varcol)#list of all unique varcol inputs
length(unique(filename$varcol))

#def'ing a fn and applying on data set
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
dataset_norm <- as.data.frame(lapply(dataset, normalize))

# Random sampling
samplesize = 0.80 * nrow(dataset)
set.seed(123)
index = sample( seq_len ( nrow ( dataset_norm ) ), size = samplesize )

#using dplyr package with example----
#method 1 for sorting
(t1=table(filename$varcol))
head(t1)  #unsorted
t2= sort(t1,decreasing=T )
head(t2)
# method 2 using dplyr
library(dplyr)
sales %>% dplyr::count(custname, sort=TRUE)
sales %>% dplyr::group_by(custname) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
#Reqmt2- Profitable parts----
#which parts are sold more frequently - count 
sales %>% dplyr::group_by(partnum) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))

# which parts have highest Profit : partno - sum(profit)
sales %>% group_by (partnum) %>% summarise(TotalMargin= sum(margin)) %>% arrange(desc(TotalMargin)) %>% head()


#Reqmt-3 : which region generated how much revenue
sales %>% group_by(region) %>% summarise(TotalRevenue=sum(revenue)) %>% arrange(desc(TotalRevenue)) 
regionRevenue = sales %>% group_by(region) %>% summarise(TotalRevenue=sum(revenue)) %>% arrange(desc(TotalRevenue)) 
regionRevenue
barplot(regionRevenue$TotalRevenue)
pie(regionRevenue$TotalRevenue)
#Reqmt-4 : which customer (top 5) gave most revenue
sales %>% group_by(custname) %>% summarise(TotalRevenue=sum(revenue)) %>% arrange(desc(TotalRevenue))  %>% head(n=5)

#Reqmt-5 : top 2 customers names by revenue from each region
sales %>% group_by(region,custname) %>% summarise(TotalRevenue=sum(revenue)) %>% arrange(desc(TotalRevenue))  %>% print(n=Inf)
#now print only top 2 for each region
sales %>% group_by(region,custname) %>% summarise(TotalRevenue=sum(revenue)) %>% arrange(desc(TotalRevenue))  %>% top_n(n=2)

#Reqmt-6 : all partnos with margin > 10000 in order region, desc(revenue)
sales %>% filter(margin > 10000) %>% arrange(region, desc(revenue))
#another way
filter(sales, margin > 10000)  #just margin

#Reqmt-7 : East Region and revenue > 4000000
sales %>% filter(region == '01-East' & revenue > 400000) %>% select(partnum, region, revenue)

#Reqmt-8 : create another column with 10% increase in column margin
sales %>% mutate(newmargin=margin * .90) %>% select(partnum, margin, newmargin)


sales %>% filter(region=='9x-Export') %>% group_by(custname) %>% summarise(max(revenue))

#plots
plot()
boxplot()
barplot(table(gradesf))
pie(table(gradesf))
hist(marks)
plot(density(marks))
pairs(filename)
dotchart(mtcars$mpg,labels=row.names(mtcars),cex=.7,
         main="Gas Milage for Car Models", 
         xlab="Miles Per Gallon")
# Stacked Bar Plot with Colors and Legend
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts))

# Grouped Bar Plot
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
#scatterplot
# Basic Scatterplot Matrix
pairs(~mpg+disp+drat+wt,data=mtcars, 
      main="Simple Scatterplot Matrix")

library(car) 
scatterplot(mpg ~ wt, data=mtcars)
scatterplot(mpg ~ wt | factor(cyl), data=mtcars)
# Violin Plots----
library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl==4]
x2 <- mtcars$mpg[mtcars$cyl==6]
x3 <- mtcars$mpg[mtcars$cyl==8]
vioplot(x1, x2, x3, names=c("4 cyl", "6 cyl", "8 cyl"), 
        col="gold")
title("Violin Plots of Miles Per Gallon")

# Pie Chart with Percentages
slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")


# 3D Exploded Pie Chart
library(plotrix)
slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of Countries ")



# Boxplot of MPG by Car Cylinders 
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon")


boxplot(mpg ~ cyl * am, data=mtcars, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="MTCars MPG", xlab="cyl & vs")
#PIVOT TABLE----
#method 1 using rpivottable
library(rpivotTable)
data(mtcars)
rpivotTable(mtcars,rows="gear", cols=c("cyl","carb"),width="100%", height="400px")

data(HairEyeColor)
rpivotTable(data = HairEyeColor, rows = "Hair",cols="Eye", vals = "Freq", aggregatorName = "Sum", rendererName = "Table", width="100%", height="400px")

#method 2 using pivottabler
library(pivottabler)
qhpvt(dataFrame, rows = NULL, columns = NULL, calculations = NULL,
      theme = NULL, replaceExistingStyles = FALSE, tableStyle = NULL,
      headingStyle = NULL, cellStyle = NULL, totalStyle = NULL, ...)


library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addColumnDataGroups("PowerType")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$evaluatePivot()
pt
#aggregate fn and writing csv
aggregate(df$marks1,by=list(df$batch),FUN=mean)
aggregate(marks1~batch,data=df,FUN=mean)
aggregate(cbind(marks1,marks2)~batch,data=df,FUN=mean)
aggregate(cbind(marks1,marks2)~batch+gender3,data=df,FUN=mean)
df2<-data.frame(aggregate(cbind(marks1,marks2)~batch+gender3,data=df,FUN=mean))
aggregate(cbind(marks1,marks2)~batch+gender3,data=df,FUN=max)
df2
write.csv()

