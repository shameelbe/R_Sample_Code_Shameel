# Shameel Mohamed Khadeer Abdul
# Inital Analysis of Data using R
# Data Analysis Project - Phase (2 / 5)
# Extract Data from CSV. Format according to business requirements.
# Display basic statistics based on business requirment
# Compute Basic Statistics on Data
# Plot various graphs to derive useful information from Data
# Implement K fold Cross Validation Algorithm
# Implement Connect the Dots, Linear Regression and Default Prediction Models
# Test which model produces better results using Leave one out cross validation
# Plot and display the results visually
# Use the Data and Prepare slides for presentation
# Note:Using Different Files Modify File Name wherever mentioned
#all required libraries
library(FNN)
library(ggplot2)
library(plyr)
library(gplots)

# Utility function for importing data from a csv (comma-separated values, flat table) file
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

#brief function to print the statistics of real valued attributes and symbolic attributes given a file Name as input
brief = function(fName) {
  my.data <- import.csv(fName)
  #remove columns that are not numeric and create a data frame to compute real valued attributes
  nList <- sapply(my.data, is.numeric)
  my.df1 <- my.data[,nList] 
  
  #create a list of column names and attribute ids
  cc.list <- colnames(my.data)
  r.num <- 1:length(nList)
  my.temp1 <- cbind(Attribute_ID=r.num, cc.list)
  colnames(my.temp1)[2]  <- "Attribute_Name"
  
  #create a data frame to store real value attributes
  my.print1 = data.frame(stringsAsFactors = FALSE,"Attribute_Name" = character(), "Missing" = numeric(),          "Mean"= numeric(), 
                         "Median"= numeric(),"Sdev"= numeric(), "Min"= numeric(), "Max"= numeric())
  
  #for all real value attributes compute missing,mean, median, sdev, min and max
  #populate the data frame with values
  for(i in names(my.df1)){
    r.count = nrow(my.print1)+1
    my.print1[[r.count,"Attribute_Name"]] <- i  
    my.print1[[r.count,"Missing"]] <- sum(is.na(my.df1[,i]))
    my.print1[[r.count,"Mean"]] <- round(mean(my.df1[,i], na.rm=TRUE),digits=2)
    my.print1[[r.count,"Median"]] <- round(median(my.df1[,i], na.rm=TRUE), digits=2)
    my.print1[[r.count,"Sdev"]] <- round(sd(my.df1[,i], na.rm=TRUE), digits=2)
    my.print1[[r.count,"Min"]] <- round(min(my.df1[,i], na.rm=TRUE), digits=2)
    my.print1[[r.count,"Max"]] <- round(max(my.df1[,i], na.rm=TRUE), digits=2)
  }
  
  #merge the computed data frame and data frame with attribute ids to add new column attribute id
  my.real <- merge( my.temp1,my.print1, by = "Attribute_Name")
  my.real <- data.frame(my.real[order(my.real$Attribute_ID), ], row.names=NULL)
  
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat("brief function output for",fName,"\n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  cat("The dataset has ",nrow(my.data)," Rows ",ncol(my.data)," Attributes\n\n" )
  cat("Real Value Attribute\n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  print(my.real)
  
  
  #create a new data frame to store symbolic attributes
  my.print2 = data.frame(stringsAsFactors = FALSE,"Attribute_Name" = character(),"Missing" = numeric(),"Arity"= numeric(), "MCVS_counts"= character() )
  
  #iterate through the dataset to find symbolic attributes and compute required stats
  #populate the data frame with values
  my.iter = names(my.data)
  for(j in my.iter) {
    if(!is.numeric(my.data[[j]]))
    {
      r.count = nrow(my.print2)+1
      #compute number of instances where the column is empty string to compute missing values
      c_na <- length(which(my.data[,j]==""))
      #remove empty values and compute unique values in the column
      s_na <- my.data[,j]
      s_na = s_na[s_na!=""]
      u_na = unique(s_na)
      
      #compute the number of occurence of each unique value to compute MCVS_Counts
      rslt <- ""
      for(k in u_na)
      {
        rslt <- paste(rslt,k, "(", length(s_na[s_na==k]), ")")
      }
      
      my.print2[[r.count,"Attribute_Name"]] <- j 
      my.print2[[r.count,"Missing"]] <- c_na
      my.print2[[r.count,"Arity"]] <- length(u_na)
      my.print2[[r.count,"MCVS_counts"]] <- rslt;
    }
  }
  
  #merge the computed data frame and data frame with attribute ids to add new column attribute id
  my.symbolic <- merge(my.temp1,my.print2 , by = "Attribute_Name")
  my.symbolic <- data.frame(my.symbolic[order(my.symbolic$Attribute_ID), ], row.names=NULL)
  
  cat("Symbolic Attribute\n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  print(my.symbolic)
  
}



cat("Basic Stats")
File1 = "house_no_missing.csv"
brief(File1)
File2 = "house_with_missing.csv"
brief(File2)


# Basic Graphs and Charts
# required libraries
library(FNN)
library(ggplot2)
library(plyr)
library(gplots)

#run the brief function for the file names
File1 = "house_no_missing.csv"
File2 = "house_with_missing.csv"
my.df1 = import.csv(File1)
my.df2 = import.csv(File2)

#Compute different plots to derive insigths about the data

plot(density(my.df1[,"house_value"]),
     main="Density Plot - House Values",
     col="yellow",
     xlab="House Value")

hist(my.df1[,"student_teacher_ratio"],
     main="Histogram - Studet-Teacher Ratio",
     col="yellow",
     xlab="Student Teacher Ratio")

qplot(x=my.df1[,"student_teacher_ratio"],y=my.df1[,"house_value"],
      geom=c("point","smooth"),
      xlab = "Student teacher ratio",
      ylab = "House Value",      
      main="Correlation between House Value & Student Teacher Ratio",method="lm")

qplot(x=my.df1[,"Crime_Rate"],y=my.df1[,"house_value"],
      geom=c("point","smooth"),
      xlab = "Crime rate",
      ylab = "House Value",      
      main="Correlation between House Value & Crime Rate",method="lm")

Charles_River_Bound <- my.df1[,"Charles_river_bound"]
House_Value <- my.df1[,"house_value"]
cdplot(Charles_River_Bound ~ House_Value)

ggplot(my.df1, aes(x=house_value, fill=Charles_river_bound)) + geom_bar(position="dodge")



library(FNN)
library(ggplot2)
library(plyr)
library(gplots)

#change the file name and output here
File3 = "house_no_missing.csv"
df <- import.csv(File3)
output <- "house_value"


# Connect-the-dots model that learns from train set and is being tested using test set
# Assumes the last column of data is the output dimension
get_pred_dots <- function(train,test){
  nf <- ncol(train)
  input <- train[,-nf]
  query <- test[,-nf]
  my.knn <- get.knnx(input,query,k=2) # Get two nearest neighbors
  nn.index <- my.knn$nn.index
  pred <- rep(NA,nrow(test))
  for (ii in 1:nrow(test)){
    y1 <- train[nn.index[ii,1],nf]
    y2 <- train[nn.index[ii,2],nf]
    pred[ii] = (y1+y2)/2
  }
  return(pred)  
}

# Linear model
# Assumes the last column of data is the output dimension
get_pred_lr <- function(train,test){
  d_Col <- ncol(train)
  linearmodel<-lm(train[,d_Col]~.,train[-d_Col])
  prediction<- predict(linearmodel,test[-d_Col])
  return(prediction)
}

# Default predictor model
# Assumes the last column of data is the output dimension
get_pred_default <- function(train,test){
  d_Col <- ncol(train)
  d_mean <- mean(train[,d_Col],na.rm=TRUE)
  d_times <- nrow(test)
  prediction  <- rep(d_mean,d_times)
  return(prediction)
}

# Implementation of k-fold cross validation
do_cv = function(df, output, k, model) {  
  #remove non numeric columns from the data frame
  nList1 <- sapply(df, is.numeric)
  df <- df[,nList1]
  #pushing the output column to last column 
  #since all models assumes last column of data as output dimension
  oIndex = which(colnames(df) == output)  
  oList = 1:ncol(df)
  oList = oList[-oIndex]
  df = df[,c(oList,oIndex)]
  my.sample <- df[sample(1:nrow(df)),]
  
  start<-1  
  #output vector to store calculated Mean Square Errors for the model
  mError = vector(length=k)
  
  #splitting the data frame into k equal folds
  #and apply k fold cross validation technique
  for(i in 1:k) {   
    end <- i*(nrow(df)/k)
    test <- my.sample[start:end,]
    train <-my.sample[-c(start:end),]
    start <- end+1    
    predicted = model(train,test)  
    # Print predicted if need be
    # print(predicted)
    mError[i] = mean((predicted - test[,ncol(test)]) ^ 2)
  } 
  return (mError)  
}


#compute Mean Squared errors for all the model
dot <- do_cv(df,output,nrow(df),get_pred_dots)
dft <- do_cv(df,output,nrow(df),get_pred_default)
lnr <- do_cv(df,output,nrow(df),get_pred_lr)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Mean Square Error - Connect the Dots Model\n")
print(dot)
cat("Mean Square Error - Default Model\n")
print(dft)
cat("Mean Square Error - Linear Regression Model\n")
print(lnr)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")


#change the file name and output here
File3 = "house_no_missing.csv"
df <- import.csv(File3)
output <- "house_value"


#take the crime_rate column and compute log of it
#create a new data frame with the output and crime rate column alone
dftemp <- df
oIndex = which(colnames(df) == output)  
df_Org <- df[oIndex]
dftemp <- df["Crime_Rate"]
dftemp <- log(dftemp)
df.temp1 <- cbind(log_Crime_Rate=dftemp, df_Org)

#compute Mean Squared errors for all the model
dot <- do_cv(df.temp1,output,nrow(df.temp1),get_pred_dots)
dft <- do_cv(df.temp1,output,nrow(df.temp1),get_pred_default)
lnr <- do_cv(df.temp1,output,nrow(df.temp1),get_pred_lr)
cat("Question 2 B i after applying log of crime rate")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("Mean Square Error - Connect the Dots Model\n")
print(dot)
cat("Mean Square Error - Default Model\n")
print(dft)
cat("Mean Square Error - Linear Regression Model\n")
print(lnr)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")


#Compute 95% Confidence interval for all three models
cDot <- t.test(dot,conf.level=0.95)
cDft <- t.test(dft,conf.level=0.95)
cLnr <- t.test(lnr,conf.level=0.95)

cdot_1 = cDot$conf.int[1]
cdot_2 = cDot$conf.int[2]

cdft_1 = cDft$conf.int[1]
cdft_2 = cDft$conf.int[2]

clnr_1 = cLnr$conf.int[1]
clnr_2 = cLnr$conf.int[2]
cat("Confidence Interval")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("95% Confidence interval for connect-the-dots","(",cdot_1,",",cdot_2,")")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("95% Confidence interval for Default","(",cdft_1,",",cdft_2,")")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("95% Confidence interval for Linear","(",clnr_1,",",clnr_2,")")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

#plot bar plot to demonstrate above results
barplot2(c(mean(dot),mean(dft),mean(lnr)),
         col = c("blue", "red", "yellow"),
         names.arg=c("Connect the Dots","Default","Linear"),         
         main = "Model Comparison", 
         xlab = "Predictive Models",
         ylab = "Mean Square Error",
         ci.u=c(cdot_2,cdft_2,clnr_2),
         ci.l=c(cdot_1,cdft_1,clnr_1), 
         ci.color = "orange", ci.lty = "solid", ci.lwd = 2, ci.width = 1,
         plot.ci=TRUE,
         beside = TRUE
)
