##PART 1

#Create a vector with the trainings method for each pilot.
training<-c(rep('CSFI', 8), rep('TFI', 10))
#Create a vector with the score for each pilot.
test_score<-c(2,5,5,6,6,7,8,9,1,1,2,3,3,4,5,7,7,8)
#Combine the vectors into a dataframe.
pilot_data<-data.frame(training, test_score)

#A t-test with the input parameters. Y is the dependent variable and x is the group variable.
MyTtest <- function(y, x) {
  #Store the two groups that people can belong to in the variable groups. I need these to match means with
  #group names later.
  groups<-unique(x)
  #Combine the group and y variable and then delete the rows with missing values.
  vars_combined<-cbind(x ,y)
  vars_combined<-na.omit(vars_combined)
  #Store the scores of group 1 and group 2 in different variables.
  #As.numeric makes sure the values in the vector are numeric and not characters.
  y_score_group_1<-as.numeric(vars_combined[vars_combined[,1]==groups[1],2])
  y_score_group_2<-as.numeric(vars_combined[vars_combined[,1]==groups[2],2])
  
  ##Formula t test
  #S pooled is calculated first.
  #First the numerator of s squared pooled is calculated. The length function is used to represent the number
  #of cases in a group.
  numerator<-((length(y_score_group_1)-1)*var(y_score_group_1))+((length(y_score_group_2)-1)*var(y_score_group_2))
  #Then the denominator is calculated.
  denominator<-length(y_score_group_1)+length(y_score_group_2)-2
  #s pooled equals the square root of numerator divided by denominator.
  s_pooled<-sqrt(numerator/denominator)
  #Then t is calculated. The difference between the means of the groups is the numerator. The denominator in this case
  #is s pooled times the square root of 1 divided by the size of group 1 plus 1 divided by the size of group 2.
  t<-(mean(y_score_group_1)-mean(y_score_group_2))/(s_pooled*sqrt((1/length(y_score_group_1))+(1/length(y_score_group_2))))
  
  #The degrees of freedom are calculated. Which equals the sample size minus 2 in this case.
  df<-length(y_score_group_1)+length(y_score_group_2)-2
  #Pt gives probability of finding a t smaller than the t used as input.
  #Use absolute value of t, because for a negative t the calculation of p-value is different
  area_left_of_t<-pt(abs(t), df=df)
  #1-area_left_of_t gives probability of finding t larger or equal than the t used as input.
  #The distribution is symmetrical so if you multiply by 2 the probability of finding same or more 
  #extreme value is found, which is the two sided p-value.
  p_value<-2*(1-area_left_of_t)
  
  #Means of the groups are stored.
  mean_1<-mean(y_score_group_1)
  mean_2<-mean(y_score_group_2)
  
  #Rounding off
  p_value<-round(p_value,3)
  t<-round(t,3)
  mean_1<-round(mean_1,3)
  mean_2<-round(mean_2,3)
  
  #Matrix with t value, p-value and df is created.
  Output_1<-matrix(c(t, p_value, df), nrow = 1, ncol = 3)
  #Column names and row names are made.
  colnames(Output_1)<-c("t", "p-value", "df")
  rownames(Output_1)<-"statistic"
  Output_2<-matrix(c(mean_1, mean_2), nrow = 1, ncol = 2)
  colnames(Output_2)<-c(groups[1], groups[2])
  rownames(Output_2)<-"Mean"
  
  #Create a list that consists out of two parts. One part with the test statistics and one part with the means.
  newList <- list("t-test"=Output_1, "means"= Output_2)
  return(newList)
  
}
model_t<-MyTtest(pilot_data$test_score, pilot_data$training)
#The strings between the brackets represent the list that is within model_t. There are two.
model_t['t-test']
model_t['means']
#The built in t-test is used. Var.equal is set to true to perform a t-test with variances assumed to be equal.
t.test(pilot_data$test_score~pilot_data$training, var.equal=TRUE)
#The built in t-test and my t-test give the same result. The t-value is 1.68 and the p-value is 0.11.


#PART 2


#The data about animals is loaded in.
library(foreign)
#Sep = " " means that in the gala file every distinct value or name is separated by a space.
data<- read.csv('gala.txt', sep = " ")

#A regression analysis of the same variables with the built in function.
#This gives the same coefficients and residuals and predicted values.
#The coefficient for the intercept is -15.8912, for area 0.0127, for elevation -0.0414 and for endemics 4.3318.
model<-lm(data$Species~data$Area+data$Elevation+data$Endemics)
summary(model)
model['residuals']
model['fitted.values']
#This gives a graph in which the residuals are plotted against predicted values.
plot(model)

#Turn the dataframe into a matrix
matrix<-as.matrix(data)
#The first column is the dependent variable so it is assigned to y.
y<-matrix[,1]
#An intercept vector is created to get correct estimates.
int <- rep(1, length(y))
#The second, third and fourth column are the independent variables. So they're assigned to x_variables.
x_variables<-matrix[,c(2,3,4)]
#The x variables and the intercept column are combined. 
x_matrix<-cbind(int, x_variables)
#The B coefficients are calculated.
#Inverse of product between transpose of x matrix and x matrix is multiplied by the transpose of the x matrix
#The result is then multiplied by the y vector
bes<-solve(t(x_matrix)%*%x_matrix)%*%t(x_matrix)%*%y
#Get predicted values by multiplying x_matrix and the matrix with coefficients.
predicted<-x_matrix%*%bes
#Get residuals by subtracting predicted values from observed values.
residuals<- y-predicted
##This gives the same coefficients and residuals and predicted values as the built in function.
#The coefficient for the intercept is -15.8912, for area 0.0127, for elevation -0.0414 and for endemics 4.3318.
print(bes)
print(residuals)
print(predicted)

##Own regression function.

#The function asks for a dataframe, the name of the y column between quotation marks and the names of
#the x variables also between quotation marks. The data should be defined first, then the y variable and
#then the x variables. The order matters. You cannot use a matrix as input!
regression<-function(data, y, ...){
  
  #The column name that is given for y is used to get the column of the dataframe that is given. This is then
  #turned into a matrix of length(y) x 1.
  y_vector<-matrix(data[y], nrow = length(data[y]), ncol = 1)
  #The matrix was in a list. Unlist gets it out of the list.
  y_vector<-unlist(y_vector)
  #All the column names of the x variables are put in a list named x.
  x<-list(...)
  #A vector with 1s is created. This is put in the 'X' matrix to get correct estimates.
  intercept<- rep(1, length(y_vector))
  #A vector is created in which the names of the x variables are to be stored. The first value is intercept.
  x_names<-c('intercept', rep(NA, length(x)))
  
  #I create a loop to create the X matrix.
  for (i in 1:length(x)){
    
    #The values in the list called x are put in the x_names variable (the names of the x variables). 
    #The double brackets are used to get a list in the list. The '[1]' is used to get the value within that list.
    x_names[i+1]<-x[[i]][1]
    
    #The values of the x variable that is put in is accessed and put in z for each x that is given.
    values_x<-data[x[[i]][1]]
    
    #The data was in a list. 
    values_x<-unlist(values_x)
    
    #A column is added to the intercept object for each x variable.
    #So at the end of the loop the intercept object is the 'X' matrix.
    intercept<-cbind(intercept,values_x)
    
  }
  
  #Combine y and xs in object called missings. 
  missings<-cbind(y_vector, intercept)
  #Drop rows with missing values.
  missings<-na.omit(missings)
  #The y vector is dropped from the missings matrix and result is stored in x_matrix.
  x_matrix<-missings[,-1]
  #Everything but the y vector is dropped from the missings matrix and the result is stored in y_vector2.
  y_vector2<-missings[,1]
  
  ##R squared is calculated now.
  #Vector with as many zeros as given x variables is created. Correlations between x and y is stored here.
  cor_matrix_xy<-rep(0,ncol(x_matrix)-1)
  #The R_squared_matrix_x is a matrix with only x variables. The intercept column which is the first column
  #of x_matrix is dropped.
  R_squared_matrix_x<-x_matrix[,-1]
  
  for (i in 1:ncol(R_squared_matrix_x)){
    #For every x its correlation with y is put in cor_matrix_xy
    cor_matrix_xy[i]<-cor(R_squared_matrix_x[,i], y_vector2)
  }
  
  #A correlation matrix is created for the x variables.
  cor_matrix_x<-cor(R_squared_matrix_x)
  #Betas are calculated which are necessary to get R squared. Inverse of correlation matrix of xs is multiplied
  #by the matrix that contains correlations between xs and y.
  Betaa<-solve(cor_matrix_x)%*%cor_matrix_xy
  #R squared is calculated by multiplying the transpose of the matrix that contains correlations between xs and y
  #by the matrix that contains the betas
  R2<-t(cor_matrix_xy)%*%Betaa
  ##
  
  #Number of missing values is calculated by subtracting the number of y scores, including NAs, from
  #the number of y scores, excluding NAs.
  n_missings<-length(y_vector)-length(y_vector2)
  
  #The B coefficients are calculated.
  #Inverse of product between transpose of x matrix and x matrix is multiplied by the transpose of the x matrix
  #The result is then multiplied by the y vector
  bes<-solve(t(x_matrix)%*%x_matrix)%*%t(x_matrix)%*%y_vector2
  
  #Column name 'B' is assigned to the matrix
  colnames(bes)<-c('B')
  #The coefficients are matched with the names of the x variables they belong to. This is why x_names in the
  #beginning was created.
  rownames(bes)<-x_names
  
  #Predicted values are found by multplying the x matrix (with intercept column) by the Bs. 
  predicted<-x_matrix %*% bes
  predicted<-round(predicted, 3)
  
  #Residuals are found by subtracting the predicted values from the observed values.
  residuals<-y_vector2-predicted
  #Round them off to three decimals.
  residuals<-round(residuals, 3)
  predicted<-round(predicted, 3)
  bes<-round(bes,3)
  
  #The residuals and predicted values are combined in a matrix.
  pred_res<-cbind(predicted, residuals)
  rownames(pred_res)<-1:length(y_vector2)
  colnames(pred_res)<-c('predicted', 'residuals')
  
  ##Output
  print('----------------')
  #Number of observations is shown. The paste function combines character with numeric. The numeric value
  #is turned into character by collapse argument.
  print(paste(c("Number of observations:", length(y_vector2)), collapse = " "))
  #Number of missing values is shown.
  print(paste(c("Number of missings:", n_missings), collapse = " "))
  print('----------------')
  print("Least squares solution:")
  #Coefficients and R squared are shown.
  print(bes)
  print(paste(c("R squared:", round(R2, 3)), collapse = " "))
  #Predicted values and residuals, and a plot of them is shown. Residuals are shown on y axis and hence the
  #second argument of plot.
  print('----------------')
  print(pred_res)
  plot(predicted, residuals, ylab = 'Residuals', xlab = 'Predicted y', main = 'Residuals against predicted')
  
}
#I compare my function with the built in regression function. The B coefficients and R squared are the same!
#The coefficient for the intercept is -15.891, for area 0.013, for elevation -0.041 and for endemics 4.332. 
#R squared is 0.949.
regression(data= data, 'Species', 'Area', 'Elevation', 'Endemics')
summary(lm(data$Species~data$Area+data$Elevation+data$Endemics))
