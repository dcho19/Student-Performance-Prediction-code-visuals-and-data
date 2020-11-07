library(tidyverse)
library(dplyr)
library(caret)
library(rpart.plot)
library(knitr)

#read in all of the .csv files as dataframes
student_dem <- read.csv("studentInfo.csv")
student_vle <- read.csv("studentVle.csv")


#attempt 1 to merge the student_dem and student_vle datasets together using id_student
#did not work and there are now too many observations
#info_and_vle <- merge(student_dem, student_vle, by = "id_student")


#specifically looking for how many categories there are in the variables : final_result, highest_education, age_band, imd_band and region
str(student_dem)


#create histogram for studied credits
studied_creds <- student_dem$studied_credits
SC_hist <- hist(studied_creds,
                main = "Distribution of Total Number of Credits Student Among Students",
                xlab = "Number of Credits Studied",
                col = "pink",
                border = "red")
text(SC_hist$mids,SC_hist$counts,labels=SC_hist$counts, adj=c(0.5, -0.5))



#test and train datasets that include the withdrawn and distinction responses of student_dem data from final_result (with na values omitted)
student_dem2 <- student_dem %>% 
  na_if("") %>% 
  na.omit

nrow(student_dem) #32593 observations
nrow(studen_dem2) #31482 observations
#1110 observations lost

dt1 = sort(sample(nrow(student_dem2), nrow(student_dem2) * .7))
train_student_dem2 <- student_dem2[dt1,]
test_student_dem2 <- studen_dem2[-dt1,]


ctrl = trainControl(method="repeatedcv",number=5,repeats=2)
#Creating Decision Tree model
dtree_fit = train(final_result ~ highest_education + imd_band + studied_credits + age_band, 
                  data = train_student_dem2, 
                  method = "rpart",
                  tuneLength = 30,
                  trControl = ctrl)


dtree_fit
dtree_table <- as.matrix(dtree_fit)
dtree_df <- as.data.frame(dtree_table)
prp(dtree_fit$finalModel)

#predict upon test data using model
dtree_pred <- predict(dtree_fit, newdata = test_student_dem2)
dtree_pred
#create confusion matrix
dt_CM <- confusionMatrix(data = dtree_pred, test_student_dem2$final_result)
dt_CM
#record accuracy of the decision tree model 1
dt1_accuracy <- dt_CM$overall["Accuracy"]
dt1_kappa <- dt_CM$overall["Kappa"]








#Creating random forests model
random_forest1 = train(final_result ~ highest_education + imd_band + studied_credits + age_band, 
                      data = train_student_dem2, 
                      method = "rf",
                      trControl = ctrl,
                      tuneLength = 12,
                      ntree = 500,
                      importance = TRUE)


plot(random_forest1$finalModel)
random_forest_pred = predict(random_forest1,test_student_dem2)
random_forest1
random_forest_pred
#create confusion matrix for random forest model 1
rf1_CM <- confusionMatrix(data = random_forest_pred, test_student_dem2$final_result)
rf1_CM
rf1_accuracy <- rf1_CM$overall["Accuracy"]
rf1_kappa <- rf1_CM$overall["Kappa"]

#find importance of each variable and plot them
Importance = varImp(random_forest1)
plot(Importance)
Importance


#Create visual tables for variable importance based on the Random Forests model
imp_table <- as.matrix(varImp(random_forest1)$importance)
imp_table
var_imp_df <- as.data.frame(imp_table)
var_imp_df <- round(var_imp_df, 2)
#export dataframe to .csv
write.csv(var_imp_df, "C:\\Users\\d_cho\\Desktop\\DENISON FALL 2020\\DA 401\\Final Project 1\\VariableImportance1.csv")











#The previous decision tree and random forests models were to take a look at how the models would fare without taking into account any
#data on the number of clicks (which serves as an indication of the student' level of interaction with the virtual learning environement, or VLE)


#check out the number of unique students and rows in each dataset
nrow(student_dem) #32593 TOTAL OBSERVATIONS
length(unique(student_dem$id_student)) #ONLY 28785 UNIQUE STUDENTS
nrow(student_vle) #10655280 TOTAL OBSERVATIONS
length(unique(student_vle$id_student)) #ONLY 26074 UNIQUE STUDENTS
length(intersect(student_dem$id_student, student_vle$id_student)) #26074 OBERVATIONS

#only keep rows where the student id, code_presentation, and code_module can be matched (in other words, all of student vle, but drop non-matching students from student_dem)
dem_with_VLE <- left_join(student_vle, student_dem)

#check out the number of unique students and rows in each dataset
nrow(dem_with_VLE) #10655280 TOTAL OBSERVATIONS
length(unique(dem_with_VLE$id_student)) #26074 UNIQUE STUDENTS


#create a new column within dem_with_VLE called total_click that simply replicates the sum_click variable
#this will later represent the total sum of the values within sum_click for each unique student id
dem_with_VLE$total_click <- dem_with_VLE$sum_click

#take the sum of sum_click and replace the values in total_click with this. That will represent the total number of clicks that the student has in a VLE, regardless of id_site
totalClick_with_studentID <- 
  dem_with_VLE  %>%
  group_by(id_student, code_module, code_presentation) %>% 
  summarize(total_click = sum(sum_click))


nrow(totalClick_with_studentID) #29228 TOTAL OBSERVATIONS
length(unique(totalClick_with_studentID$id_student)) #26074 UNIQUE STUDENTS
#HERE WE CAN SEE THAT THERE IS A DISCREPENCY BETWEEN THE NUMBER OF TOTAL OBSERVATIONS AND THE TOTAL NUMBER OF UNIQUE STUDENTS
#THUS, IT CAN BE SEEN THAT THERE ARE SOME STUDENTS THAT ARE ENROLLED IN MORE THAN ONE CLASS

#take out all rows where the student id repeats itself more than once
totalClick_with_studentID <-
  totalClick_with_studentID %>% 
  group_by(id_student) %>% 
  filter(n() == 1)

#check the number of unique students that are only taking one class
nrow(totalClick_with_studentID) #23805 TOTAL OBSERVATIONS
length(unique(totalClick_with_studentID$id_student)) #23805 UNIQIE STUDENTs

#next join the dem_with_VLE dataset and totalClick_with_studentID dataset such that non-matching students in dem_with_VLE are dropped
final_dem_with_VLE <- left_join(totalClick_with_studentID, dem_with_VLE)

#since we now have a variable for total_click, the id_site, sum_click and date variables are not required. Drop these variables
final_dem_with_VLE <- final_dem_with_VLE[, -c(5:7)]


#remove the duplicated rows
final_dem_with_VLE <- final_dem_with_VLE %>% 
  distinct()

#check the number of unique students that are only taking one class
nrow(final_dem_with_VLE) #23805 TOTAL OBSERVATIONS
length(unique(final_dem_with_VLE$id_student)) #23805 UNIQUE STUDENTS

#remove NA values
final_dem_with_VLE <- final_dem_with_VLE %>% 
  na_if("") %>% 
  na.omit
#1521 observations were lost by removing NA values



#create histogram of total clicks for explanatory analysis
totalClicks <- final_dem_with_VLE$total_click
TC_hist <- hist(totalClicks,
     main = "Distribution of Total Number of Clicks on VLE Among Students",
     xlab = "Number of Clicks on VLE",
     xlim = c(0,20000),
     ylim = c(0,15000),
     col = "pink",
     border = "red")
text(TC_hist$mids,TC_hist$counts,labels=TC_hist$counts, adj=c(0.5, -0.5))


#create testing and training datasets from final_dem_with_VLE
dt2 = sort(sample(nrow(final_dem_with_VLE), nrow(final_dem_with_VLE) * .7))
train_demVLE <- final_dem_with_VLE[dt2,]
test_demVLE <- final_dem_with_VLE[-dt2,]


#Creating second Decision Tree model
dtree_fit2 = train(final_result ~ highest_education + imd_band + studied_credits + age_band + total_click, 
                  data = train_demVLE, 
                  method = "rpart",
                  tuneLength = 30,
                  trControl = ctrl)

dtree_fit2
dtree_table2 <- as.matrix(dtree_fit2)
dtree_df2 <- as.data.frame(dtree_table2)
prp(dtree_fit2$finalModel)

#predict upon test data
dtree_pred2 <- predict(dtree_fit2, newdata = test_demVLE)
dtree_pred2
#create confusion matrix
dt2_CM <- confusionMatrix(data = dtree_pred2, test_demVLE$final_result)
dt2_CM
dt2_accuracy <- dt2_CM$overall["Accuracy"]
dt2_kappa <- dt2_CM$overall["Kappa"]









#Creating second random forests model
random_forest2 = train(final_result ~ highest_education + imd_band + studied_credits + age_band + total_click, 
                       data = train_demVLE, 
                       method = "rf",
                       trControl = ctrl,
                       tuneLength = 12,
                       ntree = 500,
                       importance = TRUE)


plot(random_forest2$finalModel)
#predict upon test data
random_forest_pred2 = predict(random_forest2,test_demVLE)
random_forest2
random_forest_pred2

rf2_CM <- confusionMatrix(data = random_forest_pred2, test_demVLE$final_result)
rf2_CM
rf2_accuracy <- rf2_CM$overall["Accuracy"]
rf2_kappa <- rf2_CM$overall["Kappa"]



#find importance of each variable and plot them
Importance2 = varImp(random_forest2)
plot(Importance2)
Importance2



#Create visual tables for variable importance based on the Random Forests model
imp_table2 <- as.matrix(varImp(random_forest2)$importance)
imp_table2
var_imp_df2 <- as.data.frame(imp_table2)
var_imp_df2 <- round(var_imp_df2, 2)
#export dataframe to .csv
write.csv(var_imp_df2, "C:\\Users\\d_cho\\Desktop\\DENISON FALL 2020\\DA 401\\Final Project 1\\VariableImportance2.csv")





#Create table of accuracies and kappa scores for all four models
#initialize a table to store the accuracies
comp_table = data.frame(matrix(ncol = 5, nrow = 2))
#create column names
colnames(comp_table) = c('Score Type','Decision Tree Set 1','Random Forest Set 1','Decision Tree Set 2','Random Forest Set 2')
#store values into table
comp_table[1,1] = "Accuracy"
comp_table[2,1] = "Kappa"
comp_table[1,2] = dt1_accuracy
comp_table[1,3] = rf1_accuracy
comp_table[1,4] = dt2_accuracy
comp_table[1,5] = rf2_accuracy
comp_table[2,2] = dt1_kappa
comp_table[2,3] = rf1_kappa
comp_table[2,4] = dt2_kappa
comp_table[2,5] = rf2_kappa
comp_table
#change index of table to represent accuracy and kappa scores
rownames(comp_table) <- comp_table$`Score Type`
comp_table
comp_table <- round(comp_table[,2:5], 3)
comp_table

#export to .csv
write.csv(comp_table, "C:\\Users\\d_cho\\Desktop\\DENISON FALL 2020\\DA 401\\Final Project 1\\ModelAccuracyKappa.csv")


