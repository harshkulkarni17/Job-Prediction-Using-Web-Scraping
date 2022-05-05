#libraries included ----
library(psych)
library(data.table)
library(dplyr)
library(reshape2)
library(DescTools)
library(VGAM)
library(ROCR)
library(Metrics)
library(caret)
library(class)
library(gmodels)
library(ggplot2)

# Retrieving the data ----
setwd("E:/SY CS-B 20-21 SEM-I/DS (DATA SCIENCE)/Course Project")
Job_Data <- read.csv("Job_Data.csv", header = T)
Job_Data <- select(Job_Data, -X)

#job_post conversion ----
for (i in 1:length(Job_Data$Job_post)) {
  if (Job_Data$Job_post[i] %like any% c("%Validation En%","%Data Sci%","%Data Ana%","%Data Eng%", "%DATA SCI%", "%Scientist%","%Data Qua%")){
    Job_Data$Job_post[i] <- "Data_Scientist"
    
  } else if (Job_Data$Job_post[i] %like any% "%Software%"){
    Job_Data$Job_post[i] <- "Software_Engineer"
    
  } else if (Job_Data$Job_post[i] %like any% c("%CDA%","%Android%","%android%","%Application Dev%", "%developer%", "%Developer%", "%backend%","%frontend%","%Stack%", "%Taxonomist%")){
    Job_Data$Job_post[i] <- "Android_developer"
    
  } else if (Job_Data$Job_post[i] %like any% c("%IT%","%Information Tech%","%Desktop Eng%", "%Support Eng%", "%System Ad%")){
    Job_Data$Job_post[i] <- "IT_Engineer"
    
    
  } else if (Job_Data$Job_post[i] %like any% c("%Hardware Eng%","%Yield Engi%","%CAM Eng%","%Electronic%","%Embedded%","%ELECTRONIC%", "%Electrical%","%electronic%", "%Industrial Eng%")){
    Job_Data$Job_post[i] <- "ENTC_Engineer"
    
    
  } else if (Job_Data$Job_post[i] %like any% c("%R&D Eng%","%Robotics%","%Accountant%","%AI%","%ML%","%Customer%", "%Principal%","%Solution%", "%Support Ana%", "%Automation%", "%Lead%", "%Cloud%", "%computer%","%Intelligence%")){
    Job_Data$Job_post[i] <- "AI_ML_Engineer"
    
    
  } else if (Job_Data$Job_post[i] %like any% c("%DATABASE%","%Database%","%PHP%","%Data Mana%", "%Node js%", "%Dot Net%")){
    Job_Data$Job_post[i] <- "Database_Developer"
    
    
  } else if (Job_Data$Job_post[i] %like any% c("%Consultant%", "%Safety%", "%Implementation%","%Marketing%", "%Product%","%Technical Ana%", "%Application Sup%", "%Customer Sup%", "%Technical%","%Test Anal%","%Analyst%", "%Design%", "%design%",
                                               "%NETWORK%", "%Test%", "%Graduate%","%Maintenance Eng%", "%Procurement Eng%","%Director, Sales%", "%Scrum Master%", "%Facility Manager%","%Executive%", "%Admissions Assistant%")){
    Job_Data$Job_post[i] <- "Technical_Engineer"
    
    
  }
}




#Visualizing data----
post_visualization <- ggplot(Job_Data, aes(Job_post, y=..count.., fill=Job_post)) + geom_histogram(stat = "count", show.legend = F, position = "identity") + theme_classic()
post_visualization <- post_visualization + ggtitle("Job Posts") + theme(plot.title = element_text(hjust = 0.5)) + stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1)
plot(post_visualization)

pairs.panels(Job_Data)

#skill matrix ----
skills <- as.data.frame(Job_Data$Skills, stringsAsFactors = F)
skills2 <- as.data.frame(tstrsplit(skills[,1],'[|]',type.convert = T),stringsAsFactors = F)
colnames(skills2) <- c("a","b","c")

skills2$a <- gsub(" ","",skills2$a)
skills2$b <- gsub(" ","",skills2$b)
skills2$c <- gsub(" ","",skills2$c)

skill_list <- c("python", "java", "R", "machine learning", "data visualization", "database handling", "presentation skills",
                "mathematical aptitude", "problem solving", "programming", "data structures and algorithms", "databases", "operating systems",
                "android studio", "android sdk", "xml", "API",
                "software development","object-oriented design","debugging", "analytical abilities",
                "circuit design", "aptitude for maths", "system design", "hardware", "analog",
                "statistics","probability","machine learning algorithms","data modeling", "linear algebra",
                "SQL","firebase","cloud","MS-Excel","data warehouse", "C++","unix",
                "web services","SQL/PL","C#")
skill_list <- gsub(" ","",skill_list)
skill_matrix <- matrix(0,length(Job_Data$Company),length(skill_list))
skill_matrix[1,] <- skill_list
colnames(skill_matrix) <- skill_list
for (i in 1:nrow(skills2)) {
  for (c in 1:ncol(skills2)) {
    if (i+1<nrow(skills2)) {
      skill_mat_col <- which(skill_matrix[1,]==skills2[i,c])
      skill_matrix[i+1,skill_mat_col] <- 1
    }
  }
}

skill_matrix2 <- as.data.frame(skill_matrix[-1,],stringsAsFactors = F)
for (i in 1:ncol(skill_matrix2)) {
  skill_matrix2[,i] <- as.integer(skill_matrix2[,i])
}

#Job_location matrix ----
locations <- as.data.frame(Job_Data$Job_location, stringsAsFactors = F)
colnames(locations) <- "Job_location"
location_list <- c("Mumbai","Chennai","Pune","Noida","Bangalore","Thiruvananthapuram","Cochin","New Delhi","Gurgaon","Ahmedabad","India")
location_matrix <- matrix(0,length(Job_Data$Company),length(location_list))
location_matrix[1,] <- location_list
colnames(location_matrix) <- location_list
for (i in 1:nrow(locations)) {
  for (c in 1:ncol(locations)) {
    if (i+1<nrow(locations)) {
      location_mat_col <- which(location_matrix[1,]==locations[i,c])
      location_matrix[i+1,location_mat_col] <- 1
    }
  }
}
location_matrix2 <- as.data.frame(location_matrix[-1,],stringsAsFactors = F)
for (i in 1:ncol(location_matrix2)) {
  location_matrix2[,i] <- as.integer(location_matrix2[,i])
}

#job post matrix ----
posts <- as.data.frame(Job_Data$Job_post, stringsAsFactors = F)
colnames(posts) <- "Job_Post"
post_list <- c("Data_Scientist","Software_Engineer","Android_developer","IT_Engineer","ENTC_Engineer","Technical_Engineer","AI_ML_Engineer")
post_matrix <- matrix(0,length(Job_Data$Company),length(post_list))
post_matrix[1,] <- post_list
colnames(post_matrix) <- post_list
for (i in 1:nrow(posts)) {
  for (c in 1:ncol(posts)) {
    if (i+1<nrow(posts)) {
      post_mat_col <- which(post_matrix[1,]==posts[i,c])
      post_matrix[i+1,post_mat_col] <- 1
    }
  }
}
post_matrix2 <- as.data.frame(post_matrix[-1,],stringsAsFactors = F)
for (i in 1:ncol(post_matrix2)) {
  post_matrix2[,i] <- as.integer(post_matrix2[,i])
}

#Combining all matrices ----
Final_Job_Data <- cbind(post_matrix2,location_matrix2,skill_matrix2)
rows <- sample(nrow(Final_Job_Data))
Final_Job_Data <- Final_Job_Data[rows,]

#Applying logistic reg algorithm ----
data1 <- cbind(Job_Post = Job_Data$Job_post[(1:nrow(location_matrix2))],location_matrix2, skill_matrix2)
rows <- sample(nrow(data1))
data1 <- data1[rows,]

#splitting data into training and testing data
train_data1 <- data1[1:800,]
test_data1 <- data1[-(1:800),]

y1 <- train_data1$Job_Post

fit_MLR <- vglm(Job_Post ~ Mumbai + Chennai + Pune +Noida + Bangalore
                + Thiruvananthapuram + Cochin +`New Delhi`+Gurgaon + Ahmedabad 
                + India + python + java + R + machinelearning + datavisualization
                +databasehandling + presentationskills + mathematicalaptitude + problemsolving
                + programming + datastructuresandalgorithms + databases + operatingsystems + androidstudio
                + androidsdk + xml + API + softwaredevelopment + `object-orienteddesign`+debugging
                + analyticalabilities +circuitdesign + aptitudeformaths + systemdesign
                + hardware + analog + statistics + probability
                + machinelearningalgorithms + datamodeling + linearalgebra
                + SQL + firebase + cloud + `MS-Excel`+ datawarehouse
                + `C++` + unix + webservices + `SQL/PL`+`C#`, family = multinomial, data = train_data1)
#summary(fit_MLR)

prob_MLR <- predict(fit_MLR, newdata=test_data1[,2:53], type = "response")
prediction1 <- apply(prob_MLR, 1, which.max)
tb <- table(test_data1$Job_Post,prediction1)
cat("\nConfusion matrix for logistic regression model - \n")
print(tb)
cmatrix_logistic <- CrossTable(x=test_data1$Job_Post, y=prediction1, prop.chisq = F)
acc_logistic <- sum(diag(tb))/sum(tb)
precision_logistic <- diag(tb)/colSums(tb)
recall_logistic <- diag(tb)/rowSums(tb)
f1score_logistic <- (2*precision_logistic*recall_logistic)/(precision_logistic+recall_logistic)
cat("\nAccuracy of the model by logistic regression = ",acc_logistic)
cat("\nPrecision of the model by logistic regression = ",precision_logistic)
cat("\nRecall of the model by logistic regression = ",recall_logistic)
cat("\nF-score of the model by logistic regression = ",f1score_logistic)

#plotting observations
results_logistic <- data.frame(Job_post = sort(unique(test_data1$Job_Post)))
results_logistic <- cbind(results_logistic,precision_logistic, recall_logistic, f1score_logistic)

dev.off() #clears previous plots
plot_prec_log <- ggplot(results_logistic, aes(Job_post,precision_logistic, fill = Job_post))+ geom_bar(stat = "identity", width = 0.5, show.legend = F)+theme_classic()
plot_prec_log <- plot_prec_log + ggtitle("Metrics : Precision (Logistic Regression)") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(label=round(precision_logistic,digits = 2),vjust = -0.2)
cat("\nBar chart for Precision of logistic regression model - ")
plot_prec_log

plot_rcall_log <- ggplot(results_logistic, aes(Job_post,recall_logistic, fill = Job_post))+ geom_bar(stat = "identity", width = 0.5, show.legend = F)+theme_classic()
plot_rcall_log <- plot_rcall_log + ggtitle("Metrics : Recall (Logistic Regression)") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(label=round(recall_logistic,digits = 2),vjust = -0.2)
cat("\nBar chart for Recall of logistic regression model - ")
plot_rcall_log

plot_f1score_log <- ggplot(results_logistic, aes(Job_post,f1score_logistic, fill = Job_post))+ geom_bar(stat = "identity", width = 0.5, show.legend = F)+theme_classic()
plot_f1score_log <- plot_f1score_log + ggtitle("Metrics : f1score (Logistic Regression)") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(label=round(f1score_logistic,digits = 2),vjust = -0.2)
cat("\nBar chart for F-score of logistic regression model - ")
plot_f1score_log

df <- melt(results_logistic, id.vars = "Job_post", variable.name = "Performance",
           value.name="scores")
plot_performance_log <- ggplot(df, aes(x = Job_post , y= scores, fill = Performance)) + geom_bar(position="dodge", stat = "identity",width = 0.5, color="black") + theme_classic()+ scale_fill_manual(labels= c("Precision", "Recall", "F-Score"),values=c("#373B44", "#4286f4", "#6DD5FA"))
plot_performance_log <- plot_performance_log + ggtitle("Performance (Logistic Regression)") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label = round(scores, 2)), vjust = -0.2,position = position_dodge(0.9))
cat("\nStacked Bar chart for Precision, Recall and F-score of logistic regression model - ")
plot_performance_log

#applying KNN algorithm ----
y_train <- train_data1[,1]
y_test <- test_data1[,1]
pred_knn <- knn(train = train_data1[,-1], test = test_data1[,-1], cl=y_train, k=10)  
tb_knn <- table(y_test,pred_knn)
cat("\nConfusion matrix for KNN algorithm model - \n")
print(tb_knn)
cmatrix_knn <- CrossTable(x=y_test, y=pred_knn, prop.chisq = F)
acc_knn <- sum(diag(tb_knn))/sum(tb_knn)
precision_knn <- diag(tb_knn)/colSums(tb_knn)  
recall_knn <- diag(tb_knn)/rowSums(tb_knn)  
f1score_knn <- (2*precision_knn*recall_knn)/(precision_knn+recall_knn)
cat("\nAccuracy of the model by KNN algorithm = ",acc_knn)
cat("\nPrecision of the model by KNN algorithm = ",precision_knn)
cat("\nRecall of the model by KNN algorithm = ",recall_knn)
cat("\nF-score of the model by KNN algorithm = ",f1score_knn)

#plotting observations
results_knn <- data.frame(Job_post = sort(unique(test_data1$Job_Post)))
results_knn <- cbind(results_knn,precision_knn, recall_knn, f1score_knn)

plot_prec_knn <- ggplot(results_knn, aes(Job_post,precision_knn, fill = Job_post))+ geom_bar(stat = "identity", width = 0.5, show.legend = F)+theme_classic()
plot_prec_knn <- plot_prec_knn + ggtitle("Metrics : Precision (KNN)") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(label=round(precision_knn,digits = 2),vjust = -0.2)
cat("\nBar chart for Precision of KNN algorithm model - ")
plot_prec_knn
  
plot_rcall_knn <- ggplot(results_knn, aes(Job_post,recall_knn, fill = Job_post))+ geom_bar(stat = "identity", width = 0.5, show.legend = F)+theme_classic()
plot_rcall_knn <- plot_rcall_knn + ggtitle("Metrics : Recall (KNN)") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(label=round(recall_knn,digits = 2),vjust = -0.2)
cat("\nBar chart for Recall of KNN algorithm model - ")
plot_rcall_knn

plot_f1score_knn <- ggplot(results_knn, aes(Job_post,f1score_knn, fill = Job_post))+ geom_bar(stat = "identity", width = 0.5, show.legend = F)+ theme_classic()
plot_f1score_knn <- plot_f1score_knn + ggtitle("Metrics : f1score (KNN)") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(label=round(f1score_knn,digits = 2),vjust = -0.2)
cat("\nBar chart for F-score of KNN algorithm model - ")
plot_f1score_knn

df1 <- melt(results_knn, id.vars = "Job_post", variable.name = "Performance",
            value.name="scores")
plot_performance_knn <- ggplot(df1, aes(x = Job_post , y= scores, fill = Performance)) + geom_bar(position="dodge", stat = "identity",width = 0.5, color="black") + theme_classic()+ scale_fill_manual(labels= c("Precision", "Recall", "F-Score"),values=c("#373B44", "#4286f4", "#6DD5FA"))
plot_performance_knn <- plot_performance_knn + ggtitle("Performance (KNN)") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label = round(scores, 2)), vjust = -0.2,position = position_dodge(0.9))
cat("\nStacked Bar chart for Precision, Recall and F-score of KNN algorithm model - ")
plot_performance_knn
