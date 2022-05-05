#taking inputs from user for job prediction

loc <- c("Mumbai","Chennai","Pune","Noida","Bangalore","Thiruvananthapuram","Cochin","New Delhi","Gurgaon","Ahmedabad","India")
skill <- c("python", "java", "R", "machine learning", "data visualization", "database handling", "presentation skills",
  "mathematical aptitude", "problem solving", "programming", "data structures and algorithms", "databases", "operating systems",
  "android studio", "android sdk", "xml", "API",
  "software development","object-oriented design","debugging", "analytical abilities",
  "circuit design", "aptitude for maths", "system design", "hardware", "analog",
  "statistics","probability","machine learning algorithms","data modeling", "linear algebra",
  "SQL","firebase","cloud","MS-Excel","data warehouse", "C++","unix",
  "web services","SQL/PL","C#")
cat("\nAvailable Locations for Job : \n")
print(loc)
ip_loc = readline(prompt = "Enter Job_location from above locations:")
cat("\nSkills for Job : \n")
print(skill)
ip_skills <- readline(prompt = "Enter 3 comma separated skills from above available skills: ")
library(data.table)

inputs <- data.frame(Location = ip_loc, Skills = ip_skills)

process_skills <- as.data.frame(inputs$Skills, stringsAsFactors = F)
process_skills2 <-
  as.data.frame(tstrsplit(process_skills[, 1], '[,]', type.convert = T), stringsAsFactors = F)
colnames(process_skills2) <- c("a", "b", "c")

process_skills2$a <- gsub(" ", "", process_skills2$a)
process_skills2$b <- gsub(" ", "", process_skills2$b)
process_skills2$c <- gsub(" ", "", process_skills2$c)
  
ip_skill_list <- c("python", "java", "R", "machine learning", "data visualization", "database handling", "presentation skills",
                  "mathematical aptitude", "problem solving", "programming", "data structures and algorithms", "databases", "operating systems",
                  "android studio", "android sdk", "xml", "API",
                  "software development","object-oriented design","debugging", "analytical abilities",
                  "circuit design", "aptitude for maths", "system design", "hardware", "analog",
                  "statistics","probability","machine learning algorithms","data modeling", "linear algebra",
                  "SQL","firebase","cloud","MS-Excel","data warehouse", "C++","unix",
                  "web services","SQL/PL","C#")
ip_skill_list <- gsub(" ", "", ip_skill_list)
ip_skill_matrix <-
  matrix(0, 2, length(ip_skill_list))
ip_skill_matrix[1, ] <- ip_skill_list
colnames(ip_skill_matrix) <- ip_skill_list
for (i in 1:nrow(process_skills2)) {
  for (c in 1:ncol(process_skills2)) {
      ip_skill_mat_col <- which(tolower(ip_skill_matrix[1, ]) == tolower(process_skills2[i, c]))
      ip_skill_matrix[i + 1, ip_skill_mat_col] <- 1
  }
}

ip_skill_matrix2 <-
  as.data.frame(ip_skill_matrix[-1, ], stringsAsFactors = F)
for (i in 1:ncol(ip_skill_matrix2)) {
  ip_skill_matrix2[, i] <- as.integer(ip_skill_matrix2[, i])
}
ip_skill_matrix2 <- transpose(ip_skill_matrix2)
colnames(ip_skill_matrix2) <- ip_skill_list


ip_locations <- as.data.frame(inputs$Location, stringsAsFactors = F)
colnames(ip_locations) <- "Job_location"
ip_location_list <- c("Mumbai","Chennai","Pune","Noida","Bangalore","Thiruvananthapuram","Cochin","New Delhi","Gurgaon","Ahmedabad","India")
ip_location_matrix <- matrix(0,2,length(location_list))
ip_location_matrix[1,] <- ip_location_list
colnames(ip_location_matrix) <- ip_location_list
for (i in 1:nrow(ip_locations)) {
  for (c in 1:ncol(ip_locations)) {
    ip_location_mat_col <- which(tolower(ip_location_matrix[1,])==tolower(ip_locations[i,c]))
    ip_location_matrix[i+1,ip_location_mat_col] <- 1
  }
}
ip_location_matrix2 <- as.data.frame(ip_location_matrix[-1,],stringsAsFactors = F)
for (i in 1:ncol(ip_location_matrix2)) {
  ip_location_matrix2[,i] <- as.integer(ip_location_matrix2[,i])
}
ip_location_matrix2 <- transpose(ip_location_matrix2)
colnames(ip_location_matrix2) <- ip_location_list

ip_job_data <- cbind(ip_location_matrix2, ip_skill_matrix2)
op_job_post <- knn(train = train_data1[,-1], test=ip_job_data, cl=y_train, k=10)
op_job_post <- as.vector(op_job_post)
pred_jobpost <- op_job_post
cat("\nRecommended job post based on location and skills given : ",pred_jobpost)

pred_company <- Job_Data[1:4][which(Job_Data$Job_post==pred_jobpost & tolower(Job_Data$Job_location)==tolower(ip_loc)),]
cat("\nRecommended Companies based on job post, location and skills given - \n")
if(nrow(pred_company)==0){
  cat("\nNo companies found based on your location.")
  cat("\nThere are some companies found related to your job post at other locations - ")
  pred_company1 <- Job_Data[1:4][which(Job_Data$Job_post==pred_jobpost),]
  print(pred_company1)
}else
  print(pred_company)