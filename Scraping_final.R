library(rvest)
library(dplyr)

Job_Data <- data.frame()
Company <- vector()
Job_post <- vector()
Job_location <- vector()
x = c(".css","Glassdoor","Employer")

#Data scientist jobs ----
ds_Data <- data.frame()
links <- vector()
for (page_number in 1:5){  #data scientist
  link <- paste0("https://www.glassdoor.co.in/Job/data-scientist-jobs-SRCH_KO0,14_IP",page_number,".htm")
  links <- c(links,link)
}


for(each_link in links){
  page <- read_html(each_link)
  company_title <- page %>%
    html_nodes(".e1n63ojh0 span") %>% html_text()
  Company <- c(Company,company_title)
  each_job_post <- page %>%
    html_nodes(".eigr9kq1 span") %>% html_text()
  Job_post <- c(Job_post, each_job_post)
  each_job_location <- page %>%
    html_nodes(".pr-xxsm") %>% html_text()
  each_job_location <- each_job_location[!each_job_location %in% grep(paste0(x, collapse = "|"), each_job_location, value = T)]
  Job_location <- c(Job_location,each_job_location)
}

Salary <- data.frame("Salary_in_thousands" = floor(runif(length(Company),min = 500, max = 1000)))
ds_Data <- cbind(Company,Job_post,Job_location,Salary)

Experience <- data.frame("Experience_in_years"= floor(runif(length(Company),min = 0, max = 7)))
ds_Data <- cbind(ds_Data, Experience)

Skills <- vector()
each_skill <- vector()

skills <- data.frame(name = c("python", "java", "R", "machine learning", "data visualization", "database handling", "presentation skills"))
for (i in 1:length(Company)) {
  each_skill <- sample_n(skills, 3, fac = "name")$name
  Skills <- rbind(Skills, each_skill)
}

ds_Data <- cbind(ds_Data, "SKILLS" = Skills)
ds_Data$Skills <- paste(ds_Data$SKILLS.1,"|",ds_Data$SKILLS.2,"|",ds_Data$SKILLS.3)
ds_Data <- subset(ds_Data,select = -c(SKILLS.1,SKILLS.2,SKILLS.3))

#Software Engineer ----
soft_Data <- data.frame()
soft_company <- vector()
soft_post <- vector()
soft_location <- vector()

links2 <- vector()
for (page_number in 1:5){  #soft engg
  link2 <- paste0("https://www.glassdoor.com/Job/india-software-engineer-jobs-SRCH_IL.0,5_IN115_KO6,23_IP",page_number,".htm")
  links2 <- c(links2, link2)
}


for(each_link in links2){
  page <- read_html(each_link)
  company_title <- page %>%
    html_nodes(".e1n63ojh0 span") %>% html_text()
  soft_company <- c(soft_company,company_title)
  
  each_job_post <- page %>%
    html_nodes(".eigr9kq1 span") %>% html_text()
  soft_post <- c(soft_post, each_job_post)
  
  each_job_location <- page %>%
    html_nodes(".pr-xxsm") %>% html_text()
  each_job_location <- each_job_location[!each_job_location %in% grep(paste0(x, collapse = "|"), each_job_location, value = T)]
  soft_location <- c(soft_location,each_job_location)
}

Salary <- data.frame("Salary_in_thousands" = floor(runif(length(soft_company),min = 200, max = 700)))
soft_Data <- cbind(Company = soft_company,Job_post = soft_post,Job_location = soft_location[1:length(soft_company)],Salary)

Experience <- data.frame("Experience_in_years"= floor(runif(length(soft_company),min = 0, max = 7)))
soft_Data <- cbind(soft_Data, Experience)

Skills <- vector()
each_skill <- vector()

soft_skills <- data.frame(name = c("mathematical aptitude", "problem solving", "programming", "data structures and algorithms", "databases", "operating systems"))
for (i in 1:length(soft_company)) {
  each_skill <- sample_n(soft_skills, 3, fac = "name")$name
  Skills <- rbind(Skills, each_skill)
}

soft_Data <- cbind(soft_Data, "SKILLS" = Skills)
soft_Data$Skills <- paste(soft_Data$SKILLS.1,"|",soft_Data$SKILLS.2,"|",soft_Data$SKILLS.3)
soft_Data <- subset(soft_Data,select = -c(SKILLS.1,SKILLS.2,SKILLS.3))

#Android developer ----
android_Data <- data.frame()
android_company <- vector()
android_post <- vector()
android_location <- vector()
links3 <- vector()
for (page_number in 1:5){ #Android developer jobs
  link3 <- paste0("https://www.glassdoor.com/Job/india-android-developer-jobs-SRCH_IL.0,5_IN115_KO6,23_IP",page_number,".htm")
  links3 <- c(links3, link3)
}


for(each_link in links3){
  page <- read_html(each_link)
  company_title <- page %>%
    html_nodes(".e1n63ojh0 span") %>% html_text()
  android_company <- c(android_company,company_title)
  each_job_post <- page %>%
    html_nodes(".eigr9kq1 span") %>% html_text()
  android_post <- c(android_post, each_job_post)
  each_job_location <- page %>%
    html_nodes(".pr-xxsm") %>% html_text()
  each_job_location <- each_job_location[!each_job_location %in% grep(paste0(x, collapse = "|"), each_job_location, value = T)]
  android_location <- c(android_location,each_job_location)
}

Salary <- data.frame("Salary_in_thousands" = floor(runif(length(android_company),min = 200, max = 500)))
android_Data <- cbind(Company = android_company,Job_post = android_post,Job_location = android_location,Salary)

Experience <- data.frame("Experience_in_years"= floor(runif(length(android_company),min = 0, max = 7)))
android_Data <- cbind(android_Data, Experience)

Skills <- vector()
each_skill <- vector()

android_skills <- data.frame(name = c("java", "android studio", "android sdk", "xml", "database handling", "API"))
for (i in 1:length(android_company)) {
  each_skill <- sample_n(android_skills, 3, fac = "name")$name
  Skills <- rbind(Skills, each_skill)
}

android_Data <- cbind(android_Data, "SKILLS" = Skills)
android_Data$Skills <- paste(android_Data$SKILLS.1,"|",android_Data$SKILLS.2,"|",android_Data$SKILLS.3)
android_Data <- subset(android_Data,select = -c(SKILLS.1,SKILLS.2,SKILLS.3))

#IT jobs ----
it_Data <- data.frame()
it_company <- vector()
it_post <- vector()
it_location <- vector()
links4 <- vector()
for (page_number in 1:5){ #IT jobs
  link4 <- paste0("https://www.glassdoor.com/Job/india-it-jobs-SRCH_IL.0,5_IN115_KO6,8_IP",page_number,".htm")
  links4 <- c(links4, link4)
}


for(each_link in links4){
  page <- read_html(each_link)
  company_title <- page %>%
    html_nodes(".e1n63ojh0 span") %>% html_text()
  it_company <- c(it_company,company_title)
  each_job_post <- page %>%
    html_nodes(".eigr9kq1 span") %>% html_text()
  it_post <- c(it_post, each_job_post)
  each_job_location <- page %>%
    html_nodes(".pr-xxsm") %>% html_text()
  each_job_location <- each_job_location[!each_job_location %in% grep(paste0(x, collapse = "|"), each_job_location, value = T)]
  it_location <- c(it_location,each_job_location)
}

Salary <- data.frame("Salary_in_thousands" = floor(runif(length(it_company),min = 200, max = 500)))
it_Data <- cbind(Company = it_company,Job_post=it_post,Job_location=it_location,Salary)

Experience <- data.frame("Experience_in_years"= floor(runif(length(it_company),min = 0, max = 7)))
it_Data <- cbind(it_Data, Experience)

Skills <- vector()
each_skill <- vector()

it_skills <- data.frame(name = c("problem solving","programming","software development","object-oriented design","debugging", "analytical abilities"))
for (i in 1:length(it_company)) {
  each_skill <- sample_n(it_skills, 3, fac = "name")$name
  Skills <- rbind(Skills, each_skill)
}

it_Data <- cbind(it_Data, "SKILLS" = Skills)
it_Data$Skills <- paste(it_Data$SKILLS.1,"|",it_Data$SKILLS.2,"|",it_Data$SKILLS.3)
it_Data <- subset(it_Data,select = -c(SKILLS.1,SKILLS.2,SKILLS.3))

#Entc jobs ----
entc_Data <- data.frame()
entc_company <- vector()
entc_post <- vector()
entc_location <- vector()
links5 <- vector()
for (page_number in 1:5){  #entc jobs
  link5 <- paste0("https://www.glassdoor.com/Job/india-electronics-engineer-jobs-SRCH_IL.0,5_IN115_KO6,26_IP",page_number,".htm")
  links5 <- c(links5, link5)
}


for(each_link in links5){
  page <- read_html(each_link)
  company_tentcle <- page %>%
    html_nodes(".e1n63ojh0 span") %>% html_text()
  entc_company <- c(entc_company,company_tentcle)
  each_job_post <- page %>%
    html_nodes(".eigr9kq1 span") %>% html_text()
  entc_post <- c(entc_post, each_job_post)
  each_job_location <- page %>%
    html_nodes(".pr-xxsm") %>% html_text()
  each_job_location <- each_job_location[!each_job_location %in% grep(paste0(x, collapse = "|"), each_job_location, value = T)]
  entc_location <- c(entc_location,each_job_location)
}

Salary <- data.frame("Salary_in_thousands" = floor(runif(length(entc_company),min = 300, max = 600)))
entc_Data <- cbind(Company=entc_company,Job_post=entc_post,Job_location=entc_location,Salary)

Experience <- data.frame("Experience_in_years"= floor(runif(length(entc_company),min = 0, max = 7)))
entc_Data <- cbind(entc_Data, Experience)

Skills <- vector()
each_skill <- vector()

entc_skills <- data.frame(name = c("circuit design", "aptitude for maths", "system design", "hardware", "analog", "programming"))
for (i in 1:length(entc_company)) {
  each_skill <- sample_n(entc_skills, 3, fac = "name")$name
  Skills <- rbind(Skills, each_skill)
}

entc_Data <- cbind(entc_Data, "SKILLS" = Skills)
entc_Data$Skills <- paste(entc_Data$SKILLS.1,"|",entc_Data$SKILLS.2,"|",entc_Data$SKILLS.3)
entc_Data <- subset(entc_Data,select = -c(SKILLS.1,SKILLS.2,SKILLS.3))

#AI/ML jobs ----
ai_ml_Data <- data.frame()
ai_ml_company <- vector()
ai_ml_post <- vector()
ai_ml_location <- vector()
links6 <- vector()
for (page_number in 1:5){ #AI/Ml jobs
  link6 <- paste0("https://www.glassdoor.com/Job/india-ai-jobs-SRCH_IL.0,5_IN115_KO6,8_IP",page_number,".htm")
  links6 <- c(links6, link6)
}


for(each_link in links6){
  page <- read_html(each_link)
  company_tai_mlle <- page %>%
    html_nodes(".e1n63ojh0 span") %>% html_text()
  ai_ml_company <- c(ai_ml_company,company_tai_mlle)
  each_job_post <- page %>%
    html_nodes(".eigr9kq1 span") %>% html_text()
  ai_ml_post <- c(ai_ml_post, each_job_post)
  each_job_location <- page %>%
    html_nodes(".pr-xxsm") %>% html_text()
  each_job_location <- each_job_location[!each_job_location %in% grep(paste0(x, collapse = "|"), each_job_location, value = T)]
  ai_ml_location <- c(ai_ml_location,each_job_location)
}

Salary <- data.frame("Salary_in_thousands" = floor(runif(length(ai_ml_company),min = 300, max = 600)))
ai_ml_Data <- cbind(Company=ai_ml_company,Job_post=ai_ml_post,Job_location=ai_ml_location,Salary)

Experience <- data.frame("Experience_in_years"= floor(runif(length(ai_ml_company),min = 0, max = 7)))
ai_ml_Data <- cbind(ai_ml_Data, Experience)

Skills <- vector()
each_skill <- vector()

ai_ml_skills <- data.frame(name = c("statistics","probability","machine learning algorithms","data modeling", "programming", "linear algebra"))
for (i in 1:length(ai_ml_company)) {
  each_skill <- sample_n(ai_ml_skills, 3, fac = "name")$name
  Skills <- rbind(Skills, each_skill)
}

ai_ml_Data <- cbind(ai_ml_Data, "SKILLS" = Skills)
ai_ml_Data$Skills <- paste(ai_ml_Data$SKILLS.1,"|",ai_ml_Data$SKILLS.2,"|",ai_ml_Data$SKILLS.3)
ai_ml_Data <- subset(ai_ml_Data,select = -c(SKILLS.1,SKILLS.2,SKILLS.3))

#DBMS jobs ----
dbms_Data <- data.frame()
dbms_company <- vector()
dbms_post <- vector()
dbms_location <- vector()
links7 <- vector()
for (page_number in 1:3){ #Database developer jobs
  link7 <- paste0("https://www.glassdoor.com/Job/india-database-jobs-SRCH_IL.0,5_IN115_KO6,14_IP",page_number,".htm")
  links7 <- c(links7, link7)
}


for(each_link in links7){
  page <- read_html(each_link)
  company_tdbmsle <- page %>%
    html_nodes(".e1n63ojh0 span") %>% html_text()
  dbms_company <- c(dbms_company,company_tdbmsle)
  each_job_post <- page %>%
    html_nodes(".eigr9kq1 span") %>% html_text()
  dbms_post <- c(dbms_post, each_job_post)
  each_job_location <- page %>%
    html_nodes(".pr-xxsm") %>% html_text()
  each_job_location <- each_job_location[!each_job_location %in% grep(paste0(x, collapse = "|"), each_job_location, value = T)]
  dbms_location <- c(dbms_location,each_job_location)
}

Salary <- data.frame("Salary_in_thousands" = floor(runif(length(dbms_company),min = 300, max = 600)))
dbms_Data <- cbind(Company=dbms_company,Job_post=dbms_post,Job_location=dbms_location,Salary)

Experience <- data.frame("Experience_in_years"= floor(runif(length(dbms_company),min = 0, max = 7)))
dbms_Data <- cbind(dbms_Data, Experience)

Skills <- vector()
each_skill <- vector()

dbms_skills <- data.frame(name = c("SQL","firebase","cloud","MS-Excel","data warehouse", "C++","unix","programming"))
for (i in 1:length(dbms_company)) {
  each_skill <- sample_n(dbms_skills, 3, fac = "name")$name
  Skills <- rbind(Skills, each_skill)
}

dbms_Data <- cbind(dbms_Data, "SKILLS" = Skills)
dbms_Data$Skills <- paste(dbms_Data$SKILLS.1,"|",dbms_Data$SKILLS.2,"|",dbms_Data$SKILLS.3)
dbms_Data <- subset(dbms_Data,select = -c(SKILLS.1,SKILLS.2,SKILLS.3))

#Technical analyst jobs ----
tech_analyst_Data <- data.frame()
tech_analyst_company <- vector()
tech_analyst_post <- vector()
tech_analyst_location <- vector()
links8 <- vector()
for (page_number in 1:3){ #Technical analyst jobs
  link8 <- paste0("https://www.glassdoor.co.in/Job/technical-analyst-jobs-SRCH_KO0,17_IP",page_number,".htm")
  links8 <- c(links8, link8)
}


for(each_link in links8){
  page <- read_html(each_link)
  company_ttech_analystle <- page %>%
    html_nodes(".e1n63ojh0 span") %>% html_text()
  tech_analyst_company <- c(tech_analyst_company,company_ttech_analystle)
  each_job_post <- page %>%
    html_nodes(".eigr9kq1 span") %>% html_text()
  tech_analyst_post <- c(tech_analyst_post, each_job_post)
  each_job_location <- page %>%
    html_nodes(".pr-xxsm") %>% html_text()
  each_job_location <- each_job_location[!each_job_location %in% grep(paste0(x, collapse = "|"), each_job_location, value = T)]
  tech_analyst_location <- c(tech_analyst_location,each_job_location)
}

Salary <- data.frame("Salary_in_thousands" = floor(runif(length(tech_analyst_company),min = 300, max = 600)))
tech_analyst_Data <- cbind(Company=tech_analyst_company,Job_post=tech_analyst_post,Job_location=tech_analyst_location,Salary)

Experience <- data.frame("Experience_in_years"= floor(runif(length(tech_analyst_company),min = 0, max = 7)))
tech_analyst_Data <- cbind(tech_analyst_Data, Experience)

Skills <- vector()
each_skill <- vector()

tech_analyst_skills <- data.frame(name = c("data warehouse","web services","hardware","SQL/PL","unix","C#"))
for (i in 1:length(tech_analyst_company)) {
  each_skill <- sample_n(tech_analyst_skills, 3, fac = "name")$name
  Skills <- rbind(Skills, each_skill)
}

tech_analyst_Data <- cbind(tech_analyst_Data, "SKILLS" = Skills)
tech_analyst_Data$Skills <- paste(tech_analyst_Data$SKILLS.1,"|",tech_analyst_Data$SKILLS.2,"|",tech_analyst_Data$SKILLS.3)
tech_analyst_Data <- subset(tech_analyst_Data,select = -c(SKILLS.1,SKILLS.2,SKILLS.3))


#creating final data - merging dataframes ----
Job_Data <- rbind(ds_Data, soft_Data, android_Data,it_Data,entc_Data,ai_ml_Data,dbms_Data,tech_analyst_Data)
write.csv(Job_Data, "Job_Data1.csv")
print(head(Job_Data, 5))
print(summary(Job_Data))

