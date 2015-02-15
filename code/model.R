setwd("/home/benbrew/Documents/tony_b/")
school <- read.csv("public_schools.csv")
setwd("/home/benbrew/Documents/private/")
tony <- read.csv("merged.csv")

#make new name holder for tony#####
tony$name <- NA

tony$School[1]

#isolate nth school 
# 
# temp <- tony$School[1] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$school)
# 
# #which was is the best
# 
# ind <-which.min(mat)
# 
# school$school[ind]

#add county to to school in both data frames 

school$School <- paste0(school$district, sep=" ", school$school, collapse=NULL)


tony$School <-paste0(tony$county, sep=" ",tony$School, collapse=NULL)


#make column lower case
school$School <- tolower(school$School)
tony$School <- tolower(tony$School)


#remove common words withing school

school$School <- gsub("elementary", "", school$School)
school$School <- gsub("elem", "", school$School)
school$School <- gsub("middle", "", school$School)
school$School <- gsub("high", "", school$School)
school$School <- gsub("school", "", school$School)
school$School <- gsub("schl", "", school$School)
school$School <- gsub("sch", "", school$School)
school$School <- gsub("scho", "", school$School)
school$School <- gsub("prog", "", school$School)
school$School <- gsub("[.]", "", school$School)
school$School <- gsub(",", "", school$School)
school$School <- gsub("/", "", school$School)
school$School <- gsub("-", "", school$School)


tony$School <- gsub("elementary", "", tony$School)
tony$School <- gsub("elem", "", tony$School)
tony$School <- gsub("middle", "", tony$School)
tony$School <- gsub("high", "", tony$School)
tony$School <- gsub("school", "", tony$School)
tony$School <- gsub("schl", "", tony$School)
tony$School <- gsub("sch", "", tony$School)
tony$School <- gsub("scho", "", tony$School)
tony$School <- gsub("prog", "", tony$School)
tony$School <- gsub("[.]", "", tony$School)
tony$School <- gsub(",", "", tony$School)
tony$School <- gsub("/", "", tony$School)
tony$School <- gsub("-", "", tony$School)

#try first row again
# temp <- tony$School[1] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# temp <- tony$School[1] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# #which was is the best
# 
# ind <-which.min(mat)
# 
# school$School[ind]
# 
# #try second row..
# 
# temp <- tony$School[2] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# temp <- tony$School[2] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# #which was is the best
# 
# ind <-which.min(mat)
# 
# school$School[ind]
# 
# #try third row..
# 
# temp <- tony$School[3] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# temp <- tony$School[3] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# #which was is the best
# 
# ind <-which.min(mat)
# 
# school$School[ind]
# 
# 
# #try 50th row..
# 
# temp <- tony$School[50] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# temp <- tony$School[50] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# #which was is the best
# 
# ind <-which.min(mat)
# 
# school$School[ind]
# 
# #try 100th row..
# 
# temp <- tony$School[100] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# temp <- tony$School[100] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# #which was is the best
# 
# ind <-which.min(mat)
# 
# school$School[ind]
# 
# #try 1000th row..
# 
# temp <- tony$School[1000] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# temp <- tony$School[1000] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# #which was is the best
# 
# ind <-which.min(mat)
# 
# school$School[ind]
# 
# #try 2000th row..
# 
# 
# temp <- tony$School[1500] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# temp <- tony$School[1500] #later replace 1 with i. 
# mat <- adist(x=temp, y=school$School)
# 
# #which was is the best
# 
# ind <-which.min(mat)
# 
# school$School[ind]
# 
# #I think it's good### now the loop

tony$name <- NA
for(i in 1:nrow(tony)){
  
  temp <- tony$School[i]  
  mat <- adist(x=temp, y=school$School)
  
  ind <-which.min(mat)
  
  tony$name[i] <- school$School[ind]
}

# Compare School and name in tony
tony[,c("School", "name")]

#make new colummn school

school$name <- school$School

#final 

final <- left_join(x=tony,
                   y=school, 
                   by="name")
