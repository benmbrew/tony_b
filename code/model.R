setwd("/home/benbrew/Documents/tony_b/")
school <- read.csv("public_schools.csv")
setwd("/home/benbrew/Documents/private/")
tony <- read.csv("merged.csv")

#make new name holder for tony#####

tony$name <- NA

tony$School[1]

#isolate nth school 

temp <- tony$School[1] #later replace 1 with i. 
mat <- adist(x=temp, y=school$school)

#which was is the best

ind <-which.min(mat)

school$school[ind]

#add county to to school in both data frames 

school$School <- paste0(school$district,school$school)

tony$School <-paste0(tony$School, tony$county)

#remove common words withing school

school$School <- gsub("ELEMENTARY", "", school$School)
school$School <- gsub("MIDDLE", "", school$School)
school$School <- gsub("HIGH", "", school$School)
school$School <- gsub("SCHOOL", "", school$School)

tony$School <- gsub("Elementary", "", tony$School)
tony$School <- gsub("Middle", "", tony$School)
tony$School <- gsub("High", "", tony$School)
tony$School <- gsub("School", "", tony$School)

#make school$School column lower case

school$School <- tolower(school$School)
table(school$School)

#try first row again

temp <- tony$School[1] #later replace 1 with i. 
mat <- adist(x=temp, y=school$School)

temp <- tony$School[1] #later replace 1 with i. 
mat <- adist(x=temp, y=school$School)

#which was is the best

ind <-which.min(mat)

school$School[ind]

#try second row..

temp <- tony$School[2] #later replace 1 with i. 
mat <- adist(x=temp, y=school$School)

temp <- tony$School[2] #later replace 1 with i. 
mat <- adist(x=temp, y=school$School)

#which was is the best

ind <-which.min(mat)

school$School[ind]

#try thirs row..

temp <- tony$School[3] #later replace 1 with i. 
mat <- adist(x=temp, y=school$School)

temp <- tony$School[3] #later replace 1 with i. 
mat <- adist(x=temp, y=school$School)

#which was is the best

ind <-which.min(mat)

school$School[ind]

