##### 
# LOAD PACKAGES
#####
library(dplyr)

#####
# DEFINE DIRECTORY LOCATIONS
#####
if(Sys.info()['user'] == 'joebrew'){
  public <- '/home/joebrew/Documents/tony_b'
  private <- '/home/joebrew/Documents/private_data/boselli/'
} else if(Sys.info()['user'] == 'benbrew'){
  public <- '/home/benbrew/Documents/tony_b'
  private <- 'home/benbrew/Documents/private'
}

#####
# READ IN TONY AND FDOE DATA
#####
setwd(public)
school <- read.csv("public_schools.csv")
setwd(private)
tony <- read.csv("merged.csv")

#####
# CLEAN UP DATAFRAME
#####

# Make lowercase columnnames in tony
names(tony) <- tolower(names(tony))

# Make lowercase district / county names
school$district <- tolower(school$district)
tony$district <- tolower(tony$county)

# Remove periods from district names
school$district <- gsub('[.]', '', school$district)

# Remove trailing/leading whitespaces from all columns
remove_trail <- function(var){
  x <- gsub("^\\s+|\\s+$", "", as.character(var))
  return(x)
}

for (i in 1:ncol(tony)){
  tony[,i] <- remove_trail(tony[,i])
}

for (i in 1:ncol(school)){
  school[,i] <- remove_trail(school[,i])
}


# Paste together school and district name
# school$school <- paste0(school$school, sep=" ", school$district, collapse=NULL)
# tony$school <-paste0(tony$school, sep=" ",tony$county, collapse=NULL)

#make column lower case
school$school <- tolower(school$school)
tony$school <- tolower(tony$school)

#remove common words withing school
remove_junk <- function(df){
  df$school <- gsub("elementary", "es", df$school)
  df$school <- gsub("middle", "ms", df$school)
  df$school <- gsub("js", "ms", df$school)
  df$school <- gsub("high", "hs", df$school)
  df$school <- gsub("school", "", df$school)
  df$school <- gsub("schl ", "", df$school)
  df$school <- gsub("sch ", "", df$school)
  df$school <- gsub("scho ", "", df$school)
  df$school <- gsub("program", "", df$school)
  df$school <- gsub("prog ", "", df$school)
  df$school <- gsub("[.]", "", df$school)
  df$school <- gsub(",", "", df$school)
  df$school <- gsub("/", "", df$school)
  df$school <- gsub("-", "", df$school)
  df$school <- gsub(" s ", "", df$school)
  df$school <- gsub(" oo ", "", df$school)
  df$school <- gsub("   ", " ", df$school)
  df$school <- gsub("  ", " ", df$school)
  df$school <- remove_trail(df$school)
  
  return(df)
}

school <- remove_junk(school)
tony <- remove_junk(tony)

#####
# LOOP THROUGH EACH NAME OF TONY TO GET BEST MATCH IN SCHOOL
#####



# OLD WAY
#for(i in 1:nrow(tony)){
 # temp <- tony$school[i]  
 # mat <- adist(x=temp, y=school$school)
  #ind <-which.min(mat)
  #tony$name[i] <- school$school[ind]
#}


#make new name holder for tony
tony$name <- NA
# make match score indicator for tony
tony$match_score <- NA
#make new colummn school
school$name <- school$school

# NEW WAY
for(i in 1:nrow(tony)){
  
  # Define which school we're working with
  temp <- tony$school[i]  
  
  # Define which county we're looking in
  county <- tony$district[i]
  
  # Define vector of possible matches (in that county)
  posibs <- school$school[which(school$district == county)]
  
  # Restrict possible matches to only those with same first letter
  #posibs <- posibs[which(substr(posibs, 1, 1) == substr(temp, 1, 1))] 
  
  # Make matrix of match scores
  mat <- adist(x=temp, y=posibs)
  
  # Reward any school with 2 points for matching first 3 letters
  mat <- ifelse(substr(posibs,1,3) == substr(temp, 1,3), mat - 2, mat)
    
  # Which of the posibs is the best match?
  ind <-which.min(mat)[1]
  
  # Set a threshold - if the best match (min(mat)) is greater than 10, 
  # we're considering this "unmatchabel"

  # Assign that best match back to tony
  tony$name[i] <- posibs[ind]
  
  # Assign match score to tony as well
  tony$match_score[i] <- min(mat, na.rm = TRUE)
}

# Explore match score
hist(tony$match_score) # most are great!

# Look at different match scores to decide what the cutoff should be
# the lower the better match
tony[which(tony$match_score <= 1),c("school", "name")] # perfect
tony[which(tony$match_score == 2),c("school", "name")] # pretty close to perfect
tony[which(tony$match_score == 3),c("school", "name")] # good
tony[which(tony$match_score == 4),c("school", "name")] # decent
tony[which(tony$match_score == 5),c("school", "name")] # mediocre

tony[which(tony$match_score >=10),c("school", "name")] # terrible



#final 

final <- left_join(x=tony,
                   y=school, 
                   by="name")
