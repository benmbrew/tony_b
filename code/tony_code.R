setwd("/home/benbrew/Documents/private")

# Read in # what does skip and stingAsFactors do?
race <- read.csv("RaceGender.csv", skip = 6, stringsAsFactors = FALSE)
pop <- read.csv("SchoolPop.csv", skip = 5, stringsAsFactors = FALSE)
lunch <- read.csv("FreeLunch.csv", skip = 5, stringsAsFactors = FALSE)

################################
# Clean up pop

# First, check out the structure of each variable
str(pop)

# Total Membership is our column of interest, but it has problems
# (commas in the numbers, and it's not numeric)
# So, we'll make a better column:

# remove commas, #here you're taking out commas and replacing them with nothing right?
pop$totmem <- gsub(",", "", pop$TOTAL.MEMBERSHIP)

# make numeric
pop$totmem <- as.numeric(pop$totmem)

# Let's do the same (remove commas and make numeric)
# for all the other grade columns
for (i in c("PK", "K", paste0("X", 1:12))){
  pop[,i ] <- 
    as.numeric(gsub(",", "", pop[, i]))
}


# Check out the structure to see if it worked
str(pop)

# Go through each school, and see if they have more 
# prek, elem, mid, or high school students
pop$type <- NA
for (i in 1:nrow(pop)){
  pk <- sum(pop[i, "PK"], na.rm = TRUE)
  elem <- sum(as.numeric(pop[i,c("K", "X1", "X2", "X3", "X4", "X5")]), na.rm = TRUE)
  mid <- sum(as.numeric(pop[i, c("X6", "X7", "X8")]), na.rm = TRUE)
  high <- sum(as.numeric(pop[i, c("X9", "X10", "X11", "X12")]), na.rm = TRUE)
  
  types <- c(pk, elem, mid, high)
  type_names <- c("pk", "elem", "mid", "high")
  pop$type[i] <-  type_names[which.max(types)][1] #I don't really understand what you're doing here
}

# Remove unecessary columns
pop <- pop[,c("DISTRICT.NAME", "SCHOOL.NAME", "totmem", "type")]

# Rename some columns
names(pop) <- c("district", "school", "totmem", "type")

# Check out our cleaned up dataframe
head(pop)


################################
# Clean up lunch

# remove rows with "District record" in it
lunch <- lunch[which(lunch$School.Name != "DISTRICT RECORD"),]

# Fix the terrible column names
names(lunch)
names(lunch) <- c("district_number", "district", "school_number", "school",
                  "totmem", "free", "reduced", "provision2", "direct_cert")
head(lunch)

# Check out the structure of each column
str(lunch)

# Loop through a few of them to remove commas and make numeric
for (i in c("totmem", "free", "reduced", "provision2", "direct_cert")){
  
  # get rid of commas and make numeric each column mentioned above
    temp <- as.numeric(gsub(",", "", lunch[,i])) 
    
    # in these same columns, fill all the NAs with 0
    temp[which(is.na(temp))] <- 0
    
    # Once the column is fixed, put it back in its place
    lunch[,i] <- temp 
}
# I don't understand why we need a temp column
head(lunch)

# Make a free/reduced total
lunch$free_reduced <- lunch$free + lunch$reduced

# Make a free/reduced percentage
lunch$per_fr <- lunch$free_reduced / lunch$totmem * 100

# Check out your fine cleaning work
head(lunch)


################################
# Clean up race
head(race)

#rename columns 

names(race) <- c("district1", "district", "school1", "school", "grade", "white", "black",
                 "hispanic","asian", "hawian", "native", "multi", "female", "male", "total" )

#remove commas
for (i in c("grade", "white", "black", "hispanic", "asian", "hawian", "native", "multi", 
            "female", "male", "total")){
  race[,i ] <- 
    as.numeric(gsub(",", "", race[,i]))
}

#structure of each column 
str(race)

#remove uneeded rows

race <- race[,c("district", "school", "grade", "white", "black", "hispanic", "asian", "hawian",
                "native", "multi", "female", "male", "total")]

#Not sure what to do with this. do we just want totals for each school, in that case
#we could use the school total row at the end of each school, but none of them have a 
#unique name. And honestly, I suck at writing loops. Can you help me with this?
library(dplyr)
pop %>%
group_by(district) %>% 
  summarise(public_schools = n())
