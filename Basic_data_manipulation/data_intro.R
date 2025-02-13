
###############~Reading in data~###############
? read.delim #brings up help menu for that command

colony_size <- read.delim('~/Documents/munich/R_Workshop/Basic_data_manipulation/colony_size.txt')

class(colony_size) #read.delim gives back a dataframe

head(colony_size) #take a look at it

#the basic layout of dataframes is df[rows, columns]
head(colony_size[, 1]) # first column of df can be accessed with coordinates,

head(colony_size[, 'Area']) #column names,

head(colony_size$Area) # or the the dollar sign,

#let's check that they're the same
table(colony_size$Area == colony_size[, 'Area'])

table(colony_size$Area == colony_size[, 1])

table(colony_size[, 'Area'] == colony_size[, 1])

colony_size[1, ] # rows can be accessed using coordinates

colony_size[1, 1] #specific cells of dataframes can be accessed using coordinates


###############~Subsetting~###############

#sometimes you want to get a subset of your dataset. 
#let's see which different variables we have and then we'll subset for one in particular

sort(table(colony_size$source)) #table shows counts of things in the source column, sort...sorts them

#we can subset with coordinates
colony_size[colony_size$source == 'deltaefpuup_deac24empty33',]

#here we combined a logical array with the coordinate system
head(colony_size$source == 'deltaefpuup_deac24empty33') #returns in which rows this statement is true

check_coor <- colony_size[colony_size$source == 'deltaefpuup_deac24empty33',]  #then we take just those rows from the original dataframe
table(check_coor$source)

#we can also use subset, a base r function -- it's more human readable
? subset 

check_subset <- subset(colony_size, source == 'deltaefpuup_deac24empty33')
table(check_subset$source)
table(check_subset == check_coor) #they're the same thing cool

#Let R do the dirty work!!
#taking the 3 samples with the most colonies
sort(table(colony_size$source)) #we can manually make a list of these
check_3_hardcoded <- subset(colony_size, source %in% c('deltaefpuup_ppu24earp33',  'deltaefpuup_heau24earp33', 'deltaefpuup_eco24earp33')) 
table(check_3_hardcoded$source)

#it's better to be lazy and not have to manually set values, we can chain functions
tail(sort(table(colony_size$source)), n = 3) #those are the three we want
names(tail(sort(table(colony_size$source)), n = 3)) #get just their names, not their frequency counts
what_we_want <- names(tail(sort(table(colony_size$source)), n = 3)) #write those to a variable
check_3_lazy <- subset(colony_size, source %in% what_we_want) #use that to subset
table(check_3_lazy$source)

table(check_3_hardcoded == check_3_lazy) 

#it's generally better not to hard-code data if it can be avoided
rm(check_3_hardcoded, check_3_lazy, what_we_want)


###############~Modifying dataframes~###############

#we can list the column names of our dataframe
colnames(check_subset)

#we can rename them
colnames(check_subset)[1] #check first column name
colnames(check_subset)[1] <- 'butts'
colnames(check_subset) #sucess

#we can add a column 
check_subset$three <- 3

#we can remove data we don't need anymore
rm(check_coor, check_subset)

#we can use ifelse statements to add conditions
colony_size$max_dimension <- ifelse(colony_size$Height > colony_size$Width, colony_size$Height, colony_size$Width)

colony_size$type <- ifelse(grepl('deltaefpuup_empty24', colony_size$source) == TRUE, 'control', 'experimental')

head(grepl('deltaefpuup_empty24', colony_size$source)) #grepl returns a boolean (true/false) for if it found the search term in each df row
table(colony_size$source, colony_size$type)


###############~Basic analysis with aggregate~###############

#we can check features of each source using aggregate
? aggregate
mean_size <- aggregate(max_dimension ~ source, colony_size, mean)
head(mean_size)
stdev_size <- aggregate(max_dimension ~ source, colony_size, sd)
stdev_size

#wait there's an NA in stdev_size, why is that?
subset(stdev_size, is.na(max_dimension))

#clean up
rm(mean_size, stdev_size)


###############~Saving your dataframe~###############

#we added two new columns to our dataframe that we'll use later on.
#so, let's save the updated file!

? write.table()
write.table(colony_size, '~/Documents/munich/R_Workshop/Basic_data_manipulation/colony_size_updated.txt', 
            row.names = FALSE, sep = '\t')



