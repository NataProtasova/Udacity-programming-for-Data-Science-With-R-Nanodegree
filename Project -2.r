
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

# Your solution code goes here

month <- function(city) {
  startDate <-
    sapply(strsplit(as.character(city$Start.Time), " "), "[", 1)
#extract month from the datatime
  month <- substr(x = startDate, 6, 7)
}

# Call the month function to New York
monthNy <- month(ny)
ny['month'] <- monthNy

# replacing numeric month value '01' with a string value 'January' for all 6 months
numericMonth <- c('01', '02', '03', '04', '05', '06')
stringMonth <- c('January', 'Febraury', 'March', 'April', 'May', 'June')

ny$month[ny$month %in% numericMonth] <-
  stringMonth[match(ny$month, numericMonth, nomatch = 0)]

# find the unique values of months
uniqv_ny <- unique(ny$month)

# find the mode of the month
commonMonth <- function(dataColumn,uniqv) {
  uniqv[which.max(tabulate(match(dataColumn, uniqv)))]   
}

# call the most common month function to New York
commonMonthNy <- commonMonth(ny$month,uniqv_ny)
cat('The most common month for New York is:', commonMonthNy,'\n')

#Visualization
library(ggplot2)
#sort the months
ny$month <- factor(ny$month,levels = c('January', 'Febraury', 'March', 'April', 'May', 'June'))
  
# Display the plot of month count with ggplot
ggplot(aes(ny$month),data=ny)+
geom_bar(color='black',fill='blue')+
labs(x='Months',y='Counts of usage for each month',title='Usage of Each Month in New York')

#Table Summary
summary(ny$month)


ny = read.csv('new_york_city.csv')
#adding new raw for the city name
ny$City <- 'New York City'
chi$City <- 'Chicago'


#Creating a function for join tables
join <- function(c1, c2) {
  return(rbind(c1, c2))
}

city <- join(chi,ny)     
# Count of Gender (Male and Female)
total = sort(table(city$Gender))



# Visualizing data with ggplot
ggplot(aes(x = Gender, fill = City), data = city) +
    geom_bar(position = 'dodge', colour="black") +
    ggtitle('Popularity thought Genders in New York and Chicago') +
    scale_x_discrete(labels = c('Prefer Not to mentioned', 'Female', 'Male')) +
    labs(y = 'Number of Riders', x = 'Gender') +
    scale_fill_manual("legend", values = c("Chicago" = "blue", "New York City" = "white"))


#Summary table
print(total)

# Adding null columns of 'Gender' and 'Birth.Year' for Washington dataset
wash$Gender <- NA
wash$Birth.Year <-NA

# Adding a new column 'City' to each dataset to retain info about city after concatenation
ny$City <- 'New York City'
wash$City <- 'Washington'
chi$City <- 'Chicago'

#Creating a function for joining tables
join <- function(c1, c2) {
  return(rbind(c1, c2))
}

# Join all 3 datasets together
city <- join(ny,wash)     
city <- join(city,chi)    

# Total users in city
totalCity = sort(table(city$City))


# Visualizing
# Display the plot of Average travel time per city with ggplot
ggplot(aes(x = City, y = Trip.Duration), data = city) +
    geom_bar(position = 'dodge', stat = "summary", fun.y = "mean", fill = "blue", colour="black") + 
    ggtitle('The average travel time for users in different cities') +
    labs(y = 'Average travel time', x = 'City') 





#Summary
my.summary <- with(city, aggregate(list(Trip.Duration), by = list(City), 
FUN = function(x) { mon.mean = mean(x, na.rm = TRUE) } ))

#Display summary table
colnames(my.summary) <- c('City', 'Average Trip Duration')
my.summary

system('python -m nbconvert Explore_bikeshare_data.ipynb')
