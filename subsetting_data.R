# An R script to prune / break a large file into more manageable chunks 
# written by kenan arica

# do not run this code unless you are kenan or you understand what it's gonna do. otherwise you're gonna get 325mb of files you probably don't need on your PC
library(plyr)
library(data.table)
# foreword: the awk script I used to re-generate a new file using only our 6 columns is in the directory as subset_cols_awk.

# data <- read.table(file="taxi_subsetted.tsv", sep = "\t", header = TRUE, row.names =NULL, quote = "", check.names = FALSE)
data_uncut <- read.table(file="taxi_trimmed_tsv.tsv", sep = "\t", header = TRUE, row.names =NULL, quote = "", check.names = FALSE)


colnames(data_uncut) <- c("seconds", "miles", "pickup", "dropoff", "company", "timestamp")
#colnames(data) <- c("timestamp", "seconds", "miles", "pickup", "dropoff", "company")


data <- subset(data, data$seconds > 60)
data <- subset(data, data$seconds < 18000)
data <- subset(data, data$miles < 100)
data <- subset(data, data$miles > .5)
data <- na.omit(data)

# make our table to reference taxi companies to numbers
taxi_companies <- as.data.frame(unique(data$company))
taxi_companies$company_num <- seq(1, 55)
colnames(taxi_companies) <- c("company", "company_number")
write.csv(taxi_companies, "taxi_companies.csv")

# switch from using taxi company names to taxi company numbers
data$company_num <- match(data$company, taxi_companies$company)
data$company <- NULL

# since we don't care about the minute on the hour, or the fact that it's in 2019. we sub out any pattern in our timestamp and replace it. 
data$start_timestamp <- NULL

d <- substr(data_uncut$timestamp, 2, 6)
d_full_date <- paste("2019/", d, sep = "")
all_weekdays <- ymd(d_full_date)
all_weekdays_numeric <- wday(all_weekdays)
data$weekday <- all_weekdays_numeric

data$hour <- substr(data$timestamp, 8, 12)
data$date <- substr(data$timestamp, 2, 6)
data$month <- strtoi(substr(data$date, 0, 2), base = 10L)
data$day <- strtoi(substr(data$date, 4, 5))

data$minutes <- round(data$seconds / 60)

data$seconds <- NULL
data$timestamp <- NULL

# make sure not to include rownames when we save the table because they take a ton of space
write.table(data, sep = "\t", file = "taxi_trimmed_tsv.tsv", row.names = FALSE)

check_if_it_works <- read.table("taxi_trimmed_tsv.tsv", sep = "\t", header = TRUE)

number_of_chunks <- 8
num_rows = 0
for(x in 1:number_of_chunks) { 
  step <- floor(nrow(data) / number_of_chunks)
  print(step)
  lowerbound = ((x - 1) * step) + 1
  upperbound = ((x * step) + 1)
  print(lowerbound)
  print(upperbound)
  current_chunk <- data[lowerbound:upperbound, ]
  num_rows <- num_rows + nrow(current_chunk)
  write.csv(current_chunk, file = paste("datachunks/", "chunk", x, ".csv", sep = ""), row.names = FALSE, quote = FALSE)
}

community <- read.csv("CommAreas.csv")
communities <- as.data.frame(community$COMMUNITY)
communities$code <- community$AREA_NUMBE
colnames(communities) <- c("name", "code")
communities <- communities[order(communities$name), ]
write.csv(communities, "communities.csv", row.names = FALSE, quote = FALSE)
# write the number of rides
