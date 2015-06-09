# pollutantmean.R - Part 1 of Assignment 1 Coursera R Programming
#
# A function that calculates the mena of a pollutant (sulfate or nitrate)
# across a specified list of monitors.
#
# Given a vector of monitor ID numbers, return the mean of the pollutant
# across all of the monitors, ignoring missing values coded as NA.
#
# The specdata directory contains 322 CSV files containg pollutant data:
#
# |------------+---------+---------+----|
# | Date       | sulfate | nitrate | ID |
# |------------+---------+---------+----|
# | 01/01/2004 | NA      | NA      | 12 |
# | 02/01/2004 | NA      | NA      | 12 |
# | 28/03/2004 | 0.0353  | 0.0598  | 12 |
# | 08/04/2004 | NA      | NA      | 12 |
# | 09/04/2004 | 1.33    | 0.95    | 12 |
# | ...        |         |         |    |
# |------------+---------+---------+----|
#
#
pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    values <- c()                                           # empty vector to accumulate result
    for (i in 1:length(id)) {                               # for each input ID..
        filename <- file.path(directory,paste0(sprintf("%03d",id[i]),".csv"))
        all_data <- read.csv(file=filename,colClasses=c("character","numeric","numeric","numeric"))
        #print(paste(filename,"has",nrow(all_data),"total rows"))
        id_data <- all_data #[which(all_data$ID==i),]         # ..select ID that match
        #print(paste(filename,"has",nrow(id_data),"ID rows"))
        all_pollutant <- id_data[[pollutant]]               # ..and only pollutant values
        #print(paste(filename,"has",nrow(all_pollutant),"pollutant rows"))
        nonmiss <- all_pollutant[!is.na(all_pollutant)]     # ..which are not missing
        #print(paste(filename,"has",length(nonmiss),"non missing values"))
        ## accumulate this ID results with all other results
        values <- c(nonmiss,values)
        
    }
    ## return the mean of the accumulated values
    mean(values,na.rm=TRUE)
    
}

