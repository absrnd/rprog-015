corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    all <- complete(directory)   # get nrows complete for all datasets
    matching <- all[which(all$nobs>threshold),"id"] # ids that match threshold

    result = vector('numeric')
    for(i in matching) {
        filename <- file.path(directory,paste0(sprintf("%03d",i),".csv"))
        data <- read.csv(file=filename)
        completed <- data[complete.cases(data),]
        result <- c(result,cor(x=completed$sulfate,y=completed$nitrate))
    } #for
    result
}
