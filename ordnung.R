## try to learn the order() function
## home made large but not superlarge data frame and experiment

ordnung1 <- function(){
        df <- data.frame(hospital=c("Ha","HB", "Hosc", "presbyt", "Mount Sinai", "xza", "qwerty","asder", "Karolinska", "ouyt", "zxcvb"), state=c("TX","NY","MI","MA","TX","NY","MI","NY","NY","TX","TX"), outcome=c(12.8,14.5,11,13.8,16.7,11.0,11,14.5,11.1,14.6,12.1), built=c(1959,1999,1996,2003,1933, 1995,2001,1984,1970,1969,1990))
        
        data(mtcars)
        
        outcome_df <- df[,order(df$outcome)]
        
        
        i <- 0
}