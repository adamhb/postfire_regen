library(tidyverse)

raw <- read_csv('~/cloud/gdrive/fire_project/local_data/field_validation/PointInterceptData/pointInterceptData.csv') 

#Clean the point intercept data
df <- raw %>%
  mutate_at(.vars = "pft",.funs = tolower) %>% 
  mutate_at(
    vars(starts_with("pft")),
    funs(case_when(
      . == "br" ~ "r",
      . == "p" ~ "b",
      TRUE ~ .
    ))
  ) %>% rename(pointID = photoID)


########################
#determine sample size##
########################
#from each photo

getPctCover <- function(df,pft = "c"){
  n <- as.numeric(table(df$pft))
  n_total <- sum(n)
  pct_cover <- n/n_total
  names(pct_cover) <- names(table(df$pft))
  return(as.numeric(pct_cover[pft]))
}

getPctCover(df,c("c","d"))


getSDofSampleMeans <- function(df, n_draws, sample_size){
  
  estimates <- c()
  
  j <- 0
  
  for(i in 1:n_draws){
    j <- j + 1
    rows <- floor(runif(n = sample_size, min = 1, max = nrow(df)))
    sample <- df[rows,]
    estimates[j] <- getPctCover(sample,"s")
  }
  
  return(sd(estimates))
}


getSampleSaturation <- function(df, min_ssize, max_ssize, n_ssizes, n_draws) {
  ssizes <- as.integer(seq(min_ssize,min(max_ssize,nrow(df)),length.out = n_ssizes))
  sds <- c()
  ssize_record <- c()
  j <- 0
  for(ssize in ssizes){
    j <- j + 1
    ssize_record[j] <- ssize
    sds[j] <- getSDofSampleMeans(df, n_draws, ssize)
  }
  return(tibble(ssize = ssize_record, sd = sds))
}


sd_vs_sample_size <- getSampleSaturation(raw,30,400,10,500)


#The image below shows that the estimate of percent cover
#continues to improve all the way to a sample size of 300.
sd_vs_sample_size %>%
  ggplot(aes(ssize,sd)) +
  geom_line() +
  theme_minimal()


