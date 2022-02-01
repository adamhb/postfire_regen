library(tidyverse)

options(max.print = 1000)
options(dplyr.print_max = 1e4)

raw <- read_csv('~/cloud/gdrive/fire_project/local_data/field_validation/lineInterceptData/lineInterceptData.csv')

vecFunc <- function(start,end){
  return(seq(start,end,by = 0.1))
}

lid <- raw %>%
  mutate(pft = case_when(
    sp %in% c("ced","yp","wf","pon","sp","c") ~ "c",
    sp %in% c("man","cean","s") ~ "s",
    sp %in% c("o","bo","ilo") ~ "r",
    sp %in% c("g") ~ "g",
    sp %in% c("rock","d") ~ "d"   
  )) %>% mutate_at(.vars = c("start","end"), .funs = function(x){round(x,digits = 1)}) %>%
  mutate(vec = pmap(.l = list(start,end),.f = vecFunc))


getVecs <- function(d, p_start = 0.5, p_end = 50.5){
  position <- seq(p_start,p_end, by = 0.1)
  pfts <- unique(lid$pft)
  e <- matrix(nrow = length(position),ncol = length(pfts),dimnames = list(1:length(position),pfts))
  for(p in pfts){
    tmp <- d %>% filter(is.na(understory), pft == p) %>% 
      pull(vec) %>% unlist()
    tmp2 <- position %in% tmp
    e[,p] <- tmp2
  }
  output <- as_tibble(e) %>% mutate(dom = case_when(
    c == T ~ "c",
    c == F & r == T ~ "r",
    c == F & r == F & s == T ~ "s",
    c == F & r == F & s == F & d == T ~ "d",
    c == F & r == F & s == F & d == F & g == T ~ "g",
    TRUE ~ "b"
  )) %>% group_by(dom) %>%
    summarise(n = length(dom))
  print(( (p_end-p_start) * 10 ) + 1)
  return(output)
}

plots <- unique(lid$plot)
transects <- unique(lid$transect)
segments <- unique(lid$segment)

pct_cover <- tibble()
for(pl in plots){
  for(tr in transects){
    for(seg in segments){
      df <- lid %>% filter(plot == pl, transect == tr, segment == seg)
      p_start.x <- df$start[1]
      if(seg == 1){
        p_end.x <- p_start.x + 50
      }else{
        p_end.x <- p_start.x + 40
      }
      tmp <- getVecs(d = df, p_start = p_start.x, p_end = p_end.x) %>%
        mutate(plot = pl, transect = tr, segment = seg)
      pct_cover <- rbind(pct_cover,tmp)
    }
  }
}


names(pct_cover) <- c("pft","n","plotID","transect","segment")

fieldValidationData_LI <- pct_cover %>% select(plotID,transect,segment,pft,n) %>%
  group_by(plotID,pft) %>%
  summarise(pctCover = sum(n)/( (501+401)*4 )) %>%
  ungroup()


write_csv(fieldValidationData_LI ,file = 'data/fieldValidationData_LI.csv') 























pctConCoverPerPlot <- pct_cover %>%
  filter(pft == "c") %>%
  group_by(plotID) %>%
  summarise(conifer_pct_cover = mean(pctCover)) 

write_csv(pctConCoverPerPlot,file = 'data/pct_coverLineIntercept.csv') 




# 
# 
# 
# 
# 
# 
# 
# t <- getVecs(d = dtest)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# getvectorOfPositions1pft <- function(plot.x = "A02", transect.x = "s", segment.x = 1, pft.x = "c", startPoint = 0.5){
#   if(startPoint == 0.5){
#     position <- seq(0.5,50.5, by =0.1)
#   }else{
#     position <- seq(0,50, by = 0.1)
#   }
#   
#   groups <- lid %>% filter(plot == plot.x, pft == pft.x, transect == transect.x, segment == segment.x) %>% 
#     select(start,end)
#   
#   if(nrow(groups) == 0){
#     return(rep(NA,length(position)))
#   }else{
#     
#     pft_positions <- c()
#     for(i in 1:nrow(groups)){
#       if(groups$end[i] < groups$start[i]) next
#       pft_positions_temp <- seq(as.numeric(groups$start[i]),as.numeric(groups$end[i]) ,by = 0.1)
#       pft_positions <- append(pft_positions,pft_positions_temp)
#     } 
#     empty <- rep(NA,length(position))
#     empty[position %in% pft_positions] <- pft.x
#     return(empty) 
#   }
# }
# 
# getvectorOfPositions1pft()
# 
# 
# getAllPFTVectors <- function(df){
#   plots <- unique(df$plot)
#   pfts <- unique(df$pft)
#   transects <- unique(df$transect)
#   segments <- c(1,2)
#   
# output <- tibble()  
#   for(p in plots){
#     for(pf in pfts){
#       for(t in transects){
#         for(seg in segments){
#           temp <- tibble(positions = getvectorOfPositions1pft(plot.x = p, 
#                                                               transect.x = t, 
#                                                               segment.x = seg, 
#                                                               pft.x = pf, 
#                                                               startPoint = 0.5),
#                          plot = p,
#                          transect = t,
#                          segment = seg,
#                          pft = pf)
#           output <- rbind(output,temp)                  
#         }
#       }
#     }
#   }
# return(output)
# }  
# 
# 
# longform <- getAllPFTVectors(df = lid)
# head(longform,100)
# 
# spread(data = longform,key = "pft", value = "positions")
# 
# getvectorOfPositions1pft(plot.x = "A02", transect.x = "s", segment.x = 1, pft.x = "c", startPoint = 0.5)
# 
#   
#   
#   
# 
# con <- c()
# 
# for(i in 1:nrow(test)){
#   con <- append(con,seq(test %>% filter(pft == "c") %>% pull(start)))
# }
# 
# 
# for(i in position_measurement){
#   position <- i
#   
# }
# 
# 
# 
# 
# position <- 1:length(seq(0.5,50.5, by =0.1))
# position_n <- length(position)
# 
# 
# empty <- tibble(position = position,
#                 position_measurement = position_measurement,
#                 con = rep(NA,position_n),
#                 shrub = rep(NA,position_n),
#                 oak = rep(NA,position_n),
#                 gf = rep(NA,position_n))
# 
# 
# fillpft <- function(p,pft,pft_code,t,s){
#   
#  
#   bounds3 <- c()
#   
#   empty <- rep(NA,position_n) 
#   empty[position_measurement %in% bounds3] <- pft_code
#   
#   return(empty)
# }
# 
# pft_codes <- unique(lid$pft)
# 
# x <- tibble(pm = position_measurement)
# for(pf in pft_codes){
#   tmp <- tibble(c = fillpft(p = "A02", t = "s", s = 1, pft_code = pf))
#   x <- cbind(x,tmp)
# }
# 
# 
# empty$con



#pick up here with figuring out a way to identify the overstory pft in situations of overlap using default pft dominance)