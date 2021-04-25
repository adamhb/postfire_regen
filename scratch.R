






old_names <- names(df) %>% tibble() 
names(old_names) <- "old_name"

old_names %>%
  mutate(col_names = case_when(
    str_detect(string = old_name, pattern = "SAP") == TRUE ~ na.omit(unlist(str_extract_all(string = old_name,years))),
    TRUE ~ old_name
  ))


write_csv(tibble(names(rast)),path = "tmp/names.csv")

old_name <- ("X1_1_LC08_041034_20130630_SAP")



str_extract_all(string = "X1_1_LC08_041034_20130630_SAP",years) %>%
  unlist() %>%
  na.omit()


str_subset(string = "X1_1_LC08_041034_20130630_SAP", pattern = years)
as.character(na.omit(str_extract(string = "X1_1_LC08_041034_20130630_SAP", pattern = years)))
str_extract_all(string = "asdf", pattern = "SAP") 18:21

\_\d{4}
str_extract("X1_1_LC08_041034_20130630_SAP","[:digit:]")


na.omit(str_extract(string = "X1_1_LC08_041034_20120630_SAP", pattern = c("2012","2013")))
