

#view histogram of patch-level RRI at year 20
df2 %>%
  group_by(patchID,year) %>%
  filter(timeSinceFire == 20) %>%
  summarise(RRI = mean(RRI)) %>%
  pull(RRI) %>% hist()

