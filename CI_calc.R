# Analyze Confidence interval

library(xlsx)
Pn_dat <-
  read.xlsx("~/Dropbox/Publish/Done!/2015Cucumber_PhysiolPlant/data.xlsx", sheetName = "RawPn")

Pn_0 <-
  Pn_dat %>%
  select(GL = Growth.light,
         ML = Measuring.light,
         Trtm, ave) %>%
  filter(GL %in% c("W", "WFr"),
         ML %in% c("BR_LED", "sunlight"),
         Trtm == "LowLight.Ca40.Pn") %>%
  select(-Trtm)

Pn_0 %>%
  arrange(ML, GL) %>%
  filter(., ML == "BR_LED") %>%
  {t.test(.[1:3, 3], .[4:6, 3])} %>%
  print

Pn_0 %>%
  arrange(ML, GL) %>%
  filter(., ML != "BR_LED") %>%
  {t.test(.[1:3, 3], .[4:6, 3])} %>%
  print
