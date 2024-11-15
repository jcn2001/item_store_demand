
library(tidyverse)
library(tidymodels) 
library(vroom)
library(forecast)
library(patchwork)

store_train <- vroom("C:/Users/Josh/Documents/stat348/item_store_demand/train.csv")
store_test <- vroom("C:/Users/Josh/Documents/stat348/item_store_demand/test.csv")

storeItem <- store_train %>%
  filter(store==1,item==1)

plot1 <- storeItem %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

plot2 <- storeItem %>%
  pull(sales) %>%
  forecast::ggAcf(.)

plot3 <- storeItem %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)

storeItem2 <- store_train %>%
  filter(store==2,item==11)

plot4 <- storeItem2 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

plot5 <- storeItem2 %>%
  pull(sales) %>%
  forecast::ggAcf(.)

plot6 <- storeItem2 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)

hw27 <- (plot1 + plot2 + plot3) / (plot4 + plot5 + plot6)
ggsave("C:/Users/Josh/Documents/stat348/item_store_demand/hw27.png")
