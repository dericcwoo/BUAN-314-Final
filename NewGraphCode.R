
####################
## NEW GRAPH CODE ##
####################

orders <- read.csv('/Users/dwoo/Downloads/Fall 2024/BUAN-314/Project Files/orders.csv')
items <- read.csv('/Users/dwoo/Downloads/Fall 2024/BUAN-314/Project Files/order_items.csv')
stores <- read.csv('/Users/dwoo/Downloads/Fall 2024/BUAN-314/Project Files/stores.csv')
customers <- read.csv('/Users/dwoo/Downloads/Fall 2024/BUAN-314/Project Files/customers.csv')
summary(orders)
class(orders$required_date)

library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggthemes)

##############
## QUERY 11 ##
##############

q11 <- full_join(items, orders, by = 'order_id') %>%
  group_by(order_date) %>%
  mutate(product_count = sum(quantity)) %>%
  mutate(month = month(order_date), year = year(order_date))

ggplot(q11, aes(x = month, fill = factor(store_id))) +
  geom_bar() +
  facet_wrap(~store_id, ncol = 1) +
  theme_light() + 
  scale_fill_discrete(name = "Store", labels = c("Santa Cruz Bikes (CA)", "Baldwin Bikes (NY)", "Rowlett Bikes (TX)")) +
  xlab('Month') +
  ylab('Count of Bikes Sold') + 
  xlim(0, 13) +
  ggtitle('Count of Bikes Sold Over Time by Store')

##############
## QUERY 14 ##
##############

q14 <- full_join(items, orders, by = 'order_id')

q14 %>%
  select(list_price, quantity, staff_id) %>%
  group_by(staff_id) %>% 
  mutate(total = quantity * list_price) %>%
  summarize(total_rev = sum(total)) %>%
  arrange(desc(total_rev))

q14 <- q14 %>%
  select(list_price, quantity, staff_id) %>%
  group_by(staff_id) %>% 
  mutate(total = quantity * list_price) %>%
  summarize(total_rev = sum(total)) %>%
  arrange(desc(total_rev)) 

q14 %>%
  mutate(percentage = total_rev / sum(total_rev) * 100, label = paste0(round(percentage, 1), "%")) %>%
  ggplot(aes(x = "", y = total_rev, fill = factor(staff_id))) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  scale_fill_discrete(name = "Staff", label = c("Mireya Copeland", "Genna Serrano", "Marcelene Boyer",
                                                "Venita Daniel", "Kali Vargas", "Layla Terrell"))

