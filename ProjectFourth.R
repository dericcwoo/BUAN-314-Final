# loading all necessary libraries
library(tidyverse)
library(sqldf)
# install.packages("treemapify")
library(treemapify)

# importing raw dataframe 'orders'
orders_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/orders.csv')
View(orders_df)

# check for Nulls
summary(orders_df)
# no Nulls

#############
## QUERY 1 ##
#############

query_1 <- "SELECT COUNT(*) FROM orders_df WHERE shipped_date = 'NULL'"
sqldf(query_1)
# 170 observations of 'NULL'

#############
## QUERY 2 ##
#############

query_2 <- "SELECT COUNT(*) FROM orders_df WHERE shipped_date = 'NULL' AND order_status = 4"
sqldf(query_2)
# however, those ARE supposed to be 'NULL' because the bikes have not yet been shipped,
# so they of course don't have a shipped date

# check classes
class(orders_df$order_id) # numeric
class(orders_df$customer_id) # numeric
class(orders_df$order_status) # numeric
class(orders_df$order_date) # Date
class(orders_df$required_date) # Date
class(orders_df$shipped_date) # character (because some observations are 'NULL')
class(orders_df$store_id) # numeric
class(orders_df$staff_id) # numeric

#####################
## VISUALIZATION 1 ##
#####################
# histograms for each store
ggplot(orders_df, aes(x=order_date, color=factor(store_id))) + geom_histogram(bins=36)

#############
## QUERY 3 ##
#############

# why is that one month count of orders so high? maybe there is one day with a huge amount of orders
query_3 <- "SELECT order_date, COUNT(order_id) AS order_count 
FROM orders_df 
GROUP BY order_date 
ORDER BY COUNT(order_id) DESC 
LIMIT 5"
sqldf(query_3)
# no the highest amount of orders on one day is 9

#####################
## VISUALIZATION 2 ##
#####################
# when looked at quarterly, that month doesn't appear so drastic
ggplot(orders_df, aes(x=order_date, color=factor(store_id))) + geom_histogram(bins=12)

#############
## QUERY 4 ##
#############

# how many staff members are there, by asking which distinct staff_id exist
query_4 <- "SELECT DISTINCT(staff_id) 
FROM orders_df"
sqldf(query_4)

#############
## QUERY 5 ##
#############

# which staff member is the most successful at selling bikes?
query_5 <- "SELECT staff_id, COUNT(order_id) AS order_count 
FROM orders_df 
GROUP BY staff_id 
ORDER BY COUNT(order_id) DESC 
LIMIT 3"
sqldf(query_5)
# but who is that? we'll answer that in query 6

# importing raw dataframe 'staffs'
staffs_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/staffs.csv')
View(staffs_df)

#############
## QUERY 6 ##
#############

# the joining query
query_6 <- "SELECT first_name, last_name, COUNT(order_id) AS order_count 
FROM orders_df 
LEFT JOIN staffs_df 
USING (staff_id) 
GROUP BY staff_id 
ORDER BY COUNT(order_id) DESC 
LIMIT 3"
sqldf(query_6)

# importing raw dataframe 'products'
products_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/products.csv')
View(products_df)

# importing raw dataframe 'categories'
categories_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/categories.csv')
View(categories_df)

#############
## QUERY 7 ##
#############

# importing raw dataframe 'order_items'
order_items_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/order_items.csv')
View(order_items_df)

# most popular category
query_7 <- "SELECT category_name, COUNT(*) 
FROM order_items_df 
LEFT JOIN products_df USING (product_id)
LEFT JOIN categories_df USING (category_id)
GROUP BY category_id 
ORDER BY COUNT(*) DESC 
LIMIT 10"
sqldf(query_7)

#############
## QUERY 8 ##
#############

# the 5 most popular bikes over time
query_8 <- "SELECT product_id, order_date 
FROM order_items_df 
LEFT JOIN orders_df USING (order_id) 
WHERE product_id IN (
SELECT product_id 
FROM order_items_df 
GROUP BY product_id 
ORDER BY COUNT(*) DESC 
LIMIT 5) 
ORDER BY order_date"
query_8 <- sqldf(query_8)
query_8 <- as.data.frame(query_8)
query_8 <- query_8 %>% 
  mutate(year = year(order_date), month = month(order_date)) %>%
  group_by(product_id, year, month) %>%
  summarise(order_count = n(), .groups = 'drop')

#####################
## VISUALIZATION 3 ##
#####################
ggplot(query_8, aes(x=month, y=order_count, color=factor(product_id))) + 
  geom_line() + 
  facet_wrap(~year) + 
  labs(x = "Month", y = "Order Count", color = "Product ID") + 
  theme(axis.text.x = element_blank())

# importing raw dataframe 'stores'
stores_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/stores.csv')
View(stores_df)

#############
## QUERY 9 ##
#############

# store sales over time
query_9 <- "SELECT order_id, order_date, store_id, SUM(list_price) AS order_price
FROM order_items_df 
LEFT JOIN orders_df USING (order_id)
GROUP BY order_id"
query_9 <- sqldf(query_9)
query_9 <- as.data.frame(query_9)
query_9 <- query_9 %>% 
  mutate(year = year(order_date), month = month(order_date)) %>%
  group_by(store_id, year, month) %>%
  summarise(order_price = n(), .groups = 'drop')

#####################
## VISUALIZATION 4 ##
#####################
ggplot(query_9, aes(x=month, y=order_price, color=factor(store_id))) + 
  geom_line() + 
  facet_wrap(~year) + 
  labs(x = "Month", y = "Order Price", color = "Store ID") + 
  theme(axis.text.x = element_blank())

##############
## QUERY 10 ##
##############

## Most loyal customers ##
query_10 <- "SELECT DISTINCT customer_id, COUNT(customer_id) AS customer_id_count
        FROM orders_df
        GROUP BY customer_id
        ORDER BY customer_id_count DESC
        LIMIT 39"
sqldf(query_10)

# There are 39 total customers who have made 3 orders in total from the bike company.
# 3 orders is the highest amount of orders made by a single customer. 

##############
## QUERY 11 ##
##############

## Count of products sold over time by store ##
# Join items and orders datasets
query_11 <- full_join(order_items_df, orders_df, by = 'order_id') %>%
  group_by(order_date) %>%
  mutate(product_count = sum(quantity)) %>%
  mutate(month = month(order_date), year = year(order_date))

#####################
## VISUALIZATION 5 ##
#####################
ggplot(query_11, aes(x = month, fill = factor(store_id))) +
  geom_bar() +
  facet_wrap(~store_id, ncol = 1) +
  theme_light() + 
  scale_fill_discrete(name = "Store", labels = c("Santa Cruz Bikes (CA)", "Baldwin Bikes (NY)", "Rowlett Bikes (TX)")) +
  xlab('Month') +
  ylab('Count of Bikes Sold') + 
  xlim(0, 13) +
  ggtitle('Count of Bikes Sold Over Time by Store')

#####################
## VISUALIZATION 6 ##
#####################
# Treemap of top 10 cities by customer count ##
# importing raw dataframe 'customers'
customers_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/customers.csv')
View(customers_df)

# Summarize customer distribution by city
city_distribution <- customers_df %>% 
  group_by(city) %>% 
  summarise(CustomerCount = n()) %>% 
  arrange(desc(CustomerCount))

top_cities <- city_distribution %>% slice_max(CustomerCount, n = 10)
city_treemap <- ggplot(top_cities, aes(area = CustomerCount, fill = city, label = paste(city, CustomerCount, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(fontface = "bold", colour = "white", place = "centre", grow = TRUE) +
  labs(title = "Top 10 Cities by Customer Count") +
  theme_minimal()
print(city_treemap)
# Mount Vernon is number 1, with 20 customers

#####################
## VISUALIZATION 7 ##
#####################
## Customer distribution by state ##
state_distribution <- customers_df %>% 
  group_by(state) %>% 
  summarise(CustomerCount = n()) %>% 
  arrange(desc(CustomerCount))

state_distribution <- state_distribution %>% 
  mutate(Percentage = round((CustomerCount / sum(CustomerCount)) * 100, 1))

state_pie <- ggplot(state_distribution, aes(x = "", y = CustomerCount, fill = state)) +
  geom_bar(stat = "identity", width = 1, show.legend = TRUE) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Customer Distribution by State", x = NULL, y = NULL) +
  theme_void() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))
print(state_pie)

##############
## QUERY 12 ##
##############

## Product that brings in the most amount of revenue ##
query_12 <- full_join(order_items_df, orders_df, by = 'order_id')

query_12 %>%
  select(product_id, list_price, quantity) %>%
  group_by(product_id) %>%
  mutate(total = quantity * list_price) %>%
  summarize(total_rev = sum(total)) %>%
  arrange(desc(total_rev))

# Product number 7, also known as the 'Trek Slash 8 27.5 - 2016 has made the most money.

##############
## QUERY 13 ##
##############

## Total Revenue ##
query_13 <- full_join(order_items_df, orders_df, by = 'order_id')

query_13 %>%
  select(list_price, quantity) %>%
  mutate(total = quantity * list_price) %>%
  summarize(sum(total))

# The total amount of money made is $8,578,989

##############
## QUERY 14 ##
##############

## Employee that brings in the most amount of money ##
query_14 <- full_join(order_items_df, orders_df, by = 'order_id')

query_14 %>%
  select(list_price, quantity, staff_id) %>%
  group_by(staff_id) %>% 
  mutate(total = quantity * list_price) %>%
  summarize(total_rev = sum(total)) %>%
  arrange(desc(total_rev))

# Employee number 6 has the most amount of sales, known as Marcelene Boyer

query_14 <- query_14 %>%
  select(list_price, quantity, staff_id) %>%
  group_by(staff_id) %>% 
  mutate(total = quantity * list_price) %>%
  summarize(total_rev = sum(total)) %>%
  arrange(desc(total_rev)) 

#####################
## VISUALIZATION 8 ##
#####################
query_14 %>%
  mutate(percentage = total_rev / sum(total_rev) * 100, label = paste0(round(percentage, 1), "%")) %>%
  ggplot(aes(x = "", y = total_rev, fill = factor(staff_id))) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  scale_fill_discrete(name = "Staff", label = c("Mireya Copeland", "Genna Serrano", "Marcelene Boyer",
                                                "Venita Daniel", "Kali Vargas", "Layla Terrell"))
##############
## QUERY 15 ##
##############

# popular bikes by store
query_15  <- "WITH ranked_products AS (
  SELECT store_name, product_name, COUNT(*) AS product_count,
         ROW_NUMBER() OVER (PARTITION BY store_name ORDER BY COUNT(*) DESC) AS rank
  FROM order_items_df
  LEFT JOIN orders_df USING (order_id)
  LEFT JOIN stores_df USING (store_id)
  LEFT JOIN products_df USING (product_id)
  GROUP BY store_name, product_name
)
SELECT store_name, product_name, product_count
FROM ranked_products
WHERE rank = 1
ORDER BY store_name;"
sqldf(query_15)

orders_df <- orders_df %>% 
  filter(order_status == 4) %>% 
  mutate(shipped_date = as.Date(shipped_date, format = "%Y-%m-%d"), 
                                order_to_ship_days = difftime(as.Date(shipped_date), order_date, units="days"))

#####################
## VISUALIZATION 9 ##
#####################
ggplot(orders_df, aes(x=order_date, y=order_to_ship_days)) + geom_point(alpha=0.5, position="jitter")

##############
## QUERY 16 ##
##############

query_16 <- "SELECT product_id, products_df.list_price, category_id, category_name
FROM products_df 
LEFT JOIN categories_df USING (category_id)"
query_16 <- sqldf(query_16)
View(query_16)

######################
## VISUALIZATION 10 ##
######################
ggplot(query_16, aes(x = list_price, y = factor(category_name))) +
  geom_boxplot() +
  labs(
    title = "List Price by Category",
    x = "Category ID",
    y = "List Price"
  )

##############
## QUERY 17 ##
##############

query_17 <- "SELECT order_id, order_date, store_id, staff_id, SUM(list_price) AS order_price
FROM order_items_df 
LEFT JOIN orders_df USING (order_id)
GROUP BY order_id"
query_17 <- sqldf(query_17)
View(query_17)

###################
## LINEAR MODELS ##
###################

# linear model for staff id
query_17$staff_id <- as.factor(query_17$staff_id)
model_1 <- lm(order_price ~ staff_id, data = query_17)
summary(model_1)
# no statistical significance

# linear model for store id
query_17$store_id <- as.factor(query_17$store_id)
model_2 <- lm(order_price ~ store_id, data = query_17)
summary(model_2)
# no statistical significance

# get the month name as a factor
query_17$month <- factor(format(query_17$order_date, "%B"), 
                        levels = month.name)  # Chronological order for months
View(query_17)
model_3 <- lm(order_price ~ month, data = query_17)
summary(model_3)
# Feb, Mar, Apr, May, Jul, Aug, Sept, Oct, and Dec are significantly 
# different from January in their effect on order_price


