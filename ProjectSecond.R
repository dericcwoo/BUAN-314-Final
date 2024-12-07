
# loading all necessary libraries
library(tidyverse)
library(sqldf)
library(lubridate)

# importing raw dataframe 'orders'
orders_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/orders.csv?token=GHSAT0AAAAAAC3LBTZII2L53UDDSIEFJ6KUZ2Q47PQ')
View(orders_df)

# check for Nulls
summary(orders_df)
# no Nulls

query_1 <- "SELECT COUNT(*) FROM orders_df WHERE shipped_date = 'NULL'"
sqldf(query_1)
# 170 observations of 'NULL'

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

# histograms for each store
ggplot(orders_df, aes(x=order_date, color=factor(store_id))) + geom_histogram(bins=36)

# why is that one month count of orders so high? maybe there is one day with a huge amount of orders
query_3 <- "SELECT order_date, COUNT(order_id) AS order_count 
FROM orders_df 
GROUP BY order_date 
ORDER BY COUNT(order_id) DESC 
LIMIT 5"
sqldf(query_3)
# no the highest amount of orders on one day is 9

# when looked at quarterly, that month doesn't appear so drastic
ggplot(orders_df, aes(x=order_date, color=factor(store_id))) + geom_histogram(bins=9)

# how many staff members are there, by asking which distinct staff_id exist
query_4 <- "SELECT DISTINCT(staff_id) 
FROM orders_df"
sqldf(query_4)

# which staff member is the most successful at selling bikes?
query_5 <- "SELECT staff_id, COUNT(order_id) AS order_count 
FROM orders_df 
GROUP BY staff_id 
ORDER BY COUNT(order_id) DESC 
LIMIT 3"
sqldf(query_5)
# but who is that?

# importing raw dataframe 'staffs'
staffs_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/staffs.csv?token=GHSAT0AAAAAAC3LBTZIRXL55QF54NG2FXE2Z2Q42VA')
View(staffs_df)

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
products_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/products.csv?token=GHSAT0AAAAAAC3LBTZJSGJCTSESE6LIVOIOZ2TTVRA')
View(products_df)

# importing raw dataframe 'categories'
categories_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/categories.csv?token=GHSAT0AAAAAAC3LBTZIWFG35F7SF2ANZZRCZ2TTMJQ')
View(categories_df)

# most popular category
query_7 <- "SELECT category_name, COUNT(*) 
FROM order_items_df 
LEFT JOIN products_df USING (product_id)
LEFT JOIN categories_df USING (category_id)
GROUP BY category_id 
ORDER BY COUNT(*) DESC 
LIMIT 10"
sqldf(query_7)

# importing raw dataframe 'order_items'
order_items_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/order_items.csv?token=GHSAT0AAAAAAC3LBTZJRK5FWPE33GSPE76EZ2TRE6A')
View(order_items_df)

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
ggplot(query_8, aes(x=month, y=order_count, color=factor(product_id))) + 
  geom_line() + 
  facet_wrap(~year) + 
  labs(x = "Month", y = "Order Count", color = "Product ID") + 
  theme(axis.text.x = element_blank())

# importing raw dataframe 'stores'
stores_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/stores.csv?token=GHSAT0AAAAAAC3LBTZJOZW5N36Z4VJHX4Z6Z2TQR2A')
View(stores_df)

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
ggplot(query_9, aes(x=month, y=order_price, color=factor(store_id))) + 
  geom_line() + 
  facet_wrap(~year) + 
  labs(x = "Month", y = "Order Price", color = "Store ID") + 
  theme(axis.text.x = element_blank())



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
