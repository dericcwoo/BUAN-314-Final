
# loading all necessary libraries
library(tidyverse)
library(sqldf)

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
query_3 <- "SELECT order_date, COUNT(order_id) AS order_count FROM orders_df GROUP BY order_date ORDER BY COUNT(order_id) DESC LIMIT 5"
sqldf(query_3)
# no the highest amount of orders on one day is 9

# when looked at quarterly, that month doesn't appear so drastic
ggplot(orders_df, aes(x=order_date, color=factor(store_id))) + geom_histogram(bins=9)

# how many staff members are there, by asking which distinct staff_id exist
query_4 <- "SELECT DISTINCT(staff_id) FROM orders_df"
sqldf(query_4)

# which staff member is the most successful at selling bikes?
query_5 <- "SELECT staff_id, COUNT(order_id) AS order_count FROM orders_df GROUP BY staff_id ORDER BY COUNT(order_id) DESC LIMIT 3"
sqldf(query_5)
# but who is that?

# importing raw dataframe 'staffs'
staffs_df <- read_csv('https://raw.githubusercontent.com/dericcwoo/BUAN-314-Final/refs/heads/main/staffs.csv?token=GHSAT0AAAAAAC3LBTZIRXL55QF54NG2FXE2Z2Q42VA')
View(staffs_df)

# the joining query
query_6 <- "SELECT first_name, last_name, COUNT(order_id) AS order_count FROM orders_df LEFT JOIN staffs_df USING (staff_id) GROUP BY staff_id ORDER BY COUNT(order_id) DESC LIMIT 3"
sqldf(query_6)
