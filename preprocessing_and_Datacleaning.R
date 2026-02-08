library(data.table)
library(readxl)
library(ggplot2)
library(stringr)
library(readr)

transactionData_excel <- read_excel("QVI_transaction_data.xlsx")
fwrite(transactionData_excel, "QVI_transaction_data.csv")

transactionData <- fread("QVI_transaction_data.csv")
customerData <- fread("QVI_purchase_behaviour.csv")

transactionData[, DATE := as.Date(DATE, origin = "1899-12-30")]

productWords <- data.table(
  words = unlist(strsplit(tolower(unique(transactionData$PROD_NAME)), " "))
)

productWords <- productWords[!grepl("[0-9]", words)]
productWords <- productWords[!grepl("&", words)]
productWords[, .N, by = words][order(-N)]

transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE][, SALSA := NULL]

outlier_transaction <- transactionData[PROD_QTY == 200]
transactionData <- transactionData[!LYLTY_CARD_NBR %in% outlier_transaction$LYLTY_CARD_NBR]

transactions_by_day <- transactionData[, .N, by = DATE]

all_dates <- data.table(
  DATE = seq(
    from = as.Date("2018-07-01"),
    to   = as.Date("2019-06-30"),
    by   = "day"
  )
)

transactions_by_day <- merge(
  all_dates,
  transactions_by_day,
  by = "DATE",
  all.x = TRUE
)

transactions_by_day[is.na(N), N := 0]

transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

pack_plot <- ggplot(transactionData, aes(x = PACK_SIZE)) +
  geom_histogram(binwidth = 10) +
  labs(
    title = "Distribution of Chip Pack Sizes",
    x = "Pack Size (grams)",
    y = "Number of Transactions"
  ) +
  theme_bw()

ggsave(
  filename = "pack_size_distribution.png",
  plot = pack_plot,
  width = 8,
  height = 5
)

transactionData[, BRAND := toupper(word(PROD_NAME, 1))]
transactionData[BRAND == "RED", BRAND := "RRD"]
transactionData[BRAND == "SNBTS", BRAND := "SUNBITES"]
transactionData[BRAND == "WW", BRAND := "WOOLWORTHS"]
customerData[, .N, by = LIFESTAGE][order(-N)]
customerData[, .N, by = PREMIUM_CUSTOMER][order(-N)]


data <- merge(transactionData, customerData, all.x = TRUE)
data[is.na(LIFESTAGE) | is.na(PREMIUM_CUSTOMER)]

fwrite(data, "task1_out.csv")
sales_by_segment <- data[
  ,
  .(TOTAL_SALES = sum(TOT_SALES)),
  by = .(LIFESTAGE, PREMIUM_CUSTOMER)
]

sales_plot <- ggplot(
  sales_by_segment,
  aes(x = LIFESTAGE, y = TOTAL_SALES, fill = PREMIUM_CUSTOMER)
) +
  geom_col() +
  labs(
    title = "Total Chip Sales by Lifestage and Premium Customer",
    x = "Lifestage",
    y = "Total Sales ($)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  filename = "total_sales_by_segment.png",
  plot = sales_plot,
  width = 10,
  height = 6
)
customers_by_segment <- data[
  ,
  .(NUM_CUSTOMERS = uniqueN(LYLTY_CARD_NBR)),
  by = .(LIFESTAGE, PREMIUM_CUSTOMER)
]
customers_plot <- ggplot(
  customers_by_segment,
  aes(x = LIFESTAGE, y = NUM_CUSTOMERS, fill = PREMIUM_CUSTOMER)
) +
  geom_col() +
  labs(
    title = "Number of Customers by Lifestage and Premium Customer",
    x = "Lifestage",
    y = "Number of Customers"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "customers_by_segment.png",
  customers_plot,
  width = 10,
  height = 6
)
units_per_customer <- data[
  ,
  .(TOTAL_UNITS = sum(PROD_QTY)),
  by = .(LYLTY_CARD_NBR, LIFESTAGE, PREMIUM_CUSTOMER)
]

avg_units_segment <- units_per_customer[
  ,
  .(AVG_UNITS = mean(TOTAL_UNITS)),
  by = .(LIFESTAGE, PREMIUM_CUSTOMER)
]
units_plot <- ggplot(
  avg_units_segment,
  aes(x = LIFESTAGE, y = AVG_UNITS, fill = PREMIUM_CUSTOMER)
) +
  geom_col() +
  labs(
    title = "Average Units Purchased per Customer",
    x = "Lifestage",
    y = "Average Units"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "avg_units_per_customer.png",
  units_plot,
  width = 10,
  height = 6
)
data[, PRICE_PER_UNIT := TOT_SALES / PROD_QTY]

avg_price_segment <- data[
  ,
  .(AVG_PRICE = mean(PRICE_PER_UNIT)),
  by = .(LIFESTAGE, PREMIUM_CUSTOMER)
]
price_plot <- ggplot(
  avg_price_segment,
  aes(x = LIFESTAGE, y = AVG_PRICE, fill = PREMIUM_CUSTOMER)
) +
  geom_col() +
  labs(
    title = "Average Price per Unit by Customer Segment",
    x = "Lifestage",
    y = "Average Price ($)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "avg_price_per_unit.png",
  price_plot,
  width = 10,
  height = 6
)
target_data <- data[
  LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
]

mainstream_prices <- target_data[
  PREMIUM_CUSTOMER == "Mainstream",
  PRICE_PER_UNIT
]

other_prices <- target_data[
  PREMIUM_CUSTOMER %in% c("Budget", "Premium"),
  PRICE_PER_UNIT
]
t_test_result <- t.test(mainstream_prices, other_prices)
t_test_result$p.value
target_segment <- data[
  LIFESTAGE == "YOUNG SINGLES/COUPLES" &
  PREMIUM_CUSTOMER == "Mainstream"
]

brand_affinity <- target_segment[
  ,
  .N,
  by = BRAND
][order(-N)]

brand_affinity
overall_brand <- data[, .N, by = BRAND][order(-N)]
pack_affinity <- target_segment[
  ,
  .N,
  by = PACK_SIZE
][order(-N)]

pack_affinity
overall_pack <- data[, .N, by = PACK_SIZE][order(-N)]








# ================================
# Libraries
# ================================
library(data.table)
library(readxl)
library(ggplot2)
library(stringr)
library(readr)

# ================================
# Load data
# ================================
transactionData_excel <- read_excel("QVI_transaction_data.xlsx")
fwrite(transactionData_excel, "QVI_transaction_data.csv")

transactionData <- fread("QVI_transaction_data.csv")
customerData <- fread("QVI_purchase_behaviour.csv")

# ================================
# Date conversion
# ================================
transactionData[, DATE := as.Date(DATE, origin = "1899-12-30")]

# ================================
# Remove salsa products
# ================================
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE][, SALSA := NULL]

# ================================
# Remove outlier customers
# ================================
outlier_transaction <- transactionData[PROD_QTY == 200]
transactionData <- transactionData[
  !LYLTY_CARD_NBR %in% outlier_transaction$LYLTY_CARD_NBR
]

# ================================
# Merge customer data
# ================================
data <- merge(transactionData, customerData, all.x = TRUE)

# ================================
# Create YEARMONTH
# ================================
data[, YEARMONTH := as.integer(format(DATE, "%Y%m"))]

# ================================
# Monthly store-level sales
# ================================
monthly_sales <- data[
  ,
  .(TOT_SALES = sum(TOT_SALES)),
  by = .(STORE_NBR, YEARMONTH)
]

# ================================
# Define Trial & Control stores
# ================================
trial_store <- 77
control_store <- 233   # اگر Control Store تو فرق دارد، این عدد را عوض کن

# ================================
# Filter trial period data
# ================================
plot_data <- monthly_sales[
  YEARMONTH >= 201902 & STORE_NBR %in% c(trial_store, control_store)
]

plot_data[, Store_Type := ifelse(
  STORE_NBR == trial_store,
  "Trial Store",
  "Control Store"
)]

# ================================
# Line chart: Trial vs Control
# ================================
p <- ggplot(
  plot_data,
  aes(
    x = YEARMONTH,
    y = TOT_SALES,
    color = Store_Type,
    group = Store_Type
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Trial vs Control Store Performance During Trial Period",
    x = "Time (Year-Month)",
    y = "Total Sales",
    color = "Store Type"
  ) +
  theme_minimal()

# Show plot
p

# ================================
# Save as PNG
# ================================
ggsave(
  filename = "trial_vs_control_performance.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)
