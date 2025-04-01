install.packages("tidyverse")
library(tidyverse)
supermarket_data <- read_csv("data/supermarket_sales - Sheet1.csv")
head(supermarket_data)

colSums(is.na(supermarket_data))



str(supermarket_data)


supermarket_data <- supermarket_data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

str(supermarket_data)



summary(supermarket_data)


unique(supermarket_data$`Product line`)

supermarket_data %>%
  count(Branch)

supermarket_data %>%
  count(Payment)

library(ggplot2)

ggplot(supermarket_data, aes(x = Date, y = Total)) +
  geom_line(stat = "summary", fun = sum, color = "blue") +
  labs(title = "Daily Sales Trend", x = "Date", y = "Total Sales") +
  theme_minimal()

ggplot(supermarket_data, aes(x = `Product line`, y = Total, fill = `Product line`)) +
  geom_boxplot() +
  labs(title = "Sales Distribution by Product Line", x = "Product Line", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(supermarket_data, aes(x = Payment, fill = Payment)) +
  geom_bar() +
  labs(title = "Preferred Payment Methods", x = "Payment Method", y = "Count") +
  theme_minimal()
