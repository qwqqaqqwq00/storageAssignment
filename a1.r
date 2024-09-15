# Dependencies
# include necessary libraries.
install.packages(c("corrplot", "caret", "tidyverse", "mice", "VIM"))
library(corrplot)
library(caret)
library(tidyverse)

# ref to ai and browser
library(mice)
library(VIM)

dataset <- read.csv("A1_data.csv")
str(dataset)
summary(dataset)

hist(dataset$isFraud)
hist(dataset$TransactionAmt)
hist(dataset$card1)
hist(dataset$card2)
ggplot(dataset, aes(x = factor(card4))) +
  geom_bar()
ggplot(dataset, aes(x = factor(card6))) +
  geom_bar()

# ref to ai
sum(is.na(dataset$addr1) & is.na(dataset$addr2))
addr_filter <- dataset %>% 
  filter(!(is.na(dataset$addr1) & is.na(dataset$addr2)))
ggplot(addr_filter, aes(x = addr1)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  theme_minimal()
ggplot(addr_filter, aes(x = addr2)) +
  geom_density(fill = "#46b44d", alpha = 0.5) +
  theme_minimal()

ggplot(dataset, aes(x = TransactionAmt)) +
  geom_density(fill = "#2fe5f1", alpha = 0.5) +
  theme_minimal()
ggplot(dataset, aes(x = TransactionDT)) +
  geom_density(fill = "#11fcbd", alpha = 0.5) +
  theme_minimal()
ggplot(dataset, aes(x = TransactionAmt, fill = factor(isFraud))) +
  xlim(0, 500) +
  geom_histogram(bins = 100) +
  facet_grid(isFraud ~ ., scales = "free_y")
group_amt <- dataset %>%
  mutate(day = floor(TransactionDT..Hour. / 24)) %>%
  group_by(day) %>%
  summarise(sumAmt = sum(TransactionAmt))
ggplot(group_amt, aes(x = day, y = log10(sumAmt))) +
  geom_line(color = "#ec1c1c")
boxplot(log10(TransactionAmt) ~ floor(TransactionDT..Hour. / 24 / 7),
  data = dataset, xlab = "week")
group_fraud <- dataset %>%
  mutate(day = floor(TransactionDT..Hour. / 24)) %>%
  group_by(day) %>%
  summarise(meanFruad = mean(isFraud))
ggplot(group_fraud, aes(x = day, y = meanFruad)) +
  geom_line(color = "#ec1c1c")
group_fraud <- dataset %>%
  mutate(day = TransactionDT..Hour. %% 24) %>%
  group_by(day) %>%
  summarise(meanFruad = mean(isFraud))
ggplot(group_fraud, aes(x = day, y = meanFruad)) +
  geom_line(color = "#ec1c1c")

# sapply, colMeans ref to ai
num_col <- sapply(dataset, is.numeric)
num_df <- dataset[, num_col]
for (col_name in colnames(num_df)) {
  cat(col_name, ",")
}
# num_df[is.na(num_df)] <- 0
col_means <- colMeans(dataset %>% select(C1), na.rm = TRUE)
num_df[is.na(num_df)] <- col_means

# ref to https://zhuanlan.zhihu.com/p/21549898
aggr_plot <- aggr(dataset[, num_col], col = c("navyblue", "red"), numbers = TRUE, labels = names(data), cex.axis = .7, gap = 3, ylab = c("Histogram  of missing data", "Pattern"))
# mice, complete, prcomp ref to ai
ds <- dataset[, num_col] %>% select(D1, D2, D3, D4, D5, D6, D7, D8, D9)
imputed_ds <- mice(ds, method = "pmm")
filled_ds <- complete(imputed_ds)
pca_ds_result <- prcomp(filled_ds, scale. = TRUE, rank. = 5)
dsx <- as.data.frame(pca_ds_result$x)

summary(dataset$C3)
count(dataset %>% filter(C3 == 0))
num_df <- dataset %>%
  select(TransactionAmt, TransactionDT, TransactionDT..Hour., isFraud, card1, C1, C13, C2, C3, C4, C6, C7, C8, C10, C11, C12, C13, C14) %>%
  mutate(hourDay = TransactionDT..Hour. %% 24)
corr_mat <- cor(cbind(num_df, dsx))
corrplot(corr_mat, method = "color", title = "Correlate Plot of Some Numeric Features and Preprocessed features", mar = c(0, 0, 3, 0))
