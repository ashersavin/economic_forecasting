library(tidyverse)
library(forecast)
library(zoo)
library(gghighlight)
library(latticeExtra)
library(stargazer)
source("~/Desktop/ECON422/subset_by_date.R")

# Read data
SENT <- read_csv("UMCSENT.csv")
PCE <- read_csv("A794RX0Q048SBEA.csv")
REC <- read_csv("USREC.csv")

# Turn SENT to quarterly
SENT <- subset_by_date(SENT, startTime = "1978-01-01", endTime = "2021-07-01")
SENT$QDATE <- as.yearqtr(SENT$DATE)
SENT$UMCSENT <- as.numeric(SENT$UMCSENT)
SENT <- SENT %>% 
  group_by(QDATE) %>% 
  summarise_all(mean)

# Add PCE year change to SENT
PCE <- subset_by_date(PCE, startTime = "1977-01-01", endTime = "2021-07-01")
year_change <- PCE$A794RX0Q048SBEA %>%
  log() %>%
  diff(4)
SENT$PCE.cha <- year_change

# Add normal PCE
PCE <- subset_by_date(PCE, startTime = "1978-01-01", endTime = "2021-07-01")
SENT$PCE <- PCE$A794RX0Q048SBEA

# Rename mega data set
df <- SENT
df <- rename(df, SENT = UMCSENT)

# Add REC
REC$QDATE <- as.yearqtr(REC$DATE)
REC <- REC %>% 
  group_by(QDATE) %>% 
  summarise_all(mean)
REC <- subset_by_date(REC, startTime = "1978-01-01", endTime = "2021-10-01")
df <- df %>%
  mutate(REC = REC$USREC)
df$REC <- (ifelse(df$REC == 0, "Expansion", "Recession"))

# Regress consumer sentiment on personal consumption
lm1 <- lm(PCE.cha ~ SENT, data = df)
summary(lm1)

# Regress consumer sentiment on personal consumption DURING RECESSIONS
df.rec <- df %>%
  filter(REC == "Recession")
lmREC <- lm(PCE.cha ~ SENT, data = df.rec)
summary(lmREC)

# Regress consumer sentiment on personal consumption DURING EXPANSIONS
df.exp <- df %>% 
  filter(REC == "Expansion")
lmEXP <- lm(PCE.cha ~ SENT, data = df.exp)
summary(lmEXP)

# Plot PCE change and Sentiment together
obj1 <- xyplot(SENT ~ DATE, df, type = "l" , lwd = 2, ylab = "Consumer Sentiment")
obj2 <- xyplot(PCE.cha ~ DATE, df, type = "l", lwd = 2, ylab = "PCE year/year change")
doubleYScale(obj1, obj2, text = c("Sentiment", "Personal Consumption"), 
             add.ylab2 = TRUE, 
             use.style = TRUE)

# Y/Y% change calculation
new = data.frame(SENT = c(70))
predict.lm(lm1, newdata = new, interval = "confidence")
print(41424 * 1.001975581)

# Recession calculation
predict.lm(lmREC, newdata = new, interval = "confidence")
print(41424 * -.009643983)
print(41423 - 399.4924)

# Expansion calculation
predict.lm(lmEXP, newdata = new, interval = "confidence")
print(41424 * 1.007815219)

# Plot regressions
ggplot(data = df, aes(x = SENT, y = PCE.cha, col = REC, fill = REC)) +
  labs(x = "Consumer Sentiment", y ="PCE (YOY%)") +
  geom_point() +
  geom_smooth(aes(x = SENT, y = PCE.cha),
              alpha = .2,
              method = "lm",
              fullrange = TRUE)

# Table showing regressions
stargazer(lm1, lmEXP, lmREC,
          type = "text",
          dep.var.caption = "Personal Consumption Expenditure",
          dep.var.labels = c("Overall", "Expansion", "Recession"),
          covariate.labels = c("(B1) Consumer Sentiment", "(B0) Constant"),
          digits = 5)


