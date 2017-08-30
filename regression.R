# install.packages(c("ggplot2", "MASS","ISLR", "DT"))
library(MASS)
library(ISLR)
library(DT)


Boston = read.csv("/home/hadoop/vanguard/dhaval/housing.csv", header = TRUE)

names(Boston)

datatable(Boston, rownames = FALSE)

lm.fit <- lm(MDEV ~ LSTAT, data = Boston)
lm.fit

summary(lm.fit)

names(lm.fit)

lm.fit$coefficients


lm.fit[[1]]
coef(lm.fit)
confint(lm.fit, level = 0.95)
summary(lm.fit)$coefficients
b1 <- summary(lm.fit)$coefficients[2, 1]
sb1 <- summary(lm.fit)$coefficients[2, 2]
ct <- qt(0.975, summary(lm.fit)$df[2])
CI <- b1 + c(-1, 1)*ct*sb1
CI

CI <- predict(object = lm.fit, newdata = data.frame(LSTAT = c(5, 10, 15)), 
              interval = "confidence")
CI

PI <- predict(object = lm.fit, newdata = data.frame(LSTAT = c(5, 10, 15)), 
              interval = "predict")
PI

plot(MDEV ~ LSTAT, data = Boston)
abline(lm.fit)
# Or using ggplot2
library(ggplot2)

ggplot(data = Boston, aes(x = LSTAT, y = MDEV)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw()


plot(MDEV ~ LSTAT, data = Boston)
abline(lm.fit, lwd = 3)

plot(MDEV ~ LSTAT, data = Boston)
abline(lm.fit, lwd = 3, col = "red")

plot(MDEV ~ LSTAT, data = Boston, pch = 20)

plot(MDEV ~ LSTAT, data = Boston, pch = "+")

plot(1:20, 1:20, pch = 1:20)

# thicker line
ggplot(data = Boston, aes(x = LSTAT, y = MDEV)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", size = 2) +
  theme_bw()

