library(readr)

data<- read.csv("FIFA 2018 Statistics.csv", check.names = FALSE,  sep = ",")


# first two rows of the dataset
head(data, 2)

# column names
colnames(data)



hist(data$`Goal Scored`,main = "Distribution of Goals Scored",
xlab = "Goals Scored",
ylab = "Frequency",
col = "azure",
probability = TRUE
)



mean_val <- mean(data$`Goal Scored`)
sd_val <- sd(data$`Goal Scored`)

# the normal curve
curve(dnorm(x, mean = mean_val, sd = sd_val),
      col = "red",
      lwd = 2,
      add = TRUE)


legend("topright", legend = "Normal Curve", col = "red", lwd = 2)

# Scatter plot for Attempts vs Goal Scored
plot(data$`Goal Scored`, data$ Attempts,
     main = "Scatter Plot of Attempts vs Goals Scored",
     xlab = "Attempts",
     ylab = "Goals Scored",
     pch = 19, # Solid circle
     col = "blue")



model <- lm(Attempts ~ `Goal Scored`, data = data)

abline(model, col = "red")


