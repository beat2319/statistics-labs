
#install.packages("ggpubr") # I wanted to use a 95% confidence interval band on my qqplots 
#install.packages("tidyverse") #I wanted to be able to make a data frame when the data has different row values
#https://cran.r-project.org/web/packages/ggpubr/readme/README.html link to info about the library
library(ggplot2)
library(tidyverse)
library(ggpubr)

#importing data
data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/School/3rd_Year/Semester_1/GEOG_3023/Labs/Lab_4/lab4.csv")

#checking normality through graph of hs_2005 and hs_2018 using hist
data_a1 <- na.omit(data$hs_2005)
data_a2 <- na.omit(data$hs_2018)

x_min <- 0.9 * min(c(data_a1, data_a2))
x_max <- 1.1 * max(c(data_a1, data_a2))

break_value <- seq(x_min, x_max, length.out = 21)

hist_value_a1 <- hist(data_a1, breaks = break_value, plot = FALSE)
hist_value_a2 <- hist(data_a2, breaks = break_value, plot = FALSE)

y_max <- max(c(hist_value_a1$counts, hist_value_a2$counts))

hist_a1 <- hist(data_a1, breaks = break_value,
            xlab = "Percentage",
            main = "High School & Above (25-44)",
            col=adjustcolor("red", alpha.f =0.2),
            xlim=c(x_min, x_max),
            ylim=c(0, y_max))

legend(x = "topright", box.col = "black", 
       box.lwd = 1, title = "Years",
       legend = c("2005", "2018"),
       fill = c("red", "blue"))

x_fit_a1 <- seq(min(data_a1), max(data_a1), length = length(data_a1))
y_fit_a1 <- dnorm(x_fit_a1, mean=mean(data_a1), sd=sd(data_a1))
y_fit_a1 <- y_fit_a1*diff(hist_value_a1$mids[1:2]) * length(data_a1)

lines(x_fit_a1, y_fit_a1, col = "red")

hist_a2 <- hist(data_a2, breaks = break_value, add=TRUE,
                col=adjustcolor("blue", alpha.f =0.3),
                xlim=c(x_min, x_max),
                ylim=c(0, y_max))

x_fit_a2 <- seq(min(data_a2), max(data_a2), length = length(data_a2))
y_fit_a2 <- dnorm(x_fit_a2, mean=mean(data_a2), sd=sd(data_a2))
y_fit_a2 <- y_fit_a2*diff(hist_value_a2$mids[1:2]) * length(data_a2)

lines(x_fit_a2, y_fit_a2, col = "blue")

#finally statistical testing to see if the hs_2005 and hs_2018 are normal, we fail to reject(larger than 0.05), we reject the hypothesis (less than 0.05)
shapiro_a1 <- shapiro.test(data_a1) #we fail to reject(p larger than 0.05)
shapiro_a2 <- shapiro.test(data_a2) #we fail to reject(p larger than 0.05)

#paired t-test for hs_2005 and hs_2018 
paired_sample_a <- t.test(data_a1, data_a2, paired = TRUE)

#one sample t-test for hs_2018 secondary school in 2018 Europe with known mu as 81
one_sample_a2.eu_2018 <- t.test(data_a2, mu = 81)

#subset data into regions for West, South, Midwest and Northeast
data_W <- subset(data, region == "W")
data_S <- subset(data, region == "S")
data_MW <- subset(data, region == "MW")
data_NE <- subset(data, region == "NE")

#checking normality through graph of hs_2005 in the NE and MW using hist
data_b1 <- na.omit(data_NE$hs_2005)
data_b2 <- na.omit(data_MW$hs_2005)

x_min <- 0.9 * min(c(data_b1, data_b2))
x_max <- 1.1 * max(c(data_b1, data_b2))

break_value <- seq(x_min, x_max, length.out = 21)

hist_value_b1 <- hist(data_b1, breaks = break_value, plot = FALSE)
hist_value_b2 <- hist(data_b2, breaks = break_value, plot = FALSE)

y_max <- max(c(hist_value_b1$counts, hist_value_b2$counts))

hist_b1 <- hist(data_b1, breaks = break_value,
                xlab = "Percentage",
                main = "High School & Above (2005)",
                col=adjustcolor("red", alpha.f =0.2),
                xlim=c(x_min, x_max),
                ylim=c(0, y_max))

legend(x = "topright", box.col = "black", 
       box.lwd = 1, title = "Location",
       legend = c("NE", "SW"),
       fill = c("red", "blue"))

x_fit_b1 <- seq(min(data_b1), max(data_b1), length = length(data_b1))
y_fit_b1 <- dnorm(x_fit_b1, mean=mean(data_b1), sd=sd(data_b1))
y_fit_b1 <- y_fit_b1*diff(hist_value_b1$mids[1:2]) * length(data_b1)

lines(x_fit_b1, y_fit_b1, col = "red")

hist_b2 <- hist(data_b2, breaks = break_value, add=TRUE,
                col=adjustcolor("blue", alpha.f =0.3),
                xlim=c(x_min, x_max),
                ylim=c(0, y_max))

x_fit_b2 <- seq(min(data_b2), max(data_b2), length = length(data_b2))
y_fit_b2 <- dnorm(x_fit_b2, mean=mean(data_b2), sd=sd(data_b2))
y_fit_b2 <- y_fit_b2*diff(hist_value_b2$mids[1:2]) * length(data_b2)

lines(x_fit_b2, y_fit_b2, col = "blue")

#checking normality through Shapiro-Wilk test, equal variance and two sample t-test of hs_2005 in the NE and MW
shapiro_b1 <- shapiro.test(data_b1) #we fail to reject(p larger than 0.05)
shapiro_b2 <- shapiro.test(data_b2) #we fail to reject(p larger than 0.05)

variance_b <- var.test(data_b1, data_b2) 

two_sample_b <- t.test(data_b1, data_b2, var.equal=TRUE, paired=FALSE)

#checking normality through graph of hs_2018 in the NE and MW using hist
data_c1 <- na.omit(data_NE$hs_2018)
data_c2 <- na.omit(data_MW$hs_2018)

x_min <- 0.9 * min(c(data_c1, data_c2))
x_max <- 1.1 * max(c(data_c1, data_c2))

break_value <- seq(x_min, x_max, length.out = 21)

hist_value_c1 <- hist(data_c1, breaks = break_value, plot = FALSE)
hist_value_c2 <- hist(data_c2, breaks = break_value, plot = FALSE)

y_max <- max(c(hist_value_c1$counts, hist_value_c2$counts))

hist_c1 <- hist(data_c1, breaks = break_value,
                xlab = "Percentage",
                main = "High School & Above (2018)",
                col=adjustcolor("red", alpha.f =0.2),
                xlim=c(x_min, x_max),
                ylim=c(0, y_max))

legend(x = "topright", box.col = "black", 
       box.lwd = 1, title = "Location",
       legend = c("NE", "SW"),
       fill = c("red", "blue"))

x_fit_c1 <- seq(min(data_c1), max(data_c1), length = length(data_c1))
y_fit_c1 <- dnorm(x_fit_c1, mean=mean(data_c1), sd=sd(data_c1))
y_fit_c1 <- y_fit_c1*diff(hist_value_c1$mids[1:2]) * length(data_c1)

lines(x_fit_c1, y_fit_c1, col = "red")

hist_c2 <- hist(data_c2, breaks = break_value, add=TRUE,
                col=adjustcolor("blue", alpha.f =0.3),
                xlim=c(x_min, x_max),
                ylim=c(0, y_max))

x_fit_c2 <- seq(min(data_c2), max(data_c2), length = length(data_c2))
y_fit_c2 <- dnorm(x_fit_c2, mean=mean(data_c2), sd=sd(data_c2))
y_fit_c2 <- y_fit_c2*diff(hist_value_c2$mids[1:2]) * length(data_c2)

lines(x_fit_c2, y_fit_c2, col = "blue")

#checking normality through Shapiro-Wilk test, equal variance and two sample t-test of hs_2018 in the NE and MW
shapiro_c1 <- shapiro.test(data_c1) #we fail to reject(p larger than 0.05)
shapiro_c2 <- shapiro.test(data_c2) #we fail to reject(p larger than 0.05)

variance_c <- var.test(data_c1, data_c2) 

two_sample_c <- t.test(data_c1, data_c2, var.equal=TRUE, paired=FALSE)

#showing that the data of the south is not normal through qqplot
data_d1 <- na.omit(data_S$hs_2005) 
data_d2 <- na.omit(data_S$hs_2018) 

df_d <- data.frame(data_d1, data_d2)

names(df_d) <- c("2005", "2018")

ggqqplot(
  data  = df_d,
  x = c("2005", "2018"),
  conf.int = TRUE,       
  conf.int.level = 0.95,
  merge = TRUE,          
  palette = c("#0073C2FF", "#FC4E07"),
  ggtheme = theme_pubclean(),
  title = "High School & Above (South) Non-Normal"
)

#taking the shapiro test of the south to confirm its not normal
shapiro_d1 <- shapiro.test(data_d1) #we fail to reject(p larger than 0.05)
shapiro_d2 <- shapiro.test(data_d2) #we reject(p less than 0.05)

#since the data is not normal we can take a non-parametric test such as the wilcox test
wilcox_test_d <- wilcox.test(data_d1, data_d2, paired=TRUE)

median_difference_d <-  median(data_d2 - data_d1)

#normalizing the data by taking the log transformation of the data, and take a qqplot to confirm
data_log_d1 <- log(na.omit(data_S$hs_2005)) 
data_log_d2 <- log(na.omit(data_S$hs_2018))

df_log_d <- data.frame(data_log_d1, data_log_d2)

names(df_log_d) <- c("2005", "2018")

ggqqplot(
  data  = df_log_d,
  x = c("2005", "2018"),
  conf.int = TRUE,       
  conf.int.level = 0.95,
  merge = TRUE,          
  palette = c("#0073C2FF", "#FC4E07"),
  ggtheme = theme_pubclean(),
  title = "High School & Above (South) Normal"
)

#confirming normality after log transformation with shapiro and paired t-test
shapiro_log_d1 <- shapiro.test(data_log_d1) #we fail to reject(p larger than 0.05)
shapiro_log_d2 <- shapiro.test(data_log_d2) #we fail to reject(p larger than 0.05)

paired_sample_d <- t.test(data_log_d1, data_log_d2, paired = TRUE)

#normalizing the data of hs_2018 in the south and west by taking the log transformation of the data, and taking a qqplot to confirm
data_log_e1 <- log(na.omit(data_W$hs_2018)) 
data_log_e2 <- log(na.omit(data_S$hs_2018))

df_log_e1 <- data.frame(region = "West", value = data_log_e1)
df_log_e2 <- data.frame(region = "South", value = data_log_e2)
df_log_e <- bind_rows(df_log_e1, df_log_e2) #only reason for tidyverse

ggqqplot(
  data = df_log_e,
  x = "value",
  color = "region",
  facet.by = "region",
  conf.int = TRUE,
  conf.int.level = 0.95,
  palette = c("#0073C2FF", "#FC4E07"),
  ggtheme = theme_pubclean(),
  title = "High School & Above (2018) Normal"
)

#confirming normality after log transformation with shapiro and paired t-test
shapiro_log_e1 <- shapiro.test(data_log_e1) #we fail to reject(p larger than 0.05)
shapiro_log_e2 <- shapiro.test(data_log_e2) #we fail to reject(p larger than 0.05)

two_sample_e <- t.test(data_log_e1, data_log_e2, paired = FALSE)


