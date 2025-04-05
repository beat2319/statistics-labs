data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/School/3rd_Year/Semester_1/GEOG_3023/Labs/Lab_5/lab5.csv")
data <- na.omit(data)

# Attach relevant variables for clarity
fftype <- data$fftype
avgelev <- data$avgelev
avgstep <- data$avgstep
mean_dist_grass <- data$mean_dist_grass
dist_rav <- data$dist_rav
covcode <- data$covcode
step1 <- data$step1

# Check normality of variables by fire frequency category
shapiro_avgelev <- by(avgelev, fftype, shapiro.test)          # All p > .05 -> use ANOVA
shapiro_avgstep <- by(avgstep, fftype, shapiro.test)          # High group p < .05 -> normalize or use Kruskal-Wallis
shapiro_grass <- by(mean_dist_grass, fftype, shapiro.test)    # Med group p < .05 -> normalize
shapiro_rav <- by(dist_rav, fftype, shapiro.test)             # Med and high p < .05 -> normalize

# Log-transform non-normal variables
log_avgstep <- log(avgstep)
log_dist_grass <- log(mean_dist_grass)
log_dist_rav <- log(dist_rav)

# Re-check normality after log-transformation
shapiro_log_avgstep <- by(log_avgstep, fftype, shapiro.test)  # Still not normal -> use Kruskal-Wallis
shapiro_log_grass <- by(log_dist_grass, fftype, shapiro.test) # All p > .05 -> use ANOVA
shapiro_log_rav <- by(log_dist_rav, fftype, shapiro.test)     # All p > .05 -> use ANOVA

# Run ANOVA tests where appropriate

# 1. Average elevation by fire frequency
anova_avgelev <- aov(avgelev ~ fftype)
summary(anova_avgelev)  # Significant difference (p < 0.001)
TukeyHSD(anova_avgelev) # Significant difference between low-high & med-high

# 2. Log-transformed distance to grassland
anova_grass <- aov(log_dist_grass ~ fftype)
summary(anova_grass)    # Significant difference (p < 0.05)
TukeyHSD(anova_grass)   # Significant between low-high & med-high

# 3. Log-transformed distance to ravine drainage
anova_rav <- aov(log_dist_rav ~ fftype)
summary(anova_rav)      # Significant difference (p < 0.05)
TukeyHSD(anova_rav)     # Significant between low-high & med-low

# Kruskal-Wallis test where data is non-normal

# 4. Average slope (avgstep) by fire frequency
kruskal_avgstep <- kruskal.test(avgstep ~ fftype)
print(kruskal_avgstep) # Not Significant (p > 0.05), but very close p = 0.05315 

# Chi-squared tests for categorical variables

# 5. Tree cover type vs fire frequency
chisq_cov <- chisq.test(covcode, fftype) # Not Significant (p > 0.05), p = 0.178

# 6. Steepness category vs fire frequency
chisq_step <- chisq.test(step1, fftype)  # Not Significant (p > 0.05), but very close p = 0.0555 




