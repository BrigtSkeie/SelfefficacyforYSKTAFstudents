# R-scripts for study on perceived self-efficacy of YSK/TAF-students in Norway

# Loading libraries
library(psych) # descriptive statistics
library(car) # Levenes test
library(ggplot2) # Box plot

# Reading data

# load raw data
raw_data <- read.csv(file = "dataset_raw.csv", sep=";")

####################################################
# Data preparation
####################################################
# separate responses that is complete based on flags from surveyxact
YSKTAFstudents <- raw_data[raw_data$stato_4 == 1,]

#remove imported raw data
rm(raw_data)


# replace variable names with list of new names
namelist <- read.csv(file = "variables_new.csv", sep=";")
names(YSKTAFstudents) <- c(namelist$variableNewNames)

# reshuffle answers to make neutral answer the middle value
reshufflelist <- c(namelist$variableNewNames[9:66])
YSKTAFstudents[,reshufflelist] <- lapply(YSKTAFstudents[,reshufflelist], function(x) ifelse(x>2,x+1,x))
YSKTAFstudents[,reshufflelist] <- lapply(YSKTAFstudents[,reshufflelist], function(x) ifelse(x==6,3,x))

# reverse variables
reverselist <- c(namelist$variableNewNames[21], namelist$variableNewNames[37:42], 
                 namelist$variableNewNames[45],namelist$variableNewNames[61:66])
YSKTAFstudents[,reverselist] <- YSKTAFstudents[,reverselist]*-1+6

# change values of 4 to 1 for the smallest size of department
YSKTAFstudents$department <- ifelse(YSKTAFstudents$department==4,1,YSKTAFstudents$department)

# preparation of programs
# move group 3 and 11 to group 9
YSKTAFstudents$program <- ifelse(YSKTAFstudents$program==3,9,YSKTAFstudents$program)
YSKTAFstudents$program <- ifelse(YSKTAFstudents$program==11,9,YSKTAFstudents$program)

# preparation of visitfrequency
# move group 1 to group 2
YSKTAFstudents$visitfrequency <- ifelse(YSKTAFstudents$visitfrequency==1,2,YSKTAFstudents$visitfrequency)
# move group 5 to group 4
YSKTAFstudents$visitfrequency <- ifelse(YSKTAFstudents$visitfrequency==5,4,YSKTAFstudents$visitfrequency)


# combine variables to new variables
#General Self-efficacy
YSKTAFstudents$GSE <- NA
# Loop through each row and calculate the sum of variables
for (i in 1:nrow(YSKTAFstudents)) {
  if (sum(is.na(YSKTAFstudents[i, c(namelist$variableNewNames[9:18])])) < 3) {
    YSKTAFstudents$GSE[i] <- sum(YSKTAFstudents[i, c(namelist$variableNewNames[9:18])], na.rm = TRUE)}
  else {YSKTAFstudents$GSE[i] <- NA}
}

#Sources of GSE
YSKTAFstudents$school_me <- NA
YSKTAFstudents$school_ve <- NA
YSKTAFstudents$school_sp <- NA
YSKTAFstudents$school_ph <- NA
YSKTAFstudents$workplace_me <- NA
YSKTAFstudents$workplace_ve <- NA
YSKTAFstudents$workplace_sp <- NA
YSKTAFstudents$workplace_ph <- NA

# Loop through each row and calculate the sum of variables
for (i in 1:nrow(YSKTAFstudents)) {YSKTAFstudents$school_me[i] <- sum(YSKTAFstudents[i, c(namelist$variableNewNames[19:24])], na.rm = FALSE)}
for (i in 1:nrow(YSKTAFstudents)) {YSKTAFstudents$school_ve[i] <- sum(YSKTAFstudents[i, c(namelist$variableNewNames[25:30])], na.rm = FALSE)}
for (i in 1:nrow(YSKTAFstudents)) {YSKTAFstudents$school_sp[i] <- sum(YSKTAFstudents[i, c(namelist$variableNewNames[31:36])], na.rm = FALSE)}
for (i in 1:nrow(YSKTAFstudents)) {YSKTAFstudents$school_ph[i] <- sum(YSKTAFstudents[i, c(namelist$variableNewNames[37:42])], na.rm = FALSE)}
for (i in 1:nrow(YSKTAFstudents)) {YSKTAFstudents$workplace_me[i] <- sum(YSKTAFstudents[i, c(namelist$variableNewNames[43:48])], na.rm = FALSE)}
for (i in 1:nrow(YSKTAFstudents)) {YSKTAFstudents$workplace_ve[i] <- sum(YSKTAFstudents[i, c(namelist$variableNewNames[49:54])], na.rm = FALSE)}
for (i in 1:nrow(YSKTAFstudents)) {YSKTAFstudents$workplace_sp[i] <- sum(YSKTAFstudents[i, c(namelist$variableNewNames[55:60])], na.rm = FALSE)}
for (i in 1:nrow(YSKTAFstudents)) {YSKTAFstudents$workplace_ph[i] <- sum(YSKTAFstudents[i, c(namelist$variableNewNames[61:66])], na.rm = FALSE)}

# Adjust sum variables to scale of 1-5
# divide with the number of variables used for the sum variable
YSKTAFstudents$GSE <- YSKTAFstudents$GSE / 10
YSKTAFstudents$school_me <- YSKTAFstudents$school_me / 6
YSKTAFstudents$school_ve <- YSKTAFstudents$school_ve / 6
YSKTAFstudents$school_sp <- YSKTAFstudents$school_sp / 6
YSKTAFstudents$school_ph <- YSKTAFstudents$school_ph / 6
YSKTAFstudents$workplace_me <- YSKTAFstudents$workplace_me / 6
YSKTAFstudents$workplace_ve <- YSKTAFstudents$workplace_ve / 6
YSKTAFstudents$workplace_sp <- YSKTAFstudents$workplace_sp / 6
YSKTAFstudents$workplace_ph <- YSKTAFstudents$workplace_ph / 6

# Create the data set with replacement values
replacement_data <- read.csv(file = "labels_replace.csv",sep = ";")

# Replace values based on the replacement data set
for (i in 1:nrow(replacement_data)) {
  variable <- replacement_data[i, "variable"]
  old_value <- replacement_data[i, "tall"]
  new_value <- replacement_data[i, "tekst"]
  
  YSKTAFstudents[[variable]] <- ifelse(YSKTAFstudents[[variable]] == old_value, new_value, YSKTAFstudents[[variable]])
}

# replace NA with Median value of variable
YSKTAFstudents$visitfrequency[51]="2-3 ganger hvert semester"

# redefine of background variables
YSKTAFstudents$program <- as.factor(YSKTAFstudents$program)
YSKTAFstudents$year <- as.factor(YSKTAFstudents$year)
YSKTAFstudents$class <- as.factor(YSKTAFstudents$class)
YSKTAFstudents$company <- as.factor(YSKTAFstudents$company)
YSKTAFstudents$department <- as.factor(YSKTAFstudents$department)
YSKTAFstudents$gender <- as.factor(YSKTAFstudents$gender)
YSKTAFstudents$visitteacher <- as.factor(YSKTAFstudents$visitteacher)
YSKTAFstudents$visitfrequency <- as.factor(YSKTAFstudents$visitfrequency)

# remove flags from surveyxact
YSKTAFstudents$stato_1 <- NULL
YSKTAFstudents$stato_2 <- NULL
YSKTAFstudents$stato_3 <- NULL
YSKTAFstudents$stato_4 <- NULL
YSKTAFstudents$stato_5 <- NULL

# save data to new file
write.table(x=YSKTAFstudents, sep=";", file = "data_YSKTAFstudents.csv")

#clear used data
rm(i)
rm(reshufflelist)
rm(reverselist)
rm(namelist)
rm(new_value)
rm(old_value)
rm(variable)
rm(replacement_data)

####################################################
# Preliminary analysis
####################################################
# Statistical descriptions
describe(YSKTAFstudents)


# Cronbachs alpha
GSE_data <- YSKTAFstudents[,c("GSE_1", "GSE_2", "GSE_3", "GSE_4", "GSE_5", 
                    "GSE_6", "GSE_7", "GSE_8", "GSE_9", "GSE_10")]
school_me_data <- YSKTAFstudents[,c("school_me1", "school_me2", "school_me3", 
                          "school_me4", "school_me5", "school_me6")]
school_ve_data <- YSKTAFstudents[,c("school_ve1", "school_ve2", "school_ve3", 
                          "school_ve4", "school_ve5", "school_ve6")]
school_sp_data <- YSKTAFstudents[,c("school_sp1", "school_sp2", "school_sp3", 
                          "school_sp4", "school_sp5", "school_sp6")]
school_ph_data <- YSKTAFstudents[,c("school_ph1", "school_ph2", "school_ph3", 
                          "school_ph4", "school_ph5", "school_ph6")]
workplace_me_data <- YSKTAFstudents[,c("workplace_me1", "workplace_me2", "workplace_me3", 
                             "workplace_me4", "workplace_me5", "workplace_me6")]
workplace_ve_data <- YSKTAFstudents[,c("workplace_ve1", "workplace_ve2", "workplace_ve3", 
                             "workplace_ve4", "workplace_ve5", "workplace_ve6")]
workplace_sp_data <- YSKTAFstudents[,c("workplace_sp1", "workplace_sp2", "workplace_sp3", 
                             "workplace_sp4", "workplace_sp5", "workplace_sp6")]
workplace_ph_data <- YSKTAFstudents[,c("workplace_ph1", "workplace_ph2", "workplace_ph3", 
                             "workplace_ph4", "workplace_ph5", "workplace_ph6")]

psych::alpha(GSE_data)

psych::alpha(school_me_data)
psych::alpha(school_ve_data)
psych::alpha(school_sp_data)
psych::alpha(school_ph_data)

psych::alpha(workplace_me_data)
psych::alpha(workplace_ve_data)
psych::alpha(workplace_sp_data)
psych::alpha(workplace_ph_data)


# Transformation of data
YSKTAFstudents$sqrt_reversed_GSE <- sqrt(max(YSKTAFstudents$GSE) - YSKTAFstudents$GSE)

YSKTAFstudents$sqrt_reversed_school_me <- sqrt(max(YSKTAFstudents$school_me) - YSKTAFstudents$school_me)
YSKTAFstudents$sqrt_reversed_school_ve <- sqrt(max(YSKTAFstudents$school_ve) - YSKTAFstudents$school_ve)
YSKTAFstudents$sqrt_reversed_school_sp <- sqrt(max(YSKTAFstudents$school_sp) - YSKTAFstudents$school_sp)
YSKTAFstudents$sqrt_reversed_school_ph <- sqrt(max(YSKTAFstudents$school_ph) - YSKTAFstudents$school_ph)

YSKTAFstudents$sqrt_reversed_workplace_me <- sqrt(max(YSKTAFstudents$workplace_me) - YSKTAFstudents$workplace_me)
YSKTAFstudents$sqrt_reversed_workplace_ve <- sqrt(max(YSKTAFstudents$workplace_ve) - YSKTAFstudents$workplace_ve)
YSKTAFstudents$sqrt_reversed_workplace_sp <- sqrt(max(YSKTAFstudents$workplace_sp) - YSKTAFstudents$workplace_sp)
YSKTAFstudents$sqrt_reversed_workplace_ph <- sqrt(max(YSKTAFstudents$workplace_ph) - YSKTAFstudents$workplace_ph)

# Statistical descriptions
describe(YSKTAFstudents)
write.table(x=describe(YSKTAFstudents), sep=";", dec = ",", file = "descriptive_stat_YSKTAFstudents.csv")


####################################################
# Primary analysis
####################################################
# Hypothesis 1 - ME the most significant source of GSE
####################################################
# regression with GSE and all variables for sources in school and workplace
h1model_allvariables <- lm(sqrt_reversed_GSE ~ sqrt_reversed_school_me + sqrt_reversed_school_ve + 
                   sqrt_reversed_school_sp + sqrt_reversed_school_ph + 
                   sqrt_reversed_workplace_me + sqrt_reversed_workplace_ve + 
                   sqrt_reversed_workplace_sp + sqrt_reversed_workplace_ph, 
                 data = YSKTAFstudents)
summary(h1model_allvariables)
confint(h1model_allvariables)

# Calculate Cook's distances
h1_cooks_d <- cooks.distance(h1model_allvariables)

# Calculate threshold for Cook's distance
h1_threshold <- 4 / (length(h1_cooks_d)-9-1)
print(h1_threshold)

# Plot Cook's distances with threshold line
plot(h1_cooks_d, type = "h", main = "Cook's Distance for hver observasjon", 
     ylab = "Cook's Distance", xlab = "Observasjonsnummer")
abline(h = h1_threshold, col = "red", lty = 2)

# Identify observations with Cook's distance above the threshold
h1_influential_obs <- which(h1_cooks_d > h1_threshold)
print(h1_influential_obs)

# New data set without outliers
YSKTAFstudents_nooutliers <- YSKTAFstudents[-h1_influential_obs, ]

# New regression without outliers
h1model_nooutliers <- lm(
  formula = sqrt_reversed_GSE ~ sqrt_reversed_school_me + sqrt_reversed_school_ve + 
    sqrt_reversed_school_sp + sqrt_reversed_school_ph + sqrt_reversed_workplace_me + 
    sqrt_reversed_workplace_ve + sqrt_reversed_workplace_sp + sqrt_reversed_workplace_ph, 
  data = YSKTAFstudents_nooutliers)
summary(h1model_nooutliers)
confint(h1model_nooutliers)

# New models with standardized variables
h1model_allvariables_standardized <- lm(scale(sqrt_reversed_GSE) ~ 
                                          scale(sqrt_reversed_school_me) + 
                                          scale(sqrt_reversed_school_ve) + 
                                          scale(sqrt_reversed_school_sp) + 
                                          scale(sqrt_reversed_school_ph) + 
                                          scale(sqrt_reversed_workplace_me) + 
                                          scale(sqrt_reversed_workplace_ve) + 
                                          scale(sqrt_reversed_workplace_sp) + 
                                          scale(sqrt_reversed_workplace_ph), 
                                        data = YSKTAFstudents)
h1model_allvariables_no_outliers_standardized <- lm(scale(sqrt_reversed_GSE) ~ 
                                          scale(sqrt_reversed_school_me) + 
                                          scale(sqrt_reversed_school_ve) + 
                                          scale(sqrt_reversed_school_sp) + 
                                          scale(sqrt_reversed_school_ph) + 
                                          scale(sqrt_reversed_workplace_me) + 
                                          scale(sqrt_reversed_workplace_ve) + 
                                          scale(sqrt_reversed_workplace_sp) + 
                                          scale(sqrt_reversed_workplace_ph), 
                                        data = YSKTAFstudents_nooutliers)

# Results with standardized coefficients
summary(h1model_allvariables_standardized)
summary(h1model_allvariables_no_outliers_standardized)

####################################################
# Hypothesis 2a - Increased ME as year increases
####################################################
# Levene's tests
leveneTest(sqrt_reversed_school_me ~ year, data = YSKTAFstudents)
leveneTest(sqrt_reversed_workplace_me ~ year, data = YSKTAFstudents)

# One-way ANOVA
h2a_yearschoolMEtest <- aov(sqrt_reversed_school_me~year, data = YSKTAFstudents)
h2a_yearworkplaceMEtest <- aov(sqrt_reversed_workplace_me~year, data = YSKTAFstudents)
summary(h2a_yearschoolMEtest)
summary(h2a_yearworkplaceMEtest)

# Calculating the effect sizes
# Effect size eta-squared for visits by teacher
ss_between <- summary(h2a_yearschoolMEtest)[[1]][1, "Sum Sq"]  # Sum of squares between groups
ss_total <- sum(summary(h2a_yearschoolMEtest)[[1]][, "Sum Sq"])  # Total sum of squares
h2a_eta_squared_yearschoolMEtest <- ss_between / ss_total
print(h2a_eta_squared_yearschoolMEtest)

# Effect size eta-squared for visits by teacher
ss_between <- summary(h2a_yearworkplaceMEtest)[[1]][1, "Sum Sq"]  # Sum of squares between groups
ss_total <- sum(summary(h2a_yearworkplaceMEtest)[[1]][, "Sum Sq"])  # Total sum of squares
h2a_eta_squared_yearworkplaceMEtest <- ss_between / ss_total
print(h2a_eta_squared_yearworkplaceMEtest)

# post hoc-testing using Turkey's HSD 
h2a_tukey_school_me <- TukeyHSD(h2a_yearschoolMEtest)
h2a_tukey_workplace_me <- TukeyHSD(h2a_yearworkplaceMEtest)
print(h2a_tukey_school_me)
print(h2a_tukey_workplace_me)

####################################################
# Hypotesis 2b - Workplace_SP increased with visits by teacher
####################################################
# Levene's tests
leveneTest(workplace_sp ~ factor(visitfrequency), data = YSKTAFstudents)
leveneTest(workplace_sp ~ factor(visitteacher), data = YSKTAFstudents)

# ANOVA
h2b_anova_workplace_sp_visitteacher <- aov(workplace_sp ~ visitteacher, data = YSKTAFstudents)
h2b_anova_workplace_sp_visitfrequency <- aov(workplace_sp ~ visitfrequency, data = YSKTAFstudents)
summary(h2b_anova_workplace_sp_visitteacher)
summary(h2b_anova_workplace_sp_visitfrequency)

# Calculating the effect sizes
# Effect size eta-squared for visits by teacher
ss_between <- summary(h2b_anova_workplace_sp_visitteacher)[[1]][1, "Sum Sq"]  # Sum of squares between groups
ss_total <- sum(summary(h2b_anova_workplace_sp_visitteacher)[[1]][, "Sum Sq"])  # Total sum of squares
h2b_eta_squared_visitteacher <- ss_between / ss_total
print(h2b_eta_squared_visitteacher)

# Effect size eta-squared for visitfrequency
ss_between <- summary(h2b_anova_workplace_sp_visitfrequency)[[1]][1, "Sum Sq"]  # Sum of squares between groups
ss_total <- sum(summary(h2b_anova_workplace_sp_visitfrequency)[[1]][, "Sum Sq"])  # Total sum of squares
h2b_eta_squared_visitfrequency <- ss_between / ss_total
print(h2b_eta_squared_visitfrequency)

####################################################
# Hyoptesis 2c and 2d - VE increases with size of groups
####################################################
# Levene's tests
leveneTest(sqrt_reversed_school_ve ~ factor(class), data = YSKTAFstudents)
leveneTest(sqrt_reversed_workplace_ve ~ factor(company), data = YSKTAFstudents)
leveneTest(sqrt_reversed_workplace_ve ~ factor(department), data = YSKTAFstudents)

# ANOVA
h2c_anova_school_ve_class <- aov(sqrt_reversed_school_ve ~ class, data = YSKTAFstudents)
h2d_anova_workplace_ve_company <- aov(sqrt_reversed_workplace_ve ~ company, data = YSKTAFstudents)
h2d_anova_workplace_ve_department <- aov(sqrt_reversed_workplace_ve ~ department, data = YSKTAFstudents)
summary(h2c_anova_school_ve_class)
summary(h2d_anova_workplace_ve_company)
summary(h2d_anova_workplace_ve_department)

# Calculating the effect sizes
# Effect size eta-squared for school VE vs class size
ss_between <- summary(h2c_anova_school_ve_class)[[1]][1, "Sum Sq"]  # Sum of squares between groups
ss_total <- sum(summary(h2c_anova_school_ve_class)[[1]][, "Sum Sq"])  # Total sum of squares
h2c_eta_squared_school_ve_class <- ss_between / ss_total
print(h2c_eta_squared_school_ve_class)

# Effect size eta-squared for workplace VE vs company size
ss_between <- summary(h2d_anova_workplace_ve_company)[[1]][1, "Sum Sq"]  # Sum of squares between groups
ss_total <- sum(summary(h2d_anova_workplace_ve_company)[[1]][, "Sum Sq"])  # Total sum of squares
h2d_eta_squared_workplace_ve_company <- ss_between / ss_total
print(h2d_eta_squared_workplace_ve_company)

# Effect size eta-squared for workplace VE vs company size
ss_between <- summary(h2d_anova_workplace_ve_department)[[1]][1, "Sum Sq"]  # Sum of squares between groups
ss_total <- sum(summary(h2d_anova_workplace_ve_department)[[1]][, "Sum Sq"])  # Total sum of squares
h2d_eta_squared_workplace_ve_department <- ss_between / ss_total
print(h2d_eta_squared_workplace_ve_department)

# post hoc-testing using Turkey's HSD
h2c_tukey_school_ve <- TukeyHSD(h2c_anova_school_ve_class)
print(h2c_tukey_school_ve)

#clear used data
rm(ss_between)
rm(ss_total)

####################################################
# Hypothesis 3a-b-c about gender differences in GSE
####################################################
# New data set filtering genders and removing instances with "annet" (other, i.e. not boy or girl)
YSKTAFstudents_genders <- subset(YSKTAFstudents, gender %in% c("Gutt", "Jente"))

# Hypothesis 3a gender differences in GSE between genders
# Levene's test
leveneTest(GSE ~ gender, data = YSKTAFstudents_genders)

# t-test
t.test(GSE ~ gender, data = YSKTAFstudents_genders, var.equal = TRUE)

# Boxplot for all boys and girls
ggplot(YSKTAFstudents_genders, aes(x = gender, y = GSE)) +
  geom_boxplot(fill = c("lightblue", "pink")) +
  labs(title = "Boksdiagram generell mestringstro for gutter vs. jenter (hele utvalget)",
       x = "Kjonn", y = "Generell opplevd mestringstro") +
  theme_minimal()

# SD for GSE for each gender
print(tapply(YSKTAFstudents_genders$GSE, YSKTAFstudents_genders$gender, sd, na.rm = TRUE))

# Cohens d calculations
print(cohen.d(YSKTAFstudents_genders$GSE, YSKTAFstudents_genders$gender))

####################################################
# Hypothesis 3b gender differences in GSE between genders in male-dominated programs
####################################################
# Filter instances from male-dominated programs
YSKTAFstudents_genders$combined_program <- ifelse(YSKTAFstudents_genders$program %in% c("Bygg- og anleggsteknikk", "Elektro og datateknologi", "Teknologi- og industrifag"),
                                "Mannsdominert_fag", YSKTAFstudents_genders$program)

# Subset of male-dominated programs
YSKTAF_maledominated_programs <- subset(YSKTAFstudents_genders, combined_program == "Mannsdominert_fag")

# Number of instances of each gender for this subset
print(table(YSKTAF_maledominated_programs$gender))

# Levene's test
leveneTest(GSE ~ gender, data = YSKTAF_maledominated_programs)

# T-test
t.test(GSE ~ gender, data = YSKTAF_maledominated_programs, var.equal = TRUE, alternative = "less")

# Boxplot for male-dominated programs
ggplot(YSKTAF_maledominated_programs, aes(x = gender, y = GSE)) +
  geom_boxplot(fill = c("lightblue", "pink")) +
  labs(title = "Boksdiagram generell opplevd mestringstro for Gutter vs. Jenter (Mannsdominerte fag)",
       x = "Kjonn", y = "Generell opplevd mestringstro") +
  theme_minimal()

# SD for GSE for each gender
print(tapply(YSKTAF_maledominated_programs$GSE, YSKTAF_maledominated_programs$gender, sd, na.rm = TRUE))

# Cohens d calculations
print(cohen.d(YSKTAF_maledominated_programs$GSE, YSKTAF_maledominated_programs$gender))

####################################################
# Hypothesis 3c gender differences in GSE between genders in genderneutral and female-dominated programs
####################################################
# Filter instances from genderneutral and female-dominated programs
YSKTAFstudents_genders$combined_program <- ifelse(YSKTAFstudents_genders$program %in% c("Helse- og oppvekstfag", "Naturbruk", "Salg, service og reiseliv"),
                                         "Kjonnsnoytrale_fag", YSKTAFstudents_genders$combined_program)

# Subset of female-dominated and genderneutral programs
YSKTAF_female_neutral_programs <- subset(YSKTAFstudents_genders, combined_program == "Kjonnsnoytrale_fag")

# Number of instances of each gender for this subset
print(table(YSKTAF_female_neutral_programs$gender))

# Levene's test
leveneTest(GSE ~ gender, data = YSKTAF_female_neutral_programs)

# T-test
t.test(GSE ~ gender, data = YSKTAF_female_neutral_programs, var.equal = TRUE)

# Boxplot for genderneutral and female-dominated programs
ggplot(YSKTAF_female_neutral_programs, aes(x = gender, y = GSE)) +
  geom_boxplot(fill = c("lightblue", "pink")) +
  labs(title = "Boksdiagram generell opplevd mestringstro for Gutter vs. Jenter (Kjonnsnoytrale/kvinnedominerte fag)",
       x = "Kjonn", y = "Generell opplevd mestringstro") +
  theme_minimal()

# SD for GSE for each gender
print(tapply(YSKTAF_female_neutral_programs$GSE, YSKTAF_female_neutral_programs$gender, sd, na.rm = TRUE))

# Cohens d calculations
print(cohen.d(YSKTAF_female_neutral_programs$GSE, YSKTAF_female_neutral_programs$gender))

