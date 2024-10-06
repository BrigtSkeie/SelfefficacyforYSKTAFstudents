# Her er ferdig skript for Masteroppgave om YSK/TAF

# description of variables

# Loading libraries
library(psych) # statistiske beskrivelser
library(car) # Levenes test
library(ggplot2) # Boksdiagram

# Reading data

# load raw data
raw_data <- read.csv(file = "dataset_raw.csv", sep=";")

# Data preparation

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

# Preliminary analysis
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
const <- max(YSKTAFstudents$GSE) + 1
YSKTAFstudents$sqrt_reversed_GSE <- sqrt(const - YSKTAFstudents$GSE)

const <- max(YSKTAFstudents$school_me) + 1
const_wp <- max(YSKTAFstudents$workplace_me) + 1
YSKTAFstudents$sqrt_reversed_school_me <- sqrt(const - YSKTAFstudents$school_me)
YSKTAFstudents$sqrt_reversed_workplace_me <- sqrt(const_wp - YSKTAFstudents$workplace_me)

const <- max(YSKTAFstudents$school_ve) + 1
const_wp <- max(YSKTAFstudents$workplace_ve) + 1
YSKTAFstudents$sqrt_reversed_school_ve <- sqrt(const - YSKTAFstudents$school_ve)
YSKTAFstudents$sqrt_reversed_workplace_ve <- sqrt(const_wp - YSKTAFstudents$workplace_ve)

const <- max(YSKTAFstudents$school_sp) + 1
const_wp <- max(YSKTAFstudents$workplace_sp) + 1
YSKTAFstudents$sqrt_reversed_school_sp <- sqrt(const - YSKTAFstudents$school_sp)
YSKTAFstudents$sqrt_reversed_workplace_sp <- sqrt(const_wp - YSKTAFstudents$workplace_sp)

const <- max(YSKTAFstudents$school_ph) + 1
const_wp <- max(YSKTAFstudents$workplace_ph) + 1
YSKTAFstudents$sqrt_reversed_school_ph <- sqrt(const - YSKTAFstudents$school_ph)
YSKTAFstudents$sqrt_reversed_workplace_ph <- sqrt(const_wp - YSKTAFstudents$workplace_ph)

# remove temporary variables
rm(const)
rm(const_wp)

# Statistical descriptions
describe(YSKTAFstudents)
write.table(x=describe(YSKTAFstudents), sep=";", dec = ",", file = "descriptive_stat_YSKTAFstudents.csv")


# Primary analysis
# Hypotesis 1 - ME the most significant source of GSE

# simple regression of ME vs GSE
summary(lm(sqrt_reversed_GSE ~ sqrt_reversed_school_me, data = YSKTAFstudents))
summary(lm(sqrt_reversed_GSE ~ sqrt_reversed_workplace_me, data = YSKTAFstudents))

# regression of school variables
summary(lm(sqrt_reversed_GSE ~ sqrt_reversed_school_me + sqrt_reversed_school_ve + 
             sqrt_reversed_school_sp + sqrt_reversed_school_ph, data = YSKTAFstudents))
# regression of workplace variables
summary(lm(sqrt_reversed_GSE ~ sqrt_reversed_workplace_me + sqrt_reversed_workplace_ve + 
             sqrt_reversed_workplace_sp + sqrt_reversed_workplace_ph, data = YSKTAFstudents))

# regression of both school and workplace variables
summary(lm(sqrt_reversed_GSE ~ sqrt_reversed_school_me + sqrt_reversed_school_ve + 
                          sqrt_reversed_school_sp + sqrt_reversed_school_ph + 
                          sqrt_reversed_workplace_me + sqrt_reversed_workplace_ve + 
                          sqrt_reversed_workplace_sp + sqrt_reversed_workplace_ph, 
                        data = YSKTAFstudents))

# Hypotesis 2a - Increased ME as year increases
# Levenes tests
leveneTest(sqrt_reversed_school_me ~ year, data = YSKTAFstudents)
leveneTest(sqrt_reversed_workplace_me ~ year, data = YSKTAFstudents)

# One-way ANOVA
yearschoolMEtest <- aov(school_me~year, data = YSKTAFstudents)
yearworkplaceMEtest <- aov(workplace_me~year, data = YSKTAFstudents)
summary(yearschoolMEtest)
summary(yearworkplaceMEtest)
plot(yearschoolMEtest)


oneway.test(school_me~year, data = YSKTAFstudents)
oneway.test(workplace_me~year, data = YSKTAFstudents)

#post hoc-testing using Turkey's HSD 
tukey_school_me <- TukeyHSD(yearschoolMEtest)
tukey_workplace_me <- TukeyHSD(yearworkplaceMEtest)
print(tukey_school_me)
print(tukey_workplace_me)

# Hypotesis 2b - Workplace_SP increased with visits by teacher
# Levene's tests
leveneTest(workplace_sp ~ factor(visitfrequency), data = YSKTAFstudents)
leveneTest(workplace_sp ~ factor(visitteacher), data = YSKTAFstudents)

# ANOVA
anova_workplace_sp_visitteacher <- aov(workplace_sp ~ visitteacher, data = YSKTAFstudents)
anova_workplace_sp_visitfrequency <- aov(workplace_sp ~ visitfrequency, data = YSKTAFstudents)
summary(anova_workplace_sp_visitteacher)
summary(anova_workplace_sp_visitfrequency)


# Hyoptesis 2c - VE increases with size of groups
# Levene's tests
leveneTest(sqrt_reversed_school_ve ~ factor(class), data = YSKTAFstudents)
leveneTest(sqrt_reversed_workplace_ve ~ factor(company), data = YSKTAFstudents)
leveneTest(sqrt_reversed_workplace_ve ~ factor(department), data = YSKTAFstudents)

# ANOVA
anova_school_ve_class <- aov(sqrt_reversed_school_ve ~ class, data = YSKTAFstudents)
anova_workplace_ve_company <- aov(sqrt_reversed_workplace_ve ~ company, data = YSKTAFstudents)
anova_workplace_ve_department <- aov(sqrt_reversed_workplace_ve ~ department, data = YSKTAFstudents)
summary(anova_school_ve_class)
summary(anova_workplace_ve_company)
summary(anova_workplace_ve_department)

# two-way ANOVA for workplace VE
anova_workplace_ve_company_department <- aov(sqrt_reversed_workplace_ve ~ company * department, data = YSKTAFstudents)
summary(anova_workplace_ve_company_department)

#post hoc-testing using Turkey's HSD 
tukey_school_ve <- TukeyHSD(anova_school_ve_class)
print(tukey_school_ve)


# Hypoteser om kjonnsforskjeller

# Nytt datasett ekskluder deltakere som er merket som "annet" i gender-variabelen
YSKTAFstudents_genders <- subset(YSKTAFstudents, gender %in% c("Gutt", "Jente"))

# Hypotese 3a kjønnsforskjeller i GSE mellom kjønnene
# Levene's test for lik varians mellom gutter og jenter i hele utvalget
leveneTest(GSE ~ gender, data = YSKTAFstudents_genders)

# Utfør en tosidig t-test for hele utvalget
t.test(GSE ~ gender, data = YSKTAFstudents_genders, var.equal = TRUE)  # eller var.equal = FALSE basert på Levene's test

# Boxplot for all boys and girls
ggplot(YSKTAFstudents_genders, aes(x = gender, y = GSE)) +
  geom_boxplot(fill = c("lightblue", "pink")) +
  labs(title = "Boksdiagram generell mestringstro for gutter vs. jenter (hele utvalget)",
       x = "Kjonn", y = "Generell opplevd mestringstro") +
  theme_minimal()



# Hypotese 3b kjønnsforskjeller i GSE innenfor mannsdominerte fag
# Kombiner Bygg- og anleggsteknikk med Teknologi- og industrifag
YSKTAFstudents_genders$combined_program <- ifelse(YSKTAFstudents_genders$program %in% c("Bygg- og anleggsteknikk", "Elektro og datateknologi", "Teknologi- og industrifag"),
                                "Mannsdominert_fag", YSKTAFstudents_genders$program)

# Levene's test for mannsdominerte fag (H3b)
leveneTest(GSE ~ gender, data = subset(YSKTAFstudents_genders, combined_program == "Mannsdominert_fag"))

# Boksdiagram for mannsdominerte fag
ggplot(subset(YSKTAFstudents_genders, combined_program == "Mannsdominert_fag"), aes(x = gender, y = GSE)) +
  geom_boxplot(fill = c("lightblue", "pink")) +
  labs(title = "Boksdiagram generell opplevd mestringstro for Gutter vs. Jenter (Mannsdominerte fag)",
       x = "Kjonn", y = "Generell opplevd mestringstro") +
  theme_minimal()

# Hypotese 3c kjønnsforskjeller i GSE innenfor kvinnedominerte og kjønnsnøytrale fag
# Kombiner Helse- og oppvekstfag, Naturbruk og Salg, service og reiseliv
YSKTAFstudents_genders$combined_program <- ifelse(YSKTAFstudents_genders$program %in% c("Helse- og oppvekstfag", "Naturbruk", "Salg, service og reiseliv"),
                                         "Kjonnsnoytrale_fag", YSKTAFstudents_genders$combined_program)

# Levene's test for kjønnsnøytrale/kvinnedominerte fag (H3c)
leveneTest(GSE ~ gender, data = subset(YSKTAFstudents_genders, combined_program == "Kjonnsnoytrale_fag"))

# Boksdiagram for kjønnsnøytrale/kvinnedominerte fag
ggplot(subset(YSKTAFstudents_genders, combined_program == "Kjonnsnoytrale_fag"), aes(x = gender, y = GSE)) +
  geom_boxplot(fill = c("lightblue", "pink")) +
  labs(title = "Boksdiagram generell opplevd mestringstro for Gutter vs. Jenter (Kjonnsnoytrale/kvinnedominerte fag)",
       x = "Kjonn", y = "Generell opplevd mestringstro") +
  theme_minimal()


