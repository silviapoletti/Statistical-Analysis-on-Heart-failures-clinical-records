
# UPLOAD DATASET
# --------------

Heart <- read.csv("data/heart_failure_clinical_records_dataset.csv", sep=",")
Heart <- na.omit(Heart) # no missing values

dim(Heart) 
n <- dim(Heart)[1]      # sample size
d <- dim(Heart)[2] - 1  # number of features

colnames(Heart)

attach(Heart)

# unbalanced dataset
sum(DEATH_EVENT==1)/length(DEATH_EVENT)  # proportion of survived patients
sum(DEATH_EVENT==0)/length(DEATH_EVENT)  # proportion of dead patients


# CORRELATION WITH THE RESPONSE
# -----------------------------

library(ellipse)
plotcorr(cor(Heart))


# AGE AND DEATH
# higher risk for old age
cor(DEATH_EVENT, age)                  

death_age <-matrix(c(length(DEATH_EVENT[age<=70&DEATH_EVENT==0]),length(DEATH_EVENT[age<=70&DEATH_EVENT==1]),
                     length(DEATH_EVENT[age>70&DEATH_EVENT==0]),length(DEATH_EVENT[age>70&DEATH_EVENT==1]) ),2)
rownames(death_age) <- c("alive","dead")
colnames(death_age) <- c("40-70",">70")
death_age
margin_age <- margin.table(death_age, 2) 

# Probability to die given you are more than 70:
death_age["dead",">70"]/margin_age[">70"]         
# Probability to die given you are less than 70:
death_age["dead","40-70"]/margin_age["40-70"]     
mosaicplot(death_age)



# SERUM CREATININE
# higher risk for high Creatinine
cor(DEATH_EVENT,serum_creatinine)      

# males   normal range: (0.7, 1.2)
# females normal range: (0.5, 1.0)

# Sex and Creatinine:
sex_creatinine <-matrix(c(length(sex[serum_creatinine>=1&sex==0]),length(sex[serum_creatinine>=1.2&sex==1]),
                          length(sex[serum_creatinine<1&sex==0]),length(sex[serum_creatinine<1.2&sex==1])),2)
colnames(sex_creatinine) <- c("high creatinine","normal creatinine")
rownames(sex_creatinine) <- c("female","male")
sex_creatinine

death_creatinine_sex <-matrix(c(length(sex[serum_creatinine>=1&sex==0&DEATH_EVENT==1]),length(sex[serum_creatinine>=1.2&sex==1&DEATH_EVENT==1]),
                                length(sex[serum_creatinine<1&sex==0&DEATH_EVENT==1]),length(sex[serum_creatinine<1.2&sex==1&DEATH_EVENT==1])),2)
colnames(death_creatinine_sex) <- c("high creatinine","normal creatinine")
rownames(death_creatinine_sex) <- c("female","male")
death_creatinine_sex

# Proportion of people with high creatinine that died:
death_creatinine_sex[,"high creatinine"]/sex_creatinine[,"high creatinine"]      
# Proportion of people with normal creatinine that died:        
death_creatinine_sex[,"normal creatinine"]/sex_creatinine[,"normal creatinine"] 



# SERUM SODIUM
# higher risk for low Sodium
cor(DEATH_EVENT,serum_sodium)        

death_sodium <-matrix(c(length(DEATH_EVENT[serum_sodium<135&DEATH_EVENT==0]),length(DEATH_EVENT[serum_sodium<135&DEATH_EVENT==1]),
                        length(DEATH_EVENT[serum_sodium>=135&DEATH_EVENT==0]),length(DEATH_EVENT[serum_sodium>=135&DEATH_EVENT==1]) ),2)
rownames(death_sodium) <- c("alive","dead")
colnames(death_sodium) <- c("low sodium","normal sodium")
death_sodium
margin_sodium <- margin.table(death_sodium, 2) 

# Probability to die given you have low sodium:
death_sodium["dead","low sodium"]/margin_sodium["low sodium"]            
# Probability to die given you have normal sodium:
death_sodium["dead","normal sodium"]/margin_sodium["normal sodium"]    
mosaicplot(death_sodium, sort=2:1)



# EJECTION FRACTION
# higher risk for low Ejection Fraction
cor(DEATH_EVENT, ejection_fraction)    

# Low Ejection Fraction (<50) and Death
death_ef <-matrix(c(length(DEATH_EVENT[ejection_fraction<50&DEATH_EVENT==0]),length(DEATH_EVENT[ejection_fraction<50&DEATH_EVENT==1]),
                    length(DEATH_EVENT[ejection_fraction>=50&DEATH_EVENT==0]),length(DEATH_EVENT[ejection_fraction>=50&DEATH_EVENT==1]) ),2)
rownames(death_ef) <- c("alive","dead")
colnames(death_ef) <- c("low ef","normal ef")
death_ef
margin_ef <- margin.table(death_ef, 2) 

# Probability to die given you have low ef:
death_ef["dead","low ef"]/margin_ef["low ef"]           
# Probability to die given you have normal ef:
death_ef["dead","normal ef"]/margin_ef["normal ef"]     
mosaicplot(death_ef, sort=2:1)



# TIME
# low time means patient died before the ending of observation period
cor(DEATH_EVENT,time)                  

mean(time[DEATH_EVENT==1])   
mean(time[DEATH_EVENT==0])   
sum(time[DEATH_EVENT==1]<=90)/length(time[DEATH_EVENT==1]) 
sum(time[DEATH_EVENT==1]>90)/length(time[DEATH_EVENT==1])  



# CORRELATION BETWEEN FEATURES
# ----------------------------

# Sex is unbalanced because the number of males are two times the number of females in our dataset:
length(sex[sex==1])/length(sex)     
length(sex[sex==0])/length(sex)   


# SEX AND SMOKING
sex_smoking <-matrix(c(length(sex[smoking==0&sex==0]),length(sex[smoking==0&sex==1]),
                       length(sex[smoking==1&sex==0]),length(sex[smoking==1&sex==1]) ),2, byrow = TRUE)
rownames(sex_smoking) <- c("non-smoker","smoker")
colnames(sex_smoking) <- c("female","male")
sex_smoking
margin_sex <- margin.table(sex_smoking, 2) 

# Probability to be a smoker given you're male:
sex_smoking["smoker","male"]/margin_sex["male"]      
# Probability to be a smoker given you're female:
sex_smoking["smoker","female"]/margin_sex["female"]  
mosaicplot(sex_smoking, sort=2:1)



# SEX AND DIABETES
sex_diabetes <-matrix(c(length(sex[diabetes==0&sex==0]),length(sex[diabetes==0&sex==1]),
                        length(sex[diabetes==1&sex==0]),length(sex[diabetes==1&sex==1]) ),2, byrow = TRUE)
rownames(sex_diabetes) <- c("no diabetes","diabetes")
colnames(sex_diabetes) <- c("female","male")
sex_diabetes
margin_sex <- margin.table(sex_diabetes, 2) 

# Probability to have diabetes given you're male:
sex_diabetes["diabetes","male"]/margin_sex["male"]     
# Probability to have diabetes given you're female:
sex_diabetes["diabetes","female"]/margin_sex["female"] 
mosaicplot(sex_diabetes, sort=2:1)


# DIABETES IN FEMALES MAY LEAD TO DEATH MORE THAN IN MALES
sex_diabetes_death <-matrix(c(length(sex[DEATH_EVENT==1&sex==0&diabetes==0]),length(sex[DEATH_EVENT==1&sex==1&diabetes==0]),
                              length(sex[DEATH_EVENT==1&sex==0&diabetes==1]),length(sex[DEATH_EVENT==1&sex==1&diabetes==1]) ),2, byrow = TRUE)
rownames(sex_diabetes_death) <- c("no diabetes","diabetes")
colnames(sex_diabetes_death) <- c("female","male")

# Probability to die given you have diabetes:
sex_diabetes_death["diabetes",]/sex_diabetes["diabetes",]            



# SEX AND ANEMIA
sex_anaemia <-matrix(c(length(sex[anaemia==0&sex==0]),length(sex[anaemia==0&sex==1]),
                       length(sex[anaemia==1&sex==0]),length(sex[anaemia==1&sex==1]) ),2, byrow = TRUE)
rownames(sex_anaemia) <- c("no anaemia","anaemia")
colnames(sex_anaemia) <- c("female","male")
sex_anaemia
margin_sex <- margin.table(sex_anaemia, 2) 

# Probability to have anaemia given you're male:
sex_anaemia["anaemia","male"]/margin_sex["male"]      # 0.3969072 (lower)
# Probability to have anaemia given you're female:
sex_anaemia["anaemia","female"]/margin_sex["female"]  # 0.4952381 
mosaicplot(sex_anaemia, sort=2:1)



# BLOOD PRESSURE IS HIGHER IN FEMALES (After age 40) 

# Sex and Age:
sex_age <-matrix(c(length(sex[age<=50&sex==0]),length(sex[age<=50&sex==1]),
                   length(sex[age>50&age<=58&sex==0]),length(sex[age>50&age<=58&sex==1]),
                   length(sex[age>58&age<=64&sex==0]),length(sex[age>58&age<=64&sex==1]),
                   length(sex[age>64&age<=70&sex==0]),length(sex[age>64&age<=70&sex==1]),
                   length(sex[age>70&sex==0]),length(sex[age>70&sex==1])),2)
colnames(sex_age) <- c("40-50","50-58","58-64","64-70",">70")
rownames(sex_age) <- c("female","male")
sex_age

# Pressure and Age:
pressure_sex_age <-matrix(c(length(sex[age<=50&sex==0&high_blood_pressure==1]),length(sex[age<=50&sex==1&high_blood_pressure==1]),
                            length(sex[age>50&age<=58&sex==0&high_blood_pressure==1]),length(sex[age>50&age<=58&sex==1&high_blood_pressure==1]),
                            length(sex[age>58&age<=64&sex==0&high_blood_pressure==1]),length(sex[age>58&age<=64&sex==1&high_blood_pressure==1]),
                            length(sex[age>64&age<=70&sex==0&high_blood_pressure==1]),length(sex[age>64&age<=70&sex==1&high_blood_pressure==1]),
                            length(sex[age>70&sex==0&high_blood_pressure==1]),length(sex[age>70&sex==1&high_blood_pressure==1])),2)
colnames(pressure_sex_age) <- c("40-50","50-58","58-64","64-70",">70")
rownames(pressure_sex_age) <- c("female","male")
pressure_sex_age

# Proportion of males having high blood pressure:
pressure_sex_age["male",]/sex_age["male",]          
# Proportion of females having high blood pressure:
pressure_sex_age["female",]/sex_age["female",]   



# SMOKING MAY LEAD TO HIGH PRESSURE (for females)
sex_smoke_pressure <-matrix(c(length(sex[smoking==0&sex==0&high_blood_pressure==1]),length(sex[smoking==0&sex==1&high_blood_pressure==1]),
                              length(sex[smoking==1&sex==0&high_blood_pressure==1]),length(sex[smoking==1&sex==1&high_blood_pressure==1]) ),2, byrow = TRUE)
rownames(sex_smoke_pressure) <- c("non-smoker","smoker")
colnames(sex_smoke_pressure) <- c("female","male")

# Probability to have high pressure given you're smoker:
sex_smoke_pressure["smoker",]/sex_smoking["smoker",]        
# Probability to have high pressure given you're not smoker:
sex_smoke_pressure["non-smoker",]/sex_smoking["non-smoker",] 



# DIABETES MAY LEAD TO HIGH CREATININE
sex_diabetes_creatinine <-matrix(c(length(sex[serum_creatinine>=1&sex==0&diabetes==0]),length(sex[serum_creatinine>=1.2&sex==1&diabetes==0]),
                                   length(sex[serum_creatinine>=1&sex==0&diabetes==1]),length(sex[serum_creatinine>=1.2&sex==1&diabetes==1]) ),2, byrow = TRUE)
rownames(sex_diabetes_creatinine) <- c("no diabetes","diabetes")
colnames(sex_diabetes_creatinine) <- c("female","male")

# Probability to have high creatinine given you have diabetes:
sex_diabetes_creatinine["diabetes",]/sex_diabetes["diabetes",]        
# Probability to have high creatinine given you're not smoker:
sex_diabetes_creatinine["no diabetes",]/sex_diabetes["no diabetes",] 



# LOW EJECTION FRACTION MAY LEAD TO HIGH CREATININE
sex_ef_creatinine <-matrix(c(length(sex[serum_creatinine>=1&sex==0&ejection_fraction<50]),length(sex[serum_creatinine>=1.2&sex==1&ejection_fraction<50]),
                             length(sex[serum_creatinine>=1&sex==0&ejection_fraction>=50]),length(sex[serum_creatinine>=1.2&sex==1&ejection_fraction>=50]) ),2, byrow = TRUE)
rownames(sex_ef_creatinine) <- c("low ef","normal ef")
colnames(sex_ef_creatinine) <- c("female","male")

# Sex and Ejection fraction:
sex_ef <-matrix(c(length(sex[ejection_fraction<50&sex==0]),length(sex[ejection_fraction<50&sex==1]),
                  length(sex[ejection_fraction>=50&sex==0]),length(sex[ejection_fraction>=50&sex==1]) ),2, byrow = TRUE)
rownames(sex_ef) <- c("low ef","normal ef")
colnames(sex_ef) <- c("female","male")

# Probability to have high creatinine given you have low ef:
sex_ef_creatinine["low ef",]/sex_ef["low ef",]             
# Probability to have high creatinine given you have normal ef:
sex_ef_creatinine["normal ef",]/sex_ef["normal ef",]   



# PAIR PLOT FOR CONTINUOUS VARIABLES
# ----------------------------------

## panel.hist function puts histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)
}

## panel.cor function puts (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


colors <- c('deeppink', 'green')[unclass(as.factor(DEATH_EVENT))]
pairs(Heart[,-c(2,4,6,10,11,13)], col=colors, diag.panel=panel.hist, upper.panel=panel.cor, lower.panel=panel.smooth)



# PREPROCESSING
# -------------

# Categorical variables as factors:
Heart$anaemia <- as.factor(Heart$anaemia)
Heart$diabetes <- as.factor(Heart$diabetes)
Heart$high_blood_pressure <- as.factor(Heart$high_blood_pressure)
Heart$sex <- as.factor(Heart$sex)
Heart$smoking <- as.factor(Heart$smoking)

# Split in train and test set:
set.seed(1)
array <- sample(c(TRUE,FALSE),n,replace=TRUE,prob=c(0.75,0.25))
train <- Heart[array,]
test <- Heart[!array,]

dim(train)[1] 
dim(test)[1] 

# we have more or less the same proportion of deaths in both sets:
sum(train$DEATH_EVENT==1)/dim(train)[1]
sum(test$DEATH_EVENT==1)/dim(test)[1]


# LINEAR REGRESSION VS LOGISTIC REGRESSION
# ----------------------------------------

# Inverse transformation for computing pi_i in case of just one regressor:
inv.logit <- function(beta0, beta1, x) {
  y <- exp(beta0+beta1*x)
  return(y/(1+y))
}

regressor <- train$time

mod.out <- lm(DEATH_EVENT~ regressor, data=train)                        # Linear Regression
logit.out <- glm(DEATH_EVENT~ regressor, data=train, family = binomial)  # Logistic Regression

plot(regressor, train$DEATH_EVENT, pch=20, xlim=c(min(regressor), max(regressor)), ylim=c(-.25, 1.25))
x <- seq(min(regressor), max(regressor), length=1000)
y <- inv.logit(coefficients(logit.out)[1], coefficients(logit.out)[2], x)
lines(x, y, col="blue", lwd=1.5)             # logistic regression curve
lines(x,rep(0.5,1000),col="gray", lwd=1.5)   # threshold
abline(mod.out, col="red", lwd=1.5)          # linear regression line 



# OUTLIERS AND HIGH LEVERAGE POINTS
# ---------------------------------

# CREATININE PHOSPHOKINASE 
logit.out <- glm(DEATH_EVENT~ creatinine_phosphokinase, data=Heart, family = binomial)
plot(creatinine_phosphokinase, DEATH_EVENT, pch=20, xlim=c(min(creatinine_phosphokinase), max(creatinine_phosphokinase)), ylim=c(-.25, 1.25))

x <- seq(min(creatinine_phosphokinase), max(creatinine_phosphokinase), length=1000)
y <- inv.logit(coefficients(logit.out)[1], coefficients(logit.out)[2], x)
lines(x, y, col="black", lwd=2.5)

data = Heart[creatinine_phosphokinase>=3000,]
text(data$DEATH_EVENT~ data$creatinine_phosphokinase, data=data, labels=rownames(data), cex=0.9, font=2, pos=3)

# High Leverage Points
#######################

# sample 2 (7861):
restricted_dataset <- Heart[-2,]
regressor <- restricted_dataset$creatinine_phosphokinase
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="red", lwd=2.5)

# sample 61 (7702):
restricted_dataset <- Heart[-61,]
regressor <- restricted_dataset$creatinine_phosphokinase
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="green", lwd=2.5)

# sample 73 (5882):
restricted_dataset <- Heart[-73,]
regressor <- restricted_dataset$creatinine_phosphokinase
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="blue", lwd=2.5)

# sample 104 (5209):
restricted_dataset <- Heart[-104,]
regressor <- restricted_dataset$creatinine_phosphokinase
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="deeppink", lwd=2.5)

# sample 135 (4540):
restricted_dataset <- Heart[-135,]
regressor <- restricted_dataset$creatinine_phosphokinase
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="orange", lwd=2.5)

# sample 172 (3966):
restricted_dataset <- Heart[-172,]
regressor <- restricted_dataset$creatinine_phosphokinase
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="cyan", lwd=2.5)

# sample 53 (3964):
restricted_dataset <- Heart[-53,]
regressor <- restricted_dataset$creatinine_phosphokinase
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="violet", lwd=2.5)



# PLATELETS
logit.out <- glm(DEATH_EVENT~ platelets, data=Heart, family = binomial)
plot(platelets, DEATH_EVENT, pch=20, xlim=c(min(platelets), max(platelets)), ylim=c(-.25, 1.25))

x <- seq(min(platelets), max(platelets), length=1000)
y <- inv.logit(coefficients(logit.out)[1], coefficients(logit.out)[2], x)
lines(x, y, col="black", lwd=2.5)

data = Heart[platelets<=60000,]
text(data$DEATH_EVENT~ data$platelets, data=data, labels=rownames(data), cex=0.9, font=2, pos=3)
data = Heart[platelets>=600000,]
text(data$DEATH_EVENT~ data$platelets, data=data, labels=rownames(data), cex=0.9, font=2, pos=3)

# Outliers:
###########

# sample 278 (25100):
restricted_dataset <- Heart[-278,]
regressor <- restricted_dataset$platelets
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="red", lwd=2.5)

# sample 16 (47000):
restricted_dataset <- Heart[-47000,]
regressor <- restricted_dataset$platelets
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="green", lwd=2.5)

# sample 282 (51000):
restricted_dataset <- Heart[-282,]
regressor <- restricted_dataset$platelets
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="blue", lwd=2.5)

# High Leverage Points:
#######################
plot(platelets, DEATH_EVENT, pch=20, xlim=c(min(platelets), max(platelets)), ylim=c(-.25, 1.25))
y <- inv.logit(coefficients(logit.out)[1], coefficients(logit.out)[2], x)
lines(x, y, col="black", lwd=2.5)
data = Heart[platelets<=60000,]
text(data$DEATH_EVENT~ data$platelets, data=data, labels=rownames(data), cex=0.9, font=2, pos=3)
data = Heart[platelets>=600000,]
text(data$DEATH_EVENT~ data$platelets, data=data, labels=rownames(data), cex=0.9, font=2, pos=3)

# sample 110 (850000):
restricted_dataset <- Heart[-110,]
regressor <- restricted_dataset$platelets
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="orange", lwd=2.5)

# sample 297 (742000):
restricted_dataset <- Heart[-297,]
regressor <- restricted_dataset$platelets
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="deeppink", lwd=2.5)

# sample 106 (621000):
restricted_dataset <- Heart[-106,]
regressor <- restricted_dataset$platelets
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="blue", lwd=2.5)



# SERUM CREATININE
logit.out <- glm(DEATH_EVENT~ serum_creatinine, data=Heart, family = binomial)
plot(serum_creatinine, DEATH_EVENT, pch=20, xlim=c(min(serum_creatinine), max(serum_creatinine)), ylim=c(-.25, 1.25))

x <- seq(min(serum_creatinine), max(serum_creatinine), length=1000)
y <- inv.logit(coefficients(logit.out)[1], coefficients(logit.out)[2], x)
lines(x, y, col="black", lwd=2.5)

data = Heart[serum_creatinine>=5,]
text(data$DEATH_EVENT~ data$serum_creatinine, data=data, labels=rownames(data), cex=0.9, font=2, pos=3)

# Outliers:
###########

# sample 10 (9.4):
restricted_dataset <- Heart[-10,]
regressor <- restricted_dataset$serum_creatinine
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="blue", lwd=2.5)

# sample 218 (9.0):
restricted_dataset <- Heart[-218,]
regressor <- restricted_dataset$serum_creatinine
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="deeppink", lwd=2.5)

# sample 53 (6.8):
restricted_dataset <- Heart[-53,]
regressor <- restricted_dataset$serum_creatinine
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="red", lwd=2.5)

# sample 29 (5.8):
restricted_dataset <- Heart[-29,]
regressor <- restricted_dataset$serum_creatinine
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="violet", lwd=2.5)

# High Leverage Points:
#######################
plot(serum_creatinine, DEATH_EVENT, pch=20, xlim=c(min(serum_creatinine), max(serum_creatinine)), ylim=c(-.25, 1.25))
y <- inv.logit(coefficients(logit.out)[1], coefficients(logit.out)[2], x)
lines(x, y, col="black", lwd=2.5)
data = Heart[serum_creatinine>=5,]
text(data$DEATH_EVENT~ data$serum_creatinine, data=data, labels=rownames(data), cex=0.9, font=2, pos=3)

# sample 132 (6.1):
restricted_dataset <- Heart[-132,]
regressor <- restricted_dataset$serum_creatinine
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="green", lwd=2.5)

# sample 229 (5.0):
restricted_dataset <- Heart[-229,]
regressor <- restricted_dataset$serum_creatinine
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="blue", lwd=2.5)



# SERUM SODIUM
logit.out <- glm(DEATH_EVENT~ serum_sodium, data=Heart, family = binomial)
plot(serum_sodium, DEATH_EVENT, pch=20, xlim=c(min(serum_sodium), max(serum_sodium)), ylim=c(-.25, 1.25))

x <- seq(min(serum_sodium), max(serum_sodium), length=1000)
y <- inv.logit(coefficients(logit.out)[1], coefficients(logit.out)[2], x)
lines(x, y, col="black", lwd=2.5)

data = Heart[serum_sodium<125,]
text(data$DEATH_EVENT~ data$serum_sodium, data=data, labels=rownames(data), cex=0.9, font=2, pos=3)

# High Leverage Points:
#######################

# sample 200 (113):
restricted_dataset <- Heart[-200,]
regressor <- restricted_dataset$serum_sodium
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="blue", lwd=2.5)

# Outliers:
###########

plot(serum_sodium, DEATH_EVENT, pch=20, xlim=c(min(serum_sodium), max(serum_sodium)), ylim=c(-.25, 1.25))
y <- inv.logit(coefficients(logit.out)[1], coefficients(logit.out)[2], x)
lines(x, y, col="black", lwd=2.5)
data = Heart[serum_sodium<125,]
text(data$DEATH_EVENT~ data$serum_sodium, data=data, labels=rownames(data), cex=0.9, font=2, pos=3)

# sample 5 (116):
restricted_dataset <- Heart[-5,]
regressor <- restricted_dataset$serum_sodium
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="deeppink", lwd=2.5)

# sample 20 (121):
restricted_dataset <- Heart[-20,]
regressor <- restricted_dataset$serum_sodium
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="green", lwd=2.5)

# sample 127 (124):
restricted_dataset <- Heart[-127,]
regressor <- restricted_dataset$serum_sodium
logit.restricted <- glm(DEATH_EVENT~ regressor, data=restricted_dataset, family = binomial)
y <- inv.logit(coefficients(logit.restricted)[1], coefficients(logit.restricted)[2], x)
lines(x, y, col="red", lwd=2.5)



# Full model outliers and high leverage points:
logit.out <- glm(DEATH_EVENT~., data=Heart, family = binomial)
plot(logit.out, id.n=10, which=5)
Heart[187,]  # outlier
Heart[196,]  # outlier 
Heart[64,]   # outlier
Heart[229,]  # outlier
Heart[132,]  # outlier 
Heart[39,]   # outlier
Heart[185,]  # high leverage



# INTERACTION EFFECTS
#--------------------

library(misc3d)
library("rgl")

# Inverse transformation for computing pi_i in case of two regressors:
fx <- function(u,v) u
fy <- function(u,v) v
fz <- function(u,v){
  exp(coefficients(logit.out)[1]+coefficients(logit.out)[2]*u+coefficients(logit.out)[3]*v)/
    (1+exp(coefficients(logit.out)[1]+coefficients(logit.out)[2]*u+coefficients(logit.out)[3]*v))
}


# EJECTION FRACTION AND SERUM CREATININE:
logit.out <- glm(DEATH_EVENT~ ejection_fraction + serum_creatinine, data=Heart, family = binomial)
plot3d(ejection_fraction, serum_creatinine, DEATH_EVENT, col="red")
x0 <- seq(min(ejection_fraction), max(ejection_fraction), length=1000)
y0 <- seq(min(serum_creatinine), max(serum_creatinine), length=1000)
parametric3d(fx, fy, fz, u=x0, v=y0, color="blue", fill = FALSE, add=TRUE)
planes3d(0,0,1,-0.5, col="gray", add=TRUE)


# AGE AND EJECTION FRACTION:
logit.out <- glm(DEATH_EVENT~ age + ejection_fraction, data=Heart, family = binomial)
plot3d(age, ejection_fraction, DEATH_EVENT, col="red")
x0 <- seq(min(age), max(age), length=1000)
y0 <- seq(min(ejection_fraction), max(ejection_fraction), length=1000)
parametric3d(fx, fy, fz, u=x0, v=y0, color="blue", fill = FALSE, add=TRUE)
planes3d(0,0,1,-0.5, col="gray", add=TRUE)


# HIGH BLOOD PRESSURE AND AGE:
# old age and high blood pressure probably lead to death
logit.out <- glm(DEATH_EVENT~ age + high_blood_pressure, data=Heart, family = binomial)
plot3d(age, high_blood_pressure, DEATH_EVENT, col="red")
x0 <- seq(min(age), max(age), length=1000)
y0 <- seq(min(high_blood_pressure), max(high_blood_pressure), length=1000)
parametric3d(fx, fy, fz, u=x0, v=y0, color="blue", fill = FALSE, add=TRUE)
planes3d(0,0,1,-0.5, col="gray", add=TRUE)


# HIGH BLOOD PRESSURE AND SERUM CREATININE:
logit.out <- glm(DEATH_EVENT~ serum_creatinine + high_blood_pressure, data=Heart, family = binomial)
plot3d(serum_creatinine, high_blood_pressure, DEATH_EVENT, col="red")
x0 <- seq(min(serum_creatinine), max(serum_creatinine), length=1000)
y0 <- seq(min(high_blood_pressure), max(high_blood_pressure), length=1000)
parametric3d(fx, fy, fz, u=x0, v=y0, color="blue", fill = FALSE, add=TRUE)
planes3d(0,0,1,-0.5, col="gray", add=TRUE)



# TRIVIAL CLASSIFIER
# ------------------

trivial.pred <- rep(0, dim(train)[1])
# Confusion Matrix:
table(trivial.pred, train$DEATH_EVENT)
# Training Error rate:
table(trivial.pred, train$DEATH_EVENT)[2]/dim(train)[1]



# BEST SUBSET SELECTION METHOD
# ----------------------------

# install.packages("foreach")
# install.packages("bestglm")
library(bestglm)

# Prepare data: Xy must contain the dataframe X and response y
train.Xy <- within(train, {
  y <- train$DEATH_EVENT
  DEATH_EVENT <- NULL
})


# BSS using BIC
###############
regfit.full <- bestglm(Xy = train.Xy, family = binomial, method="exhaustive", IC="BIC", nvmax=d)
best.model.BIC <- regfit.full$BestModel
summary(best.model.BIC)
coefficients(best.model.BIC)

# Accuracy on train set:
pred.BIC <- predict(best.model.BIC, newdata=train, type="response")
pred.BIC[pred.BIC<0.5] <- 0
pred.BIC[pred.BIC>=0.5] <- 1
sum(train$DEATH_EVENT == pred.BIC)/dim(train)[1]   

# Confusion Matrix:
table(pred.BIC, train$DEATH_EVENT)
TN <- table(pred.BIC, train$DEATH_EVENT)[1] 
TP <- table(pred.BIC, train$DEATH_EVENT)[4]
FP <- table(pred.BIC, train$DEATH_EVENT)[2] 
FN <- table(pred.BIC, train$DEATH_EVENT)[3] 
P <- sum(train$DEATH_EVENT==1) 
N <- sum(train$DEATH_EVENT==0)
# Training error rate:
(FP+FN)/(P+N)

# ROC Curve:
library(pROC)
pred.BIC <- predict(best.model.BIC, newdata=train, type="response")
roc.out <- roc(train$DEATH_EVENT, pred.BIC, levels=c(0,1))
plot(roc.out, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

# best threshold:
coords(roc.out, "best")
best.thr.BIC <- as.double(coords(roc.out, "best")[1])
points(coords(roc.out, "best")[2], coords(roc.out, "best")[3], col="blue", pch=19, lwd=4)
text(coords(roc.out, "best")[2], coords(roc.out, "best")[3], labels="best=0.24", pos=2,offset=0.5, col="blue")

# compare with default threshold:
points(coords(roc.out, 0.5)[2], coords(roc.out, 0.5)[3], col="red", pch=19, lwd=4)
text(coords(roc.out, 0.5)[2], coords(roc.out, 0.5)[3], labels="default=0.5", pos=4,offset=0.5, col="red")

# Accuracy on test set with best threshold:
test.BIC <- predict(best.model.BIC, newdata=test, type="response")
test.BIC[test.BIC<best.thr.BIC] <- 0
test.BIC[test.BIC>=best.thr.BIC] <- 1
sum(test$DEATH_EVENT == test.BIC)/dim(test)[1] 

# Confusion Matrix on the test set
confusion_matrix <- table(test.BIC, test$DEATH_EVENT)
confusion_matrix
TN <- confusion_matrix[1] 
TP <- confusion_matrix[4]
FP <- confusion_matrix[2] 
FN <- confusion_matrix[3] 
P <- sum(Heart$DEATH_EVENT==1)
N <- sum(Heart$DEATH_EVENT==0)
P_ <- TP + FP
N_ <- TN + FN

false.negative.rate <- FN/P
false.negative.rate

false.positive.rate <- FP/N
false.positive.rate

true.positive.rate <- TP/P
true.positive.rate

positive.predicted.value <- TP/P_
positive.predicted.value

negative.predicitve.value <- TN/N_
negative.predicitve.value

# BSS using AIC
###############
regfit.full <- bestglm(Xy = train.Xy, family = binomial, method="exhaustive", IC="AIC", nvmax=d)
best.model.AIC <- regfit.full$BestModel
summary(best.model.AIC)
coefficients(best.model.AIC)

# Accuracy on train set:
pred.AIC <- predict(best.model.AIC, newdata=train, type="response")
pred.AIC[pred.AIC<0.5] <- 0
pred.AIC[pred.AIC>=0.5] <- 1
sum(train$DEATH_EVENT == pred.AIC)/dim(train)[1]

# Confusion Matrix:
table(pred.AIC, train$DEATH_EVENT)
TN <- table(pred.AIC, train$DEATH_EVENT)[1] 
TP <- table(pred.AIC, train$DEATH_EVENT)[4]
FP <- table(pred.AIC, train$DEATH_EVENT)[2] 
FN <- table(pred.AIC, train$DEATH_EVENT)[3] 
P <- sum(train$DEATH_EVENT==1) 
N <- sum(train$DEATH_EVENT==0)
# Training error rate:
(FP+FN)/(P+N)

# ROC Curve:
pred.AIC <- predict(best.model.AIC, newdata=train, type="response")
roc.out <- roc(train$DEATH_EVENT, pred.AIC, levels=c(0,1))
plot(roc.out, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

# best threshold:
coords(roc.out, "best")
best.thr.AIC <- as.double(coords(roc.out, "best")[1])
points(coords(roc.out, "best")[2], coords(roc.out, "best")[3], col="blue", pch=19, lwd=4)
text(coords(roc.out, "best")[2], coords(roc.out, "best")[3], labels="best=0.44", pos=3,offset=1, col="blue")

# compare with default threshold:
points(coords(roc.out, 0.5)[2], coords(roc.out, 0.5)[3], col="red", pch=19, lwd=4)
text(coords(roc.out, 0.5)[2], coords(roc.out, 0.5)[3], labels="default=0.5", pos=4,offset=0.5, col="red")

# Accuracy on test set with best threshold:
test.AIC <- predict(best.model.AIC, newdata=test, type="response")
test.AIC[test.AIC<best.thr.AIC] <- 0
test.AIC[test.AIC>=best.thr.AIC] <- 1
sum(test$DEATH_EVENT == test.AIC)/dim(test)[1] 

# Confusion Matrix on the test set
confusion_matrix <- table(test.AIC, test$DEATH_EVENT)
confusion_matrix
TN <- confusion_matrix[1] 
TP <- confusion_matrix[4]
FP <- confusion_matrix[2] 
FN <- confusion_matrix[3] 
P <- sum(Heart$DEATH_EVENT==1)
N <- sum(Heart$DEATH_EVENT==0)
P_ <- TP + FP
N_ <- TN + FN

false.negative.rate <- FN/P
false.negative.rate

false.positive.rate <- FP/N
false.positive.rate

true.positive.rate <- TP/P
true.positive.rate

positive.predicted.value <- TP/P_
positive.predicted.value

negative.predicitve.value <- TN/N_
negative.predicitve.value

# RIDGE REGERSSION
# ----------------

library(glmnet)  

X <- model.matrix(DEATH_EVENT~., data=train)   
X <- X[,-1]                               
y <- train$DEATH_EVENT                    
grid <- 10^seq(10, -5, length=100)  

# LEAVE-ONE-OUT Cross-Validation for finding optimal lambda:
cv.out <- cv.glmnet(X, y, family = "binomial", alpha=0, nfolds=n, lambda=grid, type.measure = "mse")

# Plot of the Cross-Validated MSE (with error bar) for all the lambda values:
plot(cv.out)

bestlam.ridge <- cv.out$lambda.min   
# Use the whole train set to fit the model with optimal lambda:
ridge.mod <- glmnet(X, y, family = "binomial", alpha=0, lambda=bestlam.ridge)

# Accuracy on train set:
pred.ridge <- predict(ridge.mod, newx=X, type="response")
pred.ridge[pred.ridge<0.5] <- 0
pred.ridge[pred.ridge>=0.5] <- 1
sum(train$DEATH_EVENT == pred.ridge)/dim(train)[1] 

# Confusion Matrix:
table(pred.ridge, train$DEATH_EVENT)
TN <- table(pred.ridge, train$DEATH_EVENT)[1] 
TP <- table(pred.ridge, train$DEATH_EVENT)[4]
FP <- table(pred.ridge, train$DEATH_EVENT)[2] 
FN <- table(pred.ridge, train$DEATH_EVENT)[3] 
P <- sum(train$DEATH_EVENT==1) 
N <- sum(train$DEATH_EVENT==0)
# Training error rate:
(FP+FN)/(P+N)

# ROC Curve:
pred.ridge <- predict(ridge.mod, newx=X, type="response")
roc.out <- roc(train$DEATH_EVENT, pred.ridge, levels=c(0,1))
plot(roc.out, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

# best threshold:
coords(roc.out, "best")
best.thr.ridge <- as.double(coords(roc.out, "best")[1])
points(coords(roc.out, "best")[2], coords(roc.out, "best")[3], col="blue", pch=19, lwd=4)
text(coords(roc.out, "best")[2], coords(roc.out, "best")[3], labels="best=0.43", pos=3,offset=1.5, col="blue")

# compare with default threshold:
points(coords(roc.out, 0.5)[2], coords(roc.out, 0.5)[3], col="red", pch=19, lwd=4)
text(coords(roc.out, 0.5)[2], coords(roc.out, 0.5)[3], labels="default=0.5", pos=4,offset=0.5, col="red")

# Accuracy on test set with best threshold:
X.test <- model.matrix(DEATH_EVENT~., data=test)   
X.test <- X.test[,-1] 
test.ridge <- predict(ridge.mod, newx=X.test, type="response")
test.ridge[test.ridge<best.thr.ridge] <- 0
test.ridge[test.ridge>=best.thr.ridge] <- 1
sum(test$DEATH_EVENT == test.ridge)/dim(test)[1] 

# Confusion Matrix on the test set
confusion_matrix <- table(test.ridge, test$DEATH_EVENT)
confusion_matrix
TN <- confusion_matrix[1] 
TP <- confusion_matrix[4]
FP <- confusion_matrix[2] 
FN <- confusion_matrix[3] 
P <- sum(Heart$DEATH_EVENT==1)
N <- sum(Heart$DEATH_EVENT==0)
P_ <- TP + FP
N_ <- TN + FN

false.negative.rate <- FN/P
false.negative.rate

false.positive.rate <- FP/N
false.positive.rate

true.positive.rate <- TP/P
true.positive.rate

positive.predicted.value <- TP/P_
positive.predicted.value

negative.predicitve.value <- TN/N_
negative.predicitve.value

# LASSO REGERSSION
# ----------------

# LEAVE-ONE-OUT Cross-Validation for finding optimal lambda:
cv.out <- cv.glmnet(X, y, family = "binomial", alpha=1, nfolds=n, lambda=grid, type.measure = "mse")

# Plot of the Cross-Validated MSE (with error bar) for all the lambda values:
plot(cv.out)

bestlam.lasso <- cv.out$lambda.min   
# Use the whole train set to fit the model with optimal lambda:
lasso.mod <- glmnet(X, y, family = "binomial", alpha=1, lambda=bestlam.lasso)

# Accuracy on train set:
pred.lasso <- predict(lasso.mod, newx=X, type="response")
pred.lasso[pred.lasso<0.5] <- 0
pred.lasso[pred.lasso>=0.5] <- 1
sum(train$DEATH_EVENT == pred.lasso)/dim(train)[1]

# Confusion Matrix:
table(pred.lasso, train$DEATH_EVENT)
TN <- table(pred.lasso, train$DEATH_EVENT)[1] 
TP <- table(pred.lasso, train$DEATH_EVENT)[4]
FP <- table(pred.lasso, train$DEATH_EVENT)[2] 
FN <- table(pred.lasso, train$DEATH_EVENT)[3] 
P <- sum(train$DEATH_EVENT==1) 
N <- sum(train$DEATH_EVENT==0)
# Training error rate:
(FP+FN)/(P+N)

# ROC Curve:
pred.lasso <- predict(lasso.mod, newx=X, type="response")
roc.out <- roc(train$DEATH_EVENT, pred.lasso, levels=c(0,1))
plot(roc.out, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(roc.out)

# best threshold:
coords(roc.out, "best")
best.thr.lasso <- as.double(coords(roc.out, "best")[1])
points(coords(roc.out, "best")[2], coords(roc.out, "best")[3], col="blue", pch=19, lwd=4)
text(coords(roc.out, "best")[2], coords(roc.out, "best")[3], labels="best=0.43", pos=3,offset=1.5, col="blue")

# compare with default threshold:
points(coords(roc.out, 0.5)[2], coords(roc.out, 0.5)[3], col="red", pch=19, lwd=4)
text(coords(roc.out, 0.5)[2], coords(roc.out, 0.5)[3], labels="default=0.5", pos=4,offset=0.5, col="red")

# Accuracy on test set with best threshold:
test.lasso <- predict(lasso.mod, newx=X.test, type="response")
test.lasso[test.lasso<best.thr.lasso] <- 0
test.lasso[test.lasso>=best.thr.lasso] <- 1
sum(test$DEATH_EVENT == test.lasso)/dim(test)[1] 

# Confusion Matrix on the test set
confusion_matrix <- table(test.lasso, test$DEATH_EVENT)
confusion_matrix
TN <- confusion_matrix[1] 
TP <- confusion_matrix[4]
FP <- confusion_matrix[2] 
FN <- confusion_matrix[3] 
P <- sum(Heart$DEATH_EVENT==1)
N <- sum(Heart$DEATH_EVENT==0)
P_ <- TP + FP
N_ <- TN + FN

false.negative.rate <- FN/P
false.negative.rate

false.positive.rate <- FP/N
false.positive.rate

true.positive.rate <- TP/P
true.positive.rate

positive.predicted.value <- TP/P_
positive.predicted.value

negative.predicitve.value <- TN/N_
negative.predicitve.value

# K-NEAREST NEIGHBORS
# -------------------

#install.packages("class")
#install.packages("gmodels")
#install.packages("caret")
#install.packages('e1071', dependencies=TRUE)
#install.packages('ROCR')
library(class)
library(caret)
library(ROCR)
set.seed(1)


# Useful Function:
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

best.k.acc <- function(train, test, y_train, y_test, k.max) {
  k.optm=0
  best.acc=0
  best.tab=0
  for (i in 1:k.max){
    pred <- knn(train=train, test=test, cl=y_train, k=i)
    tab=table(pred,y_test)
    acc=accuracy(tab)
    if(acc>best.acc){
      best.acc=acc
      best.tab=tab
    }
    k.optm[i] = acc
  }
  cat("\nMax Accuracy: ", max(k.optm))
  cat("\nBest K: ",which(k.optm==max(k.optm)), "\n")
  print(best.tab)
  return(k.optm)
}


# KNN (Full Model):
k.optm=best.k.acc(train,test,train$DEATH_EVENT,test$DEATH_EVENT,20)
plot(k.optm, type="o", xlab="K- Value",ylab="Accuracy level", main="Accuracy for different K-Values")


# KNN (BIC Best Model):
BIC_var_train= train[c("age","serum_creatinine","time", "ejection_fraction", "DEATH_EVENT")]
BIC_var_test= test[c("age","serum_creatinine","time", "ejection_fraction", "DEATH_EVENT")]
BIC_var = Heart[c("age","serum_creatinine","time", "ejection_fraction", "DEATH_EVENT")]

k.optm=best.k.acc(BIC_var_train,BIC_var_test,BIC_var_train$DEATH_EVENT,BIC_var_test$DEATH_EVENT,30)
plot(k.optm, type="o", xlab="K- Value",ylab="Accuracy level", main="Accuracy for different K-Values")


# KNN (AIC Best Model):
AIC_var_train=train[c("age","creatinine_phosphokinase","time", "ejection_fraction","sex", "serum_sodium","serum_creatinine", "DEATH_EVENT")]
AIC_var_test= test[c("age","creatinine_phosphokinase","time", "ejection_fraction", "sex", "serum_sodium","serum_creatinine","DEATH_EVENT")]
AIC_var = Heart[c("age","creatinine_phosphokinase","time", "ejection_fraction", "sex", "serum_sodium","serum_creatinine","DEATH_EVENT")]

k.optm=best.k.acc(AIC_var_train,AIC_var_test,AIC_var_train$DEATH_EVENT,AIC_var_test$DEATH_EVENT,30)
plot(k.optm, type="o", xlab="K- Value",ylab="Accuracy level", main="Accuracy for different K-Values")


# ROC Curves for the best models:
BIC_var_train= train[c("age","serum_creatinine","time", "ejection_fraction", "DEATH_EVENT")]
BIC_var_test= test[c("age","serum_creatinine","time", "ejection_fraction", "DEATH_EVENT")]
BIC_var = Heart[c("age","serum_creatinine","time", "ejection_fraction", "DEATH_EVENT")]

# Plot ROC for k=5 
kNN.mod <- knn(train=BIC_var_train, test=BIC_var_test, cl=BIC_var_train$DEATH_EVENT, k=5,prob=TRUE)
prob <- attr(kNN.mod, 'prob')
prob <- ifelse(kNN.mod == "0", 1-prob, prob)
pred.knn <- prediction(prob, BIC_var_test$DEATH_EVENT)
perf.knn <- performance(pred.knn, measure='tpr', x.measure='fpr')
auc=performance(pred.knn, measure='auc')
auc_5 = auc@y.values[[1]]
plot(perf.knn, lwd= 2, lty=1, col="red")
abline(a=0,b=1)

# Test accuracy:
kNN.mod <- knn(train=BIC_var_train, test=BIC_var_test, cl=BIC_var_train$DEATH_EVENT, k=5)
tab = table(kNN.mod,BIC_var_test$DEATH_EVENT)
accuracy(tab)


#Plot ROC for k=10
kNN.mod <- knn(train=BIC_var_train, test=BIC_var_test, cl=BIC_var_train$DEATH_EVENT, k=10,prob=TRUE)
prob <- attr(kNN.mod, 'prob')
prob <- ifelse(kNN.mod == "0", 1-prob, prob)
pred.knn <- prediction(prob, BIC_var_test$DEATH_EVENT)
perf.knn <- performance(pred.knn, measure='tpr', x.measure='fpr')
auc=performance(pred.knn, measure='auc')
auc_10 =auc@y.values[[1]]
plot(perf.knn, col="blue", lwd= 2, lty=1, add=TRUE)

# Test accuracy:
kNN.mod <- knn(train=BIC_var_train, test=BIC_var_test, cl=BIC_var_train$DEATH_EVENT, k=10)
tab = table(kNN.mod,BIC_var_test$DEATH_EVENT)
accuracy(tab)


#Plot ROC for k=15
kNN.mod <- knn(train=BIC_var_train, test=BIC_var_test, cl=BIC_var_train$DEATH_EVENT, k=15,prob=TRUE)
prob <- attr(kNN.mod, 'prob')
prob <- ifelse(kNN.mod == "0", 1-prob, prob) 
pred.knn <- prediction(prob, BIC_var_test$DEATH_EVENT)
perf.knn <- performance(pred.knn, measure='tpr', x.measure='fpr')
auc=performance(pred.knn, measure='auc')
auc_15 = auc@y.values[[1]]
plot(perf.knn, col="green", lwd= 2, lty=1, add=TRUE)

# Test accuracy:
kNN.mod <- knn(train=BIC_var_train, test=BIC_var_test, cl=BIC_var_train$DEATH_EVENT, k=15)
tab = table(kNN.mod,BIC_var_test$DEATH_EVENT)
accuracy(tab)


#Plot ROC for k=20
kNN.mod <- knn(train=BIC_var_train, test=BIC_var_test, cl=BIC_var_train$DEATH_EVENT, k=20,prob=TRUE)
prob <- attr(kNN.mod, 'prob')
prob <- ifelse(kNN.mod == "0", 1-prob, prob)
pred.knn <- prediction(prob, BIC_var_test$DEATH_EVENT)
perf.knn <- performance(pred.knn, measure='tpr', x.measure='fpr')
auc = performance(pred.knn, measure='auc')
auc_20 = auc@y.values[[1]]
plot(perf.knn, col="violet", lwd= 2, lty=1, add=TRUE)
legend(x=0.5,y=0.7, legend=c("k=5, AUC=0.769","k=10, AUC=0.780","k=15, AUC=0.801","k=20, AUC=0.799"), col=c("red","blue","green","violet"), lty=1, lwd=2)

# Test accuracy:
kNN.mod <- knn(train=BIC_var_train, test=BIC_var_test, cl=BIC_var_train$DEATH_EVENT, k=20)
tab = table(kNN.mod,BIC_var_test$DEATH_EVENT)
accuracy(tab)


# Linear Discriminant Analysis (LDA)
######################################

library(MASS)

# functions
###########

accuracy <- function(y, predicted, threshold)
{
  a1 = sum(rep(1, length(y[predicted$posterior[,2]>= threshold])) == y[predicted$posterior[,2] >= threshold])
  a0 = sum(rep(0, length(y[predicted$posterior[,2]< threshold])) == y[predicted$posterior[,2] < threshold])
  (a1 + a0) / length(y)
  return((a1 + a0) / length(y))
}

best_accuracy <- function(y, predicted)
{
  best_acc <- 0
  best_threshold <- 0
  for(threshold in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    acc <- accuracy(y, predicted, threshold)
    
    print(paste("threshold: ", threshold))
    print(paste("Accuracy: ", acc))
    
    if (acc >= best_acc) {
      best_acc <- acc
      best_threshold <- threshold
    }
  }
  
  return(c(best_acc, best_threshold))
}

plot_accuracies <- function(y, predicted) {
  accuracies <- c()
  thresholds <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  for(threshold in thresholds) {
    accuracies <- c(accuracies, accuracy(y, predicted, threshold))
  }
  plot(thresholds,accuracies)
  lines(loess(accuracies~thresholds))
}


# FULL MODEL
############

lda.fit <- lda(DEATH_EVENT~.,data=train)
lda.fit
plot(lda.fit)

# Training Accuracy
# -----------------
lda.train.pred <- predict(lda.fit, train)
mean(lda.train.pred$class==train$DEATH_EVENT) 

# ROC Curve
# ---------
lda.roc.out <- roc(train$DEATH_EVENT, lda.train.pred$posterior[,2], levels=c(0,1))
plot(lda.roc.out, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(lda.roc.out) 
coords(lda.roc.out, "best")

# Test Accuracy
# -------------
lda.pred <- predict(lda.fit, test)
mean(lda.pred$class==test$DEATH_EVENT)
best_threshold = 0.43
accuracy(test$DEATH_EVENT, lda.pred, best_threshold)


# BIC BEST MODEL 
################

lda.reduced <- lda(train$DEATH_EVENT~age+time+ejection_fraction+serum_creatinine,data=train)
lda.reduced

# Training Accuracy
# -----------------
lda.train.pred <- predict(lda.reduced, train)
mean(lda.train.pred$class==train$DEATH_EVENT) 

# ROC Curve
# ---------
lda.roc.out <- roc(train$DEATH_EVENT, lda.train.pred$posterior[,2], levels=c(0,1))
plot(lda.roc.out, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(lda.roc.out)
coords(lda.roc.out, "best")

# Test Accuracy
# -------------
lda.pred <- predict(lda.reduced, test)
mean(lda.pred$class==test$DEATH_EVENT)
best_threshold = 0.26
accuracy(test$DEATH_EVENT, lda.pred, best_threshold)

# Confusion matrix on the test set
# --------------------------------
lda.fit <- lda(train$DEATH_EVENT~age+time+ejection_fraction+serum_creatinine, data=train)
lda.pred <- predict(lda.fit, test)

best_threshold =  0.26
a = table(rep(1, length(test$DEATH_EVENT[lda.pred$posterior[,2]>=best_threshold])), test$DEATH_EVENT[lda.pred$posterior[,2]>=best_threshold])
b = table(rep(0, length(test$DEATH_EVENT[lda.pred$posterior[,2]<best_threshold])), test$DEATH_EVENT[lda.pred$posterior[,2]<best_threshold])

confusion_matrix <- rbind(b, a)

confusion_matrix <- table(lda.pred$class, test$DEATH_EVENT)
confusion_matrix
TN <- confusion_matrix[1] 
TP <- confusion_matrix[4]
FP <- confusion_matrix[2] 
FN <- confusion_matrix[3] 
P <- sum(Heart$DEATH_EVENT==1)
N <- sum(Heart$DEATH_EVENT==0)
P_ <- TP + FP
N_ <- TN + FN

false.negative.rate <- FN/P
false.negative.rate

false.positive.rate <- FP/N
false.positive.rate

true.positive.rate <- TP/P
true.positive.rate

positive.predicted.value <- TP/P_
positive.predicted.value

negative.predicitve.value <- TN/N_
negative.predicitve.value


# Quadratic Discriminant Analysis (QDA)
########################################

# FULL MODEL
############

qda.fit <- qda(DEATH_EVENT~.,data=train)
qda.fit

# Training Accuracy
# -----------------
qda.train.pred <- predict(qda.fit, train)
mean(qda.train.pred$class==train$DEATH_EVENT) 

# ROC Curve
# ---------
qda.roc.out <- roc(train$DEATH_EVENT, qda.train.pred$posterior[,2], levels=c(0,1))
plot(qda.roc.out, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(qda.roc.out)
coords(qda.roc.out, "best")

# Test Accuracy
# -------------
qda.pred <- predict(qda.fit,test)
mean(qda.pred$class==test$DEATH_EVENT) 
best_threshold = 0.19
accuracy(test$DEATH_EVENT, qda.pred, best_threshold)

# BIC BEST MODEL
################

qda.reduced <- qda(train$DEATH_EVENT~age+time+ejection_fraction+serum_creatinine, data=train)
qda.reduced

# Training Accuracy
# -----------------
qda.train.pred <- predict(qda.reduced, train)
mean(qda.train.pred$class==train$DEATH_EVENT)

# ROC Curve
# ---------
qda.roc.out <- roc(train$DEATH_EVENT, qda.train.pred$posterior[,2], levels=c(0,1))
plot(qda.roc.out, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
auc(qda.roc.out)
coords(qda.roc.out, "best")

# Test Accuracy
# -------------
qda.pred <- predict(qda.reduced, test)
mean(qda.pred$class==test$DEATH_EVENT) 
best_threshold = 0.28
accuracy(test$DEATH_EVENT, qda.pred, best_threshold)

# Confusion matrix on the test set
# --------------------------------
qda.fit <- qda(train$DEATH_EVENT~age+time+ejection_fraction+serum_creatinine, data=train)
qda.pred <- predict(qda.fit, test)

best_threshold = 0.28
a = table(rep(1, length(test$DEATH_EVENT[qda.pred$posterior[,2]>=best_threshold])), test$DEATH_EVENT[qda.pred$posterior[,2]>=best_threshold])
b = table(rep(0, length(test$DEATH_EVENT[qda.pred$posterior[,2]<best_threshold])), test$DEATH_EVENT[qda.pred$posterior[,2]<best_threshold])

confusion_matrix <- rbind(b, a)
confusion_matrix <- table(qda.pred$class, test$DEATH_EVENT)
confusion_matrix
TN <- confusion_matrix[1] 
TP <- confusion_matrix[4]
FP <- confusion_matrix[2] 
FN <- confusion_matrix[3] 
P <- sum(Heart$DEATH_EVENT==1)
N <- sum(Heart$DEATH_EVENT==0)
P_ <- TP + FP
N_ <- TN + FN

false.negative.rate <- FN/P
false.negative.rate

false.positive.rate <- FP/N
false.positive.rate

true.positive.rate <- TP/P
true.positive.rate

positive.predicted.value <- TP/P_
positive.predicted.value

negative.predicitve.value <- TN/N_
negative.predicitve.value

# LDA AND QDA COMPARISON
########################

# Full model
# ----------
lda.fit <- lda(DEATH_EVENT~.,data=train)
lda.train.pred <- predict(lda.fit, train)
lda.roc.out <- roc(train$DEATH_EVENT, lda.train.pred$posterior[,2], levels=c(0,1))
qda.fit <- qda(DEATH_EVENT~.,data=train)
qda.train.pred <- predict(qda.fit, train)
qda.roc.out <- roc(train$DEATH_EVENT, qda.train.pred$posterior[,2], levels=c(0,1))

plot(lda.roc.out, col = "red", xlab="False positive rate", ylab="True positive rate")
plot(qda.roc.out, add = TRUE, col = "blue")


# BIC best model
# --------------
lda.reduced <- lda(train$DEATH_EVENT~age+time+ejection_fraction+serum_creatinine,data=train)
lda.train.pred <- predict(lda.reduced, train)
lda.roc.out <- roc(train$DEATH_EVENT, lda.train.pred$posterior[,2], levels=c(0,1))
qda.reduced <- qda(train$DEATH_EVENT~age+time+ejection_fraction+serum_creatinine,data=train)
qda.train.pred <- predict(qda.reduced, train)
qda.roc.out <- roc(train$DEATH_EVENT, qda.train.pred$posterior[,2], levels=c(0,1))

plot(lda.roc.out, col = "red", xlab="False positive rate", ylab="True positive rate")
plot(qda.roc.out, add = TRUE, col = "blue")


# CONFUSION MATRIX FOR BEST MODEL ON THE ENTIRE DATASET
# -----------------------------------------------------

# KNN (BIC Best Model):
BIC_var_train= train[c("age","serum_creatinine","time", "ejection_fraction", "DEATH_EVENT")]
BIC_var_test= test[c("age","serum_creatinine","time", "ejection_fraction", "DEATH_EVENT")]

pred <- knn(train=BIC_var_train, test=BIC_var_test, cl=train$DEATH_EVENT, k=5)
confusion_matrix <- table(pred, test$DEATH_EVENT)
confusion_matrix

TN <- confusion_matrix[1] 
TP <- confusion_matrix[4]
FP <- confusion_matrix[2] 
FN <- confusion_matrix[3] 
P <- sum(Heart$DEATH_EVENT==1)
N <- sum(Heart$DEATH_EVENT==0)
P_ <- TP + FP
N_ <- TN + FN

false.negative.rate <- FN/P
false.negative.rate

false.positive.rate <- FP/N
false.positive.rate

true.positive.rate <- TP/P
true.positive.rate

positive.predicted.value <- TP/P_
positive.predicted.value

negative.predicitve.value <- TN/N_
negative.predicitve.value


detach(Heart)
