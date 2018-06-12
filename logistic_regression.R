## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

#...aside testing:
test <- NH11[NH11$sex == "2 Female" & age_p == 33,]
mean(test$bmi)
# [1] 30.24467
# confirms that the mean bmi calculated is for the whole dataset, not each
# group

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

#install.packages("effects")
library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

# Investigating the NAs to clean
str(NH11)
summary(NH11[,names(NH11) %in% c("everwrk", "age_p", "r_maritl")])
# -> NAs in everwrk
# Fix is to make all non-Yes/Nos into "Other" grouping
NH11$everwrk2 <- as.character(NH11$everwrk) 
NH11$everwrk2[as.character(NH11$everwrk) == "7 Refused" | as.character(NH11$everwrk) == "8 Not ascertained" | as.character(NH11$everwrk) == "9 Don't know" | is.na(NH11$everwrk) == TRUE] <- "3 Other"
NH11$everwrk2 <- as.factor(NH11$everwrk2)
NH11$r_maritl2 <- droplevels(NH11$r_maritl)
table(NH11$everwrk2) # checking everything worked

# Modeling and predicting
everwrk.out <- glm(everwrk2 ~ age_p + r_maritl2, data = NH11, family = "binomial")
coef(summary(everwrk.out))

plot(allEffects(everwrk.out))
plot(Effect("r_maritl2", everwrk.out))

pred_everwrk <- with(NH11, expand.grid(r_maritl2 = levels(r_maritl2), age_p = mean(age_p)))
cbind(pred_everwrk, predict(everwrk.out, type = "response", newdata = pred_everwrk))

# Notes:
# Divorced and Living with partner have the lowest ever worked percentage, while 
# Widowed and Never Married have the highest

#From the solutions:
nh11.wrk.age.mar <- subset(NH11, select = c("everwrk", "age_p", "r_maritl"))
summary(nh11.wrk.age.mar)
NH112 <- transform(NH11,
                  everwrk = factor(everwrk,
                                   levels = c("1 Yes", "2 No")),
                  r_maritl = droplevels(r_maritl))
# investigating changes
summary(subset(NH112, select = c("everwrk", "age_p", "r_maritl")))

mod.wk.age.mar <- glm(everwrk ~ age_p + r_maritl, data = NH112,
                      family = "binomial")

summary(mod.wk.age.mar)
data.frame(Effect("r_maritl", mod.wk.age.mar))
plot(Effect("r_maritl", mod.wk.age.mar))

pred_everwrk <- with(NH112, expand.grid(r_maritl = r_maritl, age_p = mean(age_p)))
cbind(pred_everwrk, predict(mod.wk.age.mar, type = "response", newdata = pred_everwrk))





#Re-creating solution with my methods:
NH11$everwrk3 <- as.character(NH11$everwrk) 
NH11$everwrk3[as.character(NH11$everwrk) == "7 Refused" | as.character(NH11$everwrk) == "8 Not ascertained" | as.character(NH11$everwrk) == "9 Don't know" | is.na(NH11$everwrk) == TRUE] <- NA
NH11$everwrk3 <- as.factor(NH11$everwrk3)
table(NH11$everwrk3)
everwrk.out2 <- glm(everwrk3 ~ age_p + r_maritl2, data = NH11, family = "binomial")
plot(Effect("r_maritl3", everwrk.out2))
