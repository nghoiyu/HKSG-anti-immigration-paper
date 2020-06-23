####R script for the manuscript "Democratic support and anti-immigration sentiment in autocracies: A comparative analysis of Singapore and Hong Kong"

### Clear memory
rm(list = ls())

### Set directory
directory <- "/Users/nghoiyu/Dropbox/Research/Singapore politics/Singapore anti-immigrant paper"

### Load packages
library(ggplot2)
library(MASS)
library(reshape2)
library(tcltk)
library(skimr)
library(psych)
library(tidyverse)
library(brant)
library(ggeffects)
library(sjmisc)
library(jtools)
library(ggstance)
library(stargazer)
library(pscl)
library(broom)
library(questionr)
library(plyr)
library(effects)
library(sjlabelled)

### Read ABS survey data
abw4_sg <- read.csv("https://raw.githubusercontent.com/nghoiyu/HKSG-anti-immigration-paper/master/W4_Singapore_20170712_release.csv", header=TRUE)
abw4_hk <- read.csv("https://raw.githubusercontent.com/nghoiyu/HKSG-anti-immigration-paper/master/W4_Hong%20Kong_merged20181206.csv", header=TRUE)

###Analysis of Singapore data
## Remove missing values
abw4_sg$q53[abw4_sg$q53 == 99] <- NA
abw4_sg$q83[abw4_sg$q83 >= 7] <- NA
abw4_sg$q84[abw4_sg$q84 >= 7] <- NA
abw4_sg$q85[abw4_sg$q85 >= 7] <- NA
abw4_sg$q86[abw4_sg$q86 >= 7] <- NA
abw4_sg$q97[abw4_sg$q97 >= 97] <- NA
abw4_sg$q125[abw4_sg$q125 >= 7] <- NA
abw4_sg$q130[abw4_sg$q130 >= 7] <- NA
abw4_sg$q131[abw4_sg$q131 >= 7] <- NA
abw4_sg$q132[abw4_sg$q132 >= 7] <- NA
abw4_sg$q133[abw4_sg$q133 >= 7] <- NA
abw4_sg$q139[abw4_sg$q139 >= 7] <- NA
abw4_sg$q140[abw4_sg$q140 >= 7] <- NA
abw4_sg$q141[abw4_sg$q141 >= 7] <- NA
abw4_sg$q142[abw4_sg$q142 >= 7] <- NA
abw4_sg$q143[abw4_sg$q143 >= 7] <- NA
abw4_sg$q144[abw4_sg$q144 >= 7] <- NA
abw4_sg$q145[abw4_sg$q145 >= 7] <- NA
abw4_sg$q146[abw4_sg$q146 >= 7] <- NA
abw4_sg$q147[abw4_sg$q147 >= 7] <- NA
abw4_sg$q148[abw4_sg$q148 >= 7] <- NA
abw4_sg$q149[abw4_sg$q149 >= 7] <- NA
abw4_sg$q153[abw4_sg$q153 >= 7] <- NA
abw4_sg$q161[abw4_sg$q161 >= 7] <- NA
abw4_sg$se4[abw4_sg$se4 >= 9] <- NA
abw4_sg$se14[abw4_sg$se14 >= 8] <- NA

## Reset variable types
abw4_sg <- within(abw4_sg, {q83_cat <- factor(q83, labels=c('strongly_agree','agree','disagree','strongly_disagree'))})
abw4_sg <- within(abw4_sg, {q84_cat <- factor(q84, labels=c('strongly_agree','agree','disagree','strongly_disagree'))})
abw4_sg <- within(abw4_sg, {q85_cat <- factor(q85, labels=c('strongly_agree','agree','disagree','strongly_disagree'))})
abw4_sg <- within(abw4_sg, {q86_cat <- factor(q86, labels=c('strongly_agree','agree','disagree','strongly_disagree'))})
abw4_sg <- within(abw4_sg, {q125_cat <- factor(q125, labels=c('always_preferable','under_some_circumstances','does_not_matter'))})
abw4_sg <- within(abw4_sg, {q130_cat <- factor(q130, labels=c('strongly_approve','approve','disapprove','strongly_disapprove'))})
abw4_sg <- within(abw4_sg, {q131_cat <- factor(q131, labels=c('strongly_approve','approve','disapprove','strongly_disapprove'))})
abw4_sg <- within(abw4_sg, {q132_cat <- factor(q132, labels=c('strongly_approve','approve','disapprove','strongly_disapprove'))})
abw4_sg <- within(abw4_sg, {q133_cat <- factor(q133, labels=c('strongly_approve','approve','disapprove','strongly_disapprove'))})
abw4_sg <- within(abw4_sg, {q153_cat <- factor(q153, labels=c('increase_immigrants','maintain_immigrants','reduce_immigrants','should_not_allow'))})
abw4_sg <- within(abw4_sg, {q161_cat <- factor(q161, labels=c('very_proud','somewhat_proud','not_very_proud','not_proud_at_all'))})

## Recode varibles
#recode gender (se2)
abw4_sg$gender[abw4_sg$se2 == 1] <- 1
abw4_sg$gender[abw4_sg$se2 == 2] <- 0
abw4_sg <- within(abw4_sg, {gender <- factor(gender, labels=c('female','male'))})

#recode marital status (se4)
abw4_sg$marital[abw4_sg$se4 == 1] <- 0
abw4_sg$marital[abw4_sg$se4 == 2] <- 1
abw4_sg$marital[abw4_sg$se4 == 3] <- 1
abw4_sg$marital[abw4_sg$se4 == 4] <- 0
abw4_sg$marital[abw4_sg$se4 == 5] <- 0
abw4_sg$marital[abw4_sg$se4 == 6] <- 0
abw4_sg <- within(abw4_sg, {marital <- factor(marital, labels=c('otherwise','married'))})

#recode education (se5)
abw4_sg$education[abw4_sg$se5 <= 3] <- 1
abw4_sg$education[abw4_sg$se5 >=4 & abw4_sg$se5 <= 7] <-2
abw4_sg$education[abw4_sg$se5 >=8 & abw4_sg$se5 <= 10] <- 3
abw4_sg$education <- factor(abw4_sg$education,
                             levels = c(1,2,3),
                             labels = c("no education to primary", "secondary","tertiary"))

#recode household income (se14)
abw4_sg <- within(abw4_sg, {
  household_income <- factor(se14, labels=c('fifth','fourth','third',
                                            'second','first'))
})

#recode employment (se9)
abw4_sg$employment <- ifelse(abw4_sg$se9 == 2, 0, 1)
abw4_sg <- within(abw4_sg, {employment <- factor(employment, labels=c('not employed','employed'))})

#recode age (se3_2)
abw4_sg$age <- abw4_sg$se3_2

#recode attitudes towards immigrantion (q153) into 2 catagories
abw4_sg$q153_dummy[abw4_sg$q153 <= 2] <-0
abw4_sg$q153_dummy[abw4_sg$q153 >= 3] <-1
abw4_sg$q153_dummy <- factor(abw4_sg$q153_dummy,
                             levels = c(0,1),
                             labels = c("increase or maintain", "reduce or not allow"))

#recode satisfaction with system of government (q83-q86) into an index
q83_q86 <- select(abw4_sg, "q83","q84","q85","q86")
alpha(q83_q86)
abw4_sg$swsofgov <- (abw4_sg$q83+abw4_sg$q84+abw4_sg$q85+abw4_sg$q86) / 4

#recode LEGITMACY OF DEMOCRACY into an index (q130-q133)
q130_q133 <- select(abw4_sg, "q130","q131","q132","q133")
alpha(q130_q133)
abw4_sg$authoritarianism <- (abw4_sg$q130+abw4_sg$q131+abw4_sg$q132+abw4_sg$q133) / 4

#recode authoritarianism/democratic values (q139-q149) into an index
abw4_sg$q141_rev_cod[abw4_sg$q141 == 1] <-4
abw4_sg$q141_rev_cod[abw4_sg$q141 == 2] <-3
abw4_sg$q141_rev_cod[abw4_sg$q141 == 3] <-2
abw4_sg$q141_rev_cod[abw4_sg$q141 == 4] <-1
q139_q149 <- select(abw4_sg, "q139","q140","q141_rev_cod","q142","q143","q144","q145","q146","q147","q148","q149")
alpha(q139_q149)
abw4_sg$auth_demo_values <- (abw4_sg$q139+abw4_sg$q140+abw4_sg$q141_rev_cod+abw4_sg$q142+abw4_sg$q143+abw4_sg$q144+abw4_sg$q145+abw4_sg$q146+abw4_sg$q147+abw4_sg$q148+abw4_sg$q149) / 11

#recode q125 democracy is always preferable into dummy variable
abw4_sg$q125_dummy[abw4_sg$q125 == 1] <- 1
abw4_sg$q125_dummy[abw4_sg$q125 == 2] <- 0
abw4_sg$q125_dummy[abw4_sg$q125 == 3] <- 0
abw4_sg <- within(abw4_sg, {q125_dummy <- factor(q125_dummy, labels=c('others','always preferable'))})

#recode national identity (citizenship, q161) into 2 catagories
abw4_sg$identity[abw4_sg$q161 <= 2] <-1
abw4_sg$identity[abw4_sg$q161 >= 3] <-2
abw4_sg$identity <- factor(abw4_sg$identity,
                             levels = c(1,2),
                             labels = c("proud", "not proud"))

#recode ethnicity (se11a)
abw4_sg$ethnicity <- ifelse(abw4_sg$se11a == 1001, 1, 0)
abw4_sg$ethnicity <- factor(abw4_sg$ethnicity, labels = c("others", "chinese"))

#recode partisanship (q53) into 2 categories (opposition, else)
abw4_sg$opposition[abw4_sg$q53 == 90] <-0
abw4_sg$opposition[abw4_sg$q53 == 1001] <-0
abw4_sg$opposition[abw4_sg$q53 == 1002] <-1
abw4_sg$opposition[abw4_sg$q53 == 1003] <-1
abw4_sg$opposition[abw4_sg$q53 == 1004] <-1
abw4_sg$opposition[abw4_sg$q53 == 1005] <-1
abw4_sg$opposition[abw4_sg$q53 == 1006] <-1
abw4_sg$opposition <- factor(abw4_sg$opposition,
                                levels = c(0,1),
                                labels = c("else", "opposition"))

#Descriptive statistics table
## -------------------------------------------------------------------------------------------------------------
##continuous variables
myvars_cont <- c("age", "swsofgov", "q97", "auth_demo_values", "authoritarianism")
newdata_sg_cont <- abw4_sg[myvars_cont]
stargazer(newdata_sg_cont, digits = 2, out = "sumtablesg.htm", 
          title = "Table 1 Descriptive statistics of all variables by societies", summary.stat = c("n", "mean", "sd", "min", "max"))

#factor variables
myvars_cat <- c("gender", "marital", "education", "household_income", "employment", 
                "ethnicity", "identity", "opposition", "q153_dummy", "q125_dummy")
newdata_sg_cat <- abw4_sg[myvars_cat]
lapply(newdata_sg_cat, freq, digits = 2)

### Binary logistics regression models
## -------------------------------------------------------------------------------------------------------------
##Dimension: Explicit democratic support
#Predictor: q97 (suitability of democracy, 1=completely unsuitable, 10=completely suitable)

#Base model
GLMsuitability1 <- glm(q153_dummy ~ q97, family=binomial(logit), data=abw4_sg)
summary(GLMsuitability1)

#Full model
GLMsuitability <- glm(q153_dummy ~ q97 + age + gender + marital + education + household_income + employment + 
                        ethnicity + identity + opposition, family=binomial(link = logit), data=abw4_sg)
summary(GLMsuitability)

#Plot the results
ggpredict(GLMsuitability, terms = "q97") #Find the predicted probabilities
ggpredict(GLMsuitability, terms = "q97") %>% plot() + 
  labs(
    x = "Suitability of democracy", 
    y = "Predicted probability of holding anti-immigration attitude", 
    title = "Singapore"
  ) + theme(axis.text = element_text(size = 20)) + theme(text = element_text(size = 20))

## -------------------------------------------------------------------------------------------------------------
##Dimension: Explicit democratic support
#Predictor: q125_dummy (1=democracy is always preferable, 0=others)

#Base model
GLMpreferable1 <- glm(q153_dummy ~ q125_dummy, family=binomial(logit), data=abw4_sg)
summary(GLMpreferable1)

#Full model
GLMpreferable <- glm(q153_dummy ~ q125_dummy + age + gender + marital + education + household_income + employment + 
                       ethnicity + identity + opposition, family=binomial(link = logit), data=abw4_sg)
summary(GLMpreferable)

#Plot the results
ggpredict(GLMpreferable, terms = "q125_dummy") #Find the predicted probabilities
ggpredict(GLMpreferable, terms = "q125_dummy") %>% plot() + 
  labs(
    x = "Preference for democracy", 
    y = "Predicted probability of holding anti-immigration attitude", 
    title = "Singapore"
  ) + theme(axis.text = element_text(size = 20)) + theme(text = element_text(size = 20))

## -------------------------------------------------------------------------------------------------------------
#Dimension: Explicit democratic support
#Predictor: swsofgov (satisfaction with system of government (regime support), q83-q86)
#1=strongly agree (strongly satisfied), 4=strongly disagree (strongly disatisfied)
#83. Over the long run, our system of government is capable of solving the problems our country faces.
#84. Thinking in general, I am proud of our system of government.
#85. A system like ours, even if it runs into problems, deserves the people's support.
#86. I would rather live under our system of government than any other that I can think of

#Base model
GLMswsofgov1 <- glm(q153_dummy ~ swsofgov, family=binomial(link = logit), data=abw4_sg)
summary(GLMswsofgov1)

#Full model
GLMswsofgov <- glm(q153_dummy ~ swsofgov + age + gender + marital + education + household_income + employment + 
                      ethnicity + identity + opposition, family=binomial(link = logit), data=abw4_sg)
summary(GLMswsofgov)

#Plot the results
ggpredict(GLMswsofgov, terms = "swsofgov") #Find the predicted probabilities
ggpredict(GLMswsofgov, terms = "swsofgov") %>% plot() + 
  labs(
    x = "Regime support", 
    y = "Predicted probability of holding anti-immigration attitude'", 
    title = "Singapore"
  ) + theme(axis.text = element_text(size = 20)) + theme(text = element_text(size = 20))

## -------------------------------------------------------------------------------------------------------------
##Dimension: Concrete democratic support
#Predictor: auth_demo_values (authoritarianism/democratic values q139-q149)
#1=strongly agree(authoritarian), 4=strongly disagree(democratic)
#For each statement, would you say you strongly agree, somewhat agree, somewhat disagree, or strongly disagree?

#Base model
GLMauth_demo_values1 <- glm(q153_dummy ~ auth_demo_values, 
                            family=binomial(logit), data=abw4_sg)
summary(GLMauth_demo_values1)

#Full model
GLMauth_demo_values <- glm(q153_dummy ~ auth_demo_values + age + gender + marital + education + household_income + employment + 
                             ethnicity + identity + opposition, family=binomial(link = logit), data=abw4_sg)
summary(GLMauth_demo_values)

#Plot the results2
ggpredict(GLMauth_demo_values, terms = "auth_demo_values [1:4]") #Find the predicted probabilities
ggpredict(GLMauth_demo_values, terms = "auth_demo_values") %>% plot() + 
  labs(
    x = "Authoritarian/democratic values", 
    y = "Predicted probability of anti-immigration attitude", 
    title = "Singapore"
  ) + theme(axis.text = element_text(size = 20)) + theme(text = element_text(size = 20))

## -------------------------------------------------------------------------------------------------------------
##Dimension: Concrete democratic support
#Predictor: Authoritariaism (q130-q133)
#1 strongly approve(strongly authoritarian), 2 approve, 3 disapprove, or 4 strongly disapprove(strongly democratic)?
#130. We should get rid of parliament and elections and have a strong leader decide things.
#131. Only one political party should be allowed to stand for election and hold office. 
#132. The army (military) should come in to govern the country.
#133. We should get rid of elections and parliaments and have experts make decisions on behalf of the people.

#Base model
GLMauthoritarianism1 <- glm(q153_dummy ~ authoritarianism, 
                            family=binomial(logit), data=abw4_sg)
summary(GLMauthoritarianism1)

#Full model
GLMauthoritarianism <- glm(q153_dummy ~ authoritarianism + age + gender + marital + education + household_income + employment + 
                             ethnicity + identity + opposition, family=binomial(link = logit), data=abw4_sg)
summary(GLMauthoritarianism)

#Plot the results2
ggpredict(GLMauthoritarianism, terms = "authoritarianism[1:4]") #Find the predicted probabilities
ggpredict(GLMauthoritarianism, terms = "authoritarianism") %>% plot() + 
  labs(
    x = "Anti-authoritarian alternatives", 
    y = "Predicted Predicted of holding anti-immigration attitude'", 
    title = "Singapore"
  ) + theme(axis.text = element_text(size = 20)) + theme(text = element_text(size = 20))

###Analysis of Hong Kong data
## Remove missing values
abw4_hk$q53[abw4_hk$q53 == 99] <- NA
abw4_hk$q83[abw4_hk$q83 >= 7] <- NA
abw4_hk$q84[abw4_hk$q84 >= 7] <- NA
abw4_hk$q85[abw4_hk$q85 >= 7] <- NA
abw4_hk$q86[abw4_hk$q86 >= 7] <- NA
abw4_hk$q97[abw4_hk$q97 >= 97] <- NA
abw4_hk$q125[abw4_hk$q125 >= 7] <- NA
abw4_hk$q130[abw4_hk$q130 >= 7] <- NA
abw4_hk$q131[abw4_hk$q131 >= 7] <- NA
abw4_hk$q132[abw4_hk$q132 >= 7] <- NA
abw4_hk$q133[abw4_hk$q133 >= 7] <- NA
abw4_hk$q139[abw4_hk$q139 >= 7] <- NA
abw4_hk$q140[abw4_hk$q140 >= 7] <- NA
abw4_hk$q141[abw4_hk$q141 >= 7] <- NA
abw4_hk$q142[abw4_hk$q142 >= 7] <- NA
abw4_hk$q143[abw4_hk$q143 >= 7] <- NA
abw4_hk$q144[abw4_hk$q144 >= 7] <- NA
abw4_hk$q145[abw4_hk$q145 >= 7] <- NA
abw4_hk$q146[abw4_hk$q146 >= 7] <- NA
abw4_hk$q147[abw4_hk$q147 >= 7] <- NA
abw4_hk$q148[abw4_hk$q148 >= 7] <- NA
abw4_hk$q149[abw4_hk$q149 >= 7] <- NA
abw4_hk$q153[abw4_hk$q153 >= 7] <- NA
abw4_hk$q161[abw4_hk$q161 >= 7] <- NA
abw4_hk$se4[abw4_hk$se4 >= 9] <- NA
abw4_hk$se14[abw4_hk$se14 >= 8] <- NA
abw4_hk$bornhk[abw4_hk$bornhk >= 3] <- NA

## Reset variable types
abw4_hk <- within(abw4_hk, {bornhk_cat <- factor(bornhk, labels=c('immigrant','native'))})
abw4_hk <- within(abw4_hk, {q83_cat_hk <- factor(q83, labels=c('strongly_agree','agree','disagree','strongly_disagree'))})
abw4_hk <- within(abw4_hk, {q84_cat_hk <- factor(q84, labels=c('strongly_agree','agree','disagree','strongly_disagree'))})
abw4_hk <- within(abw4_hk, {q85_cat_hk <- factor(q85, labels=c('strongly_agree','agree','disagree','strongly_disagree'))})
abw4_hk <- within(abw4_hk, {q86_cat_hk <- factor(q86, labels=c('strongly_agree','agree','disagree','strongly_disagree'))})
abw4_hk <- within(abw4_hk, {q125_cat_hk <- factor(q125, labels=c('always_preferable','under_some_circumstances','does_not_matter'))})
abw4_hk <- within(abw4_hk, {q130_cat_hk <- factor(q130, labels=c('strongly_approve','approve','disapprove','strongly_disapprove'))})
abw4_hk <- within(abw4_hk, {q131_cat_hk <- factor(q131, labels=c('strongly_approve','approve','disapprove','strongly_disapprove'))})
abw4_hk <- within(abw4_hk, {q132_cat_hk <- factor(q132, labels=c('strongly_approve','approve','disapprove','strongly_disapprove'))})
abw4_hk <- within(abw4_hk, {q133_cat_hk <- factor(q133, labels=c('strongly_approve','approve','disapprove','strongly_disapprove'))})
abw4_hk <- within(abw4_hk, {q153_cat_hk <- factor(q153, labels=c('increase_immigrants','maintain_immigrants','reduce_immigrants','should_not_allow'))})
abw4_hk <- within(abw4_hk, {q161_cat_hk <- factor(q161, labels=c('very_proud','somewhat_proud','not_very_proud','not_proud_at_all'))})

## Recode varibles
#recode gender (se2)
abw4_hk$gender_hk[abw4_hk$se2 == 1] <- 1
abw4_hk$gender_hk[abw4_hk$se2 == 2] <- 0
abw4_hk <- within(abw4_hk, {gender_hk <- factor(gender_hk, labels=c('female','male'))})

#recode marital status (se4)
abw4_hk$marital_hk[abw4_hk$se4 == 1] <- 0
abw4_hk$marital_hk[abw4_hk$se4 == 2] <- 1
abw4_hk$marital_hk[abw4_hk$se4 == 3] <- 1
abw4_hk$marital_hk[abw4_hk$se4 == 4] <- 0
abw4_hk$marital_hk[abw4_hk$se4 == 5] <- 0
abw4_hk$marital_hk[abw4_hk$se4 == 6] <- 0
abw4_hk <- within(abw4_hk, {marital_hk <- factor(marital_hk, labels=c('otherwise','married'))})

#recode education (se5)
abw4_hk$education_hk[abw4_hk$se5 <= 3] <- 1
abw4_hk$education_hk[abw4_hk$se5 >=4 & abw4_hk$se5 <= 7] <-2
abw4_hk$education_hk[abw4_hk$se5 >=8 & abw4_hk$se5 <= 10] <- 3
abw4_hk$education_hk <- factor(abw4_hk$education_hk,
                            levels = c(1,2,3),
                            labels = c("no education to primary", "secondary","tertiary"))

#recode household income (se14)
abw4_hk <- within(abw4_hk, {
  household_income_hk <- factor(se14, labels=c('fifth','fourth','third',
                                               'second','first'))
})

#recode employment (se9)
abw4_hk$employment_hk <- ifelse(abw4_hk$se9 == 2, 0, 1)
abw4_hk <- within(abw4_hk, {employment_hk <- factor(employment_hk, labels=c('not employed','employed'))})

#recode age (se3_2)
abw4_hk$age_hk <- abw4_hk$se3_2

#recode attitudes towards immigrantion (q153) into 2 catagories
abw4_hk$q153_dummy[abw4_hk$q153 <= 2] <-0
abw4_hk$q153_dummy[abw4_hk$q153 >= 3] <-1
abw4_hk$q153_dummy <- factor(abw4_hk$q153_dummy,
                             levels = c(0,1),
                             labels = c("increase or maintain", "reduce or not allow"))

#recode satisfaction with system of government (q83-q86) into an index
q83_q86 <- select(abw4_hk, "q83","q84","q85","q86")
alpha(q83_q86)
abw4_hk$swsofgov_hk <- (abw4_hk$q83+abw4_hk$q84+abw4_hk$q85+abw4_hk$q86) / 4

#recode LEGITMACY OF DEMOCRACY into an index (q130-q133)
q130_q133 <- select(abw4_hk, "q130","q131","q132","q133")
alpha(q130_q133)
abw4_hk$authoritarianism_hk <- (abw4_hk$q130+abw4_hk$q131+abw4_hk$q132+abw4_hk$q133) / 4

#recode authoritarianism/democratic values (q139-q149) into an index
abw4_hk$q141_rev_cod_hk[abw4_hk$q141 == 1] <-4
abw4_hk$q141_rev_cod_hk[abw4_hk$q141 == 2] <-3
abw4_hk$q141_rev_cod_hk[abw4_hk$q141 == 3] <-2
abw4_hk$q141_rev_cod_hk[abw4_hk$q141 == 4] <-1
q139_q149 <- select(abw4_sg, "q139","q140","q141_rev_cod","q142","q143","q144","q145","q146","q147","q148","q149")
alpha(q139_q149)
abw4_hk$auth_demo_values_hk <- (abw4_hk$q139+abw4_hk$q140+abw4_hk$q141_rev_cod_hk+abw4_hk$q142+
                                  abw4_hk$q143+abw4_hk$q144+abw4_hk$q145+
                                  abw4_hk$q146+abw4_hk$q147+abw4_hk$q148+abw4_hk$q149) / 11

#recode q125 democracy is always preferable into dummy variable
abw4_hk$q125_dummy_hk[abw4_hk$q125 == 1] <- 1
abw4_hk$q125_dummy_hk[abw4_hk$q125 == 2] <- 0
abw4_hk$q125_dummy_hk[abw4_hk$q125 == 3] <- 0
abw4_hk <- within(abw4_hk, {q125_dummy_hk <- factor(q125_dummy_hk, labels=c('others','always preferable'))})

#recode national identity (citizenship, q161) into 2 catagories
abw4_hk$identity_hk[abw4_hk$q161 <= 2] <-1
abw4_hk$identity_hk[abw4_hk$q161 >= 3] <-2
abw4_hk$identity_hk <- factor(abw4_hk$identity_hk,
                           levels = c(1,2),
                           labels = c("proud", "not proud"))

#recode partisanship (q53) into 2 categories (opposition, else)
abw4_hk$opposition_hk[abw4_hk$q53 == 201] <-1
abw4_hk$opposition_hk[abw4_hk$q53 == 204] <-1
abw4_hk$opposition_hk[abw4_hk$q53 == 207] <-1
abw4_hk$opposition_hk[abw4_hk$q53 == 208] <-1
abw4_hk$opposition_hk[abw4_hk$q53 == 209] <-1
abw4_hk$opposition_hk[abw4_hk$q53 == 210] <-1
abw4_hk$opposition_hk[abw4_hk$q53 == 211] <-1
abw4_hk$opposition_hk[abw4_hk$q53 == 212] <-1
abw4_hk$opposition_hk[abw4_hk$q53 == 220] <-1
abw4_hk$opposition_hk[abw4_hk$q53 == 90] <-0
abw4_hk$opposition_hk[abw4_hk$q53 == 202] <-0
abw4_hk$opposition_hk[abw4_hk$q53 == 203] <-0
abw4_hk$opposition_hk[abw4_hk$q53 == 205] <-0
abw4_hk$opposition_hk[abw4_hk$q53 == 206] <-0
abw4_hk$opposition_hk[abw4_hk$q53 == 299] <-NA
abw4_hk$opposition_hk <- factor(abw4_hk$opposition_hk,
                           levels = c(0,1),
                           labels = c("else", "opposition"))

#Descriptive statistics table
## -------------------------------------------------------------------------------------------------------------
##continuous variables
myvars_cont_hk <- c("age_hk", "swsofgov_hk", "q97", "auth_demo_values_hk", "authoritarianism_hk")
newdata_hk_cont <- abw4_hk[myvars_cont_hk]
stargazer(newdata_hk_cont, digits = 2, out = "sumtablehk1.htm", 
          title = "Table 1 Descriptive statistics of all variables by societies", summary.stat = c("n", "mean", "sd", "min", "max"))

#factor variables
myvars_cat_hk <- c("gender_hk", "marital_hk", "education_hk", "household_income_hk", 
                   "employment_hk", "bornhk_cat", "identity_hk", "opposition_hk", "q153_dummy", "q125_dummy_hk")
newdata_hk_cat <- abw4_hk[myvars_cat_hk]
lapply(newdata_hk_cat, freq, digits = 2)

### Binary logistics regression models
## -------------------------------------------------------------------------------------------------------------
##Dimension: Explicit democratic support
#Predictor: q97 (suitability of democracy, 1=completely unsuitable, 10=completely suitable)

#Base model
GLMsuitability1_hk <- glm(q153_dummy ~ q97, family=binomial(logit), data=abw4_hk)
summary(GLMsuitability1_hk)

#Full model
GLMsuitability_hk <- glm(q153_dummy ~ q97 + age_hk + gender_hk + marital_hk + education_hk + household_income_hk + employment_hk + 
                           bornhk_cat + identity_hk + opposition_hk, family=binomial(link = logit), data=abw4_hk)
summary(GLMsuitability_hk)

#Plot the results
ggpredict(GLMsuitability_hk, terms = "q97") #Find the predicted probabilities
ggpredict(GLMsuitability_hk, terms = "q97") %>% plot() + 
  labs(
    x = "Suitability of democracy", 
    y = "Predicted probability of holding anti-immigration attitude", 
    title = "Hong Kong"
  ) + theme(axis.text = element_text(size = 20)) + theme(text = element_text(size = 20))

## -------------------------------------------------------------------------------------------------------------
##Dimension: Explicit democratic support
#Predictor: q125_dummy (1=democracy is always preferable, 0=others)

#Base model
GLMpreferable1_hk <- glm(q153_dummy ~ q125_dummy_hk, family=binomial(logit), data=abw4_hk)
summary(GLMpreferable1_hk)

#Full model
GLMpreferable_hk <- glm(q153_dummy ~ q125_dummy_hk + age_hk + gender_hk + marital_hk + education_hk + household_income_hk + employment_hk + 
                          bornhk_cat + identity_hk + opposition_hk, family=binomial(link = logit), data=abw4_hk)
summary(GLMpreferable_hk)

#Plot the results
ggpredict(GLMpreferable_hk, terms = "q125_dummy_hk") #Find the predicted probabilities
ggpredict(GLMpreferable_hk, terms = "q125_dummy_hk") %>% plot() + 
  labs(
    x = "Preference for democracy", 
    y = "Predicted probability of holding anti-immigration attitude", 
    title = "Hong Kong"
  ) + theme(axis.text = element_text(size = 20)) + theme(text = element_text(size = 20))

## -------------------------------------------------------------------------------------------------------------
#Dimension: Explicit democratic support
#Predictor: swsofgov (satisfaction with system of government (regie support), q83-q86)
#1=strongly agree (strongly satisfied), 4=strongly disagree (strongly disatisfied)
#83. Over the long run, our system of government is capable of solving the problems our country faces.
#84. Thinking in general, I am proud of our system of government.
#85. A system like ours, even if it runs into problems, deserves the people's support.
#86. I would rather live under our system of government than any other that I can think of.

#Base model
GLMswsofgov1_hk <- glm(q153_dummy ~ swsofgov_hk, family=binomial(link = logit), data=abw4_hk)
summary(GLMswsofgov1_hk)

#Full model
GLMswsofgov_hk <- glm(q153_dummy ~ swsofgov_hk + age_hk + gender_hk + 
                        marital_hk + education_hk + household_income_hk + employment_hk + 
                         bornhk_cat + identity_hk + opposition_hk, family=binomial(link = logit), data=abw4_hk)
summary(GLMswsofgov_hk)

#Plot the results2
ggpredict(GLMswsofgov_hk, terms = "swsofgov_hk[1:4]") #Find the predicted probabilities
ggpredict(GLMswsofgov_hk, terms = "swsofgov_hk") %>% plot() + 
  labs(
    x = "Satisfaction with system of government", 
    y = "Predicted probability of holding anti-immigration attitude'", 
    title = "Hong Kong"
  )

## -------------------------------------------------------------------------------------------------------------
##Dimension: Concrete democratic support
#Predictor: auth_demo_values (authoritarianism/democratic values q139-q149)
#1=strongly agree(authoritarian), 4=strongly disagree(democratic)
#For each statement, would you say you strongly agree, somewhat agree, somewhat disagree, or strongly disagree?

#Base model
GLMauth_demo_values1_hk <- glm(q153_dummy ~ auth_demo_values_hk, 
                               family=binomial(logit), data=abw4_hk)
summary(GLMauth_demo_values1_hk)

#Full model
GLMauth_demo_values_hk <- glm(q153_dummy ~ auth_demo_values_hk + age_hk + 
                                gender_hk + marital_hk + education_hk + household_income_hk + employment_hk + 
                                bornhk_cat + identity_hk + opposition_hk, family=binomial(link = logit), data=abw4_hk)
summary(GLMauth_demo_values_hk)

#Plot the results2
ggpredict(GLMauth_demo_values_hk, terms = "auth_demo_values_hk[1:4]") #Find the predicted probabilities
ggpredict(GLMauth_demo_values_hk, terms = "auth_demo_values_hk") %>% plot() + 
  labs(
    x = "Authoritarian/democratic values", 
    y = "Predicted probability of holding anti-immigration attitude'", 
    title = "Hong Kong"
  )

## -------------------------------------------------------------------------------------------------------------
##Dimension: Concrete democratic support
#Predictor: Authoritariaism (q130-q133)
#1 strongly approve(strongly authoritarian), 2 approve, 3 disapprove, or 4 strongly disapprove(strongly democratic)?
#130. We should get rid of parliament and elections and have a strong leader decide things.
#131. Only one political party should be allowed to stand for election and hold office. 
#132. The army (military) should come in to govern the country.
#133. We should get rid of elections and parliaments and have experts make decisions on behalf of the people.

#Base model
GLMauthoritarianism1_hk <- glm(q153_dummy ~ authoritarianism_hk, 
                               family=binomial(logit), data=abw4_hk)
summary(GLMauthoritarianism1_hk)

#Full model
GLMauthoritarianism_hk <- glm(q153_dummy ~ authoritarianism_hk + age_hk + gender_hk + marital_hk + 
                                education_hk + household_income_hk + employment_hk + 
                                bornhk_cat + identity_hk + opposition_hk, family=binomial(link = logit), data=abw4_hk)
summary(GLMauthoritarianism_hk)

#Plot the results
ggpredict(GLMauthoritarianism_hk, terms = "authoritarianism_hk[1:4]") #Find the predicted probabilities
ggpredict(GLMauthoritarianism_hk, terms = "authoritarianism_hk") %>% plot() + 
  labs(
    x = "Rejection of alternatives to democracy", 
    y = "Predicted probability of holding anti-immigration attitude'", 
    title = "Hong Kong"
  )

###Create regression tables
##Singapore
stargazer(GLMsuitability1, GLMsuitability, 
          GLMpreferable1, GLMpreferable, 
          GLMswsofgov1, GLMswsofgov,
          GLMauth_demo_values1, GLMauth_demo_values, 
          GLMauthoritarianism1, GLMauthoritarianism, type = "html", star.cutoffs = c(0.05, 0.01, 0.001),
          title="Binary logit regression of electoral preference",
          align=TRUE, digits = 2, out = "models_sg.htm")

##Hong Kong
stargazer(GLMsuitability1_hk, GLMsuitability_hk, 
          GLMpreferable1_hk, GLMpreferable_hk, 
          GLMswsofgov1_hk, GLMswsofgov_hk,
          GLMauth_demo_values1_hk, GLMauth_demo_values_hk, 
          GLMauthoritarianism1_hk, GLMauthoritarianism_hk, type = "html", star.cutoffs = c(0.05, 0.01, 0.001),
          title="Binary logit regression of electoral preference",
          align=TRUE, digits = 2, out = "models_hk.htm")

### Create a barplot about attitude towards immigration in Singapore and Hong Kong (q153_dummy) (Figure 1)
data <- data.frame(Attitude_towards_immigration = c("Increase or maintain", "Increase or maintain", "Reduce or not allow", 
                                                    "Reduce or not allow"), 
                   Percentage = c(37.39, 23.03, 62.61, 76.97), 
                   Society = c("Singapore","Hong Kong", "Singapore","Hong Kong"))

ggplot(data, aes(fill=Society, y=Percentage, x=Attitude_towards_immigration)) + 
  geom_bar(position="dodge", stat="identity") + labs(y="Percentage of responses", x = "Attitude towards immigration") + 
  scale_fill_grey() + theme_minimal() + labs(title = "", 
                                             caption = "Source: ABS wave 4, n=920 (Singapore); n=1,042 (Hong Kong)") +
  theme(axis.text = element_text(size = 20)) + theme(text = element_text(size = 20))



