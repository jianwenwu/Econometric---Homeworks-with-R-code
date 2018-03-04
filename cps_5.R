# cps_5.R
# looking at CPS 2013 data
# uses file from dataferret download of CPS March 2013 supplement, downloaded June 12 2014
# accompanying lecture notes for KFoster class ECO B2000 in fall 2014 at CCNY


rm(list = ls(all = TRUE))
setwd("C:\\Users\\Kevin\\Documents\\CCNY\\data for classes\\CPS_Mar2013")
load("cps_mar2013.RData")

attach(dat_CPSMar2013)
# use prime-age,fulltime, yearround workers
use_varb <- (Age >= 25) & (Age <= 55) & work_fullt & work_50wks
dat_use <- subset(dat_CPSMar2013,use_varb) # 47,550 out of 202,634 obs

detach(dat_CPSMar2013)

# so start with a logit model of 'treatment'
model1 <- glm(female ~ Age + I(Age^2) + AfAm + Asian + Amindian + race_oth 
             + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
             + married + divwidsep + union_m + veteran + immigrant + immig2gen, 
             family= binomial, data = dat_use)
X_dist <- model1$fitted 
Y_est <- dat_use$WSAL_VAL
tr_est <- dat_use$female

require('Matching')
# this is numerically intensive 
model_match <- Match(Y=Y_est, Tr=tr_est, X=X_dist, M=1, version='fast')
summary(model_match)
# produces Estimate...  -18776 - compare with estimate from below of -19296

modelcompare <- lm(WSAL_VAL ~ Age + I(Age^2) + female + AfAm + Asian + Amindian + race_oth 
              + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
              + married + divwidsep + union_m + veteran + immigrant + immig2gen, 
              data = dat_use)
summary(modelcompare)

# -----------------
use_varb2 <- as.logical(dat_use$educ_bach + dat_use$educ_adv)
dat_use2 <- subset(dat_use,use_varb2) # 19231 obs

model2 <- glm(educ_adv ~ Age + I(Age^2) + female + AfAm + Asian + Amindian + race_oth 
              + Hispanic  
              + married + divwidsep + union_m + veteran + immigrant + immig2gen, 
              family= binomial, data = dat_use2)
X_dist <- model2$fitted 
Y_est <- dat_use2$WSAL_VAL
tr_est <- dat_use2$educ_adv

require('Matching')
# this is numerically intensive 
model_match2 <- Match(Y=Y_est, Tr=tr_est, X=X_dist, M=1, version='fast')
summary(model_match2)

modelcompare2 <- lm(WSAL_VAL ~ Age + I(Age^2) + female + AfAm + Asian + Amindian + race_oth 
                   + Hispanic + educ_adv 
                   + married + divwidsep + union_m + veteran + immigrant + immig2gen, 
                   data = dat_use2)
summary(modelcompare2)

