# cps_4.R
# looking at CPS 2013 data
# uses file from dataferret download of CPS March 2013 supplement, downloaded June 12 2014
# accompanying lecture notes for KFoster class ECO B2000 at CCNY

# look at HI, which codes for health insurance

rm(list = ls(all = TRUE))
setwd("C:\\Users\\Kevin\\Documents\\CCNY\\data for classes\\CPS_Mar2013")
load("cps_mar2013.RData")

attach(dat_CPSMar2013)
# use prime-age,fulltime, yearround workers
use_varb <- (Age >= 25) & (Age <= 55) 
dat_use_hi <- subset(dat_CPSMar2013,use_varb) # 85,133 out of 202,634 obs

detach(dat_CPSMar2013)

summary(dat_use_hi)

summary(dat_use_hi$HHI_YN)
# note that for this case (looking back at initial recoding) 1 is yes and 2 is no
# also note that variable HI codes if person has insurance through own employer
dat_use_hi$health_ins <- as.numeric(dat_use_hi$HHI_YN == 1)
summary(dat_use_hi$health_ins)


model_logit1 <- glm(health_ins ~ Age + I(Age^2) + female + AfAm + Asian + Amindian + race_oth 
                   + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                   + married + divwidsep + union_m + veteran + immigrant + immig2gen,
                   family = binomial, data = dat_use_hi)

summary(model_logit1)

# with a lot of factors you will notice a slow down!
model_logit2 <- glm(health_ins ~ Age + I(Age^2) + female + AfAm + Asian + Amindian + race_oth 
                    + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                    + married + divwidsep + union_m + veteran + immigrant + immig2gen
                    + as.factor(A_DTOCC) + as.factor(A_DTIND) + as.factor(GESTCEN), 
                    family = binomial, data = dat_use_hi)

summary(model_logit2)

regn_probit1 <- glm(health_ins ~ Age + I(Age^2) + female + AfAm + Asian + Amindian + race_oth 
                    + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                    + married + divwidsep + union_m + veteran + immigrant + immig2gen, 
                    family = binomial (link = 'probit'), data = dat_use_hi)
summary(regn_probit1)

regn_probit2 <- glm(health_ins ~ Age + I(Age^2) + female + AfAm + Asian + Amindian + race_oth 
                    + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                    + married + divwidsep + union_m + veteran + immigrant + immig2gen 
                    + as.factor(A_DTOCC) + as.factor(A_DTIND) + as.factor(GESTCEN), 
                    family = binomial (link = 'probit'), data = dat_use_hi)
summary(regn_probit2)

