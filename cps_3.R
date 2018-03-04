# cps_3.R
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

restrict2 <- as.logical(dat_use$educ_bach)
data3 <- subset(dat_use, restrict2)

Age25 <- data3$WSAL_VAL[data3$Age == 25]
Age30 <- data3$WSAL_VAL[data3$Age == 30]
Age35 <- data3$WSAL_VAL[data3$Age == 35]
Age40 <- data3$WSAL_VAL[data3$Age == 40]
Age45 <- data3$WSAL_VAL[data3$Age == 45]
Age50 <- data3$WSAL_VAL[data3$Age == 50]

p_tiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
q_25 <- quantile(Age25, p_tiles)
q_30 <- quantile(Age30, p_tiles)
q_35 <- quantile(Age35, p_tiles)
q_40 <- quantile(Age40, p_tiles)
q_45 <- quantile(Age45, p_tiles)
q_50 <- quantile(Age50, p_tiles)

q_plot <- cbind(q_25,q_30,q_35,q_40,q_45,q_50)
x_val <- as.matrix(c(25, 30, 35, 40, 45, 50))
matplot(x_val,t(q_plot), type = "l", lty = c(1,2,3,4,5), xlab= "Age", ylab = "Wage of college grads at Percentiles")
legend("topleft", c("90", "75", "50", "25", "10"), lty = c(5,4,3,2,1), bty = "n", cex=0.75)

# so it would be sensible to look at quantiles in regression context
# instead of this:
model1 <- lm(WSAL_VAL ~ Age + I(Age^2) + female + AfAm + Asian + Amindian + race_oth 
             + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
             + married + divwidsep + union_m + veteran + immigrant + immig2gen,  data = dat_use)
summary(model1)

# Quantile Regression instead
# note that it is numerically intensive so might slow down your computer
library(quantreg)
quantreg1 <- rq(WSAL_VAL ~ Age + I(Age^2) + female + AfAm + Asian + Amindian + race_oth 
                + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                + married + divwidsep + union_m + veteran + immigrant + immig2gen,  
                tau=p_tiles, data = dat_use)
summary(quantreg1)
plot(quantreg1)


