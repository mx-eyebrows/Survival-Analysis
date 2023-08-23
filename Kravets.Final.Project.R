
# Load Survival Library and data set ------------------------------------------
library(survival)
setwd("/Users/Brittany/Documents/My Schoolwork/MS Biostatistics Degree/PHST 683-Survival Analysis/Homework")
load("AbuseStudy.Rdata")
attach(dat)


# Restructure data set --------------------------------------------------------

project.frame <- data.frame(matrix(ncol = 15, nrow = 0))
colnames(project.frame) <- c('id','start','stop','event','outofhome','program',
                             'age','sex', 'minority','poverty','subst.abuse',
                             'crim.hist','first','region','agency')

for (i in 1:nrow(dat)) {
    start <- 0
    stop <- numeric(length=0)
    outofhome <- 0
    event <- numeric(length=0)
    if (dat$removal[i]==1) {
      start <- c(start, dat$time.removal[i])
      stop <- c(stop, dat$time.removal[i])
      outofhome <- c(outofhome,1)
      event <- c(event, 0)
    }
    if (dat$return[i]==1) {
      start <- c(start, dat$time.return[i])
      stop <- c(stop, dat$time.return[i])
      outofhome <- c(outofhome,0)
      event <- c(event, 0)
    }
    stop <- c(stop, dat$time[i])
    event <- c(event, dat$event[i])
    program <- dat$program[i]
    age <- dat$age[i]
    sex <- dat$sex[i]
    minority <- dat$minority[i]
    poverty <- dat$poverty[i]
    subst.abuse <- dat$subst.abuse[i]
    crim.hist <- dat$crim.hist[i]
    first <- dat$first[i]
    region <- dat$region[i]
    agency <- dat$agency[i]
    temp.frame <- data.frame(id=dat$id[i],start,stop,event,outofhome,program,
                             age,sex,minority,poverty,subst.abuse,crim.hist,first,
                             region,agency)
    project.frame <- rbind(project.frame, temp.frame)
}
save(project.frame, file = "projectframe.RData")

# Detach and clear environment -------------------------------------------------
detach(dat)
rm(list = ls())
load("projectframe.RData")
attach(project.frame)


# Checking for balance ---------------------------------------------------------

# Sex
table.sex <- table(program, sex)
prop.test(table.sex)
# Minority
table.min <- table(program, minority)
prop.test(table.min)
# Poverty
table.pov <- table(program, poverty)
prop.test(table.pov)
# Substance Abuse
table.sub <- table(program, subst.abuse)
prop.test(table.sub)
# Criminal History
table.crim <- table(program, crim.hist)
prop.test(table.crim)
# First CPS contact
table.first <- table(program, first)
prop.test(table.first)
# Region
table.reg<- table(program, region)
prop.test(table.reg)
# Agency
table.agen <- table(program, agency)
prop.test(table.agen)
# Out of home
table.home <- table(program, outofhome)
prop.test(table.home)

#Age
# Shapiro-Wilk normality tests
with(project.frame, shapiro.test(age[program=="Pilot"]))
with(project.frame, shapiro.test(age[program=="Standard"]))

# Variance test
var.test(age ~ program, data = project.frame)
 
# Mann-Whitney test
x <- project.frame[ which(program=="Pilot"), ]
x <- x$age
y <- project.frame[ which(program=="Standard"), ]
y <- y$age
wilcox.test(x,y)


# Create surv object -----------------------------------------------------------
project.surv <- Surv(start,stop,event,type = "counting")
project.surv

# Create model without interactions  -------------------------------------------

# Model with only treatment factor
mod1 <- coxph(project.surv~program)
summary(mod1) # time-dependent variable is significant


# Test for covariates 
mod.out <- coxph(project.surv~program+outofhome)
mod.age <- coxph(project.surv~program+age)
mod.sex <- coxph(project.surv~program+sex)
mod.min <- coxph(project.surv~program+minority)
mod.pov <- coxph(project.surv~program+poverty)
mod.sub <- coxph(project.surv~program+subst.abuse)
mod.crim <- coxph(project.surv~program+crim.hist)
mod.first <- coxph(project.surv~program+first)

anova(mod1, mod.out) #Out of home is significant
anova(mod1, mod.age) #Age is significant
anova(mod1, mod.sex) #Sex is NOT significant
anova(mod1, mod.min) #Minority is NOT significant
anova(mod1, mod.pov) #Poverty is significant
anova(mod1, mod.sub) #Substance abuse is NOT significant
anova(mod1, mod.crim) #Criminal history is significant
anova(mod1, mod.first) #First is significant

mod2 <- coxph(project.surv~program+age)

mod.out <- coxph(project.surv~program+age+outofhome)
mod.sex <- coxph(project.surv~program+age+sex)
mod.min <- coxph(project.surv~program+age+minority)
mod.pov <- coxph(project.surv~program+age+poverty)
mod.sub <- coxph(project.surv~program+age+subst.abuse)
mod.crim <- coxph(project.surv~program+age+crim.hist)
mod.first <- coxph(project.surv~program+age+first)

anova(mod2, mod.out) #Out of home is significant
anova(mod2, mod.sex) #Sex is NOT significant
anova(mod2, mod.min) #Minority is NOT significant
anova(mod2, mod.pov) #Poverty is significant
anova(mod2, mod.sub) #Substance abuse is NOT significant
anova(mod2, mod.crim) #Criminal history is significant
anova(mod2, mod.first) #First is significant

mod3 <- coxph(project.surv~program+age+crim.hist)

mod.out <- coxph(project.surv~program+age+crim.hist+outofhome)
mod.sex <- coxph(project.surv~program+age+crim.hist+sex)
mod.min <- coxph(project.surv~program+age+crim.hist+minority)
mod.pov <- coxph(project.surv~program+age+crim.hist+poverty)
mod.sub <- coxph(project.surv~program+age+crim.hist+subst.abuse)
mod.first <- coxph(project.surv~program+age+crim.hist+first)

anova(mod3, mod.out) #Out of home is significant
anova(mod3, mod.sex) #Sex is NOT significant
anova(mod3, mod.min) #Minority is NOT significant
anova(mod3, mod.pov) #Poverty is NOT significant
anova(mod3, mod.sub) #Substance abuse is significant
anova(mod3, mod.first) #First is significant

mod4 <- coxph(project.surv~program+age+crim.hist+first)

mod.out <- coxph(project.surv~program+age+crim.hist+first+outofhome)
mod.sex <- coxph(project.surv~program+age+crim.hist+first+sex)
mod.min <- coxph(project.surv~program+age+crim.hist+first+minority)
mod.pov <- coxph(project.surv~program+age+crim.hist+first+poverty)
mod.sub <- coxph(project.surv~program+age+crim.hist+first+subst.abuse)

anova(mod4, mod.out) #Out of home is significant
anova(mod4, mod.sex) #Sex is NOT significant
anova(mod4, mod.min) #Minority is NOT significant
anova(mod4, mod.pov) #Poverty is NOT significant
anova(mod4, mod.sub) #Substance abuse is significant

mod5 <- coxph(project.surv~program+age+crim.hist+first+outofhome)

mod.sex <- coxph(project.surv~program+age+crim.hist+first+outofhome+sex)
mod.min <- coxph(project.surv~program+age+crim.hist+first+outofhome+minority)
mod.pov <- coxph(project.surv~program+age+crim.hist+first+outofhome+poverty)
mod.sub <- coxph(project.surv~program+age+crim.hist+first+outofhome+subst.abuse)

anova(mod5, mod.sex) #Sex is NOT significant
anova(mod5, mod.min) #Minority is NOT significant
anova(mod5, mod.pov) #Poverty is NOT significant
anova(mod5, mod.sub) #Substance abuse is significant

mod6 <- coxph(project.surv~program+age+crim.hist+first+outofhome+subst.abuse)

mod.sex <- coxph(project.surv~program+age+crim.hist+first+outofhome+subst.abuse+sex)
mod.min <- coxph(project.surv~program+age+crim.hist+first+outofhome+subst.abuse+minority)
mod.pov <- coxph(project.surv~program+age+crim.hist+first+outofhome+subst.abuse+poverty)

anova(mod6, mod.sex) #Sex is NOT significant
anova(mod6, mod.min) #Minority is NOT significant
anova(mod6, mod.pov) #Poverty is NOT significant

# Final model without interactions
mod.noint <- mod6
summary(mod.noint)
drop1(mod.noint, test="Chisq")

# Testing for interactions  ----------------------------------------------------

mod.age <- update(mod.noint, .~.+program*age)
mod.crim <- update(mod.noint, .~.+program*crim.hist)
mod.first <- update(mod.noint, .~.+program*first)
mod.out <- update(mod.noint, .~.+program*outofhome)
mod.sub <- update(mod.noint, .~.+program*subst.abuse)

anova(mod.noint, mod.age)   #NOT significant
anova(mod.noint, mod.crim)  #NOT significant
anova(mod.noint, mod.first) #NOT significant
anova(mod.noint, mod.out)   #NOT significant
anova(mod.noint, mod.sub)   #NOT significant

mod.final <- mod.noint

# Check Proportional Hazards Assumption ---------------------------------------

cox.zph(mod.final)
drop1(mod.final, test="Chisq")    #Stratification is not required


# Stratification just for additional information ------------------------------

mod.regstrat <- update(mod.final, .~.+strata(region))
mod.agstrat <- update(mod.final, .~.+strata(agency))

summary(mod.regstrat)
summary(mod.agstrat)

cox.zph(mod.regstrat)
cox.zph(mod.agstrat)

drop1(mod.regstrat, test = "Chisq")
drop1(mod.agstrat, test = "Chisq")

# Plots ------------------------------------

mean(age)
new.frame <- data.frame(age=7.078519, program=c("Pilot","Standard"), crim.hist=0, 
                        first=0, outofhome=0, subst.abuse=0)
fit1 <- survfit(mod.final, new.frame)
plot(fit1, col=c(2,4), conf.int = T, xlab="Time (days)", ylab="Survival", 
     main="Survival for Secondary Occurence of Abuse")
legend(0, 0.4, legend=c("Pilot Program", "Standard Program", "", "Survival", 
      "95% Confidence Interval"), lty=c(1,1,0,1,2), col=c(2,4,0,1,1), cex = 0.9)

fit2 <- survfit(mod.regstrat, new.frame)
plot(fit2, col=c(2,2,4,4), lty=c(1,5,1,5), conf.int = F, xlab="Time (days)", ylab="Survival", 
     main="Survival for Secondary Occurence of Abuse - Stratified by Region")
legend(0, 0.4, legend=c("Pilot Program", "Standard Program", "", "North Region", "South Region"), 
       lty=c(1,1,0,1,5), col=c(2,4,0,1,1), cex = 0.9)

fit3 <- survfit(mod.agstrat, new.frame)
plot(fit3, col=c(2,2,4,4), lty=c(1,5,1,5), conf.int = F, xlab="Time (days)", ylab="Survival", 
     main="Survival for Secondary Occurence of Abuse - Stratified by Agency")
legend(0, 0.4, legend=c("Pilot Program", "Standard Program", "", "Local Agency", "State Agency"), 
       lty=c(1,1,0,1,5), col=c(2,4,0,1,1), cex = 0.9)
