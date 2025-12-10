
library(tidyverse)
library(lme4)

chimp_all=read.csv("anon_chimp_age.csv", header=TRUE, 
                   stringsAsFactors = FALSE)

chimp_ad=read.csv("anon_adults.csv", header=TRUE, 
                   stringsAsFactors = FALSE)

chimp_adF=subset(chimp_ad, sex2=="F")



## mixed model of all age classes at 4 m cut off
glme_class4 = glmer(risk ~ sex2 + age_class2 + above4 + (1|id_anon), 
      family=binomial, data=chimp_all,
      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(glme_class4)
exp(summary(glme_class4)$coef)
exp(confint(glme_class4, parm="beta_", method="Wald"))



## mixed model of all age classes at 10 m cut off
glme_class10 = glmer(risk ~ sex2 + age_class2 + above10 + (1|id_anon), 
                     family=binomial, data=chimp_all,
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(glme_class10)
exp(summary(glme_class10)$coef)
exp(confint(glme_class10, parm="beta_", method="Wald"))



## mixed model of absolute ages at 4 m cut off
glme_age4 = glmer(risk ~ sex2 + age2 + above4 + (1|id_anon), 
                  family=binomial, data=chimp_all,
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(glme_age4)
exp(summary(glme_age4)$coef)
exp(confint(glme_age4, parm="beta_", method="Wald"))

## mixed model of absolute ages at 10 m cut off
glme_age10 = glmer(risk ~ sex2 + age2 + above10 + (1|id_anon), 
      family=binomial, data=chimp_all,
      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(glme_age10)
exp(summary(glme_age10)$coef)
exp(confint(glme_age10, parm="beta_", method="Wald"))



## mixed model of adult females at 4 m cut off
glme_adF4 = glmer(risk ~ infant2 + above4 + (1|id_anon), 
                  family=binomial, data=chimp_adF,
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(glme_adF4)
exp(summary(glme_adF4)$coef)
exp(confint(glme_adF4, parm="beta_", method="Wald"))

## mixed model of adult females at 10 m cut off
glme_adF10 = glmer(risk ~ infant2 + above10 + (1|id_anon), 
        family=binomial, data=chimp_adF,
        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(glme_adF10)
exp(summary(glme_adF10)$coef)
exp(confint(glme_adF10, parm="beta_", method="Wald"))



## mixed model of adults at 4 m cut off
glme_ad4 = glmer(risk ~ Icompare + above4 + (1|id_anon), 
                 family=binomial, data=chimp_ad,
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(glme_ad4)
exp(summary(glme_ad4)$coef)
exp(confint(glme_ad4, parm="beta_", method="Wald"))

## mixed model of adults at 10 m cut off
glme_ad10 = glmer(risk ~ Icompare + above10 + (1|id_anon), 
        family=binomial, data=chimp_ad,
        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(glme_ad10)
exp(summary(glme_ad10)$coef)
exp(confint(glme_ad10, parm="beta_", method="Wald"))


#comparisons to a null model
null_all=glmer(risk ~ 1 + (1|id_anon), family=binomial, data=chimp_all,
        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
anova(null_all, glme_class4)
anova(null_all, glme_class10)
anova(null_all, glme_age4)
anova(null_all, glme_age10)




