library(lme4)
str(sleepstudy)
require(lattice)
xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)", aspect = "xy")
(fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
(fm2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy))


AIC(fm1,fm2)

library(ggplot2)
ggplot(sleepstudy,aes(Days,Reaction))+geom_point()+theme_bw()+geom_jitter()+
  geom_smooth(method='lm',se=F)

ggplot(sleepstudy,aes(Days,Reaction))+geom_point()+geom_jitter()+
  geom_smooth(method='lm')+theme_minimal()

lm1<-lm(Reaction~Days,sleepstudy)
summary(lm1)

library(broom)
model.diag.metrics <- augment(lm1)
head(model.diag.metrics)

ggplot(model.diag.metrics, aes(Days,Reaction)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Days,
                   yend = .fitted),
               color = "red", size = 0.3)
#cool, can I jitter it

ggplot(model.diag.metrics, aes(Days,Reaction)) +
  geom_jitter() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Days,
                   yend = .fitted),
               color = "red", size = 0.3)
#hum, not quite

par(mfrow = c(2, 2))
plot(lm1)


library(ggfortify)
autoplot(lm1)

#good review of diagnostic
#http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

sleepstudy$Days<-as.factor(sleepstudy$Days)
ggplot(sleepstudy,aes(Days,Reaction))+geom_boxplot()+
  theme_minimal()
