library(aod)
library(ggplot2)

https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
  
  mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)

summary(mydata)

sapply(mydata, sd)

mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

summary(mylogit)

mydata$predicts=predict(mylogit,mydata)

head(mydata)

exp(coef(mylogit))


###Logistic Binomial

mydata <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2016/02/sample.csv")

head(mydata)

mylogit<-glm(hon~math+female+read,data=mydata, family="binomial")

summary(mylogit)

mydata$pre=predict(mylogit,mydata[1:20,])

mydata$pre1=-11.77025+(0.12296*mydata$math)+(0.97995*mydata$female)+(0.05906*mydata$read)

mydata$p=exp(-11.77025+(0.12296*mydata$math)+(0.97995*mydata$female)+(0.05906*mydata$read))/(1+exp(-11.77025+(0.12296*mydata$math)+(0.97995*mydata$female)+(0.05906*mydata$read)))

head(mydata)

###Logistic Poisson

p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)

with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")

summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))

library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

r.est

with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


## calculate and store predicted values
p$phat <- predict(m1, type="response")

## order by program and then by math
p <- p[with(p, order(prog, math)), ]

## create the plot
ggplot(p, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")
