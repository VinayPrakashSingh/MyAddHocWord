library(dplyr)

list.of.packages <- c("data.table","bit64","optimx","lme4",'languageR','lmerTest','glmnet','lsmeans','car','RDS','DMwR','stringr','nloptr','ggplot2','reshape2','plyr','viridis','gmodels')
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,require,character.only=TRUE)

path="D:\\MCOE_DEV\\MMT_NEW_THINGS\\Ancova\\48\\"

setwd(path)

model_data_final=read.csv("model_data_after_outlier.csv")
names(model_data_final)
model_data_final_1=model_data_final
model_data_final$PERIOD=as.factor(model_data_final$PERIOD)
model_data_final$TEST_CONTROL=as.factor(model_data_final$TEST_CONTROL)

model_data_final$PERIOD=relevel(model_data_final$PERIOD,1)
model_data_final$TEST_CONTROL=relevel(model_data_final$TEST_CONTROL,1)
levels(model_data_final$PERIOD)
######Fixed Effect #########################
formula="DOLACV1 ~ TEST_CONTROL * PERIOD"
#model_full = fit!(lm(formula, train, wts = convert(Array{Float64,1}, train[Symbol("ACV",string(cfg["PID_CNT"]))])))
model_full = lm(formula, model_data_final,weights = model_data_final$ACV1)
summary(model_full)

model_data_final$predicted1=predict(model_full,model_data_final)
model_data_final$residuals=residuals(model_full)
model_data_final_1=model_data_final
model_data_final_1$TEST_CONTROL=ifelse(as.numeric(model_data_final_1$TEST_CONTROL)==2,1,0)
model_data_final_1$PERIOD=ifelse(as.numeric(model_data_final_1$PERIOD)==2,1,0)

model_data_final_1$predicted1 = 24.4117+ (-1.5184* model_data_final_1$TEST_CONTROL)+
  (1.7103* model_data_final_1$PERIOD) + (-0.9224* model_data_final_1$TEST_CONTROL* model_data_final_1$PERIOD ) 
model_data_final_1$residuals=residuals(model_full)
sum(model_data_final_1$predicted1)


model_data_final[model_data_final$STORE==33423,c("STORE","PERIOD","TEST_CONTROL","predicted1","residuals")]
model_data_final_1[model_data_final_1$STORE==33423,c("STORE","PERIOD","TEST_CONTROL","predicted1","residuals")]

model_data_final[model_data_final$STORE==100127,c("STORE","PERIOD","TEST_CONTROL","predicted1","residuals")]
model_data_final_1[model_data_final_1$STORE==100127,c("STORE","PERIOD","TEST_CONTROL","predicted1","residuals")]

cor(model_data_final$predicted1,model_data_final$DOLACV1)
######Store as random effect#############
formula="DOLACV1 ~ TEST_CONTROL * PERIOD + (1|STORE)"
model_full_1 = lmer(formula, model_data_final,weights= model_data_final$ACV1)
summary(model_full_1)
ranef(model_full_1)

model_data_final$predicted1=predict(model_full_1,model_data_final)
a=as.data.frame(ranef(model_full_1))

model_data_final_1=model_data_final
model_data_final_1=merge(model_data_final_1,a,by.x = "STORE",by.y = "grp")

model_data_final_1$TEST_CONTROL=ifelse(as.numeric(model_data_final_1$TEST_CONTROL)==2,1,0)
model_data_final_1$PERIOD=ifelse(as.numeric(model_data_final_1$PERIOD)==2,1,0)

model_data_final_1$predicted1 = 25.1182+ (-1.8359* model_data_final_1$TEST_CONTROL)+
                            (1.6910* model_data_final_1$PERIOD) +
                            (-0.9516* model_data_final_1$TEST_CONTROL* model_data_final_1$PERIOD) +model_data_final_1$condval


model_data_final[model_data_final$STORE==33423,c("STORE","PERIOD","TEST_CONTROL","predicted1")]
model_data_final_1[model_data_final_1$STORE==33423,c("STORE","PERIOD","TEST_CONTROL","predicted1","condval")]

model_data_final[model_data_final$STORE==100127,c("STORE","PERIOD","TEST_CONTROL","predicted1","residuals")]
model_data_final_1[model_data_final_1$STORE==100127,c("STORE","PERIOD","TEST_CONTROL","predicted1","residuals")]

cor(model_data_final$predicted1,model_data_final$DOLACV1)
##############Random Slope of DOLACV0############
formula="DOLACV1 ~ TEST_CONTROL * PERIOD + (0+DOLACV0|STORE)"
model_full_1 = lmer(formula, model_data_final,weights= model_data_final$ACV1)
summary(model_full_1)

ranef(model_full_1)
model_data_final$predicted=predict(model_full_1,model_data_final)

a=as.data.frame(ranef(model_full_1))

model_data_final_1=model_data_final
model_data_final_1=merge(model_data_final_1,a,by.x = "STORE",by.y = "grp")

model_data_final_1$TEST_CONTROL=ifelse(as.numeric(model_data_final_1$TEST_CONTROL)==2,1,0)
model_data_final_1$PERIOD=ifelse(as.numeric(model_data_final_1$PERIOD)==2,1,0)

model_data_final_1=model_data_final_1[model_data_final_1$STORE==33423,]
model_data_final_1$predicted1 = 23.0946+ (-2.1985* model_data_final_1$TEST_CONTROL)+
                            (1.7910* model_data_final_1$PERIOD) +
                            (-0.8331* model_data_final_1$TEST_CONTROL* model_data_final_1$PERIOD) +(model_data_final_1$condval*model_data_final_1$DOLACV0)



model_data_final[model_data_final$STORE==33423,c("STORE","PERIOD","TEST_CONTROL","predicted")]
model_data_final_1[model_data_final_1$STORE==33423,c("STORE","PERIOD","TEST_CONTROL","DOLACV0","predicted1")]

model_data_final[model_data_final$STORE==100127,c("STORE","PERIOD","TEST_CONTROL","predicted")]
model_data_final_1[model_data_final_1$STORE==100127,c("STORE","PERIOD","TEST_CONTROL","predicted1")]

cor(model_data_final$predicted,model_data_final$DOLACV1)
##############Random Intercept with Random Slope of DOLACV0############


formula="DOLACV1 ~ TEST_CONTROL * PERIOD + (DOLACV0|STORE)"
model_full_1 = lmer(formula, model_data_final,weights= model_data_final$ACV1)
summary(model_full_1)

ranef(model_full_1)
model_data_final$predicted=predict(model_full_1,model_data_final)

formula="DOLACV1 ~ TEST_CONTROL * PERIOD + (1+DOLACV0|STORE)"
model_full_2 = lmer(formula, model_data_final,weights= model_data_final$ACV1)
summary(model_full_2)

ranef(model_full_2)
model_data_final$predicted1=predict(model_full_2,model_data_final)
sum(predict(model_full_1,model_data_final))
sum(predict(model_full_2,model_data_final))

cor(predict(model_full_1,model_data_final),model_data_final$DOLACV1)
cor(predict(model_full_2,model_data_final),model_data_final$DOLACV1)

a=as.data.frame(ranef(model_full_2))
b=merge(a[a$term=="(Intercept)",],a[a$term!="(Intercept)",],by.x = "grp", by.y = "grp")
model_data_final_1=model_data_final
model_data_final_1=merge(model_data_final_1,b,by.x = "STORE",by.y = "grp")

model_data_final_1$TEST_CONTROL=ifelse(as.numeric(model_data_final_1$TEST_CONTROL)==2,1,0)
model_data_final_1$PERIOD=ifelse(as.numeric(model_data_final_1$PERIOD)==2,1,0)


head(model_data_final_1)
model_data_final_1$predicted1 = 24.1430+ (-2.1095* model_data_final_1$TEST_CONTROL)+
                            (1.7894* model_data_final_1$PERIOD) +
                            (-0.7986* model_data_final_1$TEST_CONTROL* model_data_final_1$PERIOD)+ model_data_final_1$condval.x +model_data_final_1$condval.y*model_data_final_1$DOLACV0

model_data_final[model_data_final$STORE==33423,c("STORE","PERIOD","TEST_CONTROL","predicted1")]
model_data_final_1[model_data_final_1$STORE==33423,c("STORE","PERIOD","TEST_CONTROL","predicted1")]
sum(model_data_final_1$predicted1)
sum(model_data_final$predicted1)
cor(model_data_final_1$predicted1,model_data_final_1$DOLACV1)




