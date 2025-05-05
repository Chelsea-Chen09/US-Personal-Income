install.packages('ipumsr')
library(ipumsr)
ddi <- read_ipums_ddi("/Users/mac/Desktop/usa_00002.xml")
data <- read_ipums_micro(ddi)
df <- as.data.frame(data)

# set the representative larger than 500
df1 <- na.omit(data[df$PERWT > 500,])
table(df1$CITIZEN)
table(df1$EDUC)

# data cleaning
df1$CITIZENR <- ifelse(df1$CITIZEN == 1, 1,
                       ifelse(df1$CITIZEN == 2, 1,
                              ifelse(df1$CITIZEN == 0, NA,0)))
df1$RACER <- ifelse(df1$RACE == 1,1,0)
df1$SEXR <- ifelse(df1$SEX == 1,1,0)
df1$INCTOTR <- ifelse(df1$INCTOT <= 0, NA, df1$INCTOT)
df1$agec <- df1$AGE - mean(df1$AGE)
dfR <- na.omit(df1[,c('INCTOTR','CITIZENR','EDUC','UHRSWORK',
                      'RACER','SEXR','NCHILD','SERIAL','agec')])
# regression model
lm_education <- lm(dfR$INCTOTR ~ dfR$EDUC)
summary(lm_education) # control model

lm_full <- lm(dfR$INCTOTR ~ dfR$EDUC + dfR$CITIZENR  + dfR$agec  + dfR$UHRSWORK)
summary(lm) # model with iv

# diagnosis
##### Linearity #####
library(car)
scatterplotMatrix(dfR[,c('INCTOTR','agec','EDUC','UHRSWORK')],
                  cex = .5,
                  pch = 16,
                  col = rgb(0,0,0,1/32),
                  diagonal=list(method ="histogram",
                                breaks = 20),
                  cex.labels = 0.5,
                  regLine=list(method = lm,
                               lty = 1,
                               lwd = 1,
                               col = 1),
                  smooth = list(method = "loessLine",
                                lty.smooth = 2,
                                lwd.smooth = 1,
                                col.smooth = 2,
                                lty.spread = 3,
                                lwd.spread = 1,
                                col.spread = 2))
# correction of INICTOT
dfR$INCTOTlog <- ifelse(dfR$INCTOTR <= 1, 0, ifelse(dfR$INCTOTR > 1, log(dfR$INCTOTR),NA))
dfR$INCTOTlog1 <- ifelse(dfR$INCTOTlog > 15, NA,
                         ifelse(dfR$INCTOTlog < 6, NA, dfR$INCTOTlog))
# only investigate log of income between 6 and 15
hist(dfR$INCTOTlog1)
scatterplotMatrix(dfR[,c('INCTOTlog1','agec','EDUC','UHRSWORK')],
                  cex = .5,
                  pch = 16,
                  col = rgb(0,0,0,1/32),
                  diagonal=list(method ="histogram",
                                breaks = 20),
                  cex.labels = 0.5,
                  regLine=list(method = lm,
                               lty = 1,
                               lwd = 1,
                               col = 1),
                  smooth = list(method = "loessLine",
                                lty.smooth = 2,
                                lwd.smooth = 1,
                                col.smooth = 2,
                                lty.spread = 3,
                                lwd.spread = 1,
                                col.spread = 2))
lm1 <- lm(dfR$INCTOTlog1 ~ dfR$EDUC + dfR$CITIZENR + dfR$agec + dfR$UHRSWORK)
summary(lm1) # new model after correction
# family size is no longer significant

##### Normality of residuals #####
df_resid <- resid(lm1)
df_fitted <- fitted(lm1)
hist(df_resid,breaks = 50)
##### Homoscedasticity #####
df_resid1 <- resid(lm1)
df_fitted1 <- fitted(lm1)
scatterplot(df_fitted1,df_resid1,
            jitter = list("x" = 1,
                          "y" = 0))

##### Omitted Relevant Variables #####
## Correlation analysis
# multicollinearity
cor_matrix <- cor(dfR[,c('CITIZENR','agec','EDUC','UHRSWORK','RACER','SEXR','NCHILD')])
cor_matrix[upper.tri(cor_matrix)] <- NA
round(cor_matrix, 4) 

cor.test(dfR$SEXR,dfR$INCTOTlog1,use = "complete.obs") #significantly positive
cor.test(dfR$RACER,dfR$INCTOTlog1,use = "complete.obs") # not significant
cor.test(dfR$NCHILD,dfR$INCTOTlog1,use = "complete.obs") #significantly positive

lm2 <- lm(dfR$INCTOTlog1 ~ dfR$EDUC + dfR$CITIZENR + dfR$agec + dfR$UHRSWORK + 
            dfR$SEXR + dfR$NCHILD) # new model with an omitted variable - gender
summary(lm2)

summary(dfR)
round(apply(dfR, 2, sd), 4)

# outlier
# influence will be used
dfR1 <- na.omit(dfR[,c('INCTOTlog1','CITIZENR','agec','EDUC','UHRSWORK',
                      'SEXR','NCHILD','SERIAL')])
dfR1$coosd <- cooks.distance(lm2)
large_cooksd <- subset(dfR1,abs(dfR1$coosd) > 4/2483)
177/2483 # 7% too large
plot(lm2,which = 4)
round(quantile(dfR1$coosd[dfR1$coosd > 4/2483],probs = seq(0,1,0.05)),4)
large_cooksd1 <- subset(dfR1,abs(dfR1$coosd) > 0.0064)
lm3 <- lm(INCTOTlog1 ~ EDUC + CITIZENR + agec + UHRSWORK+ SEXR + NCHILD, data = dfR1[dfR1$coosd <= 0.0064,])
summary(lm3)

# interaction
dfR1$EDUC_CITIZENR <- dfR1$EDUC * dfR1$CITIZENR
dfR1$EDUC_agec <- dfR1$EDUC * dfR1$agec
dfR1$EDUC_UHRSWORK <- dfR1$EDUC * dfR1$UHRSWORK
dfR1$EDUC_SEXR <- dfR1$EDUC * dfR1$SEXR
dfR1$EDUC_NCHILD <- dfR1$EDUC * dfR1$NCHILD

cor_matrix <- cor(dfR[,c('CITIZENR','agec','EDUC','UHRSWORK','SEXR','NCHILD','EDUC_NCHILD','EDUC_UHRSWORK','EDUC_SEXR')])
cor_matrix[upper.tri(cor_matrix)] <- NA
round(cor_matrix, 4) 

dfR1$EDUCc <- dfR1$EDUC-mean(dfR1$EDUC)
dfR1$EDUCc_NCHILD <- dfR1$EDUCc * dfR1$NCHILD
dfR1$EDUCc_SEXR <- dfR1$EDUCc * dfR1$SEXR
dfR1$EDUCc_UHRSWORK <- dfR1$EDUCc * dfR1$UHRSWORK

cor_matrix <- cor(dfR1[,c('CITIZENR','agec','EDUCc','UHRSWORK','SEXR','NCHILD','EDUCc_NCHILD','EDUCc_UHRSWORK','EDUCc_SEXR')])
cor_matrix[upper.tri(cor_matrix)] <- NA
round(cor_matrix, 4) 

lm4 <- lm(INCTOTlog1 ~ EDUCc + CITIZENR + agec + UHRSWORK + SEXR  + NCHILD + EDUCc_NCHILD +
            EDUCc_UHRSWORK + EDUCc_SEXR, data = dfR1[dfR1$coosd < 0.0064,])
summary(lm4)
# regression models summary
lm_intercept2 <- lm(dfR$INCTOTlog1 ~ 1)
lm_intercept3 <- lm(INCTOTlog1 ~ 1, data = dfR1[dfR1$coosd < 0.0064,])
library(stargazer)
stargazer(lm_education, lm_full, lm1, lm2, lm3, lm4,
          type = "text", 
          header = F, 
          intercept.bottom = F, 
          no.space = T, 
          single.row = T)

anova(lm_intercept2,lm1,lm2)

AIC(lm2)/(2483)
AIC(lm3)/(2483-9)
AIC(lm4)/(2483-9)

# Hierarchical modeling
library(nlme)
dfR1$family <- dfR1$SERIAL
table(table(dfR1$family))
##Fully Unconditional Model##
model1 <- lme(INCTOTlog1 ~ 1,
              random = ~ 1| family,
              data = dfR1[dfR1$coosd < 0.0064,],
              method = 'ML')
summary(model1)
# 0.5274321/(0.5274321+0.971962) = 35.18%
## multiple model ##
# educ: level 1 and random effects
# CHILDR: level 2 and fixed effects
# Interaction is based on the assumption that 
# the difference in EDUC impact may be explained by the level-2 IV
model2 <- lme(INCTOTlog1 ~ EDUCc + CITIZENR + agec + UHRSWORK + SEXR  + NCHILD + EDUCc_NCHILD +
                EDUCc_UHRSWORK,
              random = ~ EDUC| family,
              data = dfR1[dfR1$coosd < 0.0064,],
              method = 'ML')
summary(model2)
0.4303 / (0.4303 + 0.7670)
## to test if EDUC has random effects
model3 <- lme(INCTOTlog1 ~ EDUCc + CITIZENR + agec + UHRSWORK + SEXR  + NCHILD + EDUCc_NCHILD +
                EDUCc_UHRSWORK,
              random = ~ 1| family,
              data = dfR1[dfR1$coosd < 0.0064,],
              method = 'ML')
summary(model3)
anova(model3,model2) # yes
