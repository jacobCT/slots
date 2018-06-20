library(fishmethods);library(sas7bdat);library(FSA)

tog <- read.csv("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/Slot-MEffects/database/raw/LIS_Tog_AL_data.csv")

tog <- tog[tog$year >= 2012,]
growth(intype = 1, unit = 1, size = tog$length, age = tog$age,calctype = 1,
       Sinf = 578 , K = 0.174, t0 = -0.409)


ct.lis <- read.sas7bdat("file:///C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/SAS_datasets_used_by_Eric/ct_len_wt_age.sas7bdat")

ct.lis <- ct.lis[ct.lis$Year >= 2012, ]
LWdata <- data.frame(CM = ct.lis$Length/10,
                     KG = ct.lis$Weight/1000)
LWdata <-  na.omit(LWdata)
LWdata$logL <- log(LWdata$CM)
LWdata$logW <- log(LWdata$KG)
lm1 <- lm(logW ~ logL, data = LWdata)
fitPlot(lm1,xlab="log Total Length (CM)",ylab="log Weight (KG)",main="")
lw.coef <- exp(summary(lm1)$coefficients[1])
lw.exp <- summary(lm1)$coefficients[2]

