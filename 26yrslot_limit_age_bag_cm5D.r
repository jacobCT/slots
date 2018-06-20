## Objective
## 1. Calculated slot yield (n) by weight
## 2. Calculate bag specific slot reductions
## 3. Selectivity as calculated by catch/population structur
## 4. Include dead discards
## 5. Explore all slot options inclusive of max/min size
## 6. Include rec harvest scaling for non compliant harvest
## 7. Recrational and commercial harvest


##Options:
## cur.catch
#### a. Is this for commercial or rec only?
#### b. Seasonal changes? They need to be inpusted here.
#### c. Are discards included?

##Inputs
## New_MRIP_Bag.r (MRIP_read_data.r)
## LIS Age-Length raw data read.csv("C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Amendment1/database/LIS_Tog_AL_data.csv
## Population age structure from ASAP (mean over last selex block)
## C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Amendment1/database/Selectivity_worksheet.csv

##Variables
max.age <- "XXXX"  #input for max age in analysis
##seasonal closing factor
scale.sc <- "XXXX"
##range of useful percent reductions for script to give as output
pr.min <- "XXXX" 0.40 ##min percent reduction
pr.max <- "XXXX" 0.50 ##max percent reduction

##Data to read
source("file:///C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/database/ALS.r")
source("file:///C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Slot-MEffects/26yrplus_slot/New_MRIP_Bag_26yr.r")
##Functions List
source("file:///C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/database/functions.r")
##CSVs to read
t9 <- read.csv("file:///C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/database/tog_9lengths_LIS_2004-15.csv")

library(ggplot2); library(FSA); library(magrittr); library(dplyr); library(nnet)
library(fishmethods); library(zoo)
options(scipen=999)

cur.catch <- data.frame(year = c(2012:2015),
                        rec.h = c(220194, 122376, 354777, 201602),
                        ##com harvest in n
                        com.h = c(14010, 17708, 24193, 18941),
                        ##n discards, not just dead discards
                        dis = c(880195, 629212, 2420049, 1031494))
cur.catch$har <- rowSums(cur.catch[2:3]) #include com if including com
mean.catch <- as.data.frame(t(round(colMeans(cur.catch[2:5])))) ##mean catch
##allows for scaling re seasonal closures
## mean.catch <- round(mean(cur.catch$har) * (1 - scale.sc))
##growth information
##"C:\Users\jacob\OneDrive\UCONN\Projects\Tautog_ASMFC\eric data analysis\February 2017\total fecundity estimate new2.xls"
##based on ETS work, pooled 2013:2015, should update to include 2012?
lwy <-  data.frame(RMSE = 0.07744, Intercept = -4.76752, exponent = 3.02371,
                   coefficent = 0.00001708)
############# START  #########################
##input measured lengths
## lengths of released fish


##TO INCLUDE ADDITIONAL MEASURED LENGTHS OUTSIDE OF MRIP#####
##VAS
df1 <- vas.US
df1$rown <- rownames(df1)
df2 <- samp.vas.US
df2$rown <- rownames(df2)
##to find the undersized fish caught in VAS not attributed to harvest
vas.r1 <- unique.rows(df1, df2)
vas.r1 <- vas.r1[, - c(2, 4)]
# CT   5/1995-2/2012 14 inches  356 cm
# CT   3/2012-present 16 inches 406 cm

##HB
nyHBR <- nyHB[nyHB$MODE == "Rec" & nyHB$KEEP_REL == "R" & nyHB$YEAR >= 2012, ]
nyHBR <- data.frame(year = nyHBR$YEAR, length_cm = nyHBR$LENGTH_CM)

##MRIP Type 9
t9 <- t9[t9$YEAR >= 2012, ]
t9 <- data.frame(year = t9$YEAR, length_cm = t9$LNGTH.CM)


##combine all release lengths
rLen <- rbind(vas.r1, nyHBR, t9, als.lis)
table(rLen$length_cm)
rLen$length_cm[rLen$length_cm < 15] <- 15 #pooling all fish < 15 cm
rLen$length_cm[rLen$length_cm > 40] <- 40 #pooling all fish < 15 cm
## no released fish >= 60
rLen16 <- rLen
##rLen16 <- as.data.frame(colMeans(table(rLen16)))
rLen16 <- as.data.frame(table(rLen16$length_cm))
colnames(rLen16) <- c("length", "dis")

## lengths of harvested fish
colnames(all_len)[7] <- "length"
hLen <- all_len
table(hLen)
hLen$length[hLen$length > 59] <- 59 #pooling all fish > 59 cm
hLen$length[hLen$length < 33] <- 33 #pooling all fish > 59 cm
hLen16 <- hLen
##hLen16 <- as.data.frame(colMeans(table(hLen16)))
hLen16 <-  as.data.frame(table(hLen16$length))
colnames(hLen16) <- c("length", "har")
##measured harvested fish years 2013-2015
############# END  #########################


############# START  #########################
##empty data frame to fill in harvest and released length dists
hr_table <- data.frame(length = c(min(as.numeric(as.character(rLen16$length)))):
                                  max(as.numeric(as.character(hLen16$length))))
##fill in empty table
hr_table[match(hLen16$length, hr_table$length), paste("har")] <-
           hLen16[, 2]
hr_table[match(rLen16$length, hr_table$length), paste("dis")] <-
           rLen16[, 2]
hr_table[is.na(hr_table)] <- "0" ##replace NA with 0
##hr_table <- hr_table[hr_table$length > 16, ]
## change characters to numeric
hr_table[, c(2:3)]<- apply(hr_table[,c(2:3)], 2, function(x) as.numeric(as.character(x)))

hr_table$catch <- rowSums(hr_table [, 2:3]) ##har + discards= catch
##scale measured length  by harvest/discards/catch
##Rec harvest including non.compliance
hr_table$s.rec.har <- round((mean.catch$rec.h * (hr_table$har / sum(hr_table$har))) * ((1 + pr.non.compliant)))
##Commercial Harvest
hr_table$s.com.har <- round(mean.catch$com.h * (hr_table$har / sum(hr_table$har)))
##SQ harvest
hr_table$sq.har <- round(mean.catch$har * (hr_table$har / sum(hr_table$har)))

hr_table$s.dis <- round(mean.catch$dis * (hr_table$dis / sum(hr_table$dis)))

hr_table$sq.catch <- hr_table$sq.har + hr_table$s.dis ##har + discards= catch
hr_table$slot.catch <- round(hr_table$s.rec.har + hr_table$s.com.har + hr_table$s.dis) ##har + discards= catch

hr_table$P.h <- hr_table$s.har / hr_table$s.catch
##hr_table$P.h.l <- hr_table$s.har / sum(hr_table$s.har)
##hr_table$P.d.l <- hr_table$s.dis / sum(hr_table$s.dis)
hr_table$P.l <- hr_table$s.catch / sum(hr_table$s.catc)

sum(hr_table$s.har[hr_table$length < m]) / sum(hr_table$s.har)

##proportion at length
##weight of fish in size class
hr_table$wt <- (lwy$coefficent * (hr_table$length * 10) ^
                                      (lwy$exponent) + lwy$Intercept) / 1000
##hr_table$h.est <- round(sum(hr_table$s.har) * hr_table$P.h.l * hr_table$wt * 1) #P.h set to 1
##hr_table$d.est <- round(sum(hr_table$s.dis) * hr_table$P.d.l * hr_table$wt * 1) #P.h set to 1
##hr_table$har.est <- hr_table$h.est + hr_table$d.est
hr_table$h.est <- sum(hr_table$s.catch) * hr_table$P.l * hr_table$wt * 1

############# START  #########################
##calculate current harvest
m <- 40 ##minimum legal size
current.harvest <- NULL
current.harvest <- rbind(current.harvest,
                   sum(hr_table$h.est[hr_table$length >= m]) +
##                   sum(hr_table$h.est[hr_table$length >= m]) * pr.non.compliant +
                   sum(hr_table$h.est[hr_table$length < m]) * .025)

############# END  #########################




############# START  #########################
##calculate harvest reduction
harvest.redux <- NULL
harvest.hal <- NULL

harvest.hal$length <- unique(hr_table$length)
for(slmin in min(hr_table$length):(max(hr_table$length) - 1)) {
    for(slmax in ((slmin + 1):max(hr_table$length))) {
        h.hal <- data.frame(length = harvest.hal$length)
        tmp.h <- sum(hr_table$h.est[hr_table$length >= slmin &
                                    hr_table$length <= slmax])
        ##add in dead discards to harvest estimate
        ##tmp.h <- tmp.h +
        ##    (.025 * (sum(hr_table$h.est) - tmp.h))
        tmp.h <- tmp.h + (tmp.h * pr.non.compliant) +
            (.025 * (sum(hr_table$h.est) - tmp.h))
        ##calculate percent redux
        redux.by <- round((tmp.h / current.harvest), 4)
        tmp.hal <- data.frame(length = c(slmin:(slmax)),
                              hr_table$h.est[hr_table$length >= slmin &
                                             hr_table$length <= slmax])
        tmp <- cbind(paste("h", slmin, slmax, sep = "_"), slmin, slmax,
                     tmp.h, redux.by)
        colnames(tmp) <- c("slot", "slmin", "slmax", "tmp.h", "redux.by")
        h.hal[match(tmp.hal$length, h.hal$length),
              paste ("h", slmin, slmax, sep = "_")] <- tmp.hal[, 2]
        harvest.redux <- rbind(harvest.redux, tmp)
        harvest.hal <- cbind(harvest.hal, h.hal[2])
    }
}

##convert factors to numeric
harvest.redux <- as.data.frame(harvest.redux) ##switch to DF
harvest.redux[, 2:5] <- apply(harvest.redux[, 2:5], 2, ##to numeric
                              function(x) as.numeric(as.character(x)))
harvest.redux$slmin_in <- round(harvest.redux$slmin * 0.393701, 2) ##in slot
harvest.redux$slmax_in <- round(harvest.redux$slmax * 0.393701, 2) ##in slot
harvest.hal <- t(harvest.hal)
colnames(harvest.hal) <- harvest.hal[1, ] ##drop length row
harvest.hal <- harvest.hal[-1, ]
harvest.redux <- cbind(harvest.redux, harvest.hal)
harvest.redux$pr.redux <- 1 - harvest.redux$redux.by
harvest.redux$min <- harvest.redux$slmin
harvest.redux$max <- harvest.redux$slmax
head(harvest.redux)


##Harvest reductions including bag limit changes
sl.bag <- merge(harvest.redux, bag.redux, by = c("min", "max"))

##calculate as pr redux
sl.bag$pr.redux.B4 <- 1 - (sl.bag$redux.by * sl.bag$New.B4.redux)
sl.bag$pr.redux.B3 <- 1 - (sl.bag$redux.by * sl.bag$New.B3.redux)
sl.bag$pr.redux.B2 <- 1 - (sl.bag$redux.by * sl.bag$New.B2.redux)
sl.bag$pr.redux.B1 <- 1 - (sl.bag$redux.by * sl.bag$New.B1.redux)
head(sl.bag)

pr.min <- .405
pr.max <- .465

candidate.slots.B4 <- sl.bag[sl.bag$pr.redux.B4 > pr.min &
                             sl.bag$pr.redux.B4 < pr.max, ]

candidate.slots.B3 <- sl.bag[sl.bag$pr.redux.B3 > pr.min &
                             sl.bag$pr.redux.B3 < pr.max, ]

candidate.slots.B2 <- sl.bag[sl.bag$pr.redux.B2 > pr.min &
                             sl.bag$pr.redux.B2 < pr.max, ]

candidate.slots.B1 <- sl.bag[sl.bag$pr.redux.B1 > pr.min &
                             sl.bag$pr.redux.B1 < pr.max, ]

candidate.slots.B4 <- candidate.slots.B4[c(-55:-67, -69, -70, -71)]
colnames(candidate.slots.B4)[55] <- "pr.redux"

candidate.slots.B3 <- candidate.slots.B3[c(-55:-68, -70, -71)]
colnames(candidate.slots.B3)[55] <- "pr.redux"

candidate.slots.B2 <- candidate.slots.B2[c(-55:-69, -71)]
colnames(candidate.slots.B2)[55] <- "pr.redux"

candidate.slots.B1 <- candidate.slots.B1[c(-55:-70)]
colnames(candidate.slots.B1)[55] <- "pr.redux"



candidate.slots.B3 <- sl.bag[sl.bag$pr.redux.B3 > pr.min &
                             sl.bag$pr.redux.B3 < pr.max, ]
candidate.slots.B3 <- candidate.slots.B3[c(-74, -76, -77)]
colnames(candidate.slots.B3)[74] <- "pr.redux"

candidate.slots.B2 <- sl.bag[sl.bag$pr.redux.B2 > pr.min &
                             sl.bag$pr.redux.B2 < pr.max, ]

candidate.slots.B2 <- candidate.slots.B2[c(-74, -75, -77)]
dim(candidate.slots.B2)
colnames(candidate.slots.B2)[74] <- "pr.redux"

candidate.slots.B1 <- sl.bag[sl.bag$pr.redux.B1 > pr.min &
                             sl.bag$pr.redux.B1 < pr.max, ]
candidate.slots.B1 <- candidate.slots.B1[c(-74, -75, -76)]
colnames(candidate.slots.B1)[74] <- "pr.redux"

##write.csv(candidate.slots.B1, "can_slots_B_1.csv" )
##write.csv(candidate.slots.B2, "can_slots_B_2.csv" )
##write.csv(candidate.slots.B3, "can_slots_B_3.csv" )
##write.csv(candidate.slots.B4, "can_slots_B_4.csv" )
range(sl.bag$slmax)

##make rainbow flyer

p <- ggplot(data = harvest.redux,
            aes(x = slmin + .5, y = slmax + .5, z = pr.redux)) +
    geom_tile(aes(fill = pr.redux)) +
    scale_y_continuous(breaks = round(seq(min(harvest.redux$slmin),
                                      max(harvest.redux$slmax), by = 2), 1)) +
    scale_x_continuous(breaks = round(seq(min(harvest.redux$slmin),
                                          max(harvest.redux$slmax), by = 2), 1),
                       sec.axis = dup_axis()) +
    scale_fill_gradientn(limits = c(min(round(harvest.redux$pr.redux)),
                                    max(round(harvest.redux$pr.redux))),
                         colours = rainbow(7)) +
    labs(x = "Slot Minimum", y = "Slot Maximum", fill = "") +
    ggtitle(paste0("Proportional Harvest Reduction", sep = " ")) +
    theme_classic() + theme_bw() +
    theme(plot.title = element_text(hjust = .5),
          legend.position = c(.8, .31),
          legend.key.size = unit(1.5, "cm"),
          legend.background = element_rect(fill = alpha("white", 0.1)),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 18))  +
    guides(color=guide_legend(override.aes=list(fill = NA)))
p


b0 <-  geom_point(data = candidate.slots.B0, colour = "black",
                  cex = 4, pch = 17)
b4 <-  geom_point(data = candidate.slots.B4, colour = "black",
                  cex = 4, pch = 1)
b3 <-  geom_point(data = candidate.slots.B3, colour = "black",
                  cex = 3, pch = 1)
b2 <-  geom_point(data = candidate.slots.B2, colour = "black",
                  cex = 2, pch = 1)
b1 <-  geom_point(data = candidate.slots.B1, colour = "black",
                  cex = 1, pch = 1)
sq <- geom_point(x = 40, y = 65, colour = "black", cex = 6, pch = 19)


f <-  p  +  sq + ##b0 + ##+ b4 + b3 + b2 + b1 + sq +
##    geom_point(x = 33, y = 28, colour = "black", cex = 4, pch = 17) +
##    geom_point(x = 35, y = 28, colour = "black",
##               cex = 3, pch = 1) +
##    geom_point(x = 35, y = 26, colour = "black",
##               cex = 2, pch = 1) +
##    geom_point(x = 35, y = 24, colour = "black",
##               cex = 1, pch = 1) +
    geom_point(x = 33, y = 30, colour = "black",
               cex = 6, pch = 19) +
##    geom_text(data = NULL, x = 43, y = 28,
##              label = "Harvest Slot 44-50% reduction") +
##    geom_text(data = NULL, x = 40, y = 28, label = "= 3 bag slot") +
##    geom_text(data = NULL, x = 40, y = 26, label = "= 2 bag slot") +
##    geom_text(data = NULL, x = 40, y = 24, label = "= 1 bag slot") +
    geom_text(data = NULL, x = 41, y = 30, label = "Current Minimum Size")
l  <- geom_text(data = NULL, x = 56, y = 45, label = "Percent Reductions")
z <- f + l
print(z)


z + geom_point(x = 41, y = 47, colour = "darkgrey",
              cex = 10, pch = 20) +
geom_point(x = 33, y = 26, colour = "darkgrey", cex = 10, pch = 20) +
geom_text(data = NULL, x = 38.5, y = 26, label = "41-47 cm slot")

##dev.off()

## ##write.csv(candidate.slots, "candidate_slots_pooled_spawning_no_wt.csv")
## write.csv(candidate.slots, "candidate_slots_pooled_spawning.csv")



############################################################################
##apply modeled Age-Length relationship to harvest to calculate selectivity
############################################################################

al <- read.csv("C:/Users/Jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Slot-Meffects/growth model/data/LIS_Tog_AL_data.csv")
al$age[al$age > 26] <- 26 ##cap age at 26 years
y <- c(2012:2015)
al.y <- al[al$year %in% y, ]
tmp.mlr <- multinom(age ~ length, data = al.y, maxit = 500)

catch <- data.frame(length = hr_table$length,
                             freq = hr_table$s.catch)
popN <- read.csv("Selectivity_worksheet.csv", header = T)


for(b in 1:4){
    sl.tmp <- sl.bag[sl.bag[ paste("pr.redux.B", b,  sep ="")] > pr.min    &
                     sl.bag[ paste("pr.redux.B", b,  sep ="")] < pr.max, ]
    harvestN <- t(sl.tmp[10:60])
    harvestN[is.na(harvestN)] <- 0
    harvestN <- round(harvestN)
    lens <- as.numeric(rownames(harvestN))
    harvestN <- rbind(t(sl.tmp[ paste("pr.redux.B", b,  sep ="")]), harvestN)
    colnames(harvestN) <- sl.tmp$slot
    prop.mlr <- predict(tmp.mlr, data.frame(length = lens),
                    type = "probs")
    rownames(prop.mlr) <- lens



    selex <- NULL
    selex2 <- NULL
    for(c in 1:ncol(harvestN)){
        tmp.age.harvest <- as.data.frame(colSums(prop.mlr  *
                                                 harvestN[2:nrow(harvestN), c]))
        colnames(tmp.age.harvest) <- "Freq"
        tmp.age.catch <-  as.data.frame(colSums(prop.mlr  *
                                                catch$freq))
        colnames(tmp.age.catch) <- "Freq"
        tmp.age.catch  <- data.frame(age = as.numeric(rownames(tmp.age.catch)),
                                     Freq = tmp.age.catch$Freq)
        ##harvest at age for one slot
        tmp.harvestN <- data.frame(age = as.numeric(rownames(tmp.age.harvest)),
                                   Freq = tmp.age.harvest$Freq)

       mis.age <- setdiff(union(popN$age, tmp.harvestN$age),
                          intersect(popN$age, tmp.harvestN$age))
       tmp.df <- data.frame(age = as.numeric(mis.age), Freq = 0)
       tmp.harvestN <- rbind(tmp.harvestN, tmp.df)
       tmp.harvestN <- tmp.harvestN[order(tmp.harvestN$age), ]

       mis.age.c <- setdiff(union(popN$age, tmp.age.catch$age),
                            intersect(popN$age, tmp.age.catch$age))
       tmp.df.c <- data.frame(age = as.numeric(mis.age.c), Freq = 0)
       tmp.age.catch <- rbind(tmp.age.catch, tmp.df.c)
       tmp.age.catch <- tmp.age.catch[order(tmp.age.catch$age), ]

        tmp.selex <- round(tmp.harvestN$Freq/popN$freq, 8)
        tmp.selex <- data.frame(age = c(1:26),
                  selex = round(tmp.selex, 6))
        tmp.selex <- c(0, rollmean(tmp.selex$selex, 3), 0)
        tmp.selex <- round(tmp.selex, 5)
        pr.redux <- harvestN[1, c]
        tmp.selex <- tmp.selex/max(tmp.selex)
        tmp.selex <- c(pr.redux, tmp.selex)

        tmp.selex2 <- round(tmp.harvestN$Freq/tmp.age.catch$Freq, 8)
        tmp.selex2[is.na(tmp.selex2)] <- 0
        tmp.selex2 <- data.frame(age = c(1:26),
                  selex = round(tmp.selex2, 6))
        tmp.selex2 <- c(0, rollmean(tmp.selex2$selex, 3), 0)
        tmp.selex2 <- round(tmp.selex2, 5)
        tmp.selex2 <- tmp.selex2/max(tmp.selex2)
        tmp.selex2 <- c(pr.redux, tmp.selex2)

        selex <- cbind(selex, as.matrix(tmp.selex))
        selex2 <- cbind(selex2, as.matrix(tmp.selex2))
}
    colnames(selex) <- colnames(harvestN)
    rownames(selex) <- c("pr.redux.pop", tmp.harvestN$age)
    assign(paste("selex.bag.pop",  b, sep = ""), selex)

    colnames(selex2) <- colnames(harvestN)
    rownames(selex2) <- c("pr.redux.catch", tmp.harvestN$age)
    assign(paste("selex.bag.catch",  b, sep = ""), selex2)
}

## write.csv(selex.bag.pop1, "selexbag_pop1.csv")
## write.csv(selex.bag.pop2, "selexbag_pop2.csv")
## write.csv(selex.bag.pop3, "selexbag_pop3.csv")
## write.csv(selex.bag.pop4, "selexbag_pop4.csv")

## write.csv(selex.bag.catch1, "selexbag_catch1.csv")
## write.csv(selex.bag.catch2, "selexbag_catch2.csv")
## write.csv(selex.bag.catch3, "selexbag_catch3.csv")
## write.csv(selex.bag.catch4, "selexbag_catch4.csv")
## write.csv(selex, "selectivity.csv")
plot(selex.bag.pop4[2:27, 4], type = "l", col = "RED", lwd = 2)
lines(selex.bag.catch4[2:27, 2])
par(mar = c(5, 4, 4, 4) + 0.3)
plot(popN$freq, type = "l", xlab = "Age", ylab = "Abundance (n)", cex.axis = 1.2,
     cex.lab = 1.5)
polygon(c(1, popN$age, 26), c(0, popN$freq, 0), col = "purple")
polygon(c(1, tmp.harvestN$age, 26), c(0, tmp.harvestN$Freq, 0), col = "yellow")
legend(18, 1000000, pch = 15, cex = 1.2,  col = c( "purple","yellow" ),
       c("Population", "Harvest"))
legend(18, 1000000, pch = 22, cex = 1.2,  col = c("black"), c("", ""), bty = "n")
par(new = TRUE)
plot(c(1:26), selex.bag.pop4[2:27, 4], axes = FALSE, bty = "n", xlab = "",
     ylab = "", type = "l")
axis(side = 4, cex.axis = 1.2)
mtext("Selectivity", side = 4, line = 3, cex = 1.5)


tmp <- data.frame(age = c(1:26),
                  selex =round(tmp.selex[-1], 6))
tmp <- c(0, rollmean(tmp$selex,3), 0)




##selex using catch as denominator
head(hr_table)


tmp.harvestN$Freq
CAA <- data.frame(age = as.numeric(colnames(prop.mlr)),
                  Freq = colSums(prop.mlr  *
                                 hr_table$s.catch[hr_table$length %in%  lens]))
mis.age <- setdiff(union(tmp.harvestN$age, CAA$age),
                       intersect(tmp.harvestN$age, CAA$age))
tmp.df <- data.frame(age = as.numeric(mis.age), Freq = 0)
CAA <- rbind(CAA, tmp.df)

tmp.harvestN <- tmp.harvestN[order(tmp.harvestN$age), ]
selex2 <- tmp.harvestN[, 2]/ CAA[, 2]
selex2[is.na(selex2)] <- 0
selex2 <- selex2/max(selex2)
plot(selex[, 4], type = "l", lwd =3)
lines(selex2, col = "red")
##harvest at age for one slot


dim(harvest.redux)
sl.tmp <- sl.bag[sl.bag[ paste("pr.redux.B", b,  sep ="")] > pr.min    &
                     sl.bag[ paste("pr.redux.B", b,  sep ="")] < pr.max, ]
harvestN <- t(harvest.redux[8:58])
harvestN[is.na(harvestN)] <- 0
harvestN <- round(harvestN)
lens <- as.numeric(rownames(harvestN))
prop.mlr <- predict(tmp.mlr, data.frame(length = lens),
                    type = "probs")
rownames(prop.mlr) <- lens
colSums(prop.mlr)
tmp.age.harvest <- as.data.frame(colSums(prop.mlr  *
                                         hr_table$s.catch))

rowSums(prop.mlr  * hr_table$s.catch)


y <- c(1:1000)
x <- c(1:1000)

plot(x, y,  xlab = "Length (mm)", ylab = "Fecundity", yaxt = "n", xaxt = "n",
     axes = FALSE, ann = FALSE)
axis(1, labels = FALSE )
title(xlab = "Weight", line = 1, cex.lab = 2)
axis(2, labels = FALSE)
title(ylab = "Fecundity", line = 1, cex.lab = 2)


par(new = TRUE)
plot(x, y^2, col = "blue", new = TRUE, xlab = "", ylab = "", axes = FALSE)

par(new = TRUE)
plot(x, y^3, col = "orange", new = TRUE, xlab = "", ylab = "", axes = FALSE)



par(new = TRUE)
plot(x, y^3, col = "blue", new = TRUE,  xlab = "Length (mm)", ylab = "Fecundity",
     axes = FALSE)

?plot
