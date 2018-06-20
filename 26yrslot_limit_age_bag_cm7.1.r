### Objective
## 1. Calculated slot yield (n) by weight
## 2. Calculate bag specific slot reductions
#### 2.b.new, bag reductions are not length specific, bag > 4 were converted to 4 fish bags, which increased the number of 4 fish bags...
## 3. Selectivity as calculated by catch/population structure asap
## 4. Include dead discards
## 5. Explore all slot options inclusive of min size, below max size
## 6. Include rec harvest scaling for non compliant
## 7. Recrational and commercial harvest


### Options:
## cur.catch
##  a. Is this for commercial or rec only?
##  b. Seasonal changes? They need to be inpusted here.
##  c. Are discards included?
##  d. Growth information
##"C:\Users\jacob\OneDrive\UCONN\Projects\Tautog_ASMFC\eric data analysis\February 2017\total fecundity estimate new2.xls"
## e. length data is based on 2012-2015
## f. age data (for selex) is based on 2012-2015
####ASK ERIC
## g. why? because ASAP popN has fish aged to 26, but if I restrict ages to fish
## measured in 2012:2015 then max age is 17
## h. smoothing function for selectivity, used 3 year average, for all ages,
## repeated age 2 and age max-1 for age 1 and age max
### Inputs
## New_MRIP_Bag.r (MRIP_read_data.r)
## LIS Age-Length raw data read.csv("C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Amendment1/database/LIS_Tog_AL_data.csv
## Population age structure from ASAP (mean over last selex block)
## C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Amendment1/database/Selectivity_worksheet.csv

### Variables
##max.age <- "XXXX"  #input for max age in analysis
### seasonal closing factor
#scale.sc <- "XXXX"
##range of useful percent reductions for script to give as output
pr.min <-  0.20 ##min percent reduction
pr.max <-  0.3 ##max percent reduction
years <- c(2012:2015) ##this is called in mrip length read script,
yr.min <- 2012 ##sets min year for other length data
yr.max <- 2015 ##sets max year for other length data
### Data to read
source("file:///C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/database/ALS.r")
source("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/Slot-MEffects/HarvestSlots/New_MRIP_Bag_26yrorig.r")
source("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/Slot-MEffects/HarvestSlots/BFG.r")
######Functions List
source("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/Slot-MEffects/database/functions/functions.r")
######CSVs to read
t9 <- read.csv("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/Slot-MEffects/database/raw/tog_9lengths_LIS_2004-15.csv")


library(ggplot2); library(FSA); library(magrittr); library(dplyr); library(nnet)
library(fishmethods); library(zoo); library(gridExtra)
options(scipen=999)

cur.catch <- data.frame(year = c(2012:2015),
                        rec.h = c(220194, 122376, 354777, 201602),
                        ##com harvest in n
                        com.h = c(14010, 17708, 24193, 18941),
                        ##n discards, not just dead discards
                        dis = c(880195, 629212, 2420049, 1031494))
cur.catch$har <- rowSums(cur.catch[2:3]) #include com if including com
mean.catch <- as.data.frame(t(round(colMeans(cur.catch[2:5])))) ##mean catch

############# START  #########################
####input measured lengths
#####lengths of released fish
##INCLUsive of ADDITIONAL MEASURED LENGTHS OUTSIDE OF MRIP#####
######VAS
df1 <- vas.US
df1$rown <- rownames(df1)
df2 <- samp.vas.US
df2$rown <- rownames(df2)
##to find the undersized fish caught in VAS not attributed to harvest
vas.r1 <- unique.rows(df1, df2)
vas.r1 <- vas.r1[, - c(2, 4)]
#####HB
nyHBR <- nyHB[nyHB$MODE == "Rec" & nyHB$KEEP_REL == "R" & nyHB$YEAR >= yr.min &
            nyHB$AREA == "LIS", ]
nyHBR <- nyHBR[c(1, 4)]
colnames(nyHBR) <- colnames(vas.r1)

#####MRIP Type 9
t9 <- t9[t9$YEAR >= 2012, ]
t9 <- data.frame(year = t9$YEAR, length_cm = t9$LNGTH.CM)


####lengths of release lengths
rLen <- rbind(vas.r1, nyHBR, t9, als.lis)
rLen$length_cm[rLen$length_cm < 13] <- 13 #pooling all fish < 15 cm
rLen$length_cm[rLen$length_cm > 40] <- 40 #pooling all fish < 15 cm
rLen16 <- as.data.frame(table(rLen$length_cm))
colnames(rLen16) <- c("length", "dis")

#### lengths of harvested fish
colnames(all_len)[7] <- "length"
hLen <- all_len
hLen$length[hLen$length > 58] <- 58 #pooling all fish > 59 cm
hLen$length[hLen$length < 34] <- 34 #pooling all fish > 59 cm
hLen16 <-  as.data.frame(table(hLen$length))
colnames(hLen16) <- c("length", "har")


############# START  #########################
####empty data frame for calculations
hr_table <- data.frame(length = c(min(as.numeric(as.character(rLen16$length)))):
                                  max(as.numeric(as.character(hLen16$length))))
#####fill in empty table for harvest
hr_table[match(hLen16$length, hr_table$length), paste("har")] <- hLen16[, 2]
#####fill in empty table for discards
hr_table[match(rLen16$length, hr_table$length), paste("dis")] <- rLen16[, 2]
hr_table[is.na(hr_table)] <- 0 ##replace NA with 0
#####Rec harvest including non.compliance
hr_table$s.rec.har <- round((mean.catch$rec.h *
                  (hr_table$har / sum(hr_table$har))) * ((1 + pr.non.compliant)))
#####Commercial Harvest
hr_table$s.com.har <- round(mean.catch$com.h *
                            (hr_table$har / sum(hr_table$har)))
#####SQ harvest
hr_table$sq.har <- round(mean.catch$har * (hr_table$har / sum(hr_table$har)))

#####scaled discards (NdOT DEAD)
hr_table$s.dis <-  round(mean.catch$dis * (hr_table$dis / sum(hr_table$dis)))

#####status quo catch = har + discards
hr_table$sq.catch <- hr_table$sq.har + hr_table$s.dis
##har a len scaled by harvest n
#####slot catch = har + discards
hr_table$slot.catch <- round(hr_table$s.rec.har +
                             hr_table$s.com.har + hr_table$s.dis)
#####proportion at length for sq and slot
hr_table$P.sq.l <- hr_table$sq.catch / sum(hr_table$sq.catch) ##sq
##catch = har + dis
hr_table$P.slot.l <- hr_table$slot.catch / sum(hr_table$slot.catch)##slot
##har + s.dis
head(hr_table)
colSums(hr_table)
#####weight of fish in size class
hr_table$wt <- (lw.coef * (hr_table$length * 10) ^ (lw.exp) ) / 1000
#####harvest estimator for sq and slot
hr_table$h.sq.est <- round(sum(hr_table$sq.catch) * hr_table$P.sq.l * hr_table$wt)
hr_table$h.slot.est <- round(sum(hr_table$slot.catch) * hr_table$P.slot.l * hr_table$wt)
############# END  #########################


####calculate current harvest
m <- 40 ##minimum legal size
current.harvest <- NULL
current.harvest <- rbind(current.harvest,
                   sum(hr_table$h.sq.est[hr_table$length >= m]) +
                   sum(hr_table$h.sq.est[hr_table$length < m]) * .025)
############# END  #########################




############# START  #########################
####calculate harvest reduction
harvest.redux <- NULL
harvest.hal <- NULL
harvest.hal$length <- unique(hr_table$length)
for(slmin in min(hr_table$length):(max(hr_table$length))) {
    for(slmax in ((slmin):max(hr_table$length))) {
        h.hal <- data.frame(length = harvest.hal$length)
#####calculate harvest in slot including noncompliance
######ifelse for slots that are 1 cm
        ifelse(slmin == slmax,
               ##14 is the harvest slot estimated weigth
               tmp.har <- hr_table[hr_table$length == slmin, ][, c(1, 14)],
               tmp.har <- hr_table[hr_table$length >= slmin &                                                    hr_table$length < slmax, ][, c(1, 14)])
######wt of discards use SQ estimated weights
        tmp.dis <-  hr_table[hr_table$length < slmin |                                                     hr_table$length >= slmax, ][, c(1, 13)]
        ##reduce to discard mortality
        tmp.dis[, 2] <- tmp.dis[, 2] * 0.025
        ##fill in harvest
        h.hal[match(tmp.har$length, h.hal$length), paste("slot.har")] <-
            tmp.har[, 2]
        ##fill in discards
        h.hal[match(tmp.dis$length, h.hal$length), paste("slot.dis")] <-
            tmp.dis[, 2]
        ##NAs to 0
        h.hal[is.na(h.hal)] <- 0
        ##sum harvest and dead discards for removals
        h.hal$removals <- round(rowSums(h.hal[, 2:3]))
        ##total weight of removals
        tmp.h <- colSums(h.hal)[4]
        ##calculate percent redux
        redux.by <- round((tmp.h / current.harvest), 4)
        ##prepare output DF
        tmp <- cbind(paste("h", slmin, slmax, sep = "_"), slmin, slmax,
                     tmp.h, redux.by)
        ##label accordingly
        colnames(tmp) <- c("slot", "min", "max", "tmp.h", "redux.by")
        colnames(h.hal)[4] <- tmp[1, 1]
        ##make final data frames
        harvest.redux <- rbind(harvest.redux, tmp)
        harvest.hal <- cbind(harvest.hal, h.hal[4])
    }
}
#####backup dfs from above loop, only for coding
buharvest.redux <- harvest.redux
bu.harvest.hal <- harvest.hal
#####rewrite dfs using backup, only for coding
##harvest.redux <- buharvest.redux
##harvest.hal <- bu.harvest.hal
#####convert factors to numeric
harvest.redux <- as.data.frame(harvest.redux) ##switch to DF
harvest.redux[, 2:5] <- apply(harvest.redux[, 2:5], 2, ##to numeric
                              function(x) as.numeric(as.character(x)))
harvest.redux$slmin_in <- round(harvest.redux$min * 0.393701, 2) ##in slot
harvest.redux$slmax_in <- round(harvest.redux$max * 0.393701, 2) ##in slot
harvest.hal <- t(harvest.hal)
colnames(harvest.hal) <- harvest.hal[1, ] ##drop length row
harvest.hal <- harvest.hal[-1, ]
rownames(harvest.redux) <- harvest.redux$slot
harvest.redux <- cbind(harvest.redux, harvest.hal)
harvest.redux$pr.redux <- 1 - harvest.redux$redux.by


####Harvest reductions including bag limit changes
######this restricts the slot options to lengths for which we have bag limit
######information
candidate.slots <- harvest.redux[rownames(harvest.redux) == "h_35_40" |
                                 rownames(harvest.redux) == "h_40_45" |
                                 rownames(harvest.redux) == "h_45_50" |
                                 rownames(harvest.redux) == "h_50_55" |
                                 rownames(harvest.redux) == "h_35_45" |
                                 rownames(harvest.redux) == "h_35_50" |
                                 rownames(harvest.redux) == "h_35_55" |
                                 rownames(harvest.redux) == "h_35_58",]


candidate.slots$pr.redux.B4 <- 1 - (candidate.slots$redux.by *
                                    bag.redux$New.B4.redux)
candidate.slots$pr.redux.B3 <- 1 - (candidate.slots$redux.by *
                                    bag.redux$New.B3.redux)
candidate.slots$pr.redux.B2 <- 1 - (candidate.slots$redux.by *
                                    bag.redux$New.B2.redux)
candidate.slots$pr.redux.B1 <- 1 - (candidate.slots$redux.by *
                                    bag.redux$New.B1.redux)



harvest.redux$pr.redux.B4 <- 1 - (harvest.redux$redux.by *
                                  bag.redux$New.B4.redux)
harvest.redux$pr.redux.B3 <- 1 -(harvest.redux$redux.by *
                                    bag.redux$New.B3.redux)
harvest.redux$pr.redux.B2 <- 1 - (harvest.redux$redux.by *
                                    bag.redux$New.B2.redux)
harvest.redux$pr.redux.B1 <- 1 - (harvest.redux$redux.by *
                                    bag.redux$New.B1.redux)





######made DF for each bag option
candidate.slots.B4 <- harvest.redux[harvest.redux$pr.redux.B4 > pr.min &
                                    harvest.redux$pr.redux.B4 < pr.max, ]
candidate.slots.B3 <- harvest.redux[harvest.redux$pr.redux.B3 > pr.min &
                                    harvest.redux$pr.redux.B3 < pr.max, ]
candidate.slots.B2 <- harvest.redux[harvest.redux$pr.redux.B2 > pr.min &
                                    harvest.redux$pr.redux.B2 < pr.max, ]
candidate.slots.B1 <- harvest.redux[harvest.redux$pr.redux.B1 > pr.min &
                                    harvest.redux$pr.redux.B1 < pr.max, ]


######simplfy those DFs by removing info from other bag options
candidate.slots.B4 <- candidate.slots.B4[, c(1:55)]
candidate.slots.B3 <- candidate.slots.B3[, c(1:54, 56)]
candidate.slots.B2 <- candidate.slots.B2[, c(1:54, 57)]
candidate.slots.B1 <- candidate.slots.B1[, c(1:54, 58)]

####make rainbow flyer bag 4
p4 <- ggplot(data = harvest.redux,
             aes(x = min + .5, y = max + .5, z = pr.redux)) +
    geom_tile(aes(fill = pr.redux)) +
    scale_y_continuous(breaks = round(seq(min(harvest.redux$min),
                                      max(harvest.redux$max), by = 2), 1)) +
    scale_x_continuous(breaks = round(seq(min(harvest.redux$min),
                   max(harvest.redux$max), by = 2), 1), sec.axis = dup_axis()) +
    scale_fill_gradientn(limits = c(min(round(harvest.redux$pr.redux)),
                     max(round(harvest.redux$pr.redux))), colours = rainbow(7)) +
    labs(x = "Slot Minimum", y = "Slot Maximum", fill = "") +
    theme_classic() + theme_bw() +
    theme(plot.title = element_text(hjust = .5), legend.position = c(.8, .31),
          legend.key.size = unit(1.5, "cm"),
          legend.background = element_rect(fill = alpha("white", 0.1)),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 18))  +
    guides(color=guide_legend(override.aes=list(fill = NA)))

sq <- geom_point(x = 40, y = max(harvest.redux$max), colour = "black",
                 cex = 3, pch = 15)
b4 <-  geom_point(data = candidate.slots.B4, colour = "black", cex = 5, pch = 20)
b3 <-  geom_point(data = candidate.slots.B3, colour = "black", cex = 3, pch = 6)
b2 <-  geom_point(data = candidate.slots.B2, colour = "black", cex = 2, pch = 1)
b1 <-  geom_point(data = candidate.slots.B1, colour = "black", cex = 2, pch = 3)


fb4 <-  p4  +  sq  + b4 + b3 + b2+ b1 +
    ggtitle(paste0("Proportional Harvest Reduction 4 bag slot", sep = " ")) +
    geom_text(data = NULL, x = 40.5, y = 30, label = "Current Minimum Size") +
    geom_text(data = NULL, x = 40, y = 28, label = "Candidate 4 bag slot") +
    geom_text(data = NULL, x = 40, y = 26, label = "Candidate 3 bag slot") +
    geom_text(data = NULL, x = 40, y = 24, label = "Candidate 2 bag slot") +
    geom_text(data = NULL, x = 40, y = 22, label = "Candidate 1 bag slot") +
    geom_point(x = 33, y = 30, colour = "black", cex = 3, pch = 15) +
    geom_point(x = 33, y = 28, colour = "black", cex = 5, pch = 20) +
    geom_point(x = 33, y = 26, colour = "black", cex = 3, pch = 6) +
    geom_point(x = 33, y = 24, colour = "black", cex = 2, pch = 1) +
    geom_point(x = 33, y = 22, colour = "black", cex = 1, pch = 3) +
    geom_text(data = NULL, x = 51, y = 40, label = "Percent Reductions")
print(fb4)

############################################################################
####apply modeled Age-Length relationship to
####harvest to calculate selectivity

al <- read.csv("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/Amendment1/database/LIS_Tog_AL_data.csv")
al$age[al$age > 26] <- 26 ##cap age at 26 years
al.y <- al[al$year %in% years, ]
tmp.mlr <- multinom(age ~ length, data = al.y, maxit = 500)
lens <- hr_table$length
prop.mlr <- predict(tmp.mlr, data.frame(length = lens), type = "probs")

catch <- data.frame(length = hr_table$length,
                             freq = hr_table$sq.catch)
####import popluation age structure from ASAP model
popN <- read.csv("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/database/Selectivity_worksheet_26_years.csv")
popN <- data.frame(age = popN[, 1], freq = popN[, 7] )##select columns
for(b in 1:4){ ##loop over each bag limit option
    ##pull out bag limit B
    sl.tmp <-  candidate.slots[, c(1:53)]
    col.tmp <- candidate.slots[paste("pr.redux.B", b,  sep ="")]
    harvestN <- t(sl.tmp[, c(2, 3, 8:53)])
    harvestN <- (rbind(t(col.tmp), harvestN))
    harvestN[is.na(harvestN)] <- 0
    selex <- NULL
    for(c in 1:ncol(harvestN)){
        ##calculate harvest-at-age
        tmp.age.har <- colSums(prop.mlr * harvestN[c(4:nrow(harvestN)), c])
        mx <- length(tmp.age.har) ##max age harvest
        ##get vector lengths same size
        n <- max(length(tmp.age.har), length(popN$age))
        length(tmp.age.har) <- n
        ##DF with harvest and pop age
        tmp.age <- data.frame(popN, harvest = tmp.age.har)
        tmp.age[is.na(tmp.age)] <- 0
        ##calculate selectivity as a rolling mean
        tmp.selex <- data.frame(age = popN$age,
                                selex = round(tmp.age$harvest/tmp.age$freq, 8))
        ## smoothing function for selectivity, used 3 year average, for all ages,
        ## repeated age 2 and age max-1 for age 1 and age max
        tmproll <- round(rollmean(tmp.selex$selex, 3), 8)
        tmp.selex$rollmean <- c(mean(tmp.selex$selex[1:2]), tmproll[2:(mx - 1)],
                                mean(tmp.selex$selex[(mx-1):mx]),
                                rep(0, (n - mx)) )
        tmp.selex$scaled.mean <- tmp.selex$rollmean/max(tmp.selex$rollmean)
        meta.data <- c(b, harvestN[1:3, c])
        names(meta.data)[1] <- "bag"
        tmp.selex <- c(meta.data, tmp.selex$scaled.mean)
        selex <- cbind(selex, as.matrix(tmp.selex))
    }
    colnames(selex) <- colnames(harvestN)
    rownames(selex)[5:30] <-  popN$age
    assign(paste("selex.bag.pop",  b, sep = ""), selex)
}
##write.csv(selex.bag.pop1, "selexbag_pop1.csv")
##write.csv(selex.bag.pop2, "selexbag_pop2.csv")
##write.csv(selex.bag.pop3, "selexbag_pop3.csv")
##write.csv(selex.bag.pop4, "selexbag_pop4.csv")
selex.f <- rbind(t(selex.bag.pop1),
                 t(selex.bag.pop2),
                 t(selex.bag.pop3),
                 t(selex.bag.pop4))

##write.csv(selex.f, "selex_f.csv")
####make selectivity plots
plot(selex.bag.pop4[2:27, 6], type = "l", col = "RED", lwd = 2)
lines(selex.bag.catch4[2:27, 2])

par(mar = c(5, 4, 4, 4) + 0.3)
plot(popN$freq, type = "l", xlab = "Age", ylab = "Abundance", cex.axis = 1.2,
     cex.lab = 1.5, log = "y")
polygon(c(1, popN$age, 26), c(1e-7, popN$freq, 1e-7), col = "purple")
tmp.age.har <- colSums(prop.mlr * harvestN[c(4:nrow(harvestN)), 6])
tmp.age.har <- data.frame(tmp.age.har)
tmp.age.har <- rbind(tmp.age.har, x18 = 0, x19 = 0, x20 = 0, x21 = 0, x22 = 0,
                     x23 = 0, x24 = 0, x25 = 0, x26 = 0)
rownames(tmp.age.har) <- c()
legend(18, 1000000, pch = c(15, 15, NA), lwd = c(NA, NA, 3), cex = 1.2,  col = c( "purple","yellow", "black" ),
       c("Population", "Harvest", "selectivity"))
legend(18, 1000000, pch = c(22, 22, NA), lwd = NA,  cex = 1.2,  col = c("black"), c("", ""), bty = "n")

polygon(c(1, as.numeric(popN$age), 26), c(1e-7, tmp.age.har$tmp.age.har, 1e-7),
           col = "yellow")


par(new = TRUE)
plot(c(1:26), selex.bag.pop4[5:30, 6], axes = FALSE, bty = "n", xlab = "",
     ylab = "", type = "l", lwd = 3)
axis(side = 4, cex.axis = 1.2)
mtext("Selectivity", side = 4, line = 3, cex = 1.5)




