##nearly same harvest length distribution as used in stock assessment
## except NO ALS
## release input files from from LIS_VAS_HB_T9
## HB keep/release length as coded from NYHB survey
## CT VAS keep/release as coded by legal minimum. Maintain this or go to what is
## coded in the database?
## ALK goes from 15 - 60 cm so all fish below <= 15 cm are pooled are all fish
## >= 60 cm
##
##

## should this be by abundance or weight?
## what year to start in? 2012 seems better because same regulations, but updated landings figures as provided by
## Wojick only go to 2013, used "older" NY 2012 harvest figures
## C:\Users\jacob\OneDrive\UCONN\Projects\Tautog_ASMFC\Amendment1\Recreational\MRIP_Tautog_2013_2016_CT_NYLIS.xlsx
library(ggplot2)
library(FSA)
library(magrittr)
library(dplyr)
library(nnet)
library(fishmethods)
library(zoo)





options(scipen=999)

##these harvest numbers are not scaled for any seasonal closure
##rec.h.sc = c(102267, 313251, 167391),
##reduced to reflect  closures as per M.Alexander email 6/20/17
##com.h.sc = c(9350, 12774, 10001), 47.2% harvest reduction achived in another way,
##only for calculationg a rec only slot
##dis.sc = c(525819, 2136790, 85645))
##reduced discards for spawning closure
##sc = SPAWNING CLOSURE
cur.catch <- NULL
cur.catch <- data.frame(year = c(2012:2015),
                      rec.h = c(220194, 122376, 354777,	201602),
                      com.h = c(14010, 17708, 24193, 18941), ##com harvest in n
                      dis = c(880195, 629212, 2420049, 1031494))
cur.catch$har    <- rowSums(cur.catch[2:3]) #include com if including com
mean.catch <-  as.data.frame(t(round(colMeans(cur.catch[2:5]))))

##cur.catch$har.sc <- rowSums(cur.catch[4]) #include com if including com
## harvest/discard information from stock assessment
#n.harvested <- (18712 + 221201) ##mean 2012:2015 com + rec
#n.release <- 31005 / .025 ##mean 2012:2015 released
############### what about doing this on a year basis??

##growth information
lwy <-  data.frame(RMSE = 0.07744,
                   Intercept = -4.76752,
                   exponent = 3.02371,
                   coefficent = 0.00001708)
##same length weight estimates used in stock assessment






############# START  #########################
##input measured lengths
## lengths of released fish
rLen <- read.csv("C:/Users/Jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Amendment1/SlotLimits/ReleasedLengths/r_lengths_LIS_proxy2.csv")
rLen <- rLen[, -1]
rLen$length_cm[rLen$length_cm < 15] <- 15 #pooling all fish < 15 cm
## no released fish >= 60 cm
rLen16 <- rLen[rLen$year >= 2012, ] ##select years of interst
#rLen16 <- as.data.frame(colMeans(table(rLen16)))
rLen16 <- as.data.frame(colSums(table(rLen16)))
##measured released fish years 2013-2015

## lengths of harvested fish
hLen <- read.csv("C:/Users/Jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Projections2016/Fall_2016_Projections/database/harvest_lengths.csv")
hLen <- hLen[, c(-1, -4)]
hLen$length_cm[hLen$length_cm > 65] <- 65 #pooling all fish > 60 cm
hLen16 <- hLen[hLen$year >= 2012, ]
##hLen16 <- as.data.frame(colMeans(table(hLen16)))
hLen16 <- as.data.frame(colSums(table(hLen16)))
##measured harvested fish years 2013-2015
############# END  #########################





############# START  #########################
##empty data frame to fill in harvest and released length dists
hr_table <- data.frame(length = c(min(as.numeric(rownames((rLen16)))):
                                  max(as.numeric(rownames(hLen16)))))
##fill in empty table
hr_table[match(rownames(hLen16), hr_table$length), paste("har")] <-
           hLen16[, 1]
hr_table[match(rownames(rLen16), hr_table$length), paste("dis")] <-
           rLen16[, 1]
hr_table[is.na(hr_table)] <- "0" ##replace NA with 0
##hr_table <- hr_table[hr_table$length > 16, ]
## change characters to numeric
hr_table[, 2] <- as.numeric(hr_table[, 2])
hr_table[, 3] <- as.numeric(hr_table[, 3])
hr_table$catch <-  rowSums(hr_table [, 2:3]) ##har + discards= catch
##write.csv(hr_table, "hr_table.csv")
hr_table$pr.harvest1 <- 1
##scale measured length  by harvest/discards/catch
hr_table[, paste("s.har",  sep = "")] <-
    round(mean.catch$har * (hr_table$har / sum(hr_table$har)))
hr_table[, paste("s.dis", sep = "")] <-
    round(mean.catch$dis * (hr_table$dis / sum(hr_table$dis)))
hr_table[, paste("s.catch", sep = "")] <- ##har + discards= catch
    hr_table[, paste("s.har", sep = "")] +
    hr_table[, paste("s.dis", sep = "")]
##hr_table[, paste("P.h", sep = "")] <-
##    hr_table[, paste("s.har", sep = "")] /
##    hr_table[, paste("s.catch", sep = "")]
hr_table[, paste("P.l", sep = "")] <- hr_table[, paste("s.catch", sep = "")] /
    sum(hr_table[, paste("s.catch", sep = "")]) ##proportion at length
##weight of fish in size class

hr_table[, paste("wt", sep = "")] <- (lwy$coefficent * (hr_table$length * 10) ^
                                      (lwy$exponent) + lwy$Intercept) / 1000

hr_table[, paste("h.est", sep ="")] <-
    sum(hr_table[, paste("s.catch", sep = "")]) *
    hr_table[, paste("P.l", sep = "")] *
    hr_table[, paste("wt", sep = "")] * 1 #P.h set to 1

##these are the same
#####hr_table[, paste("s.catch", sep = "")] * hr_table[, paste("wt", sep = "")]

## ##harvest reductions for inclusion of seasonal closures
##hr_table[, paste("s.har.sc",  sep = "")] <-
##    round(mean.catch$har.sc * (hr_table$har / sum(hr_table$har)))
##hr_table[, paste("s.dis.sc", sep = "")] <-
##    round(mean.catch$dis.sc * (hr_table$dis / sum(hr_table$dis)))
##hr_table[, paste("s.catch.sc", sep = "")] <-
##    hr_table[, paste("s.har.sc", sep = "")] +
##   hr_table[, paste("s.dis.sc", sep = "")]
##hr_table[, paste("P.h", sep = "")] <-
##    hr_table[, paste("s.har", sep = "")] /
##    hr_table[, paste("s.catch", sep = "")]
##hr_table[, paste("P.l.sc", sep = "")] <-
##    hr_table[, paste("s.catch.sc", sep = "")] /
##    sum(hr_table[, paste("s.catch.sc", sep = "")])
##hr_table[, paste("h.est.sc", sep ="")] <-
##    sum(hr_table[, paste("s.catch.sc", sep = "")]) *
##    hr_table[, paste("P.l.sc", sep = "")] *
##    hr_table[, paste("wt", sep = "")] * 1 #P.h set to 1
############# END  #########################

############# START  #########################
##calculate current harvest
m <- 40 ##minimum legal size
current.harvest <- NULL
current.harvest <- rbind(current.harvest,
                         sum(hr_table$h.est[hr_table$length >= m])  +
                         sum(hr_table$h.est[hr_table$length < m]) * .025)
current.harvest
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
        tmp.h <- tmp.h + (.025 * (sum(hr_table$h.est) - tmp.h))
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
harvest.redux          <- as.data.frame(harvest.redux)
harvest.redux$slmin    <- as.numeric(as.character(harvest.redux$slmin))
harvest.redux$slmax    <- as.numeric(as.character(harvest.redux$slmax))
harvest.redux$tmp.h    <- as.numeric(as.character(harvest.redux$tmp.h))
harvest.redux$redux.by <- as.numeric(as.character(harvest.redux$redux.by))
harvest.redux$slmin_in <- round(harvest.redux$slmin * 0.393701, 2) ##in slot
harvest.redux$slmax_in <- round(harvest.redux$slmax * 0.393701, 2) ##in slot
harvest.hal <- t(harvest.hal)
colnames(harvest.hal) <- harvest.hal[1, ] ##drop length row
harvest.hal <- harvest.hal[-1, ]
harvest.redux <- cbind(harvest.redux, harvest.hal)
head(harvest.redux)

## Harvest reductions including bag limit changes
bag <- read.csv("C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Amendment1/Recreational/MRIP Data/ps_2015_csv/slot_bag_length.csv")
head(bag) ##pr reduction for bag limit for each length
##merge harvest redux and bag limit calculations
harvest.redux$min <- harvest.redux$slmin
harvest.redux$max <- harvest.redux$slmax
sl.bag <- merge(harvest.redux, bag, by = c("min", "max"))

##calculate as pr redux
sl.bag$pr.redux.B4 <- 1 - (sl.bag$redux.by * sl.bag$New.B4.redux)
sl.bag$pr.redux.B3 <- 1 - (sl.bag$redux.by * sl.bag$New.B3.redux)
sl.bag$pr.redux.B2 <- 1 - (sl.bag$redux.by * sl.bag$New.B2.redux)
sl.bag$pr.redux.B1 <- 1 - (sl.bag$redux.by * sl.bag$New.B1.redux)
head(sl.bag)

pr.min <- .42
pr.max <- .45

candidate.slots.B4 <- sl.bag[sl.bag$pr.redux.B4 > pr.min &
                             sl.bag$pr.redux.B4 < pr.max, ]
candidate.slots.B3 <- sl.bag[sl.bag$pr.redux.B3 > pr.min &
                             sl.bag$pr.redux.B3 < pr.max, ]
candidate.slots.B2 <- sl.bag[sl.bag$pr.redux.B2 > pr.min &
                             sl.bag$pr.redux.B2 < pr.max, ]
candidate.slots.B1 <- sl.bag[sl.bag$pr.redux.B1 > pr.min &
                             sl.bag$pr.redux.B1 < pr.max, ]


##write.csv(candidate.slots.B1, "can_slots_B_1.csv" )
##write.csv(candidate.slots.B2, "can_slots_B_2.csv" )
##write.csv(candidate.slots.B3, "can_slots_B_3.csv" )
##write.csv(candidate.slots.B4, "can_slots_B_4.csv" )


###make rainbow flyer
## p <-    ggplot(data = harvest.redux, aes(x = slmin + .5,
##                                          y = slmax + .5, z = pr.redux)) +
##     geom_tile(aes(fill = pr.redux)) +
##     scale_y_continuous(breaks = round(seq(min(harvest.redux$slmin),
##                                           max(harvest.redux$slmax), by = 2), 1)) +
##     scale_x_continuous(breaks = round(seq(min(harvest.redux$slmin),
##                                           max(harvest.redux$slmax), by = 2), 1),
##                        sec.axis = dup_axis()) +
##     scale_fill_gradientn(limits = c(-4.9, 1),
##                          colours = rainbow(7)) +
##     labs(x = "Slot Minimum", y = "Slot Maximum", fill = "") +
##     ggtitle(paste0("Proportional Harvest Reduction", sep = " ")) +
##     theme_classic() + theme_bw() +
##     theme(plot.title = element_text(hjust = .5),
##           legend.position = c(.8, .31),
##           legend.key.size = unit(1.5, "cm"),
##           axis.text = element_text(size = 10),
##           axis.title = element_text(size = 18))
## q <-  geom_point(data = candidate.slots, colour = "black", cex = 9, pch = "*")
## sq <- geom_point(data = candidate.slots[candidate.slots$slmin == 40 &
##                                         candidate.slots$slmax == 60, ],
##                  colour = "black", cex = 5, pch = "#")
## f <-  p + q + sq + geom_text(data = NULL, x = 30, y = 20, label = "* candidate reductions") +
##       geom_text(data = NULL, x = 30, y = 18, label = "# status quo")
## print(f)
## dev.off()

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

pr.min <- .42
pr.max <- .45

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
    tmp.selex <- tmp.selex/max(tmp.selex)
    pr.redux <- harvestN[1, c]
    tmp.selex <- c(pr.redux, tmp.selex)

    tmp.selex2 <- round(tmp.harvestN$Freq/tmp.age.catch$Freq, 8)
    tmp.selex2[is.na(tmp.selex2)] <- 0
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
write.csv(selex.bag.pop1, "selexbag_pop1.csv")
write.csv(selex.bag.pop2, "selexbag_pop2.csv")
write.csv(selex.bag.pop3, "selexbag_pop3.csv")
write.csv(selex.bag.pop4, "selexbag_pop4.csv")

write.csv(selex.bag.catch1, "selexbag_catch1.csv")
write.csv(selex.bag.catch2, "selexbag_catch2.csv")
write.csv(selex.bag.catch3, "selexbag_catch3.csv")
write.csv(selex.bag.catch4, "selexbag_catch4.csv")
##write.csv(selex, "selectivity.csv")
plot(selex.bag.pop4[2:27, 2], type = "l", col = "RED", lwd = 2)
lines(selex.bag.catch4[2:27, 2])

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
