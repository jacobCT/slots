##nearly same harvest length distribution as used in stock assessment
## except NO ALS
## release input files from from LIS_VAS_HB_T9
## HB keep/release length as coded from NYHB survey
## CT VAS keep/release as coded by legal minimum. Maintain this or go to what is
## coded in the database?
## ALK goes from 15 - 60 cm so all fish below <= 15 cm are pooled are all fish
## >= 60 cm
## THIS VERSION DROPS THE 14" CALCLATIONS AND SCALES THE HARVEST ON AN ANUAL BASIS
## this uses a POOLED ALK
library(ggplot2)
library(FSA)
library(magrittr)
library(dplyr)
library(nnet)
library(fishmethods)
library(zoo)





options(scipen=999)
## lengths of released fish
rLen <- read.csv("C:/Users/Jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Amendment1/SlotLimits/ReleasedLengths/r_lengths_LIS_proxy2.csv")
rLen <- rLen[, -1]
##rLen$length_cm[rLen$length_cm < 15] <- 15 #pooling all fish < 15 cm
## no released fish >= 60 cm
rLen16 <- rLen[rLen$year >= 2013, ] ##select years of interst
#rLen16 <- as.data.frame(colMeans(table(rLen16)))
rLen16 <- as.data.frame(colSums(table(rLen16)))

## lengths of harvested fish
hLen <- read.csv("C:/Users/Jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Projections2016/Fall_2016_Projections/database/harvest_lengths.csv")
hLen <- hLen[, c(-1, -4)]
hLen$length_cm[hLen$length_cm > 65] <- 65 #pooling all fish > 60 cm
hLen16 <- hLen[hLen$year >= 2013, ]
##hLen16 <- as.data.frame(colMeans(table(hLen16)))
hLen16 <- as.data.frame(colSums(table(hLen16)))

table(hLen16$length_cm)

cur.catch <- NULL
cur.catch <- data.frame(year = c(2013:2015),
                      rec.h = c(122376, 354777,	201602),
                      com.h = c(17708, 24193, 18941), ##com harvest in n
                      dis = c(629212, 2420049, 1031494))
                     ## rec.h.sc = c(102267, 313251, 167391),
##reduced to reflect  closures as per M.Alexander email 6/20/17
##                      com.h.sc = c(9350, 12774, 10001), ##47.2% harvest reduction achived in another way, only for calculationg a rec only slot
                      ##dis.sc = c(525819, 2136790, 85645))
##reduced discards for spawning closure
##sc = SPAWNING CLOSURE
cur.catch$har    <- rowSums(cur.catch[2:3]) #include com if including com
##cur.catch$har.sc <- rowSums(cur.catch[4]) #include com if including com
dim(cur.catch)
mean.catch <-  as.data.frame(t(round(colMeans(cur.catch[2:5]))))

##empty data frame to fill in harvest and released length dists
hr_table <- data.frame(length = c(min(rownames(rLen16)):
                                  max(rownames(hLen16))))
##fill in empty table
hr_table[match(rownames(hLen16), hr_table$length), paste("har")] <-
           hLen16[, 1]
hr_table[match(rownames(rLen16), hr_table$length), paste("dis")] <-
           rLen16[, 1]
hr_table[is.na(hr_table)] <- "0" ##replace NA with 0
hr_table <- hr_table[hr_table$length > 16, ]
## change characters to numeric
hr_table[, 2] <- as.numeric(hr_table[, 2])
hr_table[, 3] <- as.numeric(hr_table[, 3])
hr_table$catch <-  rowSums(hr_table [, 2:3])
##write.csv(hr_table, "hr_table.csv")
## harvest/discard information from stock assessment
#n.harvested <- (18712 + 221201) ##mean 2012:2015 com + rec
#n.release <- 31005 / .025 ##mean 2012:2015 released
############### what about doing this on a year basis??


#cur.catch$har <- cur.catch$har + (cur.catch$dis * .025)
##same length weight estimates used in stock assessment

lwy <-  data.frame(RMSE = 0.07744,
                   Intercept = -4.76752,
                   exponent = 3.02371,
                   coefficent = 0.00001708)

hr_table$pr.harvest1 <- 1
current.harvest <- NULL

##scale length distributions according to mean harvest/discards
hr_table[, paste("s.har",  sep = "")] <-
    round(mean.catch$har * (hr_table$har / sum(hr_table$har)))
hr_table[, paste("s.dis", sep = "")] <-
    round(mean.catch$dis * (hr_table$dis / sum(hr_table$dis)))
hr_table[, paste("s.catch", sep = "")] <-
    hr_table[, paste("s.har", sep = "")] +
    hr_table[, paste("s.dis", sep = "")]
##hr_table[, paste("P.h", sep = "")] <-
##    hr_table[, paste("s.har", sep = "")] /
##    hr_table[, paste("s.catch", sep = "")]
hr_table[, paste("P.l", sep = "")] <- hr_table[, paste("s.catch", sep = "")] /
    sum(hr_table[, paste("s.catch", sep = "")])
hr_table[, paste("wt", sep = "")] <- (lwy$coefficent * (hr_table$length * 10) ^
                                    (lwy$exponent) + lwy$Intercept) / 1000
hr_table[, paste("h.est", sep ="")] <-
    sum(hr_table[, paste("s.catch", sep = "")]) *
    hr_table[, paste("P.l", sep = "")] *
    hr_table[, paste("wt", sep = "")] * 1 #P.h set to 1

current.harvest <- rbind(current.harvest,
                         sum(hr_table$h.est[hr_table$length >= 40]) +
                         (sum(hr_table$h.est) -
                          sum(hr_table$h.est[hr_table$length >= 40])) * .025)
current.harvest
#with(hr_table, plot(length, pr.har2015, type = "l", lwd = 2, ylab = "pr har"),)
#with(hr_table, lines(length, pr.har2014, type = "l", col = "red", lwd = 2))
#with(hr_table, lines(length, pr.har2013, type = "l", col = "blue", lwd = 2))
#with(hr_table, lines(length, pr.har2012, type = "l", col = "green", lwd = 2))

##work up harvest reductions for all length slots >1 cm
hr_table[, paste("s.har.sc",  sep = "")] <-
    round(mean.catch$har.sc * (hr_table$har / sum(hr_table$har)))
hr_table[, paste("s.dis.sc", sep = "")] <-
    round(mean.catch$dis.sc * (hr_table$dis / sum(hr_table$dis)))
hr_table[, paste("s.catch.sc", sep = "")] <-
    hr_table[, paste("s.har.sc", sep = "")] +
    hr_table[, paste("s.dis.sc", sep = "")]
##hr_table[, paste("P.h", sep = "")] <-
##    hr_table[, paste("s.har", sep = "")] /
##    hr_table[, paste("s.catch", sep = "")]
hr_table[, paste("P.l.sc", sep = "")] <-
    hr_table[, paste("s.catch.sc", sep = "")] /
    sum(hr_table[, paste("s.catch.sc", sep = "")])

hr_table[, paste("h.est.sc", sep ="")] <-
    sum(hr_table[, paste("s.catch.sc", sep = "")]) *
    hr_table[, paste("P.l.sc", sep = "")] *
    hr_table[, paste("wt", sep = "")] * 1 #P.h set to 1











########################BAG
harvest.redux <- NULL
harvest.hal <- NULL
harvest.hal$length <- unique(hr_table$length)
for(slmin in min(hr_table$length):(max(hr_table$length) - 1)) {
    for(slmax in ((slmin + 1):max(hr_table$length))) {
        h.hal <- data.frame(length = harvest.hal$length)
        tmp.h <- sum(hr_table[, paste("h.est", sep = "")]
                     [hr_table$length >= slmin &
                      hr_table$length <= slmax])
        tmp.h <- tmp.h + (.025 * (sum(hr_table[, paste("h.est",
                                        sep = "")]) - tmp.h))
        redux.by <- round((tmp.h / current.harvest), 4)
        tmp.hal <- data.frame(length = c(slmin:(slmax)),
                              hr_table[, paste("h.est", sep = "")]
                              [hr_table$length >= slmin & hr_table$length <=
                               slmax])
        tmp <- cbind(paste("h", slmin, slmax, sep = "_"), slmin, slmax,
                     tmp.h, redux.by)
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
harvest.redux$redux.by <- as.numeric(as.character(harvest.redux[, 5]))
harvest.redux[, 5] <- NULL
harvest.redux$slmin_in <- round(harvest.redux$slmin * 0.393701, 2)
harvest.redux$slmax_in <- round(harvest.redux$slmax * 0.393701, 2)
harvest.hal <- t(harvest.hal)
colnames(harvest.hal) <- harvest.hal[1, ]
harvest.hal <- harvest.hal[-1, ]
harvest.redux <- cbind(harvest.redux, harvest.hal)
head(harvest.redux)

bag <- read.csv("C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Amendment1/Recreational/MRIP Data/ps_2015_csv/slot_bag_length.csv")
head(bag)

harvest.redux$min <- harvest.redux$slmin
harvest.redux$max <- harvest.redux$slmax
sl.bag <- merge(harvest.redux, bag, by = c("min", "max"))

##calculate as pr redux
sl.bag$pr.redux.B4 <- 1 - (sl.bag$redux.by * sl.bag$New.B4.redux)
sl.bag$pr.redux.B3 <- 1 - (sl.bag$redux.by * sl.bag$New.B3.redux)
sl.bag$pr.redux.B2 <- 1 - (sl.bag$redux.by * sl.bag$New.B2.redux)
sl.bag$pr.redux.B1 <- 1 - (sl.bag$redux.by * sl.bag$New.B1.redux)
head(sl.bag)


candidate.slots.B4 <- sl.bag[sl.bag$pr.redux.B4 > 0.08 &
                             sl.bag$pr.redux.B4 < .11, ]
candidate.slots.B3 <- sl.bag[sl.bag$pr.redux.B3 > 0.08 &
                             sl.bag$pr.redux.B3 < .11, ]
candidate.slots.B2 <- sl.bag[sl.bag$pr.redux.B2 > 0.08 &
                             sl.bag$pr.redux.B2 < .11, ]
candidate.slots.B1 <- sl.bag[sl.bag$pr.redux.B1 > 0.08 &
                             sl.bag$pr.redux.B1 < .11, ]

head(candidate.slots.B1)

##write.csv(candidate.slots.B1, "can_slots_B_1.csv" )
##write.csv(candidate.slots.B2, "can_slots_B_2.csv" )
##write.csv(candidate.slots.B3, "can_slots_B_3.csv" )
##write.csv(candidate.slots.B4, "can_slots_B_4.csv" )


head(candidate.slots.B1)


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


## read in ALK to calculate selectivity














al <- read.csv("C:/Users/Jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Slot-Meffects/growth model/data/LIS_Tog_AL_data.csv")
al$age[al$age > 26] <- 26 ##cap age at 26 years
dim(candidate.slots.B4)
t(candidate.slots.B4[10:58])

unaged <- data.frame(length = hr_table$length,
                             freq = hr_table$s.catch)

##yrs.tmp <- rollapply(c(1983:2017), 5, FUN = unique)
##rownames(yrs.tmp) <- c(1985:2015)
##yrs.tmp[1, ] <- seq(1985, 1989, 1)
##yrs.tmp[2, ] <- seq(1985, 1989, 1)
##yrs.tmp[30, ] <- seq(2011, 2015, 1)
##yrs.tmp[31, ] <- seq(2011, 2015, 1)

lens <- catch.len.freq$length

yr.len.freq <- data.frame(matrix(NA, nrow = 26, ncol = 0))
alk.mlr <- NULL

##for (r in 1:nrow(yrs.tmp)){
##for (r in 31){
##    y <- yrs.tmp[r, ]
##    y2 <- rownames(yrs.tmp)[r]

##range for lens is 6-65
##range for hr_table is 17-65

##al.y <- al##[al$year %in% y, ]
##using all years for the growth model, otherwise no fish older than 17 yrs, but then I have 26 yr old fish in last years, which is not true.

##y <- c(2012)
##y <- c(2013)
##y <- c(2014)
##y <- c(2015)
y <- c(2012:2015)
al.y <- al[al$year %in% y, ]
tmp.mlr <- multinom(age ~ length, data = al.y, maxit = 500)
prop.mlr <- predict(tmp.mlr, data.frame(length = lens),
                    type = "probs")
rownames(prop.mlr) <- lens
    ##tmp.unage <- as.data.frame(table(unaged.y$length_cm))
    ##tmp.unage$Var1 <- as.numeric(as.character(tmp.unage$Var1))

##    mis.len <- setdiff(union(lens, tmp.unage$Var1),
  ##                     intersect(lens, tmp.unage$Var1))

    ##tmp.df <- data.frame(Var1 = as.numeric(mis.len), Freq = 0)
    ##tmp.unage <- rbind(tmp.unage, tmp.df)
    ##tmp.unage <- tmp.unage[order(tmp.unage$Var1), ]
   ## tmp.unage$pr.freq <- tmp.unage$Freq/sum(tmp.unage$Freq)

##tmp.len.mod <-  colSums(
  ##      (prop.mlr * as.numeric(tmp.unage$pr.freq))

tmp.age.catch <-
    colSums(prop.mlr * unaged[, 2])
##exploring selex, but this is by length, I want by age
sel.tmp <- candidate.slots.B4[2, c(10:58)]
class(unaged[, 2])
sel.tmp[is.na(sel.tmp)] <- 0
sel.tmp <- as.numeric(t(sel.tmp)) #harvest within slot
sel.tmp

tmp.age.harvest <- colSums(prop.mlr * sel.tmp)
selex <- tmp.age.harvest / tmp.age.catch ##selectivity curve for slot
round(selex/max(selex), 10)

## so, we lose the 18 year older group, the model assumes that those fish will not return, but they may.
##could use ages 1-26 for catch and ages 1-17 for harvest
##what about using information on the age stucture of the population from ASAP?
popN <- read.csv("Selectivity_worksheet.csv", header = T)
tmp.age.harvest <- as.data.frame(tmp.age.harvest)
harvestN <- data.frame(age = as.numeric(rownames(tmp.age.harvest)),
                       Freq = tmp.age.harvest$tmp.age.harvest)

mis.age <- setdiff(union(popN$age, harvestN$age),
                   intersect(popN$age, harvestN$age))
tmp.df <- data.frame(age = as.numeric(mis.age), Freq = 0)
harvestN <- rbind(harvestN, tmp.df)
harvestN <- harvestN[order(harvestN$age), ]

##sel2012 <- harvestN$Freq/(popN$X2012 * 1000)
##sel2013 <- harvestN$Freq/(popN$X2013 * 1000)
##sel2014 <- harvestN$Freq/(popN$X2014 * 1000)
##sel2015 <- harvestN$Freq/(popN$X2015 * 1000)

selex <- harvestN$Freq/popN$freq
selex/max(selex)

##max((sel2012 +  sel2013+ sel2014+ sel2015) /4)

length(harvestN$Freq)
length(popN$freq)
tmp.len.mod <- as.data.frame(tmp.len.mod)
    tmp.ages <- as.numeric(as.character( rownames(tmp.len.mod)))
    ifelse(length(tmp.ages) == 26, tmp.mod.age <- tmp.len.mod,
           {mis.age <- setdiff(union(c(1:26), tmp.ages),
                               intersect(c(1:26), tmp.ages))
                               age.df <- data.frame(lens = mis.age,
                                                    tmp.len.mod = 0)
                               rownames(age.df) <- age.df$lens
                               age.df <- age.df[-1]
                               tmp.mod.age <- rbind(tmp.len.mod, age.df)})
    yr.len.freq <- cbind(yr.len.freq, tmp.mod.age)
 }
























alk <- read.csv("C:/Users/Jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Projections_Winter_2016-2017/SlotLimits/pooledALK2013-2015.csv")

## truncate ALK into 12Plus, as in SA
alk <- data.frame(alk[1:12], Age12PLUS = rowSums(alk[13:32]))
colnames(alk)[1] <- c("length")
##write.csv(candidate.slots, "can_slots_555_redux.csv")
#slot_selectivity <- NULL
slot_sel <- NULL
##for (r in 1:nrow(candidate.slots)){
##tmp.slot <- candidate.slots[ r, ]
for (r in 1:nrow(harvest.redux)){
    tmp.slot <- harvest.redux[ r, ]
    tmp.alk <- alk[alk$length >= tmp.slot$slmin &
                   alk$length <  tmp.slot$slmax, ]
    sel <- colSums(tmp.alk[,  -1]) / colSums(alk[,  -1])
    sel <- sel / max(sel) #scale max selectivity to 1 for AgePro
    sel <- as.data.frame(sel)
    colnames(sel) <-  paste(tmp.slot$slmin, tmp.slot$slmax, sep = "_")
    sel <- as.matrix(sel)
    sel <- rbind(sel, round(tmp.slot$tmp.h / 1000))
    slot_sel <- cbind(slot_sel, sel)
    assign(paste0("slot_sel", sep = ""), slot_sel)
}

slot_sel <- as.data.frame(slot_sel)
slot_sel <- t(slot_sel)
##write.csv(slot_sel, "slot_selectivity_pooled_no_wt.csv")
##write.csv(slot_sel, "slot_selectivity_pooled__wt.csv")
##length and age data, proportion in each size that are aboe and below slot, include discards as the smallest size caught? what is availble to fishery vs what they keep?
## simpiliest would be saa distr. for each age you have a normal distribution, proportion of fish in each age group that is larger than min and smaller than max. what if you have a strong year class? what are the gear affects? saa selectiviy vs assessment selectivity
## truncate ALK into 12Plus, as in SA

##15plus
alk <- read.csv("C:/Users/Jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Projections_Winter_2016-2017/SlotLimits/pooledALK2013-2015.csv")
alk <- data.frame(alk[1:15], Age15PLUS = rowSums(alk[16:32]))
colnames(alk)[1] <- c("length")

#slot_selectivity <- NULL
slot_sel <- NULL
##for (r in 1:nrow(candidate.slots)){
##tmp.slot <- candidate.slots[ r, ]
for (r in 1:nrow(candidate.slots)){
    tmp.slot <-candidate.slots [ r, ]
    tmp.alk <- alk[alk$length >= tmp.slot$slmin &
                   alk$length <  tmp.slot$slmax, ]
    sel <- colSums(tmp.alk[,  -1]) / colSums(alk[,  -1])
    sel <- sel / max(sel) #scale max selectivity to 1 for AgePro
    sel <- as.data.frame(sel)
    colnames(sel) <-  paste(tmp.slot$slmin, tmp.slot$slmax, sep = "_")
    sel <- as.matrix(sel)q
    sel <- rbind(sel, round(tmp.slot$tmp.h / 1000), tmp.slot$slmin, tmp.slot$slmax,
                 tmp.slot$pr.redux)
    slot_sel <- cbind(slot_sel, sel)
    assign(paste0("slot_sel", sep = ""), slot_sel)
}

slot_sel <- as.data.frame(slot_sel)
slot_sel <- t(slot_sel)
colnames(slot_sel)[16:19] <- c("wt", "minCM", "maxCM", "prRedux")
slot_sel <- as.data.frame(slot_sel)
slot_sel$minIN <- round(slot_sel$minCM  * 0.393701, 2)
slot_sel$maxIN <- round(slot_sel$maxCM  * 0.393701, 2)
write.csv(slot_sel, "slot_selectivity_pooled__wt_15plus.csv")


library(plotly)
g <- plot_ly(x = ~hr_table$length, y = ~hr_table$s.catch,
             type = 'scatter', name = "Catch",
             mode = 'lines', fill = 'tozeroy') %>% add_trace(x = ~hr_table$length,
         y = ~hr_table$s.har, name = 'Harvested', fill = 'tozeroy') %>%
         layout(xaxis = list(title = 'Length (cm)'), yaxis = list(title = 'Frequency'))
g
add_lines(g, x = 40.6, y = c(0, max(hr_table$s.har2012)), line = list(color = "black"))
library(reshape2)
head(hr_table)
t.cat <- with(hr_table, rep(length, s.catch))
t.har <- with(hr_table, rep(length, s.har))
hist(t.har, breaks = 20)
hist(t.cat, breaks = 17)
range(t.cat)
with(hr_table, hist(x = length), times = s.har)
ggplot(hr_table, aes(x=length, y =s.har)) + geom_bar()#stat = "count", width = 1)
untable(hr_table[, c(s)])



