##Read in MRIP data for slot limit analysis
##  1. Samples measured fish to assign a length to each unmeasured fish
##  2. non compliance is only calculated over years of same regs (2012:2015)
##  3. Includes additional measured lengths (CTVAS, NYHB) in the pool of
##     unmeasured fish
##      3a. VAS- includes all fish >16 in, and same pr of non-compliant
##      fish as in MRIP
##      3b. NYHB- includes all harvested fish as that is the same pr of
##      non-compliant fish as MRIP
##  4. Output is an organized catch table for use for catch length data or
##     bag size analysis
##  5. MRIP data for 2004-2015 are extracted
##  6. this setups up a CM slot limit calculation, such that all mm lengths are
##  round(length/10) and IN lengths are randomly rounded, then converted to round## cm
##  7. imputede lengths are dropped from pooled data, but used in org data
## NOTE: This code DOES NOT include the MRIP weighting factors!!!  This assumes
## that size distribution and catch rates are similar across interview sites.
###############################################################################
#########################
### PREPARE
setwd("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/Slot-MEffects/database/raw/ps_2015_csv")
library(reshape2)
options(scipen=999)
# Specify criteria
##years <- c(2012:2015)
##yr.min <- 2012

waves <- c(2, 4, 5, 6) #specify state and wave
sp1 <- 8839010101            # MRIP SP_CODE
sp2 <- "TAUTOG"              # MRIP common name
set.seed(1126)               # Code does some sampling,
## so this ensures consitent results. Can obviously be turned off if desired.

#####################
### READ IN DATA  ###
#####################


#############CT CT CT CT
# Trip data CTt
states <- c(9)              # MRIP state codes #ny 36, ct 9
trips <- data.frame(NULL)
for(y in years) {
    for (w in waves) {
        fname <- paste("trip_", y, w, ".csv", sep="")
      tmp <- read.csv(fname, head=TRUE)
      tmp <- tmp[tmp$ST %in% states, c("ST", "YEAR", "WAVE",
           "MODE_FX", "AREA", "AREA_X", "ID_CODE", "CNTRBTRS", "LEADER",
           "PARTY", "PRT_CODE")]
      trips <- rbind(trips, tmp)
    }
}

## Import catch data CT

catch <- data.frame(NULL)
for(y in years) {
    for (w in waves) {
       fname <- paste("catch_", y, w, ".csv", sep="")
       tmp <- read.csv(fname, head=TRUE)
       tmp <- tmp[tmp$ST %in% states & tmp$common == sp2, c("ST", "YEAR", "WAVE",
               "MODE_FX", "AREA_X", "ID_CODE", "SP_CODE", "common", "CLAIM",
               "RELEASE", "HARVEST") ]
       catch <- rbind(catch, tmp)
    }
}

## Import length data CT
lengths <- data.frame(NULL)
for(y in years) {
  for (w in waves) {
      fname <- paste("size_", y, w, ".csv", sep="")
      tmp <- read.csv(fname, head=TRUE)
      tmp$targ <- 0
      tmp$targ[tmp$SP_CODE == sp1] <- 1
      tmp <- tmp[tmp$ST %in% states & tmp$targ == 1, c("ST", "YEAR", "WAVE",
             "MODE_FX", "AREA_X", "ID_CODE", "LNGTH", "lngth_imp")]
      lengths <- rbind(lengths, tmp)
  }
}

##NY NY NY YN
area <- "C"
states <- c(36) ##NY
waves <- c(5, 6) #specify state and wave

##Trip data
tripsNY <- data.frame(NULL)
for(y in years) {
    for (w in waves) {
        fname <- paste("trip_", y, w, ".csv", sep="")
        tmp <- read.csv(fname, head=TRUE)
        tmp <- tmp[tmp$ST %in% states & tmp$AREA == area, c("ST", "YEAR", "WAVE",
        "MODE_FX", "AREA", "AREA_X", "ID_CODE", "CNTRBTRS", "LEADER",
        "PARTY", "PRT_CODE")]
        tripsNY <- rbind(tripsNY, tmp)
    }
}
tapply(tripsNY$ID_CODE, list(tripsNY$YEAR), length) # Count total number of interviews conducted per year

##Import catch data NY

catchNY <- data.frame(NULL)
for(y in years) {
    for (w in waves) {
     fname <- paste("catch_", y, w, ".csv", sep="")
   tmp <- read.csv(fname, head=TRUE)
   tmp <- tmp[tmp$ST %in% states & tmp$common == sp2, c("ST", "YEAR", "WAVE",
              "MODE_FX", "AREA_X", "ID_CODE", "SP_CODE", "common", "CLAIM",
                                                        "RELEASE", "HARVEST") ]
   catchNY <- rbind(catchNY, tmp)
 }
}

## Import length data
lengthsNY <- data.frame(NULL)
for(y in years) {
    for (w in waves) {
        fname <- paste("size_", y, w, ".csv", sep="")
        tmp <- read.csv(fname, head=TRUE)
        tmp$targ <- 0
        tmp$targ[tmp$SP_CODE == sp1] <- 1
        tmp <- tmp[tmp$ST %in% states & tmp$targ == 1, c("ST", "YEAR", "WAVE",
           "MODE_FX", "AREA_X", "ID_CODE", "LNGTH", "lngth_imp")]
        lengthsNY <- rbind(lengthsNY, tmp)
    }
}

########### MERGE NY and CT
trips <- rbind(trips, tripsNY)
tripsNY <- NULL

ALLcatch <- rbind(catch, catchNY)
catchNY <- NULL
catch <- ALLcatch ##renames ALLcatch, but preserves it for future use

lengths <- rbind(lengths, lengthsNY)
lengthsNY <- NULL

## Drop imputed lengths so code is only sampling true
## Maybe it would be better to inlcude imputed lengths and only sample for
## lengths without true or  imputed value???
## For tautog, had to include imputed lengths since some states/regions had no
## observed lengths at all!

######################################################################
### SAMPLE FROM MEASURED FISH TO ASSIGN LENGTHS TO UNMEASURED FISH ###
######################################################################
# Number of fish removed per trip
catch$tot_harv <- round(catch$CLAIM + catch$HARVEST, 0)
catch <- catch[catch$tot_harv > 0, ]

# Number of true lengths per trip
n_msrd <- aggregate(lengths$LNGTH, list(lengths$ID_CODE), length)
names(n_msrd) <- c("ID_CODE", "N_msrd")

# How many unmeasured fish
catch <- merge(catch, n_msrd, by = "ID_CODE", all = TRUE)
catch$N_msrd[is.na(catch$N_msrd)] <- 0
catch$miss_len <- catch$tot_harv - catch$N_msrd
miss <- catch[catch$miss_len > 0, ]

## Not sure why, but some values are less than 0 -->
## more fish measured than caught!
miss <- na.omit(miss)

## Create a pool of lengths to sample from.
## Currently all waves, modes etc combined, but could set up by stratum

##TO INCLUDE ADDITIONAL MEASURED LENGTHS OUTSIDE OF MRIP#####
vas <- read.csv("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/Slot-MEffects/database/raw/VAS_Taug_one_inch_Bins2015.csv")
colnames(vas) <- c("inch", c(1997:2015))
vas  <- melt(vas, id = c("inch"))
vas  <- data.frame(year = vas$variable, length_in = vas$inch,
                   freq = vas$value)
vas.exp <- vas[rep(1:nrow(vas), vas[["freq"]]), ]
vas.exp$length_cm <- round((vas.exp$length_in +
                           runif(length(vas.exp$length_in), -0.50, 0.49)) * 2.54)
vas.exp$freq <- NULL
vas.exp$year <- as.numeric(paste(vas.exp$year))
vas.exp <- vas.exp[vas.exp$year >= yr.min, ]

vas.legal <- vas.exp[vas.exp$length_cm >= 40, ]
n.legal.vas <- dim(vas.legal)[1]

#### percent noncompliant from MRIP
hLen <- lengths[lengths$YEAR >=  yr.min, ]
hLen <- hLen[hLen$lngth_imp == 0, ]
n.har <- length(hLen$LNGTH[hLen$YEAR >= yr.min])
n.illegal <- length(hLen$LNGTH[hLen$LNGTH < 400 & hLen$YEAR >= yr.min])
pr.non.compliant <- n.illegal / n.har
n.sample.vas <- round(n.legal.vas / (1 - pr.non.compliant)) - n.legal.vas

##VAS Under Size
vas.US <- vas.exp[vas.exp$length_cm < 40, ]
samp.vas.US <- vas.US[sample(nrow(vas.US), n.sample.vas), ]
vas.h1 <- rbind(vas.legal, samp.vas.US)


nyHB <- read.csv("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/Slot-MEffects/database/raw/NYHB_length_1995-2015.csv")
nyHBK <- nyHB[nyHB$MODE == "Rec" & nyHB$YEAR >= yr.min & nyHB$KEEP_REL == "K" &
            nyHB$AREA == "LIS", ]

pool <- c(round(lengths$LNGTH / 10)[lengths$lngth_imp == 0],
          vas.h1$length_cm,
          nyHBK$LENGTH_CM)
# Sample from pool for each missing length and assign it to an ID_CODE
new_len <- data.frame(NULL)
for(i in 1:nrow(miss)) {
    tmp <- data.frame(ID_CODE = rep(miss$ID_CODE[i], miss$miss_len[i]),
                      LNGTH = sample(pool, miss$miss_len[i], TRUE))
    new_len <- rbind(new_len, tmp)
}

new_len$lngth_imp <- 1 # To identify which are true lengths and which were sampled

## Combine observed lengths with sampled lengths
trip_info <- miss[,c("ST", "YEAR", "WAVE", "MODE_FX", "AREA_X", "ID_CODE")]
new_len <- merge(trip_info, new_len, by = "ID_CODE", all = TRUE)
lengths$LNGTH <- round(lengths$LNGTH/10)
all_len <- rbind(lengths, new_len)
all_len$Inch <- round(all_len$LNGTH / 2.54, 2)

# At this point, there should be a length (observed or sampled) for each harvested fish

######################################################
### DISTRIBUTE GROUP CATCHES TO INDIVIDUAL ANGLERS ###
######################################################
tmp.group <- subset(trips, trips$CNTRBTRS > 1)
tmp.group <- tmp.group[, c("ID_CODE", "CNTRBTRS")]
catch <- merge(catch, tmp.group, by = "ID_CODE", all = TRUE)
catch <- catch[!is.na(catch$tot_harv), ]
catch$CNTRBTRS[is.na(catch$CNTRBTRS)] <- 1

setwd("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/Slot-MEffects/HarvestSlots")

