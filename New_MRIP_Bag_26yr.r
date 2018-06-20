###
##Input: MRIP_read_data.r
##Output: Slot specific bag reductions "bag.redux"
## Assumptions
## 1. FULL compliance, all bags > 4 are dropped. Perhaps it would be better to
## convert these to 4 bag trips?
#####look at length distribution of n bag fish, cum dis of sizes
## 2. Some trips had more lengths than fish reported, so randomly drop the extra
## lengths
## 4. bag-length figures at end of script
## 5. this assumes no bag size -length corellation, that can be turned on late in
## script

library(ggplot2)

####Troubleshooting options
## years <- c(2012:2015)
## yr.min <- 2012
## yr.max <- 2015


source("C:/Users/jacob/OneDrive - University of Connecticut/UCONN/Projects/Tautog_ASMFC/Slot-MEffects/HarvestSlots/MRIP_read_data_26yr.r")

groups <- catch[catch$CNTRBTRS > 1, ]    # Idenitfy group trips
groups.out <- data.frame(NULL)


for(i in 1:nrow(groups)) {              # Split group catch among anglers
    tmp <- groups[i, ]
    base_fish <- tmp$tot_harv %/% tmp$CNTRBTRS # Division with no remainder
    extra_fish <- tmp$tot_harv %% tmp$CNTRBTRS # Remainder only
    tmp.out <- data.frame(NULL)
    for(cc in 1:tmp$CNTRBTRS) {           # Create record for each angler
        tmp$Angler <- cc
        tmp.out <- rbind(tmp.out, tmp)
    }
    tmp.out$base_fish <- base_fish        # Everyone gets "base" number of fish
    tmp.out$extra_fish <- 0
    for(cc in 1:nrow(tmp.out)) {
        ## One "extra" fish given to each angler until there are no more fish
        if(cc <= extra_fish) tmp.out$extra_fish[cc] <- 1
    }
    groups.out <- rbind(groups.out, tmp.out)
}
groups.out$Angler_fish <- groups.out$base_fish + groups.out$extra_fish

## Each angler's total harvest

## Add similar new variable for trips with only one angler
singles <- catch[catch$CNTRBTRS == 1, ]
singles$Angler <- 1
singles$base_fish <- singles$tot_harv
singles$extra_fish <- 0
singles$Angler_fish <- singles$base_fish + singles$extra_fish

# Combine expanded group trips and individual trips
combined <- rbind(singles, groups.out)
combined <- combined[, c("ST", "YEAR", "WAVE", "MODE_FX", "AREA_X", "ID_CODE",
            "Angler", "Angler_fish")]
combined$new_ID <- paste(combined$ID_CODE, combined$Angler, sep = "_")

###Make original backups######################
combined_original <- combined
all_len_original <- all_len
################################################

## At this point, there should be a record for each individual angler
## and their personal total harvest

##########################################################
### ASSIGN LENGTHS TO EACH FISH IN EACH ANGLER'S CATCH ###
##########################################################
angler_lengths <- data.frame(NULL)
dim(combined)
for(ID in unique(combined$ID_CODE)) {
    tmp.trip <- combined[combined$ID_CODE == ID,]
    ## Grab trip and length info one ID_CODE at a time
    tmp.len <- all_len[all_len$ID_CODE == ID,]
    tmp.len$random <- runif(nrow(tmp.len), 0, 1e6)
    ## Re-order length data to mix in the true and imputed lengths
    tmp.len <- tmp.len[order(tmp.len$random), ]
    if(nrow(tmp.len) > sum(tmp.trip$Angler_fish)) {
        ## Some trips had more lengths than fish reported, so do this to
        ## randomly drop the extra lengths
        kick <- sample(1:nrow(tmp.len), (nrow(tmp.len) -
                sum(tmp.trip$Angler_fish)), FALSE)
        tmp.len <- tmp.len[-kick, ]
    }
    tmp.len$Angler <- rep(tmp.trip$Angler, tmp.trip$Angler_fish)
    ## Assign appropriate number of lengths to each angler
    angler_lengths <- rbind(angler_lengths, tmp.len)
}
angler_lengths$new_ID <- paste(angler_lengths$ID_CODE, angler_lengths$Angler,
                               sep = "_")

# Every fish for every angler has a length assigned to it


###Slot limit bag work#############################

tmp.bag <- table(round(angler_lengths$LNGTH), angler_lengths$new_ID )
bag.redux <- table(colSums(tmp.bag))
bag.redux <- data.frame(bag1 = bag.redux[1],
                        bag2 = bag.redux[2],
                        bag3 = bag.redux[3],
                        bag4 = sum(bag.redux[4:8]))
sum(bag.redux)
bag.redux$New.B1 <- rowSums(bag.redux[1:4]) ##nFish
bag.redux$New.B2 <- rowSums(bag.redux[2:4])*2 + bag.redux$bag1 ##nFish
bag.redux$New.B3 <- rowSums(bag.redux[3:4])*3 + bag.redux$bag1 +
    bag.redux$bag2 * 2
bag.redux$New.B4 <- bag.redux$bag4 * 4 + bag.redux$bag3 * 3 + bag.redux$bag1 +
    bag.redux$bag2 * 2##nFish

bag.redux$New.B4.redux <- bag.redux$New.B4 / bag.redux$New.B4
bag.redux$New.B3.redux <- bag.redux$New.B3 / bag.redux$New.B4
bag.redux$New.B2.redux <- bag.redux$New.B2 / bag.redux$New.B4
bag.redux$New.B1.redux <- bag.redux$New.B1 / bag.redux$New.B4

## bag.slot <- data.frame(min = NA, max = NA, B0 = NA, B1 = NA, B2 = NA, B3 = NA,
##                        B4 = NA)##, B5 = NA, B6 = NA, B7 = NA, B8 = NA)
## bag.tmp <- bag.slot

## for(rm in 1:(nrow(tmp.bag)-1)){ ##Row Min this is to loop over nrow
##     for(rx in c(rm + 1):nrow(tmp.bag)){
##         mi <- as.numeric(as.character(rownames(tmp.bag)[rm]))
##         mx <- as.numeric(as.character(rownames(tmp.bag)[rx]))
##         bag.tmp[3:ncol(bag.tmp)] <- 0
##         tmp.t <- table(
##             colSums(tmp.bag[as.numeric(as.character(rownames(tmp.bag))) >= mi &
##                            as.numeric(as.character(rownames(tmp.bag))) <= mx, ]))
##         ifelse(min(names(tmp.t)) == 1,
##                tmp.t <- c("0" = 0, tmp.t), tmp.t)
##         ifelse(max(names(tmp.t)) > 4,
##         {tmp.t[5] <- sum(tmp.t[5:length(tmp.t)])
##            tmp.t <- tmp.t[1:5]},  tmp.t)
##         tmp.t <- c(min = mi, max = mx, tmp.t)
##         names(tmp.t)[3:length(tmp.t)] <-
##             paste("B", names(tmp.t)[3:length(tmp.t)], sep ="")
##         bag.tmp[match(names(tmp.t), colnames(bag.tmp))] <- tmp.t
##         bag.slot <- rbind(bag.slot, bag.tmp)
##     }
## }
## bag.slot <- bag.slot[-1, -3]
## head(bag.slot) ##nTrips which caught B bags

## bag.redux <- bag.slot
## bag.redux$New.B1 <- rowSums(bag.redux[3:6]) ##nFish
## bag.redux$New.B2 <- rowSums(bag.redux[4:6])*2 + bag.redux$B1 ##nFish
## bag.redux$New.B3 <- rowSums(bag.redux[5:6])*3 + bag.redux$B1 + bag.redux$B2 * 2 ##nFish
## bag.redux$New.B4 <- bag.redux$B4 * 4 + bag.redux$B3 * 3 + bag.redux$B1 + bag.redux$B2 * 2##nFish

## bag.redux$New.B4.redux <- bag.redux$New.B4/bag.redux$New.B4
## bag.redux$New.B3.redux <- bag.redux$New.B3/bag.redux$New.B4
## bag.redux$New.B2.redux <- bag.redux$New.B2/bag.redux$New.B4
## bag.redux$New.B1.redux <- bag.redux$New.B1/bag.redux$New.B4
## head(bag.redux)


###End slot limit bag work###############################
#######################################################################

####bag-length figures
## bag.length <- function(l){
## bag.len <- colSums(tmp.bag)
## id.tmp <- names(bag.len[bag.len == l ])
## xbag <- rowSums(tmp.bag[, id.tmp])
## bag.l <- cumsum(xbag)
## bag.l <- bag.l/max(bag.l)
## return(bag.l)}

## baglength <- data.frame(
##     length = as.numeric(rownames(tmp.bag)),
##     baglength1 = bag.length(1),
##     baglength2 = bag.length(2),
##     baglength3 = bag.length(3),
##     baglength4 = bag.length(4),
##     baglength5 = bag.length(5),
##     baglength6 = bag.length(6),
##     baglength7 = bag.length(7),
##     baglength8 = bag.length(8))

## df <- melt(baglength, id.vars = "length", variable.name = "bags" )

## ggplot(df, aes(length,value)) + geom_line(size = 1.4, aes(colour = bags))

## bag.length2 <- function(l1, l2){
## bag.len <- colSums(tmp.bag)
## id.tmp <- names(bag.len[bag.len == l1 | bag.len ==l2 ])
## xbag <- rowSums(tmp.bag[, id.tmp])
## bag.l <- cumsum(xbag)
## bag.l <- bag.l/max(bag.l)
## return(bag.l)}

## baglength2 <- data.frame(
##     length = as.numeric(rownames(tmp.bag)),
##     bag1.2 = bag.length2(1, 2),
##     bag2.3 = bag.length2(2, 3),
##     bag3.4 = bag.length2(3, 4),
##     bag4.5 = bag.length2(4, 5),
##     bag5.6 = bag.length2(5, 6),
##     bag6.7 = bag.length2(6, 7),
##     bag7.8 = bag.length2(7, 8))
## boxplot(baglength2[2:8], las = 2)
