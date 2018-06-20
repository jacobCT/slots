##source("C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Amendment1/Scripts/New_MRIP_Bag.r")
source("C:/Users/jacob/OneDrive/UCONN/Projects/Tautog_ASMFC/Amendment1/Scripts/MRIP_read_data.r")
############# START  #########################
hLen <- lengths[lengths$YEAR >=  2013, ]
hLen <- hLen[hLen$lngth_imp == 0, ]
hist(hLen$LNGTH, breaks = 200 )

n.har <- length(hLen$LNGTH)
n.illegal <- length(hLen$LNGTH[hLen$LNGTH < 406])
n.illegal / n.har ##23%fish below 406mm (16 in)

n.illegal<- length(hLen$LNGTH[hLen$LNGTH < 394])
n.illegal/n.har ##15% fish below 394mm (15.5 in)

##NYHBLengths
nyHB <- read.csv("C:\\Users\\jacob\\OneDrive\\UCONN\\Projects\\Tautog_ASMFC\\Projections2016\\Fall_2016_Projections\\lengths\\NYHB_length_1995-2015.csv")
nyHB <- nyHB[nyHB$MODE == "Rec", ]
nyHBK <- nyHB[nyHB$KEEP_REL == "K", ]
nyHBK <- nyHBK[nyHBK$YEAR >= yr.min, ]
n.har <- length(nyHBK$LENGTH_CM)

n.illegal <- length(nyHBK$LENGTH_CM[nyHBK$LENGTH_CM < 40.6])
n.illegal / n.har ##23% fish below 40.6 cm (16 in)

n.illegal <- length(nyHBK$LENGTH_CM[nyHBK$LENGTH_CM < 39.4])
n.illegal / n.har ##14% fish below 39.4 cm (15.5 in)

table(nyHBK$LENGTH_CM)
