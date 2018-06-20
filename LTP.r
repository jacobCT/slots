ms <- read.csv("LTP_SQ_N_at_Age.csv")
slot <- read.csv("LTP_slot_N_at_Age.csv")
##slot.sqF <- read.csv("LTP_Slot-SQ-F_N_at_Age.csv")
head(ms)
range(ms[, 21])
range(slot[, 21])

mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 4, 0, 0))

plot(ms[, 9] ~ ms$year, type = "l", ylim = c(0, 250), col = "blue", lty = 2,
     yaxt = "n", xaxt = "n", ann = FALSE, lwd = 2,  axes = F)
axis(1, las = 1, cex.axis = 2 )
title(xlab = "Year", cex.lab = 2)
axis(2, las = 1, cex.axis = 2 )
title(ylab = "Abundance x 1000", cex.lab = 2, line = 4)
title("N-at-age 8", cex.main = 2)
lines(slot[, 9] ~ ms$year , type = "l",  ylim = c(0, 250), lwd = 2)
legend(2040, 100, c("Minimum Size", "Harvest Slot"), lty = c(2, 1), lwd = 2,
       col = c("blue", "black"), cex = 2)


plot(ms[, 21] ~ ms$year, type = "l", col = "blue", lty = 2, ylim = c(0, 5),
     yaxt = "n", xaxt = "n", ann = FALSE, lwd = 2,  axes = F)
axis(1, las = 1, cex.axis = 2 )
title(xlab = "Year", cex.lab = 2)
axis(2, las = 1, cex.axis = 2 )
title(ylab = "Abundance x 1000", cex.lab = 2, line = 4)
title("N-at-age 20", cex.main = 2)
lines(slot[, 21] ~ ms$year , type = "l",  ylim = c(0, 5), lwd = 2)
legend(2040, 2, c("Minimum Size", "Harvest Slot"), lty = c(2, 1), lwd = 2,
       col = c("blue", "black"), cex = 2)



ms.n <- sum(ms[ms$year == 2042, 2:27]) * 1000
ms.as <- ms[ms$year == 2042, ]
ms.as <- (ms.as[, 2:27] / sum(ms.as[, 2:27]))
ms.as <- data.frame(age = c(1:26),
                    pr = t(ms.as))

slot.n <- sum(slot[slot$X == 2042, 2:27]) * 1000
slot.as <- slot[slot$X == 2042, ]
slot.as <- (slot.as[, 2:27] / sum(slot.as[, 2:27]))
slot.as  <- data.frame(age = c(1:26),
                    pr = t(slot.as))

plot(ms.as, type = "l", col = "blue", lty = 2,
     yaxt = "n", xaxt = "n", ann = FALSE, lwd = 2,  axes = F)
axis(1, las = 1, cex.axis = 2 )
title(xlab = "Age", cex.lab = 2)
axis(2, las = 1, cex.axis = 2 )
title(ylab = "P", cex.lab = 2, line = 5)
title("Proportion-at-age", cex.main = 2)
lines(slot.as, type = "l",  ylim = c(0, 250), lwd = 2)
legend(9, .1, c("Minimum Size", "Harvest Slot"), lty = c(2, 1), lwd = 2,
       col = c("blue", "black"), cex = 1.8)


plot(log(slot.as[, 2] / ms.as[, 2]) ~ c(1:26), xlab = "Age",
     ylab = "ln(P[Slot]/P[MS])", cex.axis = 2, cex.lab = 2,
     pch = 19, las = 1, main = "Relative Proportion", cex.main = 2)
abline(h = 0, col = "orange", lwd = 4)
points(log(slot.as[, 2] / ms.as[, 2]) ~ c(1:26), pch = 19, las = 1)


head(sq)
sq.head <-  colMeans(sq[1:11, 2:27]    )
slot.head <- colMeans(slot[1:11, 2:27]    )
slot.sqF.head <- colMeans(slot.sqF[1:11, 2:27]    )







par(mfcol = c(1, 2 ))
plot(slot.head - sq.head, type = "l", ylim = c(-35, 20), xlab = "Age",
     ylab = "Abundance Difference x1000", main = "Mean 2016 - 2026", las = 1,
     cex.axis = 1.2, cex.lab = 1.2)
abline(0, 0, col = "lightblue")
legend(10, -20, lty = c(2), expression(paste(" Slot - SQ (Slot F)", [2])))
legend(10 , -20, lty = c(2), expression("Slot - SQ (Slot F)" [threshold]))
c("Slot - SQ (SQ F, SQ F)"))
                                 ##"Slot - SQ (Slot F, SQ F)"))



= expression(paste("Area (", cm^2, ")", sep = "")))


par(mfcol = c(1, 2 ))
plot(slot.head - sq.head, type = "l", ylim = c(-40, 20), xlab = "Age",
     ylab = "Abundance Difference x1000", main = "Mean 2016 - 2026", las = 1,
     cex.axis = 1.2, cex.lab = 1.2)
lines(slot.sqF.head - sq.head, lty = 2)
abline(0, 0, col = "lightblue")
legend(1, -34, lty = c(2, 1),
       c(expression("Slot - SQ (SQ F"[threshold]* ", SQ F"[threshold]* ")"),
         expression("Slot - SQ (Slot F"[threshold]* ", SQ F"[threshold] * ")")))


sq.terminal <-  colMeans(sq[91:101, 2:27]    )
slot.terminal <- colMeans(slot[91:101, 2:27]    )
slot.sqF.terminal <- colMeans(slot.sqF[91:101, 2:27]    )

plot(slot.terminal - sq.terminal, type = "l", ylim = c(-40, 20), xlab = "Age",
     ylab = "Difference in Abundance x1000", main = "Mean 2106 - 2116", las = 1,
     cex.axis = 1.2, cex.lab = 1.2)
lines(slot.sqF.terminal- sq.terminal, lty = 2)
abline(0, 0, col = "lightblue")
legend(1, -34, lty = c(2, 1),
       c(expression("Slot - SQ (SQ F"[threshold]* ", SQ F"[threshold]* ")"),
         expression("Slot - SQ (Slot F"[threshold]* ", SQ F"[threshold] * ")")))

 legend(10, -20, lty = c(2, 1), c("Slot - SQ (SQ F, SQ F)",
                                 "Slot - SQ (Slot F, SQ F)"))

