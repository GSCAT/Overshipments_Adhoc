library(dplyr)
library(readr)
library(rChoiceDialogs)

choose_file_directory <- function()
{
  v <- jchoose.dir()
  return(v)
}


Overages <- read_delim(paste(choose_file_directory(), 'MKT_PO_CLEANUP_ALL.csv' , sep = '/'), delim="^")
# Overages2 <- subset(Overages, INV_RCPT_QTY < 200000)
Overages2 <- Overages %>% subset( BRD_NM == "OLD NAVY") %>%
              subset(BRD_DIV_DESC != "FUN ZONE")%>% droplevels()
# Check droped records
Overages2 %>% group_by(BRD_DIV_DESC) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))
Overages2 %>% group_by(BRD_NM) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))


max(Overages$INV_RCPT_QTY)


# hist(na.omit(Overages2$INV_RCPT_QTY), plot = TRUE, xlim = c(0, 20000),col = "#99000050", breaks= 2000, freq = TRUE)

# hist(na.omit(Overages2$XPLD_LN_ORD_QTY), add= TRUE, col = "#00009950", plot = TRUE, xlim = c(0, 20000), breaks= 2000, freq = TRUE)


# hist(na.omit(Overages2$INV_RCPT_QTY), plot = TRUE, xlim = c(0, 20000),col = "#99000050", breaks= 2000, freq = TRUE)
# hist(na.omit(Overages2$XPLD_LN_ORD_QTY), add= TRUE, col = "#00009950", plot = TRUE, xlim = c(0, 20000), breaks= 2000, freq = TRUE)



Overages2 <- Overages2 %>% mutate("Percent_diff" = ((INV_RCPT_QTY-XPLD_LN_ORD_QTY)/XPLD_LN_ORD_QTY)*100)
# Overages <- Overages %>% mutate("Percent_diff" = ((INV_RCPT_QTY-XPLD_LN_ORD_QTY)/XPLD_LN_ORD_QTY)*100)
# head(Overages2)
# str(Overages2)

hist(na.omit(Overages2$Percent_diff), main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 30), ylim= c(0,3500), breaks= 500000, freq = TRUE)
axis(1, at=c(-5, 5))

hist(na.omit(subset(Overages2$Percent_diff, Overages2$Percent_diff < 1)), main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 30), ylim= c(0,3500), breaks= 500000/3, freq = TRUE)
hist(na.omit(subset(Overages2$Percent_diff, Overages2$Percent_diff >= 1 | Overages2$Percent_diff <= 5)), add= TRUE, col = "#99009950", plot = TRUE, xlim = c(-10, 30), ylim= c(0,3500), breaks= 500000/3, freq = TRUE)
hist(na.omit(subset(Overages2$Percent_diff, Overages2$Percent_diff > 5)), add=TRUE, col = "#00009950", plot = TRUE, xlim = c(-10, 30), ylim= c(0,3500), breaks= 500000/3, freq = TRUE)
axis(1, at=c(-5, 5))

hist(na.omit(Overages$Percent_diff), main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 400), ylim= c(0,2000), breaks= 1000000, freq = TRUE)
axis(1, at=c(-5, 5))

Overages_greater5 <- subset(Overages2, Perent_diff > 5)

# Histogram for Dates greater than 5% subset
hist(na.omit(Overages_greater5$InDC_YR_MO[is.finite(Overages_greater5$InDC_YR_MO)]), 
     main = "Frequency of Planned INDC_YR_DT", 
     xlab = "INDC_YR_DT", col = "#99000050", 
     plot = TRUE, 
     xlim= c(201507,201512), 
     ylim= c(0,4000), 
     breaks= 1000, 
     freq = TRUE)

axis(1, at=c(-5, 5))

Overages_greater5 <- subset(Overages2, Perent_diff > 5)
str(Overages_greater5)

Overages_Wt <- Overages_greater5 %>% mutate("WT_Percent_diff" = ((INV_RCPT_QTY-XPLD_LN_ORD_QTY)/XPLD_LN_ORD_QTY)*100*INV_RCPT_QTY)

hist(na.omit(Overages_Wt$WT_Percent_diff), main = "Histogram of WT % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 400), ylim= c(0,2000), breaks= 100000000, freq = TRUE)
axis(1, at=c(-5, 5))


Overages2 %>% group_by(BRD_DEPT_DESC) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))
hist(na.omit(subset(Overages2$Percent_diff, grepl("Knits",Overages2$BRD_DEPT_DESC ))) , main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 30), ylim= c(0,7000), breaks= 1000000, freq = TRUE)
axis(1, at=c(-5, 5))

# hist(na.omit(subset(Overages_Wt$Percent_diff,  )), main = "Histogram of WT % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 400), ylim= c(0,2000), breaks= 100000000, freq = TRUE)
# axis(1, at=c(-5, 5))

Overages2 <- subset(Overages2, Percent_diff >  -10 & Percent_diff <  30 )
hist(na.omit(Overages2$Percent_diff), main = "Histogram of % Diff", xlab = "Percent Difference", col = "#99559950", plot = TRUE, xlim = c(-10, 30), ylim= c(0,3500), breaks= seq(-10, 30, by = .5), freq = TRUE)
# hist(na.omit(subset(Overages2$Percent_diff, Overages2$Percent_diff >  5)), add=TRUE, breaks= 500000,  xlim = c(-10, 30), ylim= c(0,3500),col = "#00009950", plot = TRUE, freq= TRUE)
# hist(na.omit(subset(Overages2$Percent_diff, Overages2$Percent_diff >  1)), add=TRUE, breaks= 500000,  xlim = c(-10, 30), ylim= c(0,3500),col = "#00009950", plot = TRUE, freq= TRUE)
# hist(na.omit(subset(Overages2$Percent_diff, Overages2$Percent_diff >  1)), add=TRUE, breaks= 500000,  xlim = c(-10, 30), ylim= c(0,3500),col = "#00009950", plot = TRUE, freq= TRUE)

hist(na.omit(subset(Overages2$Percent_diff, Overages2$Percent_diff < 1.0 | Overages2$Percent_diff > 5.0)), add=TRUE, col = "#00999950", plot = TRUE, xlim = c(-10, 30), ylim= c(0,3500), breaks= seq(-10, 30, by = .5), freq= TRUE)
#hist(na.omit(subset(Overages2$Percent_diff, Overages2$Percent_diff >= 1 | Overages2$Percent_diff <= 5)), add=TRUE, , xlim = c(-10, 30), ylim= c(0,3500), breaks= 500000, col = "#00999999", plot = TRUE, freq= TRUE)

# hist(na.omit(subset(Overages2$Percent_diff, Overages2$Percent_diff <  1)), add=TRUE, breaks= 500000,  xlim = c(-10, 30), ylim= c(0,3500),col = "#99009950", plot = TRUE, freq= TRUE)
axis(1, at=c(-5, 5))
