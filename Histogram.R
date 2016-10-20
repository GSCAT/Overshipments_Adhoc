library(dplyr)
library(readr)
library(rChoiceDialogs)

choose_file_directory <- function()
{
  v <- jchoose.dir()
  return(v)
}


# Overages <- read_delim(paste(choose_file_directory(), 'Market_PO_Cleanup_ALL_v3.txt' , sep = '/'), delim="^")
# Overages2 <- subset(Overages, INV_RCPT_QTY < 200000)

Overages2 <- Overages
Overages2$Program_Type <- trimws(Overages$Program_Type, which = "both")

Overages2 <- Overages2 %>% subset( BRD_NM == "OLD NAVY") %>%
              subset(BRD_DIV_DESC != "FUN ZONE")%>% 
              subset(Program_Type != "VMI") %>%
              subset(XPLD_LN_ORD_QTY != 0) %>%
              na.omit() %>%
              droplevels()

Overages2$INV_RCPT_QTY <- as.numeric(Overages2$INV_RCPT_QTY)
Overages2$XPLD_LN_ORD_QTY <- as.numeric(Overages2$XPLD_LN_ORD_QTY)
Overages2$ORIGIN_COUNTRY_CODE <- as.factor(Overages2$ORIGIN_COUNTRY_CODE)
Overages2$Total_FCST_ELC <- as.numeric(Overages2$Total_FCST_ELC)
# Check droped records
Overages2 %>% group_by(BRD_DIV_DESC) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))
Overages2 %>% group_by(BRD_NM) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))

Overages2 %>% group_by(Program_Type) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))


# Remove NA's
# Overages2 <- na.omit(Overages2)
# Remove zero 
# Overages2 <- subset(Overages2, XPLD_LN_ORD_QTY != 0)

Overages2 <- Overages2 %>% mutate("Percent_diff" = ((INV_RCPT_QTY-XPLD_LN_ORD_QTY)/XPLD_LN_ORD_QTY)*100)
Overages2 <- Overages2 %>% mutate("Delta_Units" = (INV_RCPT_QTY - XPLD_LN_ORD_QTY))

count_of_dupes <- count(Overages2, MKT_PO_ID)

Overages_nodupe <- left_join(Overages2, count_of_dupes, by = "MKT_PO_ID" ) %>% droplevels()
Overages_nodupe <- subset(Overages_nodupe, n == 1)
Overages_nodupe$n <- as.factor(Overages_nodupe$n)

Overages_nodupe <- Overages_nodupe[order(-Overages_nodupe$Percent_diff), ]

hist(Overages_nodupe$Percent_diff, main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 30), ylim= c(0,3500), breaks= 3000, freq = TRUE)
axis(1, at=c(-5, 5))

write.csv(Overages_nodupe[,1:18], paste(choose_file_directory(), 'Raw_Data_wo_VMIorjpf_nodup_20161019_V2.csv' , sep = '/'))

View(Overages_nodupe)



