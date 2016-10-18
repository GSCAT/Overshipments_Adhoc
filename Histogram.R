library(dplyr)
library(readr)
library(rChoiceDialogs)

choose_file_directory <- function()
{
  v <- jchoose.dir()
  return(v)
}


Overages <- read_delim(paste(choose_file_directory(), 'Market_PO_Cleanup_ALL_v4.txt' , sep = '/'), delim="^")
# Overages2 <- subset(Overages, INV_RCPT_QTY < 200000)
Overages2 <- Overages %>% subset( BRD_NM == "OLD NAVY") %>%
              subset(BRD_DIV_DESC != "FUN ZONE")%>% 
              droplevels()

Overages2$INV_RCPT_QTY <- as.numeric(Overages2$INV_RCPT_QTY)
Overages2$XPLD_LN_ORD_QTY <- as.numeric(Overages2$XPLD_LN_ORD_QTY)
Overages2$ORIGIN_COUNTRY_CODE <- as.factor(Overages2$ORIGIN_COUNTRY_CODE)
Overages2$Total_FCST_ELC <- as.numeric(Overages2$Total_FCST_ELC)
# Check droped records
Overages2 %>% group_by(BRD_DIV_DESC) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))
Overages2 %>% group_by(BRD_NM) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))

Overages2 %>% group_by(Program_Type) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))
max(Overages$INV_RCPT_QTY)

# Remove NA's
Overages2 <- na.omit(Overages2)
# Remove zero 
Overages2 <- subset(Overages2, XPLD_LN_ORD_QTY != 0)

Overages2 <- Overages2 %>% mutate("Percent_diff" = ((INV_RCPT_QTY-XPLD_LN_ORD_QTY)/XPLD_LN_ORD_QTY)*100)
Overages2 <- Overages2 %>% mutate("Delta_Units" = (INV_RCPT_QTY-XPLD_LN_ORD_QTY- XPLD_LN_ORD_QTY))

count_of_dupes <- count(Overages2, MKT_PO_ID)

Overages_nodupe <- left_join(Overages2, count_of_dupes, by = "MKT_PO_ID" ) %>% droplevels()
Overages_nodupe <- subset(Overages_nodupe, n == 1)

hist(na.omit(Overages2$Percent_diff), main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 30), ylim= c(0,3500), breaks= 500000, freq = TRUE)
axis(1, at=c(-5, 5))

write.csv(Overages_nodupe[,1:17], paste(choose_file_directory(), 'Raw_Data_wo_VMIorjpf_noNA.csv' , sep = '/'))



