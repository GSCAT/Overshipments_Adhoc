library(dplyr)
library(readr)
library(rChoiceDialogs)

choose_file_directory <- function()
{
  v <- jchoose.dir()
  return(v)
}

Overages_VMI <- Overages
Overages_VMI$Program_Type <- trimws(Overages$Program_Type, which = "both")

Overages_VMI <- Overages_VMI %>% subset( BRD_NM == "OLD NAVY") %>%
  subset(BRD_DIV_DESC != "FUN ZONE")%>% 
  subset(Program_Type != "VMI") %>%
  subset(XPLD_LN_ORD_QTY != 0) %>%
  na.omit() %>%
  droplevels()

Overages_VMI <- Overages_VMI %>% mutate("Percent_diff" = ((INV_RCPT_QTY-XPLD_LN_ORD_QTY)/XPLD_LN_ORD_QTY)*100)
Overages_VMI <- Overages_VMI %>% mutate("Delta_Units" = (INV_RCPT_QTY-XPLD_LN_ORD_QTY- XPLD_LN_ORD_QTY))

count_of_dupes <- count(Overages_VMI, MKT_PO_ID)

Overages_nodupe <- left_join(Overages_VMI, count_of_dupes, by = "MKT_PO_ID" ) %>% droplevels()
Overages_nodupe <- subset(Overages_nodupe, n == 1)

hist(na.omit(Overages_nodupe$Percent_diff), main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 30), ylim= c(0,500), breaks= 100, freq = TRUE)
axis(1, at=c(-5, 5))
