library(dplyr)
library(readr)
library(rChoiceDialogs)
library(xtable)
library(ggvis)

choose_file_directory <- function()
{
  v <- jchoose.dir()
  return(v)
}


# Overages <- read_delim(paste(choose_file_directory(), 'Market_PO_Cleanup_ALL_v3.txt' , sep = '/'), delim="^")
# Overages2 <- subset(Overages, INV_RCPT_QTY < 200000)
# Overages_save <- Overages2


Overages2 <- Overages_Git
# save(Overages_Git, file = "Overages_Git.rtf")
# load(file = "Overages_Git.rtf")
Overages2$Program_Type <- trimws(Overages2$Program_Type, which = "both")

Overages2 <- Overages2 %>% subset( BRD_NM == "OLD NAVY") %>%
              subset(BRD_DIV_DESC != "FUN ZONE")%>% 
              subset(Program_Type != "VMI") %>%
              subset(XPLD_LN_ORD_QTY != 0) %>%
              subset(INV_RCPT_QTY != 0) %>%
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

summary(Overages_nodupe)

hist(Overages_nodupe$Percent_diff, main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 30), ylim= c(0,3500), breaks= 1500, freq = TRUE)
axis(1, at=c(-5, 5))

write.csv(Overages_nodupe[,1:18], paste(choose_file_directory(), 'Raw_Data_wo_VMIorjpf_nodup_20161019_V2.csv' , sep = '/'))

View(Overages_nodupe)

# corrected time bound
Overages_nodupe2 <- Overages_nodupe



hist(subset(Overages_nodupe$InDC_YR_MO, Overages_nodupe$Percent_diff >10), main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(201600, 201606),ylim= c(0,500), breaks= 300, freq = TRUE)
hist(subset(Overages_nodupe$InDC_YR_MO, Overages_nodupe$Percent_diff >10), main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(201506, 201512),ylim= c(0,500), breaks= 300, freq = TRUE)


hist(subset(Overages_nodupe$Percent_diff, Overages_nodupe$Percent_diff >10), main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(10, 300),ylim= c(0,500), breaks= 300, freq = TRUE)
hist(subset(Overages_nodupe$Percent_diff, Overages_nodupe$Percent_diff < -10), main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-100, -10),ylim= c(0,100), breaks= 300, freq = TRUE)



Overages %>% group_by(BRD_DIV_DESC) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))
Overages %>% group_by(BRD_NM) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))

Overages %>% group_by(Program_Type) %>% summarise("Units" = sum(INV_RCPT_QTY, na.rm=TRUE))

Overages_nodupe$Percent_diff %>% 
  subset(Overages_nodupe$Percent_diff > 0) %>% 
hist( main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 50), ylim= c(0,5500), breaks= c(0, 1,2,3,4,5,10,15,20, max(Overages_nodupe$Percent_diff)), freq = TRUE)
axis(1, at=c(-5, 5))


Overages_nodupe$Percent_diff %>% 
  subset(Overages_nodupe$Percent_diff > 0) %>% 
hist( main = "Histogram of % Diff", xlab = "Percent Difference", col = "#00009950", plot = TRUE, xlim = c(-10, 50), ylim= c(0,1), breaks= 10000, freq = FALSE)
axis(1, at=c(-5, 5))

Hist_values <- Overages_nodupe$Delta_Units %>% 
  subset(Overages_nodupe$Delta_Units > 0) %>% 
hist( main = "Histogram of % Diff", 
      xlab = "Units", 
      col = "#00009950", 
      plot = FALSE, ylim= c(0,5500), breaks= 10, freq = TRUE)
axis(1, at=c(-5, 5))

Overages_nodupe$Delta_Units %>% 
  subset(Overages_nodupe$Delta_Units > 0) %>%
  group_by()
  plot()
  
  
  mx <- Overages_nodupe$Percent_diff
  my <- Overages_nodupe$Delta_Units
  
  h <- Overages_nodupe$Percent_diff %>% 
    subset(Overages_nodupe$Percent_diff > 0) %>% 
    hist( main = "Histogram of % Diff", 
          xlab = "Percent Difference", 
          col = "#00009950", 
          plot = FALSE, 
          xlim = c(-10, 50), 
          ylim= c(0,5500), 
          breaks= c(0,1,2,3,4,5,10,15,20,Inf), freq = FALSE)
  
  breaks <- data.frame("beg"= h$breaks[-length(h$breaks)], "end"=h$breaks[-1])
  # counts <- data.frame("beg"= h$counts[-length(h$counts)], "end"=h$counts[-1])
  
  sums <- apply(breaks, MARGIN=1, FUN=function(x) { sum(my[ mx >= x[1] & mx < x[2] ]) })
  
  #Total_overage <- Overages_nodupe %>% subset(Delta_Units > 0) %>% 
  #summarise(Summary_of_Delta = sum(Delta_Units))
  # sums[10] <- Total_overage - sum(sums)
  h$counts <- sums
  
  buckets <- apply(breaks, MARGIN=1, FUN=function(x) {paste(x[1], "to", x[2])})
  h_table <- buckets[1:9]
  h_table <- as.data.frame(h_table)
  h_table[,2] <- h$counts
  names(h_table) <- c("Percent Bucket", "Unit Count")
  
  
  
  plot(x=h$breaks[1:9], y=h$counts, xlab = "% Percent Difference", ylab="Sum", main="Sum Delta vs Percent", xlim = c(0, 20))
  
  