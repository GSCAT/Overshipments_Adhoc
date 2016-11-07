library(dplyr)

Over_20 <- Overages_nodupe %>% subset(Percent_diff>20)
Over_5 <- Overages_nodupe %>% subset(Percent_diff>5)
Under_neg_5 <- Overages_nodupe %>% subset(Percent_diff < c(-5))
Under_neg_20 <- Overages_nodupe %>% subset(Percent_diff < c(-20))

Over_table <- Over_5 %>% 
  select(BRD_NM, XPLD_LN_ORD_QTY, INV_RCPT_QTY, Percent_diff, Delta_Units ) %>%
  group_by(BRD_NM) %>%
  summarise(sum_XPLD_LN_ORD_QTY = sum(XPLD_LN_ORD_QTY), sum_INV_RCPT_QTY = sum(INV_RCPT_QTY), mean_Percent = mean(Percent_diff), sum_Delta_Units = sum(Delta_Units))

Overages_nodupe %>% summarise(Summary_of_Delta = sum(Delta_Units))
Overages_nodupe %>% subset(Delta_Units > 0) %>% summarise(Summary_of_Delta = sum(Delta_Units))
Overages_nodupe %>% subset(Delta_Units < 0) %>% summarise(Summary_of_Delta = sum(Delta_Units))

Overages_nodupe %>% group_by(PAR_VENDOR_LEGAL_DESC)%>%
  subset(Percent_diff >= 0 & Percent_diff <= 20) %>% 
  summarise(Summary_of_Delta = sum(Delta_Units), mean_overage = mean(Percent_diff)) %>%
  subset(Summary_of_Delta > 50000) %>%
  arrange(desc(mean_overage))