#setwd("~/Documents/R/Claro/IUB")
setwd("~/Iub_Traffic")
library("dplyr")
#con <- gzfile("ExportPerformanceQueryResult_20160518_095400.tar")
iubtraffic_raw <- read.table("ExportPerformanceQueryResult_20160526_151417.csv", header = TRUE, sep = ",", skip = 7, na.strings = "NIL")
iubtraffic_all <- mutate(iubtraffic_raw, Date = as.POSIXct(Start.Time, format ="%m/%d/%Y %H:%M:%S"))
iubtrafficatm <- select(iubtraffic_all, NE.Name, VS.ATMDlMaxUsed.1..kbit.s., VS.IPDlMaxUsed.1..kbit.s.) %>%
  group_by(NE.Name) %>%
  summarise(delta_atm = quantile(VS.ATMDlMaxUsed.1..kbit.s.,2/4,na.rm = TRUE )/((1.5*IQR(VS.ATMDlMaxUsed.1..kbit.s.,na.rm = TRUE)+quantile(VS.ATMDlMaxUsed.1..kbit.s.,3/4,na.rm = TRUE))-quantile(VS.ATMDlMaxUsed.1..kbit.s.,2/4,na.rm = TRUE))) %>%
  arrange(desc(delta_atm))

iubtrafficip <- select(iubtraffic_all, NE.Name, VS.ATMDlMaxUsed.1..kbit.s., VS.IPDlMaxUsed.1..kbit.s.) %>%
  group_by(NE.Name) %>%
  summarise(delta_ip = quantile(VS.IPDlMaxUsed.1..kbit.s.,2/4,na.rm = TRUE )/((1.5*IQR(VS.IPDlMaxUsed.1..kbit.s.,na.rm = TRUE)+quantile(VS.IPDlMaxUsed.1..kbit.s.,3/4,na.rm = TRUE))-quantile(VS.IPDlMaxUsed.1..kbit.s.,2/4,na.rm = TRUE))) %>%
  arrange(desc(delta_ip))

write.csv(iubtrafficatm, file = "iubgeralatm.csv", row.names = FALSE)
write.csv(iubtrafficip, file = "iubgeralip.csv", row.names = FALSE)

boxatm <- function(siteatm) {
  
  atm <- select(filter(iubtraffic_all, NE.Name==siteatm), Date, NE.Name, VS.ATMDlMaxUsed.1..kbit.s.)
  boxplot(atm$VS.ATMDlMaxUsed.1..kbit.s., main=siteatm,ylab="kBPS",col="gold",outline=F)
}

hitatm <- function(siteatm) {
  atm <- select(filter(iubtraffic_all, NE.Name==siteatm), Date, NE.Name, VS.ATMDlMaxUsed.1..kbit.s.)
  hist(atm$VS.ATMDlMaxUsed.1..kbit.s.)
}

ptatm <- function(siteatm) {
  atm <- select(filter(iubtraffic_all, NE.Name==siteatm), Date, NE.Name, VS.ATMDlMaxUsed.1..kbit.s.)
  plot(atm$Date,atm$VS.ATMDlMaxUsed.1..kbit.s., main = siteatm, type = "l")
}
  

boxip <- function(siteip) {
  
  ip <- select(filter(iubtraffic_all, NE.Name==siteip), Date, NE.Name, VS.IPDlMaxUsed.1..kbit.s.)
  boxplot(ip$VS.IPDlMaxUsed.1..kbit.s., main=siteip,ylab="kBPS",col="gold",outline=F)
}

boxatm <- function(siteip) {
  
  ip <- select(filter(iubtraffic_all, NE.Name==siteip), Date, NE.Name, VS.IPDlMaxUsed.1..kbit.s.)
  hist(ip$VS.IPDlMaxUsed.1..kbit.s.)
}

ptip <- function(siteip) {
  
  ip <- select(filter(iubtraffic_all, NE.Name==siteip), Date, NE.Name, VS.IPDlMaxUsed.1..kbit.s.)
  plot(ip$Date,ip$VS.IPDlMaxUsed.1..kbit.s., main = siteip, type = "l")
}

