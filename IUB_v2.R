#Analizes Iub Congestion using a statistical data.

setwd("~/Documents/R/Claro/IUB")
library("dplyr")
#con <- gzfile("ExportPerformanceQueryResult_20160518_095400.tar")
iubtraffic_raw <- read.table("ExportPerformanceQueryResult_20160518_095400.csv", header = TRUE, sep = ",", skip = 7, na.strings = "NIL")
library("dplyr")
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

siteatm <- "UBAALC80"
n <- filter(iubtrafficatm, NE.Name==siteatm)
t <- select(filter(iubtraffic_all, NE.Name==siteatm), Date, NE.Name, VS.ATMDlMaxUsed.1..kbit.s.)
boxplot(t$VS.ATMDlMaxUsed.1..kbit.s., main=siteatm,ylab="kBPS",col="gold",outline=F)
hist(t$VS.ATMDlMaxUsed.1..kbit.s.)
plot(t$Date,t$VS.ATMDlMaxUsed.1..kbit.s., main = siteatm, type = "l")

siteip <- "UPETAC01"
n <- filter(iubtrafficip, NE.Name==siteip)
t <- select(filter(iubtraffic_all, NE.Name==siteip), Date, NE.Name, VS.IPDlMaxUsed.1..kbit.s.)
boxplot(t$VS.IPDlMaxUsed.1..kbit.s., main=siteip,ylab="kBPS",col="gold",outline=F)
hist(t$VS.IPDlMaxUsed.1..kbit.s.)
plot(t$Date,t$VS.IPDlMaxUsed.1..kbit.s., main = siteip, type = "l")