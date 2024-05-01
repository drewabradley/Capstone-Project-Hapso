rm(list=ls())

getwd()
setwd("C:/Users/Drewb/Downloads")

library(dplyr)

rm(list=ls())





nycregistration <- read.csv("nycregistration_cleanish.csv")

View(nycregistration)

unique(nycregistration$Model)

nycregistration <- nycregistration[nycregistration$Make == "TOYOT",]

nycregistration <- nycregistration[nycregistration$Model.Year >= "2010",]

#First Character of VIN - Country
nycregistration$Model <- substr(as.character(nycregistration$VIN), 4, 8)

nycregistration$Model[nycregistration$Model %in% c('AAABA','ZY68A','ZY64A','ZY5G1','ZT38A','ZT34A','YY5G1','KY5G1','AY5B1','BM5G1','BW5G1','BY5G1','DW5G1','DY5G1','FY5B1','GY5B1','HY5B1','JW5G1','JY5G1','EY5B1','')] <- "Sequoia"
nycregistration$Model[nycregistration$Model %in% c('FBABG','EAABG','DAABG','CAABG','BAABG','AAABG','CAAAG','AAAAG')] <- "Corolla Cross"
nycregistration$Model[nycregistration$Model %in% c('VPMAE','T4RCE','T4MCE','S4RCE','S4MCE','P4RCE','P4MCE','M4RCE','HPRAE','FPRAE','FPMAE','EPRAE','EPMAE','DPRAE','DPMAE','B4RBE','B4MDE','BU4EE','BURHE','BPRHE','VPRAE','B4MCE')] <- "Corolla"
nycregistration$Model[nycregistration$Model %in% c('WA5DB','VC5DB','UY5F1','UX4EN','UW5F17','UU4EN','UM5F1','TY5F1','TX4GN','SZ5AN','SV541','RZ5CN','RY5F1','RX5GN','RW5F1','RU5F1','RT541','RM5F1','PX4EN','PC5DB','NA5DB','MV521','MC5EC','MA5EC','MA5DB','LV521','LU521','LA5EC','LA5DB','LA5DA','KT521','JU521','JT521','JC5EC','JA5EC','JA5DB','JA5DA','HY5F1','HW5F1','FW5F1','EY5F1','EW5F1','EV541','EM5F1','EM5F1','DY5F1','DW5F1','DV581','DV541','DM5F1','CY5F1','CW5F1','CT541','BY5F1','BW541','BV581','BV541','BT541','AY5F1','AW5F1','KB5GD','WC5DB','WA5EC','UW5F1','NC5EC','NC5DB','NA5EC','MC5DB','KB5DB','JC5DB','BW5F1')] <- "Tundra"
nycregistration$Model[nycregistration$Model %in% c('MU4FN','LU4EN','KU4HN','JX4GN','JU4GN','GZ5AN','DZ5BN','CZ5AN','AZ5CN','AX5GN','SX5EN','TX4CN','TU4GN','NX4CN','JX4CN','HZ5BN','BZ5DN','TU4CN')] <- "Tacoma"
nycregistration$Model[nycregistration$Model %in% c('AAABA','ZZRFH','ZZRAH','ZK3EH','ZKRFH','ZA3EH','ZARFH','ZARAH','YZRFH','YZRAH','YK3EH','YKRFH','YARAH','XBRCH','LZRBH','LZRAH','KZRFH','KK3EH','KKRFH','AARAH','ABRCH','KDRBH','KDRAH','KBRCH','KARAH','JZ3DC','JZRFH','JZRBH','JZRAH','JK3EH','JKRFH','JGRFH','JCRFH','HZRBH','HZRAH','HBRCH','HARAH','GZRBH','GZRAH','GBRCH','GARAH','FZRBH','FZRAH','FARAH','EK3EH','EBRCH','DZRFH','DK3EH','DKRFH','DGRFH','DCRFH','DBRCH','CZRBH','CZRAH','BZRFH','BZRBH','BK3EH','BKRFH','BGRFH','BCRFH','BBRCH','DZRBH')] <- "Highlander"
nycregistration$Model[nycregistration$Model %in% c('ZZ3DC','ZSKFC','ZRKEC','ZK3DC','ZK23C','ZK22C','ZA29C','ZA23C','ZA22C','YZ3DC','KZ3DC','YSKFC','YRKEC','YK4CC','YK3DC','XZ3DC','XRKEC','XK3DC','KSKFC','KRKEC','KK4CC','KK3DC','KA3DC','JSKFC','JRKEC','JK4CC','JK3DC','GRKEC','EZ3DC','ESKFC','ERKEC','DZ3DC','DSKFC','DRKEC','DK4CC','DK3DC','CSKFC','CRKEC','BSKFC','BRKEC','ASKFC','XSKFC')] <- "Sienna"
nycregistration$Model[nycregistration$Model %in% c('A1RFV','BFREV','B1RFV','BF4DV','BK4DV','B6RFV','C1RFV','DFREV','DF4DV','DK4DV','DWRFV','D6RFV','EWRFV','E6RFV','F1RFV','G1RFV','H1RFV','JFREV','JF4DV','JK4DV','J1RFV','KF4DV','LWRFV','L6RFV','MWRFV','M6RFV','NFREV','N1RFV','P1RFV','RFREV','RF4DV','RWRFV','R6RFV','S1RFV','T6RFV','UWRFV','WFREV','WF4DV','W1RFV','YFREV','YF4DV','YK4DV','Y1RFV','ZFREV','ZF4DV','B6RFV','DWRFV','D6RFV','EWRFV','E6RFV','LWRFV','L6RFV','MWRFV','M6RFV','RWRFV','R6RFV','T6RFV','K1RFV','RK4DV')] <- "RAV4"
nycregistration$Model[nycregistration$Model %in% c('DLBJV','DLBYV')] <- "Yaris"
nycregistration$Model[nycregistration$Model %in% c('AZ1FB','BD1EB','BK1EB','BK3DB','BZ1FB','B21FB','CZ1FB','GZ1FB','HZ1FB','JA1AB','JZ1FB','J21FB','LB1BB','NB1BB','FZ1FB','E21FB','EZ1FB','AA1AB','A21FB','C21FB','DA1AB','DZ1FB','D21FB','EA1AB')] <- "Avalon"
nycregistration$Model[nycregistration$Model %in% c('BB3EK','BD1FK','BF1FK','BF3EK','BK1FK','BK3EK','BZ1HK','B11HK','B21HK','B31HK','B61HK','C11AK','C31AK','F11BK','F31AK','G11AK','G11BK','G31AK','H31AK','J11AK','J31AK','KZ1AK','K31AK','K61AK','K61BK','L11AK','L11BK','L31AK','M11AK','M11BK','NZ1AK','R11AK','R11BK','S11AK','S31AK','T11AK','T11BK','T31AK','BF1FK','BF3EK','C11BK','E11AK','E31AK','FZ1AK','F11AK','J11BK')] <- "Camry"
nycregistration$Model[nycregistration$Model %in% c('BA3BB','ZA3BB','ZK3BB','BK3BB')] <- "Venza"
nycregistration$Model[nycregistration$Model %in% c('AAAB5','ADAB5','ACAB5')] <- "Grand Highlander"
nycregistration$Model[nycregistration$Model %in% c('DLBZV')] <- "Scion"
nycregistration$Model[nycregistration$Model %in% c('ME4EE','KE4EE','KU4EE','K44EE','LE4EE')] <- "Corolla Matrix"


model_counts <- table(nycregistration$Model)
View(model_counts)


model_counts_by_city <- nycregistration %>%
  group_by(Model, City) %>%
  summarise(Count = n(), .groups = 'drop')

model_counts_by_year <- nycregistration %>%
  group_by(Model, Model.Year) %>%
  summarise(Count = n(), .groups = 'drop')


View(model_counts_by_city)
View(model_counts_by_year)
##########################################################################################






























