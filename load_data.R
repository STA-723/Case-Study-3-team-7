data_format <- dataset <- read_csv("Data_Format_93.csv")
dat93 = read.fwf("06577-0001-Data.txt", widths=data_format$Col_Width, na.strings=c("NA", " ", "."), col.names = paste0(data_format$Name) )
save(dat93, file="data_1993.rdata")

data_format <- dataset <- read_csv("Data_Format_97.csv")
dat97 = read.fwf("03163-0001-Data.txt", widths=data_format$Col_Width, na.strings=c("NA", " ", "."), col.names = paste0(data_format$Name) )
save(dat97, file="data_1997.rdata")

data_format <- dataset <- read_csv("Data_Format_99.csv")
dat99 = read.fwf("03818-0001-Data.txt", widths=data_format$Col_Width, na.strings=c("NA", " ", "."), col.names = paste0(data_format$Name) )
save(dat99, file="data_1999.rdata")

data_format <- dataset <- read_csv("Data_Format_01.csv")
dat01 = read.fwf("04291-0001-Data.txt", widths=data_format$Col_Width, na.strings=c("NA", " ", "."), col.names = paste0(data_format$Name) )
save(dat01, file="data_2001.rdata")
