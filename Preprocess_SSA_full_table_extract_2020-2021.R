prefix="2022-08-14"
filename="220814COVID19MEXICO.csv"
# data_all <- read.table(unz("datos_abiertos_covid19.zip", "220123COVID19MEXICO.csv"), header=T, quote="\"", sep=",")
data_all <- read.table(filename, header=T, quote="\"", sep=",")
vect <- as.Date(data_all[,"FECHA_INGRESO"])
data_all <- data_all[vect < "2022-01-01",]
dim(data_all)
# [1] 12634194       40
write.table(data_all, "data_2020-2021.tsv", sep='\t', col.names = TRUE, row.names = FALSE, quote=FALSE)
