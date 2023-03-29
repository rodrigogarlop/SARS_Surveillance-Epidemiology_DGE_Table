# Started 2023-03-02 by Rodrigo García-López for Prof. Carlos Arias Virology Lab at IBt, UNAM, Cuernavaca, México
# This scripts get a table from the DGE for SARS-CoV-2, then appends important information for easier processing
# R version 4.2.2 (2022-10-31) -- "Innocent and Trusting"
########################## Load Functions ##########################
twoway_table <- function(mat){ # Cross two variables from a 2 column matrix (var1, var2), return a table of var2 x var1 with n columns depending on total var2 items
	xtable <- xtabs(rep(1,nrow(mat))~mat[,1]+mat[,2], data=mat)
	return(xtable)
}
fill_missing_items <- function(mat,vector){ # Gets a table and a vector with all rows that should be present, outputs an expanded row collection with missing dates for complete calendar in that range. rownames should have date format as %Y-%m-%d
	xtable <- as.data.frame(matrix(0,nrow=length(vector),ncol=ncol(mat)), stringsAsFactors = FALSE) # Create empty vessel for output
	rownames(xtable) <- vector # use the vector as rownames
	colnames(xtable) <- colnames(mat) # and inherit the names
	invisible(sapply(rownames(mat),function(x) {xtable[x,] <<- mat[x,]}))	# append the original values in the corresponding places (write to higher env variable
	return(xtable)
}
rolling_N_avg <- function(mat,int){ # Input should be a table with continuous data at rows and the desired interval for the mean. If an even number is provided, the average will be placed one position to the right of as there is no single middle number
	before <- after <- trunc(int/2) # initialize with same range before and after each position
	if(int%%2==0){after <- after-1} # If even, shift the upper half of the range by 1 position (the mean will be calculated for the next value next to the middle as there is no exact number in it)
	roll <- sapply((before+1):(nrow(mat)-after),function(y) {apply(mat,2, function(x) mean(as.numeric(x[(y-before):(y+after)])))})
	colnames(roll) <- row.names(mat)[(before+1):(nrow(mat)-after)]
	roll <- t(roll)
	return(roll)
}
strip <- function(string){ # Takes a string object and strips rare characters
	string <- iconv(string,from="UTF-8",to="ASCII//TRANSLIT") # Remove accents and rare chars
# 	string <- tolower(string) # change to lower
	string <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", string, perl=TRUE) # Remove multiple spaces
	string <- gsub("\\+","",string) # remove + characters
	return(string)
}
fix_age <- function(vect){ # Ages may be missing. Fix all and convert them to character.
# 	vect[vect=="u"]=NA # change "u"s to NAs to prevent warnings DEPRECATED
	vect <- sprintf('%0.3d',as.numeric(vect))
# 	vect[vect=="NA"]="u" # Revert NAs (they are now text) to "u"s DEPRECATED
	vect[vect=="999"] <- "u"
	return(vect)
}
########################## Load Input ##########################
args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 2) { # at least, two arguments should be included: <prefix> <file>
  stop("A minimum of 2 arguments are mandatory: cat table.tsv|Rscript Append_extra_metadata.R <prefix> <filename>", call.=FALSE)
}
prefix <- as.character(args[1]) # Get a string handle to create output names
filename <- as.character(args[2])  # and the name of the metric (free strings)

# prefix="2023-02-28"
# filename="220221COVID19MEXICO.csv"
# filename="out.tsv"
# filename="dge_unida_2023-02-28.tsv"
ignored_days <- 8 # This changes the days that should be ignored from the dates
finalcut <- ignored_days-3
date <- prefix
entidades <- read.table("Entidades.tsv", sep="\t",header=T, skip=0, comment.char='',quote="",fill=F, row.names=1, check.names=F) #
sectores <- read.table("Sectores.tsv", sep="\t",header=T, skip=0, comment.char='',quote="",fill=F, row.names=1, check.names=F) #
# data_all <- read.table(unz("datos_abiertos_covid19.zip", "220123COVID19MEXICO.csv"), header=T, quote="\"", sep=",")
# data_all <- read.table(filename, header=T, quote="\"", sep=",")
data_all <- read.table(filename, header=T, quote="", sep="\t")
data_all <- read.table(filename, header=T, quote="", sep="\t")
# save.image("chkpt1.Rdata")

########################## MAIN ##########################
# load("chkpt1.Rdata")
nam <- colnames(data_all); nam
#  [1] "FECHA_ACTUALIZACION"   "ID_REGISTRO"           "ORIGEN"
#  [4] "SECTOR"                "ENTIDAD_UM"            "SEXO"
#  [7] "ENTIDAD_NAC"           "ENTIDAD_RES"           "MUNICIPIO_RES"
# [10] "TIPO_PACIENTE"         "FECHA_INGRESO"         "FECHA_SINTOMAS"
# [13] "FECHA_DEF"             "INTUBADO"              "NEUMONIA"
# [16] "EDAD"                  "NACIONALIDAD"          "EMBARAZO"
# [19] "HABLA_LENGUA_INDIG"    "INDIGENA"              "DIABETES"
# [22] "EPOC"                  "ASMA"                  "INMUSUPR"
# [25] "HIPERTENSION"          "OTRA_COM"              "CARDIOVASCULAR"
# [28] "OBESIDAD"              "RENAL_CRONICA"         "TABAQUISMO"
# [31] "OTRO_CASO"             "TOMA_MUESTRA_LAB"      "RESULTADO_LAB"
# [34] "TOMA_MUESTRA_ANTIGENO" "RESULTADO_ANTIGENO"    "CLASIFICACION_FINAL"
# [37] "MIGRANTE"              "PAIS_NACIONALIDAD"     "PAIS_ORIGEN"
# [40] "UCI"
# dim(data_all)
# [1] 19518643       40
data_all[,"PAIS_NACIONALIDAD"] <- strip(data_all[,"PAIS_NACIONALIDAD"])
data_all[,"PAIS_ORIGEN"] <- strip(data_all[,"PAIS_ORIGEN"])

# General search:
# i=10;test <- table(data[i]);test <- test[test>0];length(test) # test how many different categories there re
# i=10;test <- table(data[i]);test*100/total_confirmed # same, with %
# i=37;test <- table(data[i]);test <- test[test>0];length(test) # test how many different categories there are
# Add test result
data_all[,"Test_Result"]="" # First, create an additional column holding the general type of result
data_all[data_all[,"CLASIFICACION_FINAL"]<=3,"Test_Result"] <- "P" # Code p for positive cases
data_all[data_all[,"CLASIFICACION_FINAL"]==4,"Test_Result"] <- "U" # Invalid results are unknown
data_all[data_all[,"CLASIFICACION_FINAL"]==5,"Test_Result"] <- "U" # Same for untested ones
data_all[data_all[,"CLASIFICACION_FINAL"]==6,"Test_Result"] <- "S" # Mark suspect
data_all[data_all[,"CLASIFICACION_FINAL"]==7,"Test_Result"] <- "N" # and Negatives
# table(data_all[,"Test_Result"])
#        N        P        S        U
# 11285375  7424884   700509   107875

# Add type of test
data_all[,"Test_type"] <- data_all[,"CLASIFICACION_FINAL"]
data_all[data_all[,"RESULTADO_ANTIGENO"]==1, "Test_type"] <- paste0(data_all[data_all[,"RESULTADO_ANTIGENO"]==1, "Test_type"],4) # if antigen was positive, then mark t as so
data_all[data_all[,"RESULTADO_LAB"]==1, "Test_type"] <- paste0(data_all[data_all[,"RESULTADO_LAB"]==1, "Test_type"],5)
data_all[data_all[,"Test_type"]==1,"Test_type"] <- "Asoc"
data_all[data_all[,"Test_type"]==2,"Test_type"] <- "Exp"
data_all[data_all[,"Test_type"]==34,"Test_type"] <- "Ant"
data_all[data_all[,"Test_type"]==345,"Test_type"] <- "PCR+Ant"
data_all[data_all[,"Test_type"]==35,"Test_type"] <- "PCR"
data_all[data_all[,"Test_type"]==4,"Test_type"] <- ""
data_all[data_all[,"Test_type"]==5,"Test_type"] <- ""
data_all[data_all[,"Test_type"]==6,"Test_type"] <- ""
data_all[data_all[,"Test_type"]==7,"Test_type"] <- ""
data_all[data_all[,"Test_type"]==74,"Test_type"] <- ""

# Add if dead
data_all[data_all[,"FECHA_DEF"]!="9999-99-99","Deaths"] <- "D"
data_all[data_all[,"FECHA_DEF"]=="9999-99-99","Deaths"] <- "S"

# Add type of patient
data_all[data_all["TIPO_PACIENTE"]==1,"Type_pat"] <- "Amb"
data_all[data_all["TIPO_PACIENTE"]==2,"Type_pat"] <- "Hosp"
data_all[data_all["INTUBADO"]==1,"Type_pat"] <- "Int"

# Append State
entidades[9,1] <- "Mexico City"
entidades[15,1] <- "State of Mexico"
data_all[, "State"] <- entidades[data_all[,"ENTIDAD_RES"],1]

# For the actual code for municipality:
data_all[,"ENTIDAD_RES"] <- sprintf("%02d",data_all[,"ENTIDAD_RES"])
data_all[,"MUNICIPIO_RES"] <- sprintf("%03d",data_all[,"MUNICIPIO_RES"])
data_all[, "Muni"] <- paste0(data_all[,"ENTIDAD_RES"],data_all[,"MUNICIPIO_RES"])

# Add other dates
data_all[,"Year"] <- format(as.Date(data_all[,"FECHA_INGRESO"]),"%Y")
data_all[,"Month"] <- format(as.Date(data_all[,"FECHA_INGRESO"]),"%Y-%m (%b)")
week <- data.frame("week"=c(paste0("20W",sprintf('%0.2d', rep(1:52,each=7))),paste0("21W",sprintf('%0.2d', rep(1:52,each=7))),paste0("22W",sprintf('%0.2d', rep(1:52,each=7))),paste0("23W",sprintf('%0.2d', rep(1:52,each=7)))))
# Now rename the rows to use them as a hash (dictionary)
rownames(week) <- seq(as.Date("2020/01/05"), as.Date("2023/12/31"), by="day")[1:nrow(week)]
week[,2] <- format(as.Date(rownames(week))," (%b)")
temp <- sapply(unique(week[,1]), function(x){week[grep(x,week[,1])[1],2]}) # Create a small named vector for identifying the month the first day of the week belongs to. This is important for weeks to match in groups of 7
week[,3] <- temp[week[,1]]
week[,4] <- paste0(week[,1], week[,3])
# head(table(week[,4]),20) # Test that every week is ok, there should be exactly 7 of each week
# 20W01 (Jan) 20W02 (Jan) 20W03 (Jan) 20W04 (Jan) 20W05 (Feb) 20W06 (Feb)
#           7           7           7           7           7           7
# 20W07 (Feb) 20W08 (Feb) 20W09 (Mar) 20W10 (Mar) 20W11 (Mar) 20W12 (Mar)
#           7           7           7           7           7           7
# 20W13 (Mar) 20W14 (Apr) 20W15 (Apr) 20W16 (Apr) 20W17 (Apr) 20W18 (May)
#           7           7           7           7           7           7
# 20W19 (May) 20W20 (May)
#           7           7
# Now used the newly created dictionary to append the corresponding week
data_all[,"Week"] <- week[as.character(data_all[,"FECHA_INGRESO"]),4]

# Now, all age categories
# First filter those with >120 years (there are too many because some birthdays were badly annotated as 1900-01-01. Take the following as example:
# 110    111    112    113    114    115    116    117    118    119    120
#    105     84     67     45     47     62     51     35     40     51    909
#    121    122    123    128    130    135    137    138    140    141    144
#   3580    101      7      1      1      1      2      2      1      1      1
#    145    147    148    154    161    162    164    229    251    266
#      1      3      1      1      1      1      1      1      1      2
data_all[data_all[,"EDAD"]>=120, "EDAD"] <- 999 # These errors are tagged with value 999
age1 <- data.frame("Age"=as.character(rep(paste0(sprintf('%0.2d',1:14),":",sapply(seq(0,130,10),function(x) paste0(x,"-",x+9))),each=10)), stringsAsFactors = FALSE)
rownames(age1) <- 0:(nrow(age1)-1) # and use age1s as rownames
age1[as.numeric(rownames(age1))>=100,1] <- "11:100+" # This was later added to collate all 100 an over
data_all[,"Age_range_by10"] <- age1[as.character(data_all[,"EDAD"]),1] # Now append it to the main dataframe
# Repeat with smaller bins
age2 <- data.frame("Age"=rep(paste0(sprintf('%0.2d',1:28),":",sapply(seq(0,135,5),function(x) paste0(x,"-",x+4))),each=5), stringsAsFactors = FALSE)
rownames(age2) <- 0:(nrow(age2)-1) # and use age2s as rownames
age2[as.numeric(rownames(age2))>=100,1] <- "21:100+" # This was later added to collate all 100 an over
data_all[,"Age_range_by5"] <- age2[as.character(data_all[,"EDAD"]),1] # Now append it to the main dataframe
# Repeat with a new classification
age3 <- data.frame("age3"=c(rep("1:0-12", 13), rep("2:13-25",13),rep("3:26-45",20), rep("4:46-60",15), rep("5:61-74",14), rep("6:75+",55)), stringsAsFactors = FALSE)
rownames(age3) <- 0:(nrow(age3)-1) # and use age3s as rownames
data_all[,"Age_range_manual"] <- age3[as.character(data_all[,"EDAD"]),1] # Now append it to the main dataframe
# Added a new age scheme matching vaccination
age4 <- data.frame("age4"=c(rep("1:0-17", 18), rep("2:18-29",12),rep("3:30-39",10), rep("4:40-49",10), rep("5:50-59",10), rep("6:60+",100)), stringsAsFactors = FALSE)
rownames(age4) <- 0:(nrow(age4)-1) # and use age4s as rownames
data_all[,"Age_vac"] <- age4[as.character(data_all[,"EDAD"]),1] #
age5 <- data.frame("age4"=c(rep("1:0-5", 6), rep("2:6-17", 12), rep("3:18-29",12),rep("4:30-39",10), rep("5:40-49",10), rep("6:50-59",10), rep("7:60+",100)), stringsAsFactors = FALSE)
rownames(age5) <- 0:(nrow(age5)-1) # and use age5s as rownames
data_all[,"Age_vac_2"] <- age5[as.character(data_all[,"EDAD"]),1] #
pdf("ages.pdf")
barplot(las=2,table(data_all[,"Age_range_by10"]),border=F, col="coral1")
barplot(las=2,table(data_all[,"Age_range_by5"]),border=F, col="coral1")
barplot(las=2,table(data_all[,"Age_range_manual"]),border=F, col="coral1")
barplot(las=2,table(data_all[,"Age_vac"]),border=F, col="coral1")
barplot(las=2,table(data_all[,"Age_vac_2"]),border=F, col="coral1")
dev.off()
#Fix age
data_all[,"EDAD"] <- fix_age(data_all[,"EDAD"])
# Now, we'll be adding regions
# Now, we'll be adding regions
edos <- read.table("EstadoRegion_v5.tsv",header=T, sep='\t', skip=0, comment.char='',fill=FALSE, check.names=FALSE, stringsAsFactors = FALSE, row.names=1)
data_all <- cbind(data_all, edos[data_all[,"State"],]); rownames(data_all) <- NULL # Append region groupings

# Add Sex
data_all[data_all[,"SEXO"]==1,"SEXO"] <- "F"
data_all[data_all[,"SEXO"]=="2","SEXO"] <- "M" # The last line caused it to turn into a char vector
write.table(data_all, "All_Cases_addtional_columns.tsv", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
save.image("chkpt2.Rdata")
