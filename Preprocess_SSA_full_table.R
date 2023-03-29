# Updated 2022-11-30: Homogenized printable dates in the x axes for graphs (controlled with printable_dates()
# Updated 2022-10-02: Added new graphs for type of test and postitives/negatives, fixed positivity
# Updated 2022-09-26: Fixed several issues concerning axes
# Updated 2022-03-08: Fixed an exception when an hospital may have missing data in last 45 days
# Updated 2022-02-14: Added new analyses for last 45 days
# Updated 2022-01-30: Added two graphs for twitter with customized wave dates and ages totals
# Updated 2021-12-08: Updated to 2021-11-16 for delta article
# Updated 2021-08-20: Some other things were automatized
# Updated 2021-07-23: New analyses on groups by age
# Updated 2021-04-19: Changed it to avoid loading the whole table
# Started 2020-10-15 by Rodrigo García López at UNAM, México
# This script is intended to check the information regarding the COVID-19 cases in Mexico.
# The raw table can be downloaded from https://datos.gob.mx/busca/dataset/informacion-referente-a-casos-covid-19-en-mexico and consists of publicly-available cases reported by the Ministry of Health (Secretaría de Salud, SSA). 
# These include confirmed and negative cases.
# R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"

# Pre-filtering
# We are are currently not using cases that turn out to be negative (no COVID-19 or SARS-CoV-2 not confirmed).
# This is carried out from the actual raw table (220703COVID19MEXICO.csv, with 16650148 cases, 2.5GiB) in the current version (2022-07-05). This file is zipped (currently, 306.2 MiB in size) but there's no need to unzip it previously
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
date_vs_X <- function(mat,string,int) { # Input should have at least a "Date" column with format as %Y-%m-%d, a target vector of dates that should be present (passed as a string vector) and an integer for the days in the rolling average
	dateX <- twoway_table(mat[,c("FECHA_INGRESO",string)])
	dates <- seq(as.Date(rownames(dateX)[1]),as.Date(rownames(dateX)[nrow(dateX)]),by="day") # create the complete date range
	dateX <- fill_missing_items(dateX,dates) # use the predicted missing days to get the whole date spectrum (adding 0s when required)
	dateX <- rolling_N_avg(dateX,int) # smoothen with a N-day average
	return(dateX)
}
week_vs_X <- function(mat,string) { # Input should have at least a Week column, and a target vector of all items should be present (passed as a string vector)
	weekX <- twoway_table(mat[,c("Week",string)])
	weeks_present <- names(table(mat[,"Week"]))
	range <- c(weeks_present[1], weeks_present[length(weeks_present)]) # get first and last items
	allweeks <- c("19W52",paste0("20W",sprintf('%0.2d', 1:52)),paste0("21W",sprintf('%0.2d',1:52)),paste0("22W",sprintf('%0.2d',1:52)),paste0("23W",sprintf('%0.2d',1:52)))
	allweeks <- allweeks[grep(range[1],allweeks):grep(range[2],allweeks)]
	weekX <- fill_missing_items(weekX,allweeks) # use the predicted missing days to get the whole date spectrum (adding 0s when required)
	return(weekX)
}
define_plot_scheme <- function(){
	lty <- c("solid","dashed","dotted","dotdash","longdash","twodash")
	col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","purple3","magenta1","limegreen","darkorange2","darkgray")
	lwd <- c(1.3,2)
	pars <- expand.grid(col = col, lty = lty, lwd = lwd, stringsAsFactors = FALSE) # This will create all
	return(pars)
}
printable_dates_old <- function(vector){ # Gets a vector with ordered dates (fomat must have %d at the end), get the position where each month stats or at day 15 and adjust them accordingly for plotting
	dates <- unique(sort(c(grep("01$|15$",vector),length(vector),1)))
	if(length(dates)>20){dates <- unique(sort(c(grep("01$",vector),length(vector),1)))}
	if(length(dates)>20){if((dates[length(dates)]-dates[length(dates)-1])<21){dates <- dates[-(length(dates)-1)]}} # Fix dates showing
	if((dates[2]-dates[1])<10){dates <- dates[-2]}
	return(dates)
}
printable_dates <- function(vector){ # Gets a vector with ordered dates (fomat must have days [%d] at the end), get the position where each month stats or at day 15 and adjust them accordingly for plotting
	dates <- unique(sort(c(grep("01$|15$",vector),length(vector),1))) # by default, start by subsetting only start and middle dates for each month
	if(length(dates)>20){dates <- unique(sort(c(grep("01$",vector),length(vector),1)))} # Switch to months only if more than 20 items are present
	if(length(dates)>=32){ # If even more than 31 items are present
		sub <- dates[grep("01$",vector[dates])] # save the index of dates of month start
		dates <- unique(sort(c(1,sub[seq(1,length(sub),2)],length(vector)))) # now, keep only half of them
	}
	dif <- round(mean(diff(dates))/2) # get half the mean difference between dates (in days)
	if((dates[length(dates)] - dates[length(dates)-1]) < dif){dates <- dates[-(length(dates)-1)]} # Delete the second to last item if it is closer to the last ones than dif
	if((dates[2]-dates[1]) < dif){dates <- dates[-2]} # the same with the first item but remove the second one
	return(dates)
}

########################## Load Input ##########################
args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 2) { # at least, two arguments should be included: <min_nonNAs> <prefix_output>  <name_of_alpha_metric>
  stop("A minimum of 2 arguments are mandatory: cat table.tsv|Rscript PreProcess_DGE.R <prefix> <file>", call.=FALSE)
}
prefix <- as.character(args[1]) # Get a string handle to create output names
filename <- as.character(args[2])  # and the name of the metric (free strings)

# prefix="2023-02-21"
# filename="220221COVID19MEXICO.csv"
# filename="out.tsv"
ignored_days <- 8 # UPDATE 2021-08-20: This now changes the days that should be ignored from the dates
finalcut <- ignored_days-3
date <- prefix
entidades <- read.table("Entidades.tsv", sep="\t",header=T, skip=0, comment.char='',quote="",fill=F, row.names=1, check.names=F) #
sectores <- read.table("Sectores.tsv", sep="\t",header=T, skip=0, comment.char='',quote="",fill=F, row.names=1, check.names=F) #
# data_all <- read.table(unz("datos_abiertos_covid19.zip", "220123COVID19MEXICO.csv"), header=T, quote="\"", sep=",")
# data_all <- read.table(filename, header=T, quote="\"", sep=",")
data_all <- read.table(filename, header=T, quote="", sep="\t")
########################## MAIN ##########################
print("Todos los Casos (Confirmados + Negativos)")
all_cases <- nrow(data_all);all_cases
# [1] 18619683
all_dates <- as.Date(data_all[,11])
MaxDate <- max(all_dates)
MinDate <- MaxDate-45
last45days <- data_all[all_dates>=MinDate,]
print("Todos los Casos (Confirmados + Negativos) - Últimos 45 días")
nrow(last45days)
# [1] 206569
# all_dates <- as.Date(last45days[,11])
# last30days_conf <- last45days[all_dates<=(MaxDate-15),]

negativos <- data_all[data_all[,36]==7,] # In case it's required, we can extract the negative cases
print("Negativos - Todos")
nrow(negativos)
# [1] 10754932
print("Negativos - Todos - Por Género")
sex_n <- table(negativos[6]);names(sex_n) <- c("F","M"); sex_n
#       F       M
# 5872833 4882099
negativos <- last45days[last45days[,36]==7,]
print("Negativos - Últimos 45 días")
nrow(negativos)
# [1] 10754932
print("Negativos - Últimos 45 días - Por Género")
sex_n <- table(negativos[6]);names(sex_n) <- c("F","M"); sex_n
#       F       M
# 5872833 4882099

total_studied <- nrow(data_all)
total_studied_45_days <- nrow(last45days)
data <- data_all[data_all[36]<=3,] # Extract only confirmed cases (1=by association, 2=committee-determined, 3=lab-tested)
# Create an extra label for type of patient and whether its dead or not
data[data[10]==1,37] <- "Amb"
data[data[10]==2,37] <- "Hosp"
data[data[14]==1,37] <- "Int"
temp <- which(data[13]!="9999-99-99")
data[temp,37] <- paste(data[temp,37],"D")
all_dates <- as.Date(data[,11])
last45days <- data[all_dates>=MinDate,]
last45days <- last45days[last45days[36]<=3,]
total_confirmed <- nrow(data)
print("Confirmados - Todos")
nrow(data)
# [1] 7089276
print("Confirmados - Últimos 45 días")
nrow(last45days)
# [1] 18878
total_confirmed_sub <- nrow(last45days)
all_dates <- as.Date(last45days[,11])
last30days_conf <- last45days[all_dates<=(MaxDate-15),]
print("Confirmados - Últimos 30 días")
nrow(last30days_conf)
# [1] 12515

# UPDATE 2021-10-14: Determine days from symptoms inset to actual attention was requested
time_to_seek_help <- data.frame("Ingreso"=as.Date(data[,11]),"Sintomas"=as.Date(data[,12]))
time_to_seek_help <- cbind(time_to_seek_help,"days"=time_to_seek_help[,1]-time_to_seek_help[,2])
print("Tiempo promedio para buscar atención - Todos (Confirmados, en días)")
mean(time_to_seek_help[,3])
# Time difference of 3.029177 days
print("Tiempo promedio para buscar atención - Todos (Confirmados, desv. est. en días)")
sd(time_to_seek_help[,3])
# [1] 2.887015
time_to_seek_help_sub <- data.frame("Ingreso"=as.Date(last45days[,11]),"Sintomas"=as.Date(last45days[,12]))
time_to_seek_help_sub <- cbind(time_to_seek_help_sub,"days"=time_to_seek_help_sub[,1]-time_to_seek_help_sub[,2])
print("Tiempo promedio para buscar atención - Últimos 45 días (Confirmados, en días)")
mean(time_to_seek_help_sub[,3])
# Time difference of 1.97362 days
print("Tiempo promedio para buscar atención - Últimos 45 días (Confirmados, desv. est. en días)")
sd(time_to_seek_help_sub[,3])
# [1] 1.843579

print("Defunciones - Todo")
nrow(data[data[13]!="9999-99-99",]) # Get those that die
# [1] 329651
print("Defunciones - Últimos 45 días")
total_deaths_sub45 <- nrow(last45days[last45days[13]!="9999-99-99",]); total_deaths_sub45 # Get those that die
# [1] 85
print("Defunciones - Últimos 30 días")
total_deaths_sub30 <- nrow(last30days_conf[last30days_conf[13]!="9999-99-99",]); total_deaths_sub30 # Get those that die
# [1] 74


##########################################################################################
# UPDATE 2021-12-08: Extract a new table with confirmed cases including only the table's ID (2), registry dates (11), ages (16), States where they live (8), patient status (10) and if they died (13)
# # # exp <- data[,c(2,11,16,8,10,13)]
# # # # recode the actual fields
# # # exp[,4] <- entidades[exp[,4],1] # for State,
# # # exp[,5] <- ifelse(exp[,5]==1, "Amb","Hosp") # status
# # # exp[,6] <- ifelse(exp[,6]=="9999-99-99", "L","D") # and fatality (if that was the case)
# # # write.table(exp, "National_cases.tsv", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
##########################################################################################


# COUNTRIES
nation <- table(data[38]) # Nationality
print("Nacionalidades totales - Confirmados - Todos")
length(nation)
# [1] 161 (All countries in the table)
nation <- sort(nation[nation>0],decreasing=T)
print("Top 25 principales nacionalidades - Confirmados - Todos")
head(nation, 25)
#                    México Estados Unidos de América                 Venezuela
#                   7064309                      7258                      2981
#                  Colombia     República de Honduras                      Cuba
#                      1817                      1021                      1018
#                 Guatemala                 Nicaragua                 Argentina
#                       965                       793                       773
#               El Salvador                    España                      Otro
#                       700                       550                       529
#                      Perú                    Brasil                     China
#                       488                       466                       460
#                     Japón                   Ecuador                    Canadá
#                       416                       376                       372
#                  Alemania                   Francia                     Haití
#                       358                       345                       310
#                   Bolivia                     Chile                    Italia
#                       263                       258                       257
#                     India
#                       138
nation <- table(last45days[38]) # Nationality
print("Nacionalidades totales - Confirmados - Últimos 45 días")
length(nation)
# [1] 30 (All countries in the table)
nation <- sort(nation[nation>0],decreasing=T)
print("Top 25 principales nacionalidades - Confirmados - Últimos 45 días")
head(nation, 25)
#                    México                 Nicaragua Estados Unidos de América
#                     18736                        32                        31
#                  Colombia                     Japón                 Venezuela
#                        12                        10                         9
#                     China                   Francia     República de Honduras
#                         7                         4                         4
#                  Alemania                      Cuba                 Guatemala
#                         3                         3                         3
#                      Otro                 Argentina                    Brasil
#                         3                         2                         2
#                   Ecuador               El Salvador                 Australia
#                         2                         2                         1
#                   Bolivia      Bosnia y Herzegovina                    Canadá
#                         1                         1                         1
#                     Chile                   Estonia                 Filipinas
#                         1                         1                         1
#                    Italia
#                         1


print("Géneros - Confirmados - Todos - Confirmados")
sex_c <- table(data[6]);names(sex_c) <- c("F","M");sex_c/total_confirmed*100
#        F        M
# 53.20312 46.79688

# print("Géneros - Confirmados - Últimos 45 días")
sex_c <- table(last45days[6]);names(sex_c) <- c("F","M");sex_c/total_confirmed_sub*100
#        F        M
# 57.80803 42.19197

### DEPRECATED - START
# pie(sex_c)
i=8;st <- sort(table(data[i]),decreasing=T); names(st) <- entidades[names(st),1];# Use this to test any graphics regarding states (col7=State where born; col8=State where lived)
pdf(paste0(date,"-states.pdf"),height=10)
par(mar=c(8,5,5,0),mfrow = c(2,1))
barplot(st,cex.axis=0.75,las=2,ylim=c(0,max(st)),ylab="Casos confirmados", col="cornflowerblue", border=NA, yaxt="n")
axis(2,las=1, at=seq(0,7e6,2e5),cex.axis=0.75)
deaths <- data[data[13]!="9999-99-99",]
i=8;st2 <- sort(table(deaths[i]),decreasing=T); names(st2) <- entidades[names(st2),1]
st2 <- st2[names(st)]
barplot(st2,cex.axis=0.75,las=2,ylim=c(0,max(st2)),ylab="Defunciones", col="violetred", border=NA, yaxt="n")
axis(2,las=1, at=seq(0,7e5,0.5e4),cex.axis=0.75)
dev.off()

### DEPRECATED - END
# All available categories (as of oct 2020)
# Nº	NOMBRE DE VARIABLE
# 1	FECHA_ACTUALIZACION
# 2	ID_REGISTRO
# 3	ORIGEN
# 4	SECTOR
# 5	ENTIDAD_UM
# 6	SEXO
# 7	ENTIDAD_NAC
# 8	ENTIDAD_RES
# 9	MUNICIPIO_RES
# 10	TIPO_PACIENTE
# 11	FECHA_INGRESO
# 12	FECHA_SINTOMAS
# 13	FECHA_DEF
# 14	INTUBADO
# 15	NEUMONIA
# 16	EDAD
# 17	NACIONALIDAD
# 18	EMBARAZO
# 19	HABLA_LENGUA_INDIG
# 20	INDIGENA
# 21	DIABETES
# 22	EPOC
# 23	ASMA
# 24	INMUSUPR
# 25	HIPERTENSION
# 26	OTRAS_COM
# 27	CARDIOVASCULAR
# 28	OBESIDAD
# 29	RENAL_CRONICA
# 30	TABAQUISMO
# 31	OTRO_CASO
# 32	TOMA_MUESTRA_LAB
# 33	RESULTADO_LAB
# 34	TOMA_MUESTRA_ANTIGENO
# 35	RESULTADO_ANTIGENO
# 36	CLASIFICACION_FINAL
# 37	MIGRANTE
# 38	PAIS_NACIONALIDAD
# 39	PAIS_ORIGEN
# 40	UCI

# General search:
# i=10;test <- table(data[i]);test <- test[test>0];length(test) # test how many different categories there re
# i=10;test <- table(data[i]);test*100/total_confirmed # same, with %

# i=37;test <- table(data[i]);test <- test[test>0];length(test) # test how many different categories there are
print("Tipo de paciente - Todos")
i=10;test <- table(data[i]);names(test) <- c("Amb","Hosp");test*100/total_confirmed # same, with %
#       Amb      Hosp
# 90.010419  9.989581
print("Tipo de paciente - Todos - Desglose:")
hosp <- data[data[10]==2,] # Extract only hospitalized patients
amb <- data[data[10]==1,] # and ambulatory patients
print("Totales Ambulatorios y Hospitalizados")
total_amb <- nrow(amb);total_amb
# [1] 6381087
total_hosp <- nrow(hosp);total_hosp
# [1] 708189
c("Amb"=total_amb,"Hosp"=total_hosp)
#     Amb    Hosp
# 6381087  708189
# Get those that die out of the subgroups
print("Porcentaje Fallecidos")
c("Amb(F)"=nrow(amb[amb[13]!="9999-99-99",])/total_amb*100, "Hosp(F)"=nrow(hosp[hosp[13]!="9999-99-99",])/total_hosp*100)
#    Amb(F)   Hosp(F)
#  0.218223 44.582167
print("Hospitalizados - Todos - Desglose")
i=14;test <- table(hosp[i]);names(test) <- c("Int","No_Int","?");test*100/total_hosp
#        Int     No_Int          ?
# 11.9326903 87.2710533  0.7962564
int <- hosp[hosp[14]==1,] # Extract only intubated patients
no_int <- hosp[hosp[14]==2,] # non-intubated
total_int <- nrow(int)
total_noint <- nrow(no_int)
print("Desglose Porcentaje hospitalizados defunciones - Todos")
c("Int (F)"=nrow(int[int[13]!="9999-99-99",])*100/total_int, "No Int (F)"=nrow(no_int[no_int[13]!="9999-99-99",])*100/total_noint) # % of intubated patients that die
#    Int (F) No Int (F)
#   83.78932   39.13184
surv_int <- sum(int[13]=="9999-99-99") # get the total ventilation-requiring patients that didn't died
sect_int <- table(int[4]) # Get the sector (type of hospital)
names(sect_int) <- sectores[names(sect_int),1]
int_d <- int[int[13]!="9999-99-99",] # Extract the total dead ventilation-requiring patients
sect_int_d <- table(int_d[4]) # Same thing but for different hospitals
names(sect_int_d) <- sectores[names(sect_int_d),1]
sect_hosp <- table(hosp[4])
names(sect_hosp) <- sectores[names(sect_hosp),1]
sect_hosp_d <- table(hosp[hosp[13]!="9999-99-99",][,4])
names(sect_hosp_d) <- sectores[names(sect_hosp_d),1]
sect_hosp_p <- sect_hosp_d/sect_hosp*100
# # # names(sect_hosp_p) <- sectores[names(sect_hosp_p),1]
surv_int <- sum(int[13]=="9999-99-99") # get the total ventilation-requiring patients that didn't died
sect_int <- table(int[4]) # Get the sector (type of hospital)
names(sect_int) <- sectores[names(sect_int),1]
int_d <- int[int[13]!="9999-99-99",] # Extract the total dead ventilation-requiring patients
sect_int_d <- table(int_d[4]) # Same thing but for different hospitals
names(sect_int_d) <- sectores[names(sect_int_d),1]
sect_hosp <- table(hosp[4])
sect_hosp_d <- table(hosp[hosp[13]!="9999-99-99",][,4])
names(sect_hosp_d) <- sectores[names(sect_hosp_d),1]
names(sect_hosp) <- sectores[names(sect_hosp),1]
temp_names <- unique(names(sect_hosp_d),names(sect_hosp))
sect_hosp_p <- sect_hosp_d[temp_names]/sect_hosp[temp_names]*100
names(sect_hosp_p) <- temp_names

# UPDATE 2021-10-14: Get total days that each type of patient took to search for help
print("Días promedio en buscar atención - Amb")
mean(time_to_seek_help[data[10]==1,3]) # ambulatory
# # # # Time difference of 2.795213 days
print("Días promedio en buscar atención - Hosp")
mean(time_to_seek_help[data[10]==2,3]) # hospitalized
# # # # Time difference of 5.137287 days
print("Días promedio en buscar atención - Def")
mean(time_to_seek_help[hosp[14]==2,3]) # intubated
# Time difference of 3.026464 days

pdf(paste0(date,"-sector_hosp.pdf"))
par(las=2)
barplot(rbind(log10(sect_hosp),log10(sect_hosp_d)),beside=T,cex.names = 0.5, main=paste0("Pacientes Hospitalizados y sus Defunciones (log10) a ", date), col=c("coral", "cornflowerblue"),border=c("coral", "cornflowerblue"), ylab="Totales log10")
# # axis(2, at=seq(0,32000,2000))
legend("top", col=c("coral", "cornflowerblue"), pch=15, legend=c("Hosp","Hosp fallecidos"))
dev.off()
pdf(paste0(date,"-sector_hosp_p.pdf"))
par(las=2)
barplot(sect_hosp_p,beside=T,cex.names = 0.5, main=paste0("Hospitalizados que fallecen por sector a ", date), col=c("coral"),border=c("coral"), ylab="Porcentaje de fallecidos")
# # axis(2, at=seq(0,32000,2000))
dev.off()

pdf(paste0(date,"-sector_defunct.pdf"))
par(las=2)
barplot(rbind(sect_int,sect_int_d),beside=T,cex.names = 0.5, main=paste0("Pacientes Intubados y sus Defunciones a ", date), col=c("coral", "cornflowerblue"),border=c("coral", "cornflowerblue"))
# axis(2, at=seq(0,32000,2000))
legend("top", col=c("coral", "cornflowerblue"), pch=15, legend=c("Intubados","Intubados fallecidos"))
dev.off()

# write.table(rbind(sect_hosp, sect_hosp_d), paste0(date,"-sector_hosp.tsv"), row.names=TRUE, col.names=NA)
# write.table(rbind(sect_int,sect_int_d), paste0(date,"-sector_defunct.tsv"), row.names=TRUE, col.names=NA)

library("plotrix")
# library("RColorBrewer")
# total_confirmed
t1 <- table(data[10])
names(t1) <- c("Ambulatorio", "Hospitalizado")
# t1 <- table(data[10])
t2 <- table(hosp[14])
names(t2) <- c("Intubado","No intubado","Se desconoce")
t2 <- c(t1[1],t2)
pdf(paste0(date,"-intubados_desglose.pdf"))
pie3D(total_confirmed,col="darkslategray", explode=0, theta=1.4,shade=1,mar=c(4,4,4,4), main=paste0("Confirmados (",total_confirmed,")"),labels=c("Confirmados"))
r=round(t1*100/total_confirmed,2)
pie3D(t1, col=c("chartreuse3","firebrick"), explode=0.1, theta=1.4,shade=1,mar=c(4,4,4,4), main="Tipo_de_Paciente",labels=c(paste0("Ambulatorio\n",r[1],"%"),paste0("Hospitalizados\n",r[2],"%")))
r=round(t2*100/total_confirmed,2)
pie3D(t2, col=c("chartreuse3","firebrick","cornflowerblue","gray"), explode=0.1, theta=1.4,shade=1,mar=c(4,4,4,4), main="Requerimiento de intubación",labels=c(paste0("Ambulatorio\n",r[1],"%"),paste0("Intubados\n",r[2],"%"),paste0("No intubados\n",r[3],"%"),paste0("Se desconoce\n",r[4],"%")))
t3 <- c(t1[1],"Viven"=nrow(int)-nrow(int_d),"Difuntos"=nrow(int_d),t2[3:4])
r=round(t3*100/total_confirmed,2)
pie3D(t3, col=c("chartreuse3","darkorange2","firebrick","cornflowerblue","gray"), explode=0.1, theta=1.4,shade=1,mar=c(4,4,4,4), main="Pacientes Intubados Difuntos",labels=c(paste0("Ambulatorio\n",r[1],"%"),paste0("Int.sobrevive\n",r[2],"%"),paste0("Int. Fallece\n",r[3],"%"),paste0("No intubado\n",r[4],"%"),paste0("Se desconoce\n",r[5],"%")))
dev.off()

# Assess total deaths per subset
# total_confirmed # Total confirmed
total_confirmed_d <- nrow(data[data[,13]!="9999-99-99",]) # Confirmed that died
total_amb <- nrow(amb) # Total ambulatory
total_amb_d <- nrow(amb[amb[,13]!="9999-99-99",])
total_hosp <- nrow(hosp)
total_hosp_d <- nrow(hosp[hosp[,13]!="9999-99-99",])
total_int <- t2[2]
total_int_d <- t3[3]
temp <- cbind(rbind(total_confirmed, total_amb, total_hosp, total_int),rbind(total_confirmed_d, total_amb_d,total_hosp_d,total_int_d))
colnames(temp) <- c("Totales", "Fallecen")
rownames(temp) <- c("Confirmados", "Ambulatorios", "Hospitalizados", "Intubados")
write.table(temp, paste0(date,"-tipo_paciente.tsv"), row.names=TRUE, col.names=NA)
sub <- as.data.frame(matrix(c(total_confirmed, total_confirmed_d, total_amb, total_amb_d, total_hosp, total_hosp_d,total_int, total_int_d),nrow=2, ncol=4))
colnames(sub) <- c("Confirmados","Ambulatorios","Hospitalizados","Intubados")
rownames(sub) <- c("Total","Difuntos")
sub <- rbind(sub, "No difuntos"=sub[1,]-sub[2,])
pdf(paste0(date,"-difuntos_desglose.pdf"))
par(oma=c(0,2,0,0))
barplot(as.matrix(sub[3:2,]),las=1,col=c("cornflowerblue","gray30"),border=NA,space=1.5, main="Defunciones totales por grupo", yaxt='n')
axis(2, las=1, at=seq(0,10e6,0.5e6), labels=format(seq(0,10e6,0.5e6),scientific=F))
legend("topright",legend=c("Defunciones","Supervivientes"),pch=15, col=c("gray30","cornflowerblue"))
dev.off()

### Same, with Últimos 45 días
print("Tipo de paciente - Últimos 45 días")
i=10;test <- table(last45days[i]);names(test) <- c("Amb","Hosp");test*100/total_confirmed_sub # same, with %
#       Amb      Hosp
# 95.682805  4.317195
print("Tipo de paciente - Últimos 45 días - Desglose:")
hosp <- last45days[last45days[10]==2,] # Extract only hospitalized patients
amb <- last45days[last45days[10]==1,] # and ambulatory patients
print("Totales Ambulatorios y Hospitalizados")
total_amb <- nrow(amb)
total_hosp <- nrow(hosp)
c("Amb"=total_amb,"Hosp"=total_hosp)
#   Amb  Hosp
# 18063   815
# Get those that die out of the subgroups
print("Porcentaje Fallecidos")
c("Amb(F)"=nrow(amb[amb[13]!="9999-99-99",])/total_amb*100, "Hosp(F)"=nrow(hosp[hosp[13]!="9999-99-99",])/total_hosp*100)
#      Amb(F)     Hosp(F)
#  0.01107236 10.18404908
print("Hospitalizados - Últimos 45 días - Desglose")
i=14;test <- table(hosp[i]);names(test) <- c("Int","No_Int");test*100/total_hosp
#       Int    No_Int
#  5.398773 94.601227

int <- hosp[hosp[14]==1,] # Extract only intubated patients
no_int <- hosp[hosp[14]==2,] # non-intubated
total_int <- nrow(int)
total_noint <- nrow(no_int)
print("Desglose Porcentaje hospitalizados defunciones - Últimos 45 días")
c("Int (F)"=nrow(int[int[13]!="9999-99-99",])*100/total_int, "No Int (F)"=nrow(no_int[no_int[13]!="9999-99-99",])*100/total_noint) # % of intubated patients that die
#    Int (F) No Int (F)
#  34.090909   8.819715
surv_int <- sum(int[13]=="9999-99-99") # get the total ventilation-requiring patients that didn't died
sect_int <- table(int[4]) # Get the sector (type of hospital)
names(sect_int) <- sectores[names(sect_int),1]
int_d <- int[int[13]!="9999-99-99",] # Extract the total dead ventilation-requiring patients
sect_int_d <- table(int_d[4]) # Same thing but for different hospitals
names(sect_int_d) <- sectores[names(sect_int_d),1]
sect_hosp <- table(hosp[4])
sect_hosp_d <- table(hosp[hosp[13]!="9999-99-99",][,4])
names(sect_hosp_d) <- sectores[names(sect_hosp_d),1]
names(sect_hosp) <- sectores[names(sect_hosp),1]
sect_hosp <- sect_hosp[sect_hosp>0]
both <- unique(c(names(sect_hosp),names(sect_hosp_d))) # 2022-03-08: Fixed this for 45 days, as not all items may be present and match between hospitalized and deceased patients
sect_hosp <- sect_hosp[both]
sect_hosp_d <- sect_hosp_d[both]
sect_hosp_d[is.na(sect_hosp_d)] <- 0
sect_hosp_p <- sect_hosp_d/sect_hosp*100
names(sect_hosp_p) <- names(sect_hosp)
print("Días promedio en buscar atención - Amb")
mean(time_to_seek_help[last45days[10]==1,3]) # ambulatory
# # # # Time difference of 3.029309 days
print("Días promedio en buscar atención - Hosp")
mean(time_to_seek_help[last45days[10]==2,3]) # hospitalized
# # # # Time difference of 3.026242 days
print("Días promedio en buscar atención - Def")
mean(time_to_seek_help[last45days[14]==2,3]) # intubated
# Time difference of 3.028205 days

sh <- log10(sect_hosp); sh[is.infinite(sh)] <- 0
shp <- log10(sect_hosp_d); shp[is.infinite(shp)] <- 0
names(shp) <- names(sh)

pdf(paste0(date,"-sector_hosp_45dias.pdf"))
par(las=2)
barplot(rbind(sh,shp),beside=T,cex.names = 0.5, main=paste0("Pacientes Hospitalizados y sus Defunciones (log10) a ", date), col=c("coral", "cornflowerblue"),border=c("coral", "cornflowerblue"), ylab="Totales log10")
mtext(las=1,"Últimos 45 días")
# # axis(2, at=seq(0,32000,2000))
legend("top", col=c("coral", "cornflowerblue"), pch=15, legend=c("Hosp","Hosp fallecidos"))
dev.off()
pdf(paste0(date,"-sector_hosp_p_45dias.pdf"))
par(las=2)
barplot(sect_hosp_p,beside=T,cex.names = 0.5, main=paste0("Hospitalizados que fallecen por sector a ", date), col=c("coral"),border=c("coral"), ylab="Porcentaje de fallecidos")
mtext(las=1,"Últimos 45 días")
# # axis(2, at=seq(0,32000,2000))
dev.off()

all_names <- unique(names(sect_int),names(sect_int_d))
temp <- cbind(sect_int[all_names],sect_int_d[all_names])
pdf(paste0(date,"-sector_defunct_45dias.pdf"))
par(las=2)
barplot(t(cbind(sect_int[all_names],sect_int_d[all_names])),beside=T,cex.names = 0.5, main=paste0("Pacientes Intubados y sus Defunciones a ", date), col=c("coral", "cornflowerblue"),border=c("coral", "cornflowerblue"))
mtext(las=1,"Últimos 45 días")
# axis(2, at=seq(0,32000,2000))
legend("top", col=c("coral", "cornflowerblue"), pch=15, legend=c("Intubados","Intubados que fallecieron"))
dev.off()

library("plotrix")
# library("RColorBrewer")
# nrow(last45days)
confirmed45 <- nrow(last45days)
t1 <- table(last45days[10])
names(t1) <- c("Ambulatorio", "Hospitalizado")
t2 <- table(hosp[14])
dict_int <- c("Intubado","No intubado","Se desconoce") # UPDATE 2022-10-27: This patch fixes the case where there is an empty category
names(dict_int) <- 1:length(dict_int)
t2 <- t2[names(dict_int)]
t2[is.na(names(t2))] <- 0.001
names(t2) <- dict_int
# t2 <- t2[dict_int]
t2 <- c(t1[1],t2)
pdf(paste0(date,"-intubados_desglose_45dias.pdf"))
pie3D(confirmed45,col="darkslategray", explode=0, theta=1.4,shade=1,mar=c(4,4,4,4), main=paste0("Confirmados (",confirmed45,")"),labels=c("Confirmados"))
r=round(t1*100/confirmed45,2)
pie3D(t1, col=c("chartreuse3","firebrick"), explode=0.1, theta=1.4,shade=1,mar=c(4,4,4,4), main="Tipo_de_Paciente",labels=c(paste0("Ambulatorio\n",r[1],"%"),paste0("Hospitalizados\n",r[2],"%")))
r=round(t2*100/confirmed45,2)
pie3D(t2, col=c("chartreuse3","firebrick","cornflowerblue","gray"), explode=0.1, theta=1.4,shade=1,mar=c(4,4,4,4), main="Requerimiento de intubación",labels=c(paste0("Ambulatorio\n",r[1],"%"),paste0("Intubados\n",r[2],"%"),paste0("No intubados\n",r[3],"%"),paste0("Se desconoce\n",r[4],"%")))
t3 <- c(t1[1],"Viven"=nrow(int)-nrow(int_d),"Difuntos"=nrow(int_d),t2[3:4])
r=round(t3*100/confirmed45,2)
pie3D(t3, col=c("chartreuse3","darkorange2","firebrick","cornflowerblue","gray"), explode=0.1, theta=1.4,shade=1,mar=c(4,4,4,4), main="Pacientes Intubados Difuntos",labels=c(paste0("Ambulatorio\n",r[1],"%"),paste0("Int.sobrevive\n",r[2],"%"),paste0("Int. Fallece\n",r[3],"%"),paste0("No intubado\n",r[4],"%"),paste0("Se desconoce\n",r[5],"%")))
dev.off()

# Assess total deaths per subset
# confirmed45 # Total confirmed
total_confirmed_d <- nrow(last45days[last45days[,13]!="9999-99-99",]) # Confirmed that died
total_amb <- nrow(amb) # Total ambulatory
total_amb_d <- nrow(amb[amb[,13]!="9999-99-99",])
total_hosp <- nrow(hosp)
total_hosp_d <- nrow(hosp[hosp[,13]!="9999-99-99",])
total_int <- t2[2]
total_int_d <- t3[3]
sub <- as.data.frame(matrix(c(nrow(last45days), total_confirmed_d, total_amb, total_amb_d, total_hosp, total_hosp_d,total_int, total_int_d),nrow=2, ncol=4))
colnames(sub) <- c("Confirmados","Ambulatorios","Hospitalizados","Intubados")
rownames(sub) <- c("Total","Difuntos")
sub <- rbind(sub, "No difuntos"=sub[1,]-sub[2,])
pdf(paste0(date,"-difuntos_desglose_45dias.pdf"))
barplot(as.matrix(sub[3:2,]),las=1,col=c("cornflowerblue","gray30"),border=NA,space=1.5, main="Defunciones totales por grupo")
legend("topright",legend=c("Defunciones","Supervivientes"),pch=15, col=c("gray30","cornflowerblue"))
dev.off()

# reset totals
total_confirmed <- nrow(data) # Total confirmed
total_confirmed_d <- nrow(data[data[,13]!="9999-99-99",]) # Confirmed that died
amb <- data[data[10]==1,] # and ambulatory patients
total_amb <- nrow(amb) # Total ambulatory
total_amb_d <- nrow(amb[amb[,13]!="9999-99-99",])
hosp <- data[data[10]==2,] # Extract only hospitalized patients
total_hosp <- nrow(hosp)
total_hosp_d <- nrow(hosp[hosp[,13]!="9999-99-99",])
t1 <- table(data[10])
names(t1) <- c("Ambulatorio", "Hospitalizado")
t2 <- table(hosp[14])
names(t2) <- c("Intubado","No intubado","Se desconoce")
t2 <- c(t1[1],t2)
total_int <- t2[2]
int <- hosp[hosp[14]==1,] # Extract only intubated patients
total_int_d <- nrow(int[int[,13]!="9999-99-99",])

######################## Tipo Prueba ########################
# UPDATE 2022-09-30: We now want to extract the actual total negative to positive totals
library(areaplot)
data[,"Tests"] <- data[,36] # First, create an additional column holding the general type of positive
data[data[,35]==1, "Tests"] <- paste0(data[data[,35]==1, "Tests"],4) # if antigen was positive, then mark t as so
data[data[,33]==1, "Tests"] <- paste0(data[data[,33]==1, "Tests"],5) # if PCR (laboratory) was positive, mark it instead (if both tests were performed, we will mark them as such)
testtype <- xtabs(rep(1,nrow(data))~data[,11]+data[,"Tests"], data=data)
colnames(testtype) <- c("Asoc","Exp","Ant","PCR+Ant","PCR")
write.table(testtype, paste0("Tipo_de_test_",gsub("-","",prefix),".tsv"), sep="\t", quote=FALSE, row.names=TRUE, col.names=NA)
dates <- seq(as.Date(rownames(testtype)[1]),as.Date(rownames(testtype)[nrow(testtype)]),by="day") # get a vector with all continuous dates
testtype <- fill_missing_items(testtype,dates) # to fill missing items
roll <- rolling_N_avg(testtype,7) # now, create a rolling average
dates <- printable_dates(dates) # and recycle the dates object to hold printable dates
pdf(paste0(prefix,"-TypeOfTest.pdf"))
# par(mar=c(5,8,5,5))
areaplot(las=2, roll[,ncol(roll):1],col=c("cornflowerblue","violetred","coral1", "forestgreen", "turquoise"), border=NA, xaxt="n", xlab="", yaxt="n", main="Casos positivos por tipo de prueba confirmatoria", ylab="Casos totales")
mtext("Promedio semanal móvil")
axis(1, las=2, at = dates, labels = rownames(testtype)[dates], cex.axis = 0.75)
axis(2, las=1, at=seq(0,1e5,1e4), labels=format(seq(0,1e5,1e4), scientific=FALSE), cex.axis=0.85)
legend("topleft", legend=rev(paste0(colnames(testtype)," (",colSums(testtype),")")), pch=15, col=c("cornflowerblue","violetred","coral1", "forestgreen", "turquoise"))
dev.off()

# same, but just show the initial part
# maxDate <- grep("2021-05-01",rownames(roll))
# roll <- roll[1:maxDate,]
# pdf(paste0(prefix,"-TypeOfTest_zoom.pdf"))
# # par(mar=c(5,8,5,5))
# areaplot(las=2, roll[,ncol(roll):1],col=c("cornflowerblue","violetred","coral1", "forestgreen", "turquoise"), border=NA, xaxt="n", xlab="", yaxt="n", main="Casos positivos por tipo de prueba confirmatoria", ylab="Casos totales")
# mtext("Promedio semanal móvil")
# axis(1, las=2, at = dates, labels = rownames(testtype)[dates], cex.axis = 0.75)
# axis(2, las=1, at=seq(0,1e5,1e3), labels=format(seq(0,1e5,1e3), scientific=FALSE), cex.axis=0.85)
# legend("topleft", legend=rev(paste0(colnames(testtype)," (",colSums(testtype),")")), pch=15, col=c("cornflowerblue","violetred","coral1", "forestgreen", "turquoise"))
# dev.off()

# same, but just show the last 30 days
roll <- roll[(nrow(roll)-29):nrow(roll),]
pdf(paste0(prefix,"-TypeOfTest_last30days.pdf"))
# par(mar=c(5,8,5,5))
areaplot(las=2, roll[,ncol(roll):1],col=c("cornflowerblue","violetred","coral1", "forestgreen", "turquoise"), border=NA, xaxt="n", xlab="", main="Casos positivos por tipo de prueba confirmatoria", ylab="Casos totales")
mtext("Promedio semanal móvil")
dates <- rownames(roll)
axis(1, las=2, at = seq(1,30,2), labels = dates[seq(1,30,2)], cex.axis = 0.75)
# axis(2, las=1, at=seq(0,1e5,1e3), labels=format(seq(0,1e5,1e3), scientific=FALSE), cex.axis=0.85)
legend("topleft", legend=rev(paste0(colnames(testtype)," (",colSums(testtype),")")), pch=15, col=c("cornflowerblue","violetred","coral1", "forestgreen", "turquoise"))
dev.off()
######################## Positivity ########################
pos <- xtabs(rep(1,nrow(data_all))~data_all[,11]+data_all[,36], data=data_all)
# pos <- cbind(rowSums(pos[,1:3]),rowSums(pos[,4:ncol(pos)]))
pos <- cbind(rowSums(pos[,1:3]),pos[,7])
colnames(pos) <- c("Pos","Neg")
# UPDATE 2022-09-30: We now want to extract the actual total negative to positive totals
dates <- seq(as.Date(rownames(pos)[1]),as.Date(rownames(pos)[nrow(pos)]),by="day") # get a vector with all continuous dates
pos <- fill_missing_items(pos,dates) # to fill missing items
roll <- rolling_N_avg(pos,7) # now, create a rolling average
dates <- printable_dates(dates) # and recycle the dates object to hold printable dates
pdf(paste0(prefix,"-Neg_pos_cases.pdf"))
areaplot(las=2, roll,col=c("cornflowerblue","coral1"), border=NA, xaxt="n", xlab="", yaxt="n", main="Casos Positivos y Negativos")
mtext("Promedio semanal móvil")
axis(1, las=2, at = dates, labels = rownames(pos)[dates], cex.axis = 0.75)
axis(2, las=1, at=seq(0,1e5,1e4), labels=format(seq(0,1e5,1e4), scientific=FALSE))
legend("topleft", legend=c("Casos Negativos", "Casos Positivos"), pch=15, col=rev(c("cornflowerblue","coral1")))
dev.off()
# End of UPDATE
pos_r <- pos/rowSums(pos)*100
roll.pos <- sapply(4:(nrow(pos_r)-3),function(y) {apply(pos_r,2, function(x) mean(as.numeric(x[(y-3):(y+3)])))})
colnames(roll.pos) <- row.names(pos_r)[4:(nrow(pos_r)-3)]
roll.pos <- t(roll.pos[,1:(ncol(roll.pos)-5)])

# dates <- unique(sort(c(grep("01$",rownames(roll.pos)),grep("15$",rownames(roll.pos)),nrow(roll.pos),1)))
# dates <- unique(sort(c(grep("01$",rownames(roll.pos)),nrow(roll.pos),1)))
# if((dates[length(dates)]-dates[length(dates)-1])<15){dates <- dates[-(length(dates)-1)]} # Fix dates showing
# if((dates[2]-dates[1])<10){dates <- dates[-2]}
dates <- rownames(roll.pos) # UPDATE 2022-11-30: there is no more space for this to fit, we have thus changed the actual printing scheme for dates
dates <- printable_dates(dates)
pdf(paste0(prefix,"-positividad.pdf"))
plot(roll.pos[,1],type='l',col="cornflowerblue", lwd=2, main="Positividad (% de pruebas positivas)",las=2,xaxt='n',xlab=NA,ylab="Porcentaje de pruebas positivas")
mtext("Promedio móvil de 7 días")
axis(1, las=2, at = dates, labels = rownames(roll.pos)[dates], cex.axis = 0.75)
dev.off()

# UPDATE 2023-02-24: Additional zoom for last 30 days
subpos <- tail(roll.pos[,1],30)
pdf(paste0(prefix,"-positividad_last30days.pdf"))
plot(subpos,type='l',col="cornflowerblue", lwd=2, main="Positividad (% de pruebas positivas) - 30 días",las=2,xaxt='n',xlab=NA,ylab="Porcentaje de pruebas positivas")
mtext("Promedio móvil de 7 días")
axis(1, las=2, at = seq(1,30,2), labels = names(subpos)[seq(1,30,2)], cex.axis = 0.75)
dev.off()

# Now, per state
data_all[,"PosNeg_State"] <- data_all[,36]
data_all[data_all[,"PosNeg_State"]<=3, "PosNeg_State"] <- "Pos" # Flag positive and negative cases
data_all[data_all[,"PosNeg_State"]==7, "PosNeg_State"] <- "Neg"
data_all[,"PosNeg_State"] <- paste(data_all[,"PosNeg_State"], data_all[,8], sep="_") # and append the state
pos <- xtabs(rep(1,nrow(data_all))~data_all[,11]+data_all[,"PosNeg_State"], data=data_all) # match both info columns
pos <- pos[,grep("Pos|Neg",colnames(pos))] # and keep only positives/negatives
dates <- seq(as.Date(rownames(pos)[1]),as.Date(rownames(pos)[nrow(pos)]),by="day")
pos <- fill_missing_items(pos,dates) # to fill missing items
# entidades[as.numeric(sub(".*_","",colnames(pos))),1]
colnames(pos) <- paste(entidades[as.numeric(sub(".*_","",colnames(pos))),1],sub("_.*","",colnames(pos)),sep="_")
positivity  <- matrix(0,ncol=32,nrow=nrow(pos)) # Create a void matrix for output container
colnames(positivity) <- entidades[1:32,1]; rownames(positivity) <- rownames(pos)
for( state in entidades[1:32,1]){
# 	print(state)
	positivity[,state] <- pos[,paste0(state,"_Pos")]*100/(pos[,paste0(state,"_Pos")]+pos[,paste0(state,"_Neg")])
}
positivity[is.nan(positivity)] <- 0
roll <- rolling_N_avg(positivity,7) # now, create a rolling average
roll <- roll[-nrow(roll),] # Remove last item
roll <- roll[grep("2020-02-27",rownames(roll)):nrow(roll),] # Remove first days until first case was officially registered


dates <- rownames(roll)# get a vector with all continuous dates
dates <- printable_dates_old(dates)
lty <- c("solid","dashed","dotted","dotdash","longdash","twodash")
col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'gold1','slategray2',"black","burlywood4","aquamarine2","blue2","violetred2","palegreen1","wheat1","magenta1","limegreen","darkorange2","darkgray")
lwd <- c(1.5,2)
cex.X=0.53
pars <- expand.grid(col = col, lty = lty, lwd = lwd, stringsAsFactors = FALSE)
pdf(paste0(prefix,"-positividad_por_estado.pdf"),width=14)
par(oma = c(1, 1, 1, 6)) #This is just a most imaginative fix to plot the legend outside, i
matplot(las=2,roll, type='l', col=pars[,1], lty=pars[,2], xaxt="n", ylab="Porcentaje de pruebas positivas", main="Positividad por Estado")
mtext("Promedio móvil de 7 días")
axis(1, las=2, at = dates, labels = rownames(roll)[dates], cex.axis = 0.60)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
legend("right", legend = gsub("\"","",colnames(roll)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(roll)], lty = pars$lty[1:ncol(roll)], lwd = 1, cex = cex.X)
dev.off()
write.table(roll, paste0("Positividad_Estados_",gsub("-","",prefix),".tsv"), sep="\t", quote=FALSE, row.names=TRUE, col.names=NA)

roll <- roll[(nrow(roll)-149):nrow(roll),]
pdf(paste0(prefix,"-positividad_por_estado_150dias.pdf"),width=7)
dates <- rownames(roll)# get a vector with all continuous dates
dates <- printable_dates(dates)
if((dates[length(dates)]-dates[length(dates)-1]) < 10){dates <- dates[-(length(dates)-1)]}
par(oma = c(1, 1, 1, 6)) #This is just a most imaginative fix to plot the legend outside, i
matplot(las=2,roll, type='l', col=pars[,1], lty=pars[,2], xaxt="n", ylab="Porcentaje de pruebas positivas", main="Positividad por Estado - Últimos 150 días")
mtext("Promedio móvil de 7 días")
axis(1, las=2, at = dates, labels = rownames(roll)[dates], cex.axis = 0.60)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
legend("right", legend = gsub("\"","",colnames(roll)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(roll)], lty = pars$lty[1:ncol(roll)], lwd = 1, cex = cex.X)
dev.off()
# Use this for states by regions
state_pos <- function(string){
	ori_par <- par()
	dates <- rownames(sub)# get a vector with all continuous dates
	dates <- printable_dates(dates)
	if((dates[length(dates)]-dates[length(dates)-1]) < 10){dates <- dates[-(length(dates)-1)]}
	par(oma = c(1, 1, 1, 6)) #This is just a most imaginative fix to plot the legend outside, i
	matplot(las=2,sub, type='l', col=pars[,1], lty=pars[,2], xaxt="n", ylab="Porcentaje de pruebas positivas", main=paste0("Positividad - Últimos 150 días -",string))
	mtext("Promedio móvil de 7 días")
	axis(1, las=2, at = dates, labels = rownames(sub)[dates], cex.axis = 0.60)
	par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
	legend("right", legend = gsub("\"","",colnames(sub)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(sub)], lty = pars$lty[1:ncol(sub)], lwd = 1, cex = cex.X)
	par(oma=ori_par$oma, mar=ori_par$mar, fig=ori_par$fig)
}
pdf(paste0(prefix,"-positividad_por_estado_150dias-SPLIT.pdf"),width=7)
sub <- roll[,c("Chihuahua", "Sinaloa", "Sonora", "Baja California", "Baja California Sur", "Durango")];state_pos("NOROESTE")
sub <- roll[,c("Tamaulipas", "Nuevo Leon", "Coahuila")];state_pos("NORESTE")
sub <- roll[,c("Aguascalientes", "San Luis Potosi", "Queretaro", "Guanajuato", "Zacatecas")];state_pos("CENTRO-NORTE")
sub <- roll[,c("CDMX", "Morelos", "Mexico", "Puebla", "Tlaxcala", "Hidalgo")];state_pos("CENTRO-SUR")
sub <- roll[,c("Jalisco", "Michoacan", "Colima", "Nayarit")];state_pos("OCCIDENTE")
sub <- roll[,c("Chiapas", "Veracruz", "Guerrero", "Oaxaca", "Tabasco")];state_pos("SUR")
sub <- roll[,c("Campeche", "Yucatan", "Quintana Roo")];state_pos("SURESTE")
dev.off()

pdf(paste0(prefix,"-positividad_por_estado_150dias-MORELOS.pdf"),width=7)
sub <- roll[,c("CDMX", "Morelos", "Mexico", "Puebla", "Guerrero")];state_pos("Morelos")
dev.off()
pdf(paste0(prefix,"-positividad_por_estado_150dias-MICHOACAN.pdf"),width=7)
sub <- roll[,c("Jalisco", "Michoacan", "Colima", "Guanajuato", "Queretaro", "Mexico", "Guerrero")];state_pos("Michoacan")
dev.off()

roll <- roll[(nrow(roll)-29):nrow(roll),]
pdf(paste0(prefix,"-positividad_por_estado_30dias.pdf"),width=7)
# dates <- rownames(roll)# get a vector with all continuous dates
dates <- seq(2,nrow(roll),2) # only print half the dates
# dates <- unique(c(seq(1,nrow(roll),2),nrow(roll))) # only print half the dates
# dates <- dates[-(length(dates)-1)]
par(oma = c(1, 1, 1, 6)) #This is just a most imaginative fix to plot the legend outside, i
matplot(las=2,roll, type='l', col=pars[,1], lty=pars[,2], xaxt="n", ylab="Porcentaje de pruebas positivas", main="Positividad por Estado - Últimos 30 días")
mtext("Promedio móvil de 7 días")
axis(1, las=2, at = dates, labels = rownames(roll)[dates], cex.axis = 0.60)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
legend("right", legend = gsub("\"","",colnames(roll)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(roll)], lty = pars$lty[1:ncol(roll)], lwd = 1, cex = cex.X)
dev.off()

######################## States table ########################
# First, confirmed
pos <- xtabs(rep(1,nrow(data))~data[,11]+data[,8], data=data)
dates <- seq(as.Date(rownames(pos)[1]),as.Date(rownames(pos)[nrow(pos)]),by="day")
xtable <- as.data.frame(matrix(0,nrow=length(dates),ncol=ncol(pos)), stringsAsFactors = FALSE) # Create empty vessel for output
rownames(xtable) <- dates # use the vector as rownames
colnames(xtable) <- colnames(pos) # and inherit the names
invisible(sapply(rownames(pos),function(x) {xtable[x,] <<- pos[x,]}))	# append the original values in the corresponding places (write to higher env variable
pos <- xtable
rownames(pos) <- format(as.Date(rownames(pos)),format = "%d-%m-%Y")
pos <- cbind(pos,"36"=rowSums(pos))
out <- as.data.frame(rbind("cve_ent"=sprintf("%02d",as.numeric(colnames(pos))),"poblacion"=entidades[colnames(pos),2], "nombre"=toupper(entidades[colnames(pos),1]),pos))
out[3,9] <- "CIUDAD DE MEXICO" # Change some names back to their original style (spanish and old names)
out[3,15] <- "MEXICO"
out <- out[,c(order(as.character(out[3,1:32])),33)] # Sort by state name
out[3,7] <- "DISTRITO FEDERAL"
out[3,33] <- "Nacional"
out[1,33] <- "000" # National cases have a 000 code
# out <- out[1:100,] # For testing
write.table(t(out), paste0("Casos_Diarios_Estado_Nacional_Confirmados_",gsub("-","",prefix),".csv"), sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
# Then, deaths
deaths_all <- data[data[,13]!="9999-99-99",] # Extract deaths
pos <- xtabs(rep(1,nrow(deaths_all))~deaths_all[,13]+deaths_all[,8], data=deaths_all) # Deaths are registered using the date they occurred
dates <- seq(as.Date(rownames(pos)[1]),as.Date(rownames(pos)[nrow(pos)]),by="day")
xtable <- as.data.frame(matrix(0,nrow=length(dates),ncol=ncol(pos)), stringsAsFactors = FALSE) # Create empty vessel for output
rownames(xtable) <- dates # use the vector as rownames
colnames(xtable) <- colnames(pos) # and inherit the names
invisible(sapply(rownames(pos),function(x) {xtable[x,] <<- pos[x,]}))	# append the original values in the corresponding places (write to higher env variable
pos <- xtable
rownames(pos) <- format(as.Date(rownames(pos)),format = "%d-%m-%Y")
pos <- cbind(pos,"36"=rowSums(pos))
out <- as.data.frame(rbind("cve_ent"=sprintf("%02d",as.numeric(colnames(pos))),"poblacion"=entidades[colnames(pos),2], "nombre"=toupper(entidades[colnames(pos),1]),pos))
out[3,9] <- "CIUDAD DE MEXICO" # Change some names back to their original style (spanish and old names)
out[3,15] <- "MEXICO"
out <- out[,c(order(as.character(out[3,1:32])),33)] # Sort by state name
out[3,7] <- "DISTRITO FEDERAL"
out[3,33] <- "Nacional"
out[1,33] <- "000" # National cases have a 000 code
# out <- out[1:100,] # For testing
write.table(t(out), paste0("Casos_Diarios_Estado_Nacional_Defunciones_",gsub("-","",prefix),".csv"), sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)

######################## Age through time ########################
#################
### Confirmed ###
#################
confirmados <- xtabs(rep(1,nrow(data))~data[,11]+data[,16], data=data) # Create a table of total per patient status
ages <- as.numeric(colnames(confirmados)) # Get a list of actual ages (individual ages)
# age_breaks <- c(seq(-1,99,10),200) # Construct desired breaks for bins
age_breaks <- c(-1,17,29,39,49,59,200) # alternative for vaccine age
bins <- cut(ages,age_breaks, include.lowest=T) # Create bins
list <- tapply(ages,bins,"[") # Create a list of bins to recover
confirmados <- sapply(list, function(x) rowSums(confirmados[,as.character(x)])) # now a collated version, using them as names
colnames(confirmados) <- paste(sapply(list, function(x) range(x))[1,],sapply(list, function(x) range(x))[2,],sep="-"); colnames(confirmados)[6]="60+"
tot.c <- rbind("Conf"=colSums(confirmados),"Conf%"=round(colSums(confirmados)*100/total_confirmed,2))
first <- sum(cumsum(rowSums(confirmados))<20)-3 # Determine the first item where cummalitive cases are >10
last <- nrow(confirmados)-finalcut # Determine the last item (the past 8 days are not useful but 3 days prior to our cutoff us requiered for the mean) # UPDATE 2021-08-20 Changed it to a variable (as well as the other afterwards)
confirmados <- confirmados[first:last,]
roll.c <- sapply(4:(nrow(confirmados)-3),function(y) {apply(confirmados,2, function(x) mean(as.numeric(x[(y-3):(y+3)])))}) # Create rolling average for 7 days (2 nested functions, one for calculation of the average, one for advancing within matrix limits)
colnames(roll.c) <- row.names(confirmados)[4:(nrow(confirmados)-3)]
roll.c <- t(roll.c) # Just aesthetic, as I'd rather used multiple rows than columns
lroll.c <- log10(roll.c) # Create an additional set for log scale
lroll.c[!is.finite(lroll.c)] <- 0
lroll.c[lroll.c<0]=0
# Finally, calculate the per 100K people
norm.c_day <- apply(roll.c, 2, function(x) x/rowSums(roll.c)*100) # This is now expressed as relative to dialy totals
norm.c_age <- t(apply(roll.c, 1, function(x) x/colSums(roll.c)*100))

write.table(confirmados, "age_vac_date.tsv", sep="\t", quote=FALSE, row.names=TRUE, col.names=NA)

########## Plot data confirmed ################
# Create combinations for line color and width
lty <- c("solid","dashed","dotted","dotdash","longdash","twodash")
# col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","wheat1","magenta1","limegreen","darkorange2","darkgray")
col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","purple3","magenta1","limegreen","darkorange2","darkgray")
lwd <- c(1.3,2)
pars <- expand.grid(col = col, lty = lty, lwd = lwd, stringsAsFactors = FALSE) # This will create all 
# First the raw rolling average
cex.X=0.53
# dates <- unique(sort(c(grep("01$",rownames(roll.c)),nrow(roll.c),1)))
# if((dates[length(dates)]-dates[length(dates)-1])<15){dates <- dates[-(length(dates)-1)]} # Fix dates showing
# if((dates[2]-dates[1])<10){dates <- dates[-2]}
dates <- rownames(roll.c) # UPDATE 2022-11-30: there is no more space for this to fit, we have thus changed the actual printing scheme for dates
dates <- printable_dates(dates)
name="Confirmados"
pdf(paste0(prefix,"-",name,"-total-NACIONAL.pdf"))
with(pars[rowSums(roll.c),],matplot(rowSums(roll.c), type = 'l', col = col, lty = 1, lwd = 2, main = paste0(name," Totales NACIONAL"), ylab = "", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(roll.c)[dates], cex.axis = 0.8)
dev.off()

# Next, confirmed cases by age
pdf(paste0(prefix,"-",name,"-total_edad.pdf"))
par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
with(pars[1:ncol(roll.c),],matplot(roll.c[,1:ncol(roll.c)], type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales por rango de edad"), ylab = "Promedio diario de confirmados", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(roll.c)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = colnames(roll.c), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(roll.c)], lty = pars$lty[1:ncol(roll.c)], lwd = pars$lwd[1:ncol(roll.c)-1], cex = cex.X)
dev.off()

# Next, a log10 visualization
pdf(paste0(prefix,"-",name,"-total_log_edad.pdf"))
par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
with(pars[1:ncol(lroll.c),],matplot(lroll.c, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales (log10) por rango de edad"), ylab = "Promedio diario de confirmados (log10)", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(lroll.c)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(lroll.c)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(lroll.c)], lty = pars$lty[1:ncol(lroll.c)], lwd = pars$lwd[1:ncol(lroll.c)], cex = cex.X)
dev.off()

# Next, same thing but normalized data
pdf(paste0(prefix,"-",name,"-total_norm_por_dia.pdf"))
par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
with(pars[1:ncol(norm.c_day),],matplot(norm.c_day, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total diario"), ylab = "Porcentaje del promedio diario", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(norm.c_day)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(norm.c_day)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(norm.c_day)], lty = pars$lty[1:ncol(norm.c_day)], lwd = pars$lwd[1:ncol(norm.c_day)], cex = cex.X)
dev.off()
# Twitter version
Upper_First <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}
all_dates <- rownames(norm.c_day)
waves <- c(0,grep("2020-05-03", all_dates), grep("2020-09-12", all_dates), grep("2020-09-13", all_dates), grep("2021-03-24", all_dates), grep("2021-06-19", all_dates), grep("2021-10-30", all_dates), grep("2021-12-19", all_dates), grep("2022-02-28", all_dates), grep("2022-05-15", all_dates),grep("2022-08-22", all_dates), grep("2022-12-10", all_dates),length(all_dates))
dates_alt <- unique(sort(c(grep("01$",rownames(roll.c)),1)))
dates_lab <- (format(as.Date(rownames(norm.c_day)[dates_alt]), "%h-%y"))
dates_lab <- sapply(dates_lab,Upper_First)
# cols=c("white","orange","white","forestgreen","white","purple","white","firebrick")
cols=c(NA,"black",NA,"gray20",NA,"gray30",NA,"gray50",NA,"gray70",NA,"gray80")
# bord=c(NA,"black",NA,"black",NA,"black",NA,"black",NA,"black",NA,"black")
bord=NA
# barplot(cbind(diff(waves)), horiz = TRUE, beside = FALSE, col=cols, border=cols, xaxt='n')
pdf(paste0(prefix,"-",name,"-total_norm_por_dia_twitter.pdf"),width=9)
par(oma = c(1, 1, 0, 4)) #This is just a creative fix to plot the legend outside, i
par(mar = c(4, 5, 4, 2.5)) #This is just a creative fix to plot the legend outside, i
with(pars[1:ncol(norm.c_day),],matplot(norm.c_day, type = 'l', col = col, lty = lty, lwd = 2, ylab = '', xlab = "", las = 2, xaxt = 'n',cex.lab=1.5, cex.axis=1.25,yaxt='n', ylim=c((0-diff(range(norm.c_day)*.02)),max(norm.c_day))))
mtext("Promedio móvil de 7 días a partir de fecha de registro",cex=1.25)
axis(1, las=2, at = dates_alt, labels = dates_lab, cex.axis = 0.85)
axis(2, las=1, at = seq(0,100,5), label = paste0(seq(0,100,5),"%"), cex.axis = 1.1)
title(ylab="Proporción de casos diarios confirmados por edad", line=3.5, cex.lab=1.2)
par(oma = c(0, 1, 0, 4)) #This is just a creative fix to plot the legend outside, i
barplot(cbind(diff(waves)), horiz = TRUE, beside = FALSE, col=cols, border=bord, xaxt='n',add=T)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = c(gsub("\"","",colnames(norm.c_day)), "Ola 1", "Ola 2", "Ola 3", "Ola 4","Ola 5","Ola 6"), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = c(pars$col[1:ncol(norm.c_day)], rep("black",6)), pt.bg = c(rep(NA,6),"black","gray20","gray30", "gray50", "gray70","gray80"), lty = c(pars$lty[1:ncol(norm.c_day)],NA,NA,NA,NA,NA,NA), lwd = 2, cex = 1, pch=c(rep(NA,6),rep(22,6)), pt.cex=c(rep(NA,6),rep(2,6)), title="Edad (grupo):")
dev.off()
# Next, same thing but normalized data
pdf(paste0(prefix,"-",name,"-total_norm_por_rango.pdf"))
par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
with(pars[1:ncol(norm.c_age),],matplot(norm.c_age, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total por rango"), ylab = "", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(norm.c_age)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(norm.c_age)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(norm.c_age)], lty = pars$lty[1:ncol(norm.c_age)], lwd = pars$lwd[1:ncol(norm.c_age)], cex = cex.X)
dev.off()

##############
### Deaths ###
##############
name="Defunciones"
deaths_all <- data[data[,13]!="9999-99-99",] # Extract deaths
total_deaths <- nrow(deaths_all)
deaths <- xtabs(rep(1,nrow(deaths_all))~deaths_all[,13]+deaths_all[,16], data=deaths_all) # Create a table of total per state
ages <- as.numeric(colnames(deaths)) # Get a list of actual ages (individual ages)
age_breaks <- c(-1,17,29,39,49,59,200) # alternative for vaccine age
bins <- cut(ages,age_breaks, include.lowest=T) # Create bins
list <- tapply(ages,bins,"[") # Create a list of bins to recover
# age_breaks <- c(seq(-1,99,10),200) # Construct desired breaks for bins # Old scheme per 10y
# bins <- cut(ages,age_breaks, include.lowest=T) # Create bins
# list <- tapply(ages,bins,"[") # Create a list of bins to recover
deaths <- sapply(list, function(x) rowSums(deaths[,as.character(x)])) # now a collated version, using them as names
colnames(deaths) <- paste(sapply(list, function(x) range(x))[1,],sapply(list, function(x) range(x))[2,],sep="-");colnames(deaths)[6]="60+"
tot.d <- rbind("Conf"=colSums(deaths),"Conf%"=round(colSums(deaths)*100/total_deaths,2))
first <- sum(cumsum(rowSums(deaths))<20)-3 # Determine the first item where cummalitive cases are >20
last <- nrow(deaths)-finalcut # Determine the last item (the past 8 days are not useful but 3 days prior to our cutoff us requiered for the mean)
deaths <- deaths[first:last,]
roll.d <- sapply(4:(nrow(deaths)-3),function(y) {apply(deaths,2, function(x) mean(as.numeric(x[(y-3):(y+3)])))}) # Create rolling average for 7 days (2 nested functions, one for calculation of the average, one for advancing within matrix limits)
colnames(roll.d) <- row.names(deaths)[4:(nrow(deaths)-3)]
roll.d <- t(roll.d) # Just aesthetic, as I'd rather used multiple rows than columns
lroll.d <- log10(roll.d) # Create an additional set for log scale
lroll.d[!is.finite(lroll.d)] <- 0
lroll.d[lroll.d<0]=0
# Finally, calculate the per 100K people
norm.d_day <- apply(roll.d, 2, function(x) x/rowSums(roll.d)*100) # This is now expressed as relative to dialy totals
norm.d_age <- t(apply(roll.d, 1, function(x) x/colSums(roll.d)*100))
write.table(deaths, "age_vac_date_deaths.tsv", sep="\t", quote=FALSE, row.names=TRUE, col.names=NA)

########## Plot data deaths ################
# Create combinations for line color and width
lty <- c("solid","dashed","dotted","dotdash","longdash","twodash")
# col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","wheat1","magenta1","limegreen","darkorange2","darkgray")
col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","purple3","magenta1","limegreen","darkorange2","darkgray")
lwd <- c(1.3,2)
pars <- expand.grid(col = col, lty = lty, lwd = lwd, stringsAsFactors = FALSE) # This will create all
# First the raw rolling average
cex.X=0.53
# dates <- unique(sort(c(grep("01$",rownames(roll.d)),nrow(roll.d),1)))
# if((dates[length(dates)]-dates[length(dates)-1])<15){dates <- dates[-(length(dates)-1)]} # Fix dates showing
# if((dates[2]-dates[1])<10){dates <- dates[-2]}
dates <- rownames(roll.d) # UPDATE 2022-11-30: there is no more space for this to fit, we have thus changed the actual printing scheme for dates
dates <- printable_dates(dates)
name="Defunciones"
pdf(paste0(prefix,"-",name,"-total-NACIONAL.pdf"))
with(pars[rowSums(roll.d),],matplot(rowSums(roll.d), type = 'l', col = col, lty = 1, lwd = 2, main = paste0(name," Totales NACIONAL"), ylab = "", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(roll.d)[dates], cex.axis = 0.8)
dev.off()

# Next, confirmed cases by age
pdf(paste0(prefix,"-",name,"-total_edad.pdf"))
par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
with(pars[1:ncol(roll.d),],matplot(roll.d[,1:ncol(roll.d)], type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales por rango de edad"), ylab = "Promedio diario de defunciones", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(roll.d)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = colnames(roll.d), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(roll.d)], lty = pars$lty[1:ncol(roll.d)], lwd = pars$lwd[1:ncol(roll.d)-1], cex = cex.X)
dev.off()

# Next, a log10 visualization
pdf(paste0(prefix,"-",name,"-total_log_edad.pdf"))
par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
with(pars[1:ncol(lroll.d),],matplot(lroll.d, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales (log10) por rango de edad"), ylab = "Promedio diario de defunciones (log10)", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(lroll.d)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(lroll.d)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(lroll.d)], lty = pars$lty[1:ncol(lroll.d)], lwd = pars$lwd[1:ncol(lroll.d)], cex = cex.X)
dev.off()

# Next, same thing but normalized data
pdf(paste0(prefix,"-",name,"-total_norm_por_dia.pdf"))
par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
with(pars[1:ncol(norm.d_day),],matplot(norm.d_day, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total diario"), ylab = "Porcentaje del promedio diario de defunciones", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(norm.d_day)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(norm.d_day)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(norm.d_day)], lty = pars$lty[1:ncol(norm.d_day)], lwd = pars$lwd[1:ncol(norm.d_day)], cex = cex.X)
dev.off()
Upper_First <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}
# Twitter version
all_dates <- rownames(norm.d_day)
waves <- c(0,grep("2020-05-03", all_dates), grep("2020-09-12", all_dates), grep("2020-09-13", all_dates), grep("2021-03-24", all_dates), grep("2021-06-19", all_dates), grep("2021-10-30", all_dates), grep("2021-12-19", all_dates), grep("2022-02-28", all_dates), grep("2022-05-15", all_dates),grep("2022-08-22", all_dates), grep("2022-12-10", all_dates),length(all_dates))
dates_alt <- unique(sort(c(grep("01$",rownames(norm.d_day)))))
dates_lab <- (format(as.Date(rownames(norm.d_day)[dates_alt]), "%h-%y"))
dates_lab <- sapply(dates_lab,Upper_First)
# cols=c("white","orange","white","forestgreen","white","purple","white","firebrick")
cols=c(NA,"black",NA,"gray20",NA,"gray30",NA,"gray50",NA,"gray70",NA,"gray80")
# bord=c(NA,"black",NA,"black",NA,"black",NA,"black",NA,"black",NA,"black")
bord=NA
# barplot(cbind(diff(waves)), horiz = TRUE, beside = FALSE, col=cols, border=cols, xaxt='n')
pdf(paste0(prefix,"-",name,"-total_norm_por_dia_twitter.pdf"),width=9)
par(oma = c(1, 1, 0, 4)) #This is just a creative fix to plot the legend outside, i
par(mar = c(4, 5, 4, 2.5))
with(pars[1:ncol(norm.d_day),],matplot(norm.d_day, type = 'l', col = col, lty = lty, lwd = 2, ylab = "", xlab = "", las = 2, xaxt = 'n',cex.lab=1.5, cex.axis=1.25,yaxt='n', ylim=c((0-diff(range(norm.d_day)*.02)),max(norm.d_day))))
mtext("Promedio móvil de 7 días a partir de fecha de registro",cex=1.25)
axis(1, las=2, at = dates_alt, labels = dates_lab, cex.axis = 0.85)
axis(2, las=1, at = seq(0,100,5), label = paste0(seq(0,100,5),"%"), cex.axis = 1.1)
title(ylab="Proporción de defunciones diarias confirmados por edad", line=3.5, cex.lab=1.2)
par(oma = c(0, 1, 0, 4)) #This is just a creative fix to plot the legend outside, i
barplot(cbind(diff(waves)), horiz = TRUE, beside = FALSE, col=cols, border=bord, xaxt='n',add=T)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = c(gsub("\"","",colnames(norm.c_day)), "Ola 1", "Ola 2", "Ola 3", "Ola 4","Ola 5","Ola 6"), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = c(pars$col[1:ncol(norm.c_day)], rep("black",6)), pt.bg = c(rep(NA,6),"black","gray20","gray30", "gray50", "gray70","gray80"), lty = c(pars$lty[1:ncol(norm.c_day)],NA,NA,NA,NA,NA,NA), lwd = 2, cex = 1, pch=c(rep(NA,6),rep(22,6)), pt.cex=c(rep(NA,6),rep(2,6)), title="Edad (grupo):")
dev.off()

# Next, same thing but normalized data
pdf(paste0(prefix,"-",name,"-total_norm_por_rango.pdf"))
par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
with(pars[1:ncol(norm.d_age),],matplot(norm.d_age, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje de defunciones por rango"), ylab = "", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(norm.d_age)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(norm.d_age)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(norm.d_age)], lty = pars$lty[1:ncol(norm.d_age)], lwd = pars$lwd[1:ncol(norm.d_age)], cex = cex.X)
dev.off()

######################## Ventilation-requiring cases (Last 45 days intubated) ########################
vent <- xtabs(rep(1,nrow(int))~int[,11]+int[,4], data=int) # Create a table of total per state
vent_d <- xtabs(rep(1,nrow(int_d))~int_d[,11]+int_d[,4], data=int_d) # Create a table of total per state
pdf(paste0(date,"-sector_intub.pdf"))
par(las=2)
barplot(t(vent), pch=20, col=col,border=col,xpd=F,cex.names=0.8,main="Pacientes Intubados por Fecha de Ingreso y Sector")
legend("topleft", pch=15, col=rev(col[1:ncol(vent)]), legend=rev(sectores[colnames(vent),]),cex=0.8)
barplot(t(vent_d), pch=20, col=col,border=col,xpd=F,cex.names=0.8,main="Pacientes Intubados Difuntos por Fecha de Ingreso y Sector")
legend("topleft", pch=15, col=rev(col[1:ncol(vent)]), legend=rev(sectores[colnames(vent),]),cex=0.8)
dev.off()

vent_estados <- xtabs(rep(1,nrow(int_d))~int_d[,11]+int_d[,8], data=int) # How many intubated have died per State
colnames(vent_estados) <- entidades[colnames(vent_estados),1]
vent_estados <- vent_estados[,rev(colnames(vent_estados))]
pdf(paste0(date,"-estados_intub.pdf"))
par(las=2)
barplot(t(vent_estados), pch=20, col=col,border=col,xpd=F,cex.names=0.8,main="Pacientes Intubados Difuntos por Fecha de Ingreso y Entidad")
legend("topleft", pch=15, col=rev(rep(col,2)[1:length(colnames(vent_estados))]), legend=rev(colnames(vent_estados)), cex=0.7)
dev.off()

### Test confirmed and deaths by ages
i=16;age_c <- table(data[i]);age_c <- age_c[age_c>0];
i=16;age_d <- table(deaths_all[i]);age_d <- age_d[age_d>0];
range_c <- range(as.numeric(names(age_c)))
age_c <- age_c[seq(range_c[1],range_c[2])]
age_c[is.na(age_c)]=0
age_d <- age_d[seq(range_c[1],range_c[2])]
age_d[is.na(age_d)]=0
yrange <- range(age_c)
p_death <- age_d*100/age_c;names(p_death)=names(age_c)
pdf(paste0(date,"-Age_grps.pdf"))
barplot(age_c,col="cornflowerblue",border="white",xaxt='n',xlab="Edad",ylab="Confirmados COVID-19",main="Confirmados COVID-19 por Edades")
mtext(paste0("Total confirmados (",date,"): ",total_confirmed))
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
barplot(age_c*100/total_confirmed,col="cornflowerblue",border="white",xaxt='n',xlab="Edad",ylab="Confirmados COVID-19 (%)",main="Confirmados COVID-19 por Edades (%)",las=1)
mtext(paste0("Total confirmados (",date,"): ",total_confirmed))
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
barplot(age_d,col="coral1",border="white",xaxt='n',xlab="Edad",ylab="Defunciones COVID-19",main="Defunciones COVID-19 por Edades")
mtext(paste0("Total defunciones (",date,"): ",total_deaths))
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
barplot(age_d*100/total_deaths,col="coral1",border="white",xaxt='n',xlab="Edad",ylab="Defunciones COVID-19 (%)",main="Defunciones COVID-19 por Edades (%)")
mtext(paste0("Total defunciones (",date,"): ",total_deaths))
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
barplot(age_c,col="cornflowerblue",border="white",xaxt='n',xlab="Edad",ylab="Casos Totales",main="Confirmados y Defunciones por COVID-19 por Edades")
barplot(age_d,col="coral1",border="white",xaxt='n',add=T)
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
legend("topright", legend=c(paste0("Confirmados: ",total_confirmed),paste0("Defunciones: ",total_deaths)), pch=15, col=c("cornflowerblue","coral1"))
barplot(age_d*100/total_deaths,col="coral1",border="white",xaxt='n',xlab="Edad",ylab="Casos Totales",main="Confirmados y Defunciones por COVID-19 por Edades (%)")
barplot(age_c*100/total_confirmed,col="cornflowerblue",border="white",xaxt='n',add=T)
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
legend("topright", legend=c(paste0("Confirmados: ",total_confirmed),paste0("Defunciones: ",total_deaths)), pch=15, col=c("cornflowerblue","coral1"))
barplot(p_death,main="Porcentaje de Diagnosticados que Fallecen por Cada Edad", xlab="Edad", ylab="Porcentaje de Defunciones (%)",border="white",col="black",las=1,xaxt='n')
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
dev.off()

# Repeat for recent 30 days (without last 15)
i=16;age_c <- table(last30days_conf[i]);age_c <- age_c[age_c>0];
i=16;age_d <- table(last30days_conf[last30days_conf[13]!="9999-99-99",i]);age_d <- age_d[age_d>0];
range_c <- c(0,122)
age_c <- age_c[as.character(seq(range_c[1],range_c[2]))]
age_c[is.na(age_c)]=0
age_d <- age_d[as.character(seq(range_c[1],range_c[2]))]
age_d[is.na(age_d)]=0
yrange <- range(age_c)
names(age_d) <- names(age_c) <- seq(range_c[1],range_c[2])
p_death <- age_d*100/age_c;names(p_death)=names(age_c)
pdf(paste0(date,"-Age_grps_30dias_conf.pdf"))
barplot(age_c,col="cornflowerblue",border="white",xaxt='n',xlab="Edad",ylab="Confirmados COVID-19",main="Confirmados COVID-19 por Edades")
mtext(paste0("Total confirmados 30 días conf(",date,"): ",total_confirmed_sub))
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
barplot(age_c*100/total_confirmed_sub,col="cornflowerblue",border="white",xaxt='n',xlab="Edad",ylab="Confirmados COVID-19 (%)",main="Confirmados COVID-19 por Edades (%)",las=1)
mtext(paste0("Total confirmados 30 días conf(",date,"): ",total_confirmed_sub))
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
barplot(age_d,col="coral1",border="white",xaxt='n',xlab="Edad",ylab="Defunciones COVID-19",main="Defunciones COVID-19 por Edades")
mtext(paste0("Total defunciones 30 días conf(",date,"): ",total_deaths_sub30))
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
barplot(age_d*100/total_deaths_sub30,col="coral1",border="white",xaxt='n',xlab="Edad",ylab="Defunciones COVID-19 (%)",main="Defunciones COVID-19 por Edades (%)")
mtext(paste0("Total defunciones 30 días conf(",date,"): ",total_deaths_sub30))
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
barplot(age_c,col="cornflowerblue",border="white",xaxt='n',xlab="Edad",ylab="Casos Totales",main="Confirmados y Defunciones por COVID-19 por Edades")
barplot(age_d,col="coral1",border="white",xaxt='n',add=T)
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
legend("topright", legend=c(paste0("Confirmados: ",total_confirmed_sub),paste0("Defunciones: ",total_deaths_sub30)), pch=15, col=c("cornflowerblue","coral1"))
barplot(age_d*100/total_deaths_sub30,col="coral1",border="white",xaxt='n',xlab="Edad",ylab="Casos Totales",main="Confirmados y Defunciones por COVID-19 por Edades (%)")
barplot(age_c*100/total_confirmed_sub,col="cornflowerblue",border="white",xaxt='n',add=T)
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
legend("topright", legend=c(paste0("Confirmados: ",total_confirmed_sub),paste0("Defunciones: ",total_deaths_sub30)), pch=15, col=c("cornflowerblue","coral1"))
barplot(p_death,main="Porcentaje de Diagnosticados que Fallecen por Cada Edad", xlab="Edad", ylab="Porcentaje de Defunciones (%)",border="white",col="black",las=1,xaxt='n')
axis(1,las=2, at=seq(.7,150,6), labels=seq(0,120,5))
dev.off()

################ Lethality (cases vs death ratio) ################
# Lethality must be calculated with the actual dates when patients were diagnosed to make confirmed cases and deaths more comparable

name="Defunciones"
deaths_all <- data[data[,13]!="9999-99-99",] # Extract deaths
total_deaths <- nrow(deaths_all)
deaths <- xtabs(rep(1,nrow(deaths_all))~deaths_all[,11]+deaths_all[,16], data=deaths_all) # Create a table of total per age #IMPORTANT: Dates are registered by diagnostic date, not by death confirmation date.
ages <- as.numeric(colnames(deaths)) # Get a list of actual ages (individual ages)
age_breaks <- c(-1,17,29,39,49,59,200) # alternative for vaccine age
bins <- cut(ages,age_breaks, include.lowest=T) # Create bins
# ages <- as.numeric(colnames(deaths)) # Get a list of actual ages (individual ages)
# age_breaks <- c(seq(-1,99,10),200) # Construct desired breaks for bins # Old scheme, 10y
# bins <- cut(ages,age_breaks, include.lowest=T) # Create bins
list <- tapply(ages,bins,"[") # Create a list of bins to recover
deaths <- sapply(list, function(x) rowSums(deaths[,as.character(x)])) # now a collated version, using them as names
colnames(deaths) <- paste(sapply(list, function(x) range(x))[1,],sapply(list, function(x) range(x))[2,],sep="-");colnames(deaths)[6]="60+"
tot.d <- rbind("Conf"=colSums(deaths),"Conf%"=round(colSums(deaths)*100/total_deaths,2))
first <- sum(cumsum(rowSums(deaths))<20)-3 # Determine the first item where cummalitive cases are >20
last <- nrow(deaths)-finalcut # Determine the last item (the past 8 days are not useful but 3 days prior to our cutoff us requiered for the mean)
deaths <- deaths[first:last,]
roll.d <- sapply(4:(nrow(deaths)-3),function(y) {apply(deaths,2, function(x) mean(as.numeric(x[(y-3):(y+3)])))}) # Create rolling average for 7 days (2 nested functions, one for calculation of the average, one for advancing within matrix limits)
colnames(roll.d) <- row.names(deaths)[4:(nrow(deaths)-3)]
roll.d <- t(roll.d) # Just aesthetic, as I'd rather used multiple rows than columns

death_ratio_dates <- rownames(roll.d)
roll_ratio <- cbind(rowSums(roll.c[death_ratio_dates,]),rowSums(roll.d[death_ratio_dates,]))
lethality <- roll_ratio[,2]*100/roll_ratio[,1]

cex.X=0.53
# dates <- unique(sort(c(grep("01$",rownames(roll_ratio)),nrow(roll_ratio),1)))
# if((dates[length(dates)]-dates[length(dates)-1])<15){dates <- dates[-(length(dates)-1)]} # Fix dates showing
# if((dates[2]-dates[1])<10){dates <- dates[-2]}
dates <- rownames(roll_ratio) # UPDATE 2022-11-30: there is no more space for this to fit, we have thus changed the actual printing scheme for dates
dates <- printable_dates(dates)
name="Letalidad"
pdf(paste0(prefix,"-",name,"-total-NACIONAL.pdf"))
plot(lethality, type = 'l', col = 1, lty = 1, lwd = 2, main = paste0(name,": % de difuntos por fecha de diagnóstico"), ylab = "", xlab = "", las = 2, xaxt = 'n')
mtext("Promedio 7 días - a partir de 20 defunciones - día de registro")
axis(1, las=2, at = dates, labels = names(lethality)[dates], cex.axis = 0.8)
dev.off()

# DEPRECATED: START
# # Now, by ages (we must return to the actual raw numbers:
# # First, get the total number of tests that were carried out on each date
# sampled <- xtabs(rep(1,nrow(data_all))~data_all[,11]+data_all[,16], data=data_all)
# ages <- as.numeric(colnames(sampled)) # Get a list of actual ages (individual ages)
# age_breaks <- c(seq(-1,99,10),200) # Construct desired breaks for bins
# bins <- cut(ages,age_breaks, include.lowest=T) # Create bins
# list <- tapply(ages,bins,"[") # Create a list of bins to recover
# sampled <- sapply(list, function(x) rowSums(sampled[,as.character(x)])) # now a collated version, using them as names
# colnames(sampled) <- paste(sapply(list, function(x) range(x))[1,],sapply(list, function(x) range(x))[2,],sep="-")
# sampled[death_ratio_dates,]
# DEPRECATED: END

death_ratio_dates <- rownames(deaths)
lethality <- deaths[death_ratio_dates,]/confirmados[death_ratio_dates,]*100
lethality[!is.finite(lethality)] <- 0
roll.l <- sapply(4:(nrow(lethality)-3),function(y) {apply(lethality,2, function(x) mean(as.numeric(x[(y-3):(y+3)])))}) # Create rolling average for 7 days (2 nested functions, one for calculation of the average, one for advancing within matrix limits)
colnames(roll.l) <- row.names(lethality)[4:(nrow(lethality)-3)]
roll.l <- t(roll.l) # Just aesthetic, as I'd rather used multiple rows than columns
# roll.l <- roll.l[1:(nrow(roll.l)-7),] # UPDATE 2022-01-27 remove last items NO SIRVE
lroll.l <- log10(roll.l) # Create an additional set for log scale
lroll.l[!is.finite(lroll.l)] <- 0
lroll.l[lroll.l<0]=0

########## Plot data lethality ################
# Create combinations for line color and width
lty <- c("solid","dashed","dotted","dotdash","longdash","twodash")
# col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","wheat1","magenta1","limegreen","darkorange2","darkgray")
col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","purple3","magenta1","limegreen","darkorange2","darkgray")
lwd <- c(1.3,2)
pars <- expand.grid(col = col, lty = lty, lwd = lwd, stringsAsFactors = FALSE) # This will create all
# First the raw rolling average
cex.X=0.53
# dates <- unique(sort(c(grep("01$",rownames(roll.l)),nrow(roll.l),1)))
# if((dates[length(dates)]-dates[length(dates)-1])<15){dates <- dates[-(length(dates)-1)]} # Fix dates showing
# if((dates[2]-dates[1])<10){dates <- dates[-2]}
dates <- rownames(roll.l) # UPDATE 2022-11-30: there is no more space for this to fit, we have thus changed the actual printing scheme for dates
dates <- printable_dates(dates)
name="Letalidad"
# pdf(paste0(prefix,"-",name,"-total-NACIONAL.pdf"))
# with(pars[rowSums(roll.l),],matplot(rowSums(roll.l), type = 'l', col = col, lty = 1, lwd = 2, main = paste0(name," Totales NACIONAL"), ylab = "", xlab = "", las = 2, xaxt = 'n'))
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(roll.l)[dates], cex.axis = 0.8)
# dev.off()

# Next, confirmed cases by age
pdf(paste0(prefix,"-",name,"-total_edad.pdf"))
par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
with(pars[1:ncol(roll.l),],matplot(roll.l[,1:ncol(roll.l)], type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales por rango de edad"), ylab = "Porcentaje diario de difuntos por grupo", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio móvil, considerando fecha de registro")
axis(1, las=2, at = dates, labels = rownames(roll.l)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = colnames(roll.l), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(roll.l)], lty = pars$lty[1:ncol(roll.l)], lwd = pars$lwd[1:ncol(roll.l)], cex = cex.X)
dev.off()
pdf(paste0(prefix,"-",name,"-total_edad_niños.pdf"))
par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
matplot(roll.l[,c(1:2)],type='l',lwd=2,col=c("chartreuse3","cornflowerblue"),lty=1, ylab = "Porcentaje diario de difuntos por grupo", xlab = "", las = 2, xaxt = 'n', main="Letalidad en grupos Jóvenes")
axis(1, las=2, at = dates, labels = rownames(roll.l)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = colnames(roll.l[,1:2]), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = c("chartreuse3","cornflowerblue"), lty = 1, lwd = 2, cex = cex.X)
dev.off()

# Next, a log10 visualization
pdf(paste0(prefix,"-",name,"-total_log_edad.pdf"))
par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
with(pars[1:ncol(lroll.l),],matplot(lroll.l[,1:ncol(roll.l)], type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales (log10) por rango de edad"), ylab = "Porcentaje diario de difuntos por grupo (log10)", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio móvil, considerando fecha de registro")
axis(1, las=2, at = dates, labels = rownames(lroll.l)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(lroll.l)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(lroll.l)], lty = pars$lty[1:ncol(lroll.l)], lwd = pars$lwd[1:ncol(lroll.l)], cex = cex.X)
dev.off()

########## Type of patient ################
ignored_days <- 8
# I will recycle some column to create a more descriptive type of patient category:
data[data[10]==1,37] <- "Amb"
data[data[10]==2,37] <- "Hosp"
data[data[14]==1,37] <- "Int"
temp <- which(data[13]!="9999-99-99")
data[temp,37] <- paste(data[temp,37],"D")
# UPDATE 2021-10-14: get the time each type of patient takes to seek for help
print("Tiempo promedio para buscar atención - Difuntos")
mean(time_to_seek_help[data[13]!="9999-99-99",3]) # dead patients
# Time difference of 5.367871
print("Tiempo promedio para buscar atención - Ambulatorios Supervivientes")
mean(time_to_seek_help[data[,37]=="Amb",3]) # ambulatory-survived
# Time difference of 2.968433
print("Tiempo promedio para buscar atención - Ambulatorios Difuntos")
mean(time_to_seek_help[data[,37]=="Amb D",3]) # ambulatory-died
# Time difference of 4.850377
print("Tiempo promedio para buscar atención - Hospitalizados Supervivientes")
mean(time_to_seek_help[data[,37]=="Hosp",3]) # hospitalized-survived
# Time difference of 5.084138
print("Tiempo promedio para buscar atención - Hospitalizados Difuntos")
mean(time_to_seek_help[data[,37]=="Hosp D",3]) # hospitalized-died
# Time difference of 5.345879
print("Tiempo promedio para buscar atención - Intubados Supervivientes")
mean(time_to_seek_help[data[,37]=="Int",3]) # Intubated-survived
# Time difference of 5.735443
print("Tiempo promedio para buscar atención - Intubados Difuntos")
mean(time_to_seek_help[data[,37]=="Int D",3]) # Intubated-died
# Time difference of 5.54612
print("Tipo de pacientes - Todos:")
i=37;test <- table(data[i]);test <- test[test>0];test
#     Amb   Amb D    Hosp  Hosp D     Int   Int D
# 5062469   13924  357986  240789   13377   70052

ignored_days <- 8
# I will recycle some column to create a more descriptive type of patient category:
last30days_conf[last30days_conf[10]==1,37] <- "Amb"
last30days_conf[last30days_conf[10]==2,37] <- "Hosp"
last30days_conf[last30days_conf[14]==1,37] <- "Int"
temp2 <- which(last30days_conf[13]!="9999-99-99")
last30days_conf[temp2,37] <- paste(last30days_conf[temp2,37],"D")
# UPDATE 2021-10-14: get the time each type of patient takes to seek for help
print("Tiempo promedio para buscar atención - Difuntos")
mean(time_to_seek_help_sub[last30days_conf[13]!="9999-99-99",3]) # dead patients
# Time difference of 2.507576
print("Tiempo promedio para buscar atención - Ambulatorios Supervivientes")
mean(time_to_seek_help_sub[last30days_conf[,37]=="Amb",3]) # ambulatory-survived
# Time difference of 2.090675
print("Tiempo promedio para buscar atención - Ambulatorios Difuntos")
mean(time_to_seek_help_sub[last30days_conf[,37]=="Amb D",3]) # ambulatory-died
# Time difference of 2.506649
print("Tiempo promedio para buscar atención - Hospitalizados Supervivientes")
mean(time_to_seek_help_sub[last30days_conf[,37]=="Hosp",3]) # hospitalized-survived
# Time difference of 2.22814
print("Tiempo promedio para buscar atención - Hospitalizados Difuntos")
mean(time_to_seek_help_sub[last30days_conf[,37]=="Hosp D",3]) # hospitalized-died
# Time difference of 2.5
print("Tiempo promedio para buscar atención - Intubados Supervivientes")
mean(time_to_seek_help_sub[last30days_conf[,37]=="Int",3]) # Intubated-survived
# Time difference of 2.020408
print("Tiempo promedio para buscar atención - Intubados Difuntos")
mean(time_to_seek_help_sub[last30days_conf[,37]=="Int D",3]) # Intubated-died
# Time difference of 2.55

print("Tipo de pacientes - Últimos 39 días confiables (sin últimos 15):")
i=37;test <- table(last30days_conf[i]);test <- test[test>0];test
#     Amb   Amb D    Hosp  Hosp D     Int   Int D
# 5388178   13935  362204  241590   13430   70263
# PART 1: First cross the type of patient and the ages
patient <- xtabs(rep(1,nrow(data))~data[,37]+data[,16], data=data)
ages <- as.numeric(colnames(patient)) # Get a list of actual ages (individual ages)
age_breaks <- c(seq(-1,99,5),200) # Construct desired breaks for bins
bins <- cut(ages,age_breaks, include.lowest=T) # Create bins
list <- tapply(ages,bins,"[") # Create a list of bins to recover
patient <- sapply(list, function(x) rowSums(patient[,as.character(x)])) # now a collated version, using them as names
colnames(patient) <- paste(sapply(list, function(x) range(x))[1,],sapply(list, function(x) range(x))[2,],sep="-")
factor <- rbind(colSums(patient[1:2,]), colSums(patient[1:2,]), colSums(patient[3:4,]), colSums(patient[3:4,]), colSums(patient[5:6,]), colSums(patient[5:6,]))
patient_r <- patient/factor*100
# matplot(t(patient_r[c(2,4,6),]), type="l",lty=1,lwd=2, main="Porcentaje de fallecimientos por tipo de paciente")
# matplot(t(patient_r[c(2,4,6),]), type="l",lty=1,lwd=2, main="Porcentaje de fallecimientos por tipo de paciente",las=2,ylim=c(0,100), ylab="% de fallecimientos por edad",xaxt='n',col=c("cornflowerblue", "coral1", "firebrick"))
# axis(1, las=2, at=seq(1,21),labels=colnames(patient))
totals <- rbind(colSums(patient[1:2,]), colSums(patient[3:4,]), colSums(patient[5:6,]))
totals <- t(apply(totals,1, function(x) x/colSums(totals)))
rownames(totals) <- c("Ambulatorio","Hospitalizado","Intubado")
pdf(paste0(prefix,"-",name,"-Tipo_paciente_edad.pdf"))
cols <- c("chartreuse3","bisque2","brown1")
par(oma = c(1, 1, 1, 3.5))
barplot(totals*100,las=2,col=cols, border=NA, main="Tipo de paciente por edad", ylab="Porcentaje de pacientes según atención requerida")
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE) # Create a new graphical scheme (right biased)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", pch=15, col=rev(cols),legend=rev(rownames(totals)), bty = "n",cex=0.8)
dev.off()
# Now in barplots
patient_r <- rbind(patient_r[1:2,], rep(10,ncol(patient_r)), patient_r[3:4,], rep(10,ncol(patient_r)), patient_r[5:6,])

pdf(paste0(prefix,"-",name,"-Tipo_paciente_edad_superv.pdf"))
cols <- c("chartreuse3","darkgreen",NA,"bisque2","darkgoldenrod3",NA,"brown1","firebrick")
cols_spaces <- c("chartreuse3","darkgreen",NA,NA,NA,NA,NA,NA,"bisque2","darkgoldenrod3",NA,NA,NA,NA,NA,NA,"brown1","firebrick")
names_spaces <- c("Sobrevive","Fallece",NA,NA,NA,NA,NA,NA,"Sobrevive","Fallece",NA,NA,NA,NA,NA,NA,"Sobrevive","Fallece")
par(oma = c(1, 1, 1, 3)) # This is just a creative fix to plot the legend outside (adjust accordingly)
barplot(las=2,patient_r, col=cols, border=NA,yaxt='n', main="Porcentaje de fallecimientos por tipo de paciente y edad", ylab="Ambulatorio               Hospitalizado               Intubado")
axis(2, las=1, at=seq(0,100,25), labels=c("0","25","50","75","100"))
axis(2, las=1, at=seq(110,210,25), labels=c("0","25","50","75","100"))
axis(2, las=1, at=seq(220,320,25), labels=c("0","25","50","75","100"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE) # Create a new graphical scheme (right biased)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", pch=15, col=rev(cols_spaces),legend=rev(names_spaces), bty = "n") # Plot legend here (to the right)
dev.off()
# Added on 2022-05-24 to cope with missing values
fill_missing_items <- function(mat,vector){ # Gets a table and a vector with all rows that should be present, outputs an expanded row collection with missing dates for complete calendar in that range. rownames should have date format as %Y-%m-%d
	xtable <- as.data.frame(matrix(0,nrow=length(vector),ncol=ncol(mat)), stringsAsFactors = FALSE) # Create empty vessel for output
	rownames(xtable) <- vector # use the vector as rownames
	colnames(xtable) <- colnames(mat) # and inherit the names
	invisible(sapply(rownames(mat),function(x) {xtable[x,] <<- mat[x,]}))	# append the original values in the corresponding places (write to higher env variable
	return(xtable)
}
# SAME but for last 30 confidence days (15 last days are ignored)
# PART 1: First cross the type of patient and the ages
temp <- last30days_conf[,37]
patient2 <- xtabs(rep(1,nrow(last30days_conf))~temp+last30days_conf[,16], data=last30days_conf)
patient2 <- t(fill_missing_items(t(fill_missing_items(patient2,c("Amb","Amb D","Hosp","Hosp D","Int","Int D"))),as.character(0:120)))
ages2 <- as.numeric(colnames(patient2)) # Get a list of actual ages (individual ages)
age_breaks2 <- c(seq(-1,99,5),200) # Construct desired breaks for bins
bins2 <- cut(ages2,age_breaks2, include.lowest=T) # Create bins
list2 <- tapply(ages2,bins2,"[") # Create a list of bins to recover
# # # # # temp <- sapply(list2, function(x) patient2[,as.character(x)])
# # # # # for(i in names(temp)){
# # # # # #    print(class(temp[[i]]))
# # # # #    if(class(temp[[i]])!="table"){
# # # # #       print(temp[i])
# # # # #       cbind(unlist(temp[i)],rep(0,length(temp[i])))
# # # # #    }
# # # # # }
# # # # #
# # # # # patient <- sapply(list, function(x) rowSums(patient[,as.character(x)]))
# # # # # temp2 <- sapply(temp[lapply(temp,class)=="table"],function(x) rowSums(x))
# # # # # lentemp <- ncol(temp2)
# # # # # namestemp <- colnames(temp2)
# # # # # temp2 <- cbind(temp2,unlist(temp[lapply(temp,class)!="table"]))
patient2 <- sapply(list2, function(x) rowSums(patient2[,as.character(x)])) # now a collated version, using them as names
colnames(patient2) <- paste(sapply(list2, function(x) range(x))[1,],sapply(list2, function(x) range(x))[2,],sep="-")
factor2 <- rbind(colSums(patient2[1:2,]), colSums(patient2[1:2,]), colSums(patient2[3:4,]), colSums(patient2[3:4,]), colSums(patient2[5:6,]), colSums(patient2[5:6,]))
patient_r2 <- patient2/factor2*100
# matplot(t(patient_r[c(2,4,6),]), type="l",lty=1,lwd=2, main="Porcentaje de fallecimientos por tipo de paciente")
# matplot(t(patient_r[c(2,4,6),]), type="l",lty=1,lwd=2, main="Porcentaje de fallecimientos por tipo de paciente",las=2,ylim=c(0,100), ylab="% de fallecimientos por edad",xaxt='n',col=c("cornflowerblue", "coral1", "firebrick"))
# axis(1, las=2, at=seq(1,21),labels=colnames(patient))
totals2 <- rbind(colSums(patient2[1:2,]), colSums(patient2[3:4,]), colSums(patient2[5:6,]))
totals2 <- t(apply(totals2,1, function(x) x/colSums(totals2)))
rownames(totals2) <- c("Ambulatorio","Hospitalizado","Intubado")
pdf(paste0(prefix,"-",name,"-Tipo_paciente_edad_30_dias_conf.pdf"))
cols <- c("chartreuse3","bisque2","brown1")
par(oma = c(1, 1, 1, 3.5))
barplot(totals2*100,las=2,col=cols, border=NA, main="Tipo de paciente por edad", ylab="Porcentaje de pacientes según atención requerida")
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE) # Create a new graphical scheme (right biased)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", pch=15, col=rev(cols),legend=rev(rownames(totals)), bty = "n",cex=0.8)
dev.off()
# Now in barplots
patient_r2 <- rbind(patient_r2[1:2,], rep(10,ncol(patient_r2)), patient_r2[3:4,], rep(10,ncol(patient_r2)), patient_r2[5:6,])

pdf(paste0(prefix,"-",name,"-Tipo_paciente_edad_superv_30_dias_conf.pdf"))
cols <- c("chartreuse3","darkgreen",NA,"bisque2","darkgoldenrod3",NA,"brown1","firebrick")
cols_spaces <- c("chartreuse3","darkgreen",NA,NA,NA,NA,NA,NA,"bisque2","darkgoldenrod3",NA,NA,NA,NA,NA,NA,"brown1","firebrick")
names_spaces <- c("Sobrevive","Fallece",NA,NA,NA,NA,NA,NA,"Sobrevive","Fallece",NA,NA,NA,NA,NA,NA,"Sobrevive","Fallece")
par(oma = c(1, 1, 1, 3)) # This is just a creative fix to plot the legend outside (adjust accordingly)
barplot(las=2,patient_r2, col=cols, border=NA,yaxt='n', main="Porcentaje de fallecimientos por tipo de paciente y edad", ylab="Ambulatorio               Hospitalizado               Intubado")
axis(2, las=1, at=seq(0,100,25), labels=c("0","25","50","75","100"))
axis(2, las=1, at=seq(110,210,25), labels=c("0","25","50","75","100"))
axis(2, las=1, at=seq(220,320,25), labels=c("0","25","50","75","100"))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE) # Create a new graphical scheme (right biased)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", pch=15, col=rev(cols_spaces),legend=rev(names_spaces), bty = "n") # Plot legend here (to the right)
dev.off()


# PART 2: How has hospitalization changed?
patient <- xtabs(rep(1,nrow(data))~data[,11]+data[,37], data=data)
# first <- sum(cumsum(rowSums(patient))<20)-3 # Determine the first item where cummalitive cases are >10
# write.table(patient, "type_of_patient_per_date.tsv", sep="\t", quote=FALSE, row.names=T, col.names=NA)
first <- 1 # start at day 1
last <- nrow(patient)-finalcut # Determine the last item (the past 8 days are not useful but 3 days prior to our cutoff us requiered for the mean)
patient <- patient[first:last,]
roll.p <- sapply(4:(nrow(patient)-3),function(y) {apply(patient,2, function(x) mean(as.numeric(x[(y-3):(y+3)])))}) # Create rolling average for 7 days (2 nested functions, one for calculation of the average, one for advancing within matrix limits)
colnames(roll.p) <- row.names(patient)[4:(nrow(patient)-3)]
roll.p <- t(roll.p) # Just aesthetic, as I'd rather used multiple rows than columns
lroll.p <- log10(roll.p) # Create an additional set for log scale
lroll.p[!is.finite(lroll.p)] <- 0
lroll.p[lroll.p<0]=0
# Finally, calculate the per 100K people
norm.p_day <- apply(roll.p, 2, function(x) x/rowSums(roll.p)*100) # This is now expressed as relative to dialy totals
norm.p_age <- t(apply(roll.p, 1, function(x) x/colSums(roll.p)*100))

########## Plot type of patient ################
# Create combinations for line color and width
lty <- c("solid","dashed","dotted","dotdash","longdash","twodash")
# col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","wheat1","magenta1","limegreen","darkorange2","darkgray")
col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","purple3","magenta1","limegreen","darkorange2","darkgray")
lwd <- c(1.3,2)
pars <- expand.grid(col = col, lty = lty, lwd = lwd, stringsAsFactors = FALSE) # This will create all
# First the raw rolling average
cex.X=0.53
# dates <- unique(sort(c(grep("01$",rownames(roll.p)),nrow(roll.p),1)))
# if((dates[length(dates)]-dates[length(dates)-1])<15){dates <- dates[-(length(dates)-1)]} # Fix dates showing
# if((dates[2]-dates[1])<10){dates <- dates[-2]}
dates <- rownames(roll.p) # UPDATE 2022-11-30: there is no more space for this to fit, we have thus changed the actual printing scheme for dates
dates <- printable_dates(dates)
name="Tipo de paciente"
colnames(roll.p) <- c("Amb Sobr","Amb Def","Hosp Sobr","Hosp Def","Int Sobr","Int Def")
# Plot by type of patient
pdf(paste0(prefix,"-",sub(" ", "_", name),"-total_Tipo_Paciente.pdf"))
par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
with(pars[1:ncol(roll.p),],matplot(roll.p[,1:ncol(roll.p)], type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales Sobrevive/Defuncion"), ylab = "Promedio diario", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(roll.p)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = colnames(roll.p), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(roll.p)], lty = pars$lty[1:ncol(roll.p)], lwd = pars$lwd[1:ncol(roll.p)-1], cex = cex.X)
dev.off()

# Next, a log10 visualization
pdf(paste0(prefix,"-",sub(" ", "_", name),"-total_log_edad.pdf"))
par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
with(pars[1:ncol(lroll.p),],matplot(lroll.p, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales (log10) por tipo de paciente"), ylab = "Promedio diario (log10)", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(lroll.p)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(lroll.p)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(lroll.p)], lty = pars$lty[1:ncol(lroll.p)], lwd = pars$lwd[1:ncol(lroll.p)], cex = cex.X)
dev.off()

# Next, same thing but normalized data
pdf(paste0(prefix,"-",sub(" ", "_", name),"-total_norm_por_dia.pdf"))
par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
with(pars[1:ncol(norm.p_day),],matplot(norm.p_day, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total diario"), ylab = "Porcentaje del promedio diario", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(norm.p_day)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(norm.p_day)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(norm.p_day)], lty = pars$lty[1:ncol(norm.p_day)], lwd = pars$lwd[1:ncol(norm.p_day)], cex = cex.X)
dev.off()

# Next, same thing but normalized data
pdf(paste0(prefix,"-",sub(" ", "_", name),"-total_norm_por_rango.pdf"))
par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
with(pars[1:ncol(norm.p_age),],matplot(norm.p_age, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total por rango"), ylab = "", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(norm.p_age)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(norm.p_age)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(norm.p_age)], lty = pars$lty[1:ncol(norm.p_age)], lwd = pars$lwd[1:ncol(norm.p_age)], cex = cex.X)
dev.off()

# By day with areaplot
library(areaplot)
pdf(paste0(prefix,"-",sub(" ", "_", name),"-por_dia.pdf"))
par(oma = c(1, 1, 1, 3)) #This is just a creative fix to plot the legend outside, i
cols <- c("chartreuse3","darkgreen","bisque2","darkgoldenrod3","brown1","firebrick")
areaplot(norm.p_day,col=cols,ylim=c(50,100),border=NA, las=2, xlab=NA, main="Porcentaje de tipo de paciente y supervivencia", ylab="Porcentaje de pacientes diario (Amb siempre >50%)", xaxt='n')
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(norm.p_day)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", pch=15, col=rev(cols),legend=rev(c("Amb sobr","Amb def","Hosp sobr","Hosp def","Int sobr","Int def")), bty = "n", cex=0.8) # Plot legend here (to the right)
dev.off()

pdf(paste0(prefix,"-",sub(" ", "_", name),"-total_norm_por_rango.pdf"))
par(oma = c(1, 1, 1, 3)) #This is just a creative fix to plot the legend outside, i
matplot(norm.p_age, type = 'l', col = cols, lty = 1, lwd = 1.5, main = paste0(name," normalizado por grupo"), ylab = "", xlab = "", las = 2, xaxt = 'n')
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(norm.p_age)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", pch=15, col=rev(cols),legend=rev(c("Amb sobr","Amb def","Hosp sobr","Hosp def","Int sobr","Int def")), bty = "n", cex=0.8) # Plot legend here (to the right)
dev.off()


### Sex ###
sex_positive <- table(data[6]);names(sex_positive) <- c("F","M"); sex_positive
#       F       M
# 1967626 1951361
data[data[6]==1,38] <- "F" # Add new column with f and m
data[data[6]==2,38] <- "M"
temp <- which(data[13]!="9999-99-99") # get those that died
data[temp,38] <- paste0(data[temp,38],"-D") # and append a -D for deceased
data[-temp,38] <- paste0(data[-temp,38],"-S") # and append a -S for surviving
i=38;test <- table(data[i]);test <- test[test>0];test
#       F       M
# 3192974 2896626
#
#     F-D     F-S     M-D     M-S
#  125377 3067597  200411 2696215


sex_conf <- xtabs(rep(1,nrow(data))~data[,11]+data[,38], data=data)
sex_conf <- cbind(sex_conf, "F"=rowSums(sex_conf[,1:2]),"M"=rowSums(sex_conf[,3:4])) # Add collated F and M

first <- sum(cumsum(rowSums(sex_conf[,1:4]))<20)-3 # Determine the first item
last <- nrow(sex_conf)-finalcut # Determine the last item (the past 8 days are not useful but 3 days prior to our cutoff us requiered for the mean) # UPDATE 2021-08-20 Changed it to a variable (as well as the other afterwards)
sex_conf <- sex_conf[first:last,]
roll.c <- sapply(4:(nrow(sex_conf)-3),function(y) {apply(sex_conf,2, function(x) mean(as.numeric(x[(y-3):(y+3)])))}) # Create rolling average for 7 days (2 nested functions, one for calculation of the average, one for advancing within matrix limits)
colnames(roll.c) <- row.names(sex_conf)[4:(nrow(sex_conf)-3)]
roll.c <- t(roll.c) # Just aesthetic, as I'd rather used multiple rows than columns
lroll.c <- log10(roll.c) # Create an additional set for log scale
lroll.c[!is.finite(lroll.c)] <- 0
lroll.c[lroll.c<0]=0
# Finally, calculate the per 100K people
norm.c_day <- apply(roll.c, 2, function(x) x/rowSums(roll.c[,5:6])*100) # This is now expressed as relative to dialy totals
norm.c_age <- t(apply(roll.c, 1, function(x) x/colSums(roll.c)*100))

########## Plot sex ################
# Create combinations for line color and width
lty <- c("solid","dashed","dotted","dotdash","longdash","twodash")
# col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","wheat1","magenta1","limegreen","darkorange2","darkgray")
col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","purple3","magenta1","limegreen","darkorange2","darkgray")
lwd <- c(1.3,2)
pars <- expand.grid(col = col, lty = lty, lwd = lwd, stringsAsFactors = FALSE) # This will create all
# First the raw rolling average
cex.X=0.53
# dates <- unique(sort(c(grep("01$",rownames(roll.c)),nrow(roll.c),1)))
# if((dates[length(dates)]-dates[length(dates)-1])<15){dates <- dates[-(length(dates)-1)]} # Fix dates showing
# if((dates[2]-dates[1])<10){dates <- dates[-2]}
dates <- rownames(roll.c) # UPDATE 2022-11-30: there is no more space for this to fit, we have thus changed the actual printing scheme for dates
dates <- printable_dates(dates)

# Next, confirmed cases by sex
pdf(paste0(prefix,"-",name,"-total_sexo.pdf"))
par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
with(pars[1:(ncol(roll.c)-2),],matplot(roll.c[,1:(ncol(roll.c)-2)], type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales por rango de sexo"), ylab = "Promedio diario de confirmados", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(roll.c)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = colnames(roll.c)[1:(ncol(roll.c)-2)], xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:(ncol(roll.c)-2)], lty = pars$lty[1:(ncol(roll.c)-2)], lwd = pars$lwd[1:(ncol(roll.c)-2)], cex = cex.X)
dev.off()

# Next, a log10 visualization
pdf(paste0(prefix,"-",name,"-total_log_sexo.pdf"))
par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
with(pars[1:(ncol(lroll.c)-2),],matplot(lroll.c[,1:(ncol(roll.c)-2)], type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales (log10) por rango de sexo"), ylab = "Promedio diario de confirmados (log10)", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(lroll.c)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(roll.c)[1:(ncol(roll.c)-2)]), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:(ncol(lroll.c)-2)], lty = pars$lty[1:(ncol(lroll.c)-2)], lwd = pars$lwd[1:(ncol(lroll.c)-2)], cex = cex.X)
dev.off()

# Next, same thing but normalized data
pdf(paste0(prefix,"-",name,"-total_norm_por_dia_sex_desglosa.pdf"))
par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
with(pars[1:(ncol(lroll.c)-2),],matplot(norm.c_day[,1:(ncol(roll.c)-2)], type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total diario"), ylab = "Porcentaje del promedio diario", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(norm.c_day)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(norm.c_day[,1:(ncol(roll.c)-2)])), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:(ncol(lroll.c)-2)], lty = pars$lty[1:(ncol(lroll.c)-2)], lwd = pars$lwd[1:(ncol(lroll.c)-2)], cex = cex.X)
dev.off()
pdf(paste0(prefix,"-",name,"-total_norm_por_dia_sex_FyM.pdf"))
par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
with(pars[5:6,],matplot(norm.c_day[,5:6], type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total diario"), ylab = "Porcentaje del promedio diario", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(norm.c_day)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(norm.c_day[,5:6])), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[5:6], lty = pars$lty[5:6], lwd = pars$lwd[5:6], cex = cex.X)
dev.off()

# Next, same thing but normalized data
pdf(paste0(prefix,"-",name,"-total_norm_por_rango.pdf"))
par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
with(pars[1:ncol(norm.c_age),],matplot(norm.c_age, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total por rango"), ylab = "", xlab = "", las = 2, xaxt = 'n'))
mtext("Promedio 7 días - a partir de 20 casos - día de registro")
axis(1, las=2, at = dates, labels = rownames(norm.c_age)[dates], cex.axis = 0.75)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = gsub("\"","",colnames(norm.c_age)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(norm.c_age)], lty = pars$lty[1:ncol(norm.c_age)], lwd = pars$lwd[1:ncol(norm.c_age)], cex = cex.X)
dev.off()

# ########## Females and Type of patient ################
# fem <- data[data[6]==1,]
# # I will recycle some column to create a more descriptive type of patient category:
# fem[fem[10]==1,37] <- "Amb"
# fem[fem[10]==2,37] <- "Hosp"
# fem[fem[14]==1,37] <- "Int"
# temp <- which(fem[13]!="9999-99-99")
# fem[temp,37] <- paste(fem[temp,37],"D")
# 
# 
# i=37;test <- table(fem[i]);test <- test[test>0];test
# #     Amb   Amb D    Hosp  Hosp D     Int   Int D
# # 2899484    4890  163119   94628    4994   25859
# 
# # PART 1: First cross the type of patient and the ages
# patient <- xtabs(rep(1,nrow(fem))~fem[,37]+fem[,16], data=fem)
# ages <- as.numeric(colnames(patient)) # Get a list of actual ages (individual ages)
# age_breaks <- c(seq(-1,99,5),200) # Construct desired breaks for bins
# bins <- cut(ages,age_breaks, include.lowest=T) # Create bins
# list <- tapply(ages,bins,"[") # Create a list of bins to recover
# patient <- sapply(list, function(x) rowSums(patient[,as.character(x)])) # now a collated version, using them as names
# colnames(patient) <- paste(sapply(list, function(x) range(x))[1,],sapply(list, function(x) range(x))[2,],sep="-")
# factor <- rbind(colSums(patient[1:2,]), colSums(patient[1:2,]), colSums(patient[3:4,]), colSums(patient[3:4,]), colSums(patient[5:6,]), colSums(patient[5:6,]))
# patient_r <- patient/factor*100
# # matplot(t(patient_r[c(2,4,6),]), type="l",lty=1,lwd=2, main="Porcentaje de fallecimientos por tipo de paciente")
# # matplot(t(patient_r[c(2,4,6),]), type="l",lty=1,lwd=2, main="Porcentaje de fallecimientos por tipo de paciente",las=2,ylim=c(0,100), ylab="% de fallecimientos por edad",xaxt='n',col=c("cornflowerblue", "coral1", "firebrick"))
# # axis(1, las=2, at=seq(1,21),labels=colnames(patient))
# totals <- rbind(colSums(patient[1:2,]), colSums(patient[3:4,]), colSums(patient[5:6,]))
# totals <- t(apply(totals,1, function(x) x/colSums(totals)))
# rownames(totals) <- c("Ambulatorio","Hospitalizado","Intubado")
# pdf(paste0(prefix,"-",name,"-Tipo_paciente_edad_fem.pdf"))
# cols <- c("chartreuse3","bisque2","brown1")
# par(oma = c(1, 1, 1, 3.5))
# barplot(totals*100,las=2,col=cols, border=NA, main="Tipo de paciente por edad", ylab="Porcentaje de pacientes según atención requerida")
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE) # Create a new graphical scheme (right biased)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", pch=15, col=rev(cols),legend=rev(rownames(totals)), bty = "n",cex=0.8)
# dev.off()
# # Now in barplots
# patient_r <- rbind(patient_r[1:2,], rep(10,ncol(patient_r)), patient_r[3:4,], rep(10,ncol(patient_r)), patient_r[5:6,])
# 
# pdf(paste0(prefix,"-",name,"-Tipo_paciente_edad_superv_fem.pdf"))
# cols <- c("chartreuse3","darkgreen",NA,"bisque2","darkgoldenrod3",NA,"brown1","firebrick")
# cols_spaces <- c("chartreuse3","darkgreen",NA,NA,NA,NA,NA,NA,"bisque2","darkgoldenrod3",NA,NA,NA,NA,NA,NA,"brown1","firebrick")
# names_spaces <- c("Sobrevive","Fallece",NA,NA,NA,NA,NA,NA,"Sobrevive","Fallece",NA,NA,NA,NA,NA,NA,"Sobrevive","Fallece")
# par(oma = c(1, 1, 1, 3)) # This is just a creative fix to plot the legend outside (adjust accordingly)
# barplot(las=2,patient_r, col=cols, border=NA,yaxt='n', main="Porcentaje de fallecimientos por tipo de paciente y edad", ylab="Ambulatorio               Hospitalizado               Intubado")
# axis(2, las=1, at=seq(0,100,25), labels=c("0","25","50","75","100"))
# axis(2, las=1, at=seq(110,210,25), labels=c("0","25","50","75","100"))
# axis(2, las=1, at=seq(220,320,25), labels=c("0","25","50","75","100"))
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE) # Create a new graphical scheme (right biased)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", pch=15, col=rev(cols_spaces),legend=rev(names_spaces), bty = "n") # Plot legend here (to the right)
# dev.off()
# 
# # PART 2: How has hospitalization changed?
# patient <- xtabs(rep(1,nrow(fem))~fem[,11]+fem[,37], data=fem)
# # first <- sum(cumsum(rowSums(patient))<20)-3 # Determine the first item where cummalitive cases are >10
# first <- 1 # start at day 1
# last <- nrow(patient)-finalcut # Determine the last item (the past 8 days are not useful but 3 days prior to our cutoff us requiered for the mean)
# patient <- patient[first:last,]
# roll.p <- sapply(4:(nrow(patient)-3),function(y) {apply(patient,2, function(x) mean(as.numeric(x[(y-3):(y+3)])))}) # Create rolling average for 7 days (2 nested functions, one for calculation of the average, one for advancing within matrix limits)
# colnames(roll.p) <- row.names(patient)[4:(nrow(patient)-3)]
# roll.p <- t(roll.p) # Just aesthetic, as I'd rather used multiple rows than columns
# lroll.p <- log10(roll.p) # Create an additional set for log scale
# lroll.p[!is.finite(lroll.p)] <- 0
# lroll.p[lroll.p<0]=0
# # Finally, calculate the per 100K people
# norm.p_day <- apply(roll.p, 2, function(x) x/rowSums(roll.p)*100) # This is now expressed as relative to dialy totals
# norm.p_age <- t(apply(roll.p, 1, function(x) x/colSums(roll.p)*100))
# 
# 
# ########## Plot type of patient ################
# # Create combinations for line color and width
# lty <- c("solid","dashed","dotted","dotdash","longdash","twodash")
# # col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","wheat1","magenta1","limegreen","darkorange2","darkgray")
# col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","purple3","magenta1","limegreen","darkorange2","darkgray")
# lwd <- c(1.3,2)
# pars <- expand.grid(col = col, lty = lty, lwd = lwd, stringsAsFactors = FALSE) # This will create all
# # First the raw rolling average
# cex.X=0.53
# dates <- unique(sort(c(grep("01$",rownames(roll.p)),nrow(roll.p),1)))
# if((dates[length(dates)]-dates[length(dates)-1])<15){dates <- dates[-(length(dates)-1)]} # Fix dates showing
# if((dates[2]-dates[1])<10){dates <- dates[-2]}
# name="Tipo de paciente"
# colnames(roll.p) <- c("Amb Sobr","Amb Def","Hosp Sobr","Hosp Def","Int Sobr","Int Def")
# # Plot by type of patient
# pdf(paste0(prefix,"-",name,"-total_Tipo_Paciente_fem.pdf"))
# par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
# with(pars[1:ncol(roll.p),],matplot(roll.p[,1:ncol(roll.p)], type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales Sobrevive/Defuncion"), ylab = "Promedio diario", xlab = "", las = 2, xaxt = 'n'))
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(roll.p)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", legend = colnames(roll.p), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(roll.p)], lty = pars$lty[1:ncol(roll.p)], lwd = pars$lwd[1:ncol(roll.p)-1], cex = cex.X)
# dev.off()
# 
# # Next, a log10 visualization
# pdf(paste0(prefix,"-",name,"-total_log_edad_fem.pdf"))
# par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
# with(pars[1:ncol(lroll.p),],matplot(lroll.p, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales (log10) por tipo de paciente"), ylab = "Promedio diario (log10)", xlab = "", las = 2, xaxt = 'n'))
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(lroll.p)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", legend = gsub("\"","",colnames(lroll.p)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(lroll.p)], lty = pars$lty[1:ncol(lroll.p)], lwd = pars$lwd[1:ncol(lroll.p)], cex = cex.X)
# dev.off()
# 
# # Next, same thing but normalized fem
# pdf(paste0(prefix,"-",name,"-total_norm_por_dia_fem.pdf"))
# par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
# with(pars[1:ncol(norm.p_day),],matplot(norm.p_day, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total diario"), ylab = "Porcentaje del promedio diario", xlab = "", las = 2, xaxt = 'n'))
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(norm.p_day)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", legend = gsub("\"","",colnames(norm.p_day)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(norm.p_day)], lty = pars$lty[1:ncol(norm.p_day)], lwd = pars$lwd[1:ncol(norm.p_day)], cex = cex.X)
# dev.off()
# 
# # Next, same thing but normalized fem
# pdf(paste0(prefix,"-",name,"-total_norm_por_rango_fem.pdf"))
# par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
# with(pars[1:ncol(norm.p_age),],matplot(norm.p_age, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total por rango"), ylab = "", xlab = "", las = 2, xaxt = 'n'))
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(norm.p_age)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", legend = gsub("\"","",colnames(norm.p_age)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(norm.p_age)], lty = pars$lty[1:ncol(norm.p_age)], lwd = pars$lwd[1:ncol(norm.p_age)], cex = cex.X)
# dev.off()
# 
# # By day with areaplot
# library(areaplot)
# pdf(paste0(prefix,"-",name,"-por_dia_cummulative_fem.pdf"))
# par(oma = c(1, 1, 1, 3)) #This is just a creative fix to plot the legend outside, i
# cols <- c("chartreuse3","darkgreen","bisque2","darkgoldenrod3","brown1","firebrick")
# areaplot(norm.p_day,col=cols,ylim=c(50,100),border=NA, las=2, xlab=NA, main="Porcentaje de tipo de paciente y supervivencia", ylab="Porcentaje de pacientes diario (Amb siempre >50%)", xaxt='n')
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(norm.p_day)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", pch=15, col=rev(cols),legend=rev(c("Amb sobr","Amb def","Hosp sobr","Hosp def","Int sobr","Int def")), bty = "n", cex=0.8) # Plot legend here (to the right)
# dev.off()
# 
# pdf(paste0(prefix,"-",name,"-total_norm_por_rango_fem.pdf"))
# par(oma = c(1, 1, 1, 3)) #This is just a creative fix to plot the legend outside, i
# matplot(norm.p_age, type = 'l', col = cols, lty = 1, lwd = 1.5, main = paste0(name," normalizado por grupo"), ylab = "", xlab = "", las = 2, xaxt = 'n')
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(norm.p_age)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", pch=15, col=rev(cols),legend=rev(c("Amb sobr","Amb def","Hosp sobr","Hosp def","Int sobr","Int def")), bty = "n", cex=0.8) # Plot legend here (to the right)
# dev.off()
# 
# ########## Males and Type of patient ################
# male <- data[data[6]==2,]
# # I will recycle some column to create a more descriptive type of patient category:
# male[male[10]==1,37] <- "Amb"
# male[male[10]==2,37] <- "Hosp"
# male[male[14]==1,37] <- "Int"
# temp <- which(male[13]!="9999-99-99")
# male[temp,37] <- paste(male[temp,37],"D")
# i=37;test <- table(male[i]);test <- test[test>0];test
# #     Amb   Amb D    Hosp  Hosp D     Int   Int D
# # 2488694    9045  199085  146962    8436   44404
# # PART 1: First cross the type of patient and the ages
# patient <- xtabs(rep(1,nrow(male))~male[,37]+male[,16], data=male)
# ages <- as.numeric(colnames(patient)) # Get a list of actual ages (individual ages)
# age_breaks <- c(seq(-1,99,5),200) # Construct desired breaks for bins
# bins <- cut(ages,age_breaks, include.lowest=T) # Create bins
# list <- tapply(ages,bins,"[") # Create a list of bins to recover
# patient <- sapply(list, function(x) rowSums(patient[,as.character(x)])) # now a collated version, using them as names
# colnames(patient) <- paste(sapply(list, function(x) range(x))[1,],sapply(list, function(x) range(x))[2,],sep="-")
# factor <- rbind(colSums(patient[1:2,]), colSums(patient[1:2,]), colSums(patient[3:4,]), colSums(patient[3:4,]), colSums(patient[5:6,]), colSums(patient[5:6,]))
# patient_r <- patient/factor*100
# # matplot(t(patient_r[c(2,4,6),]), type="l",lty=1,lwd=2, main="Porcentaje de fallecimientos por tipo de paciente")
# # matplot(t(patient_r[c(2,4,6),]), type="l",lty=1,lwd=2, main="Porcentaje de fallecimientos por tipo de paciente",las=2,ylim=c(0,100), ylab="% de fallecimientos por edad",xaxt='n',col=c("cornflowerblue", "coral1", "firebrick"))
# # axis(1, las=2, at=seq(1,21),labels=colnames(patient))
# totals <- rbind(colSums(patient[1:2,]), colSums(patient[3:4,]), colSums(patient[5:6,]))
# totals <- t(apply(totals,1, function(x) x/colSums(totals)))
# rownames(totals) <- c("Ambulatorio","Hospitalizado","Intubado")
# pdf(paste0(prefix,"-Tipo_paciente_edad_male.pdf"))
# cols <- c("chartreuse3","bisque2","brown1")
# par(oma = c(1, 1, 1, 3.5))
# barplot(totals*100,las=2,col=cols, border=NA, main="Tipo de paciente por edad", ylab="Porcentaje de pacientes según atención requerida")
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE) # Create a new graphical scheme (right biased)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", pch=15, col=rev(cols),legend=rev(rownames(totals)), bty = "n",cex=0.8)
# dev.off()
# # Now in barplots
# patient_r <- rbind(patient_r[1:2,], rep(10,ncol(patient_r)), patient_r[3:4,], rep(10,ncol(patient_r)), patient_r[5:6,])
# 
# pdf(paste0(prefix,"-",name,"-Tipo_paciente_edad_superv_male.pdf"))
# cols <- c("chartreuse3","darkgreen",NA,"bisque2","darkgoldenrod3",NA,"brown1","firebrick")
# cols_spaces <- c("chartreuse3","darkgreen",NA,NA,NA,NA,NA,NA,"bisque2","darkgoldenrod3",NA,NA,NA,NA,NA,NA,"brown1","firebrick")
# names_spaces <- c("Sobrevive","Fallece",NA,NA,NA,NA,NA,NA,"Sobrevive","Fallece",NA,NA,NA,NA,NA,NA,"Sobrevive","Fallece")
# par(oma = c(1, 1, 1, 3)) # This is just a creative fix to plot the legend outside (adjust accordingly)
# barplot(las=2,patient_r, col=cols, border=NA,yaxt='n', main="Porcentaje de fallecimientos por tipo de paciente y edad", ylab="Ambulatorio               Hospitalizado               Intubado")
# axis(2, las=1, at=seq(0,100,25), labels=c("0","25","50","75","100"))
# axis(2, las=1, at=seq(110,210,25), labels=c("0","25","50","75","100"))
# axis(2, las=1, at=seq(220,320,25), labels=c("0","25","50","75","100"))
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE) # Create a new graphical scheme (right biased)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", pch=15, col=rev(cols_spaces),legend=rev(names_spaces), bty = "n") # Plot legend here (to the right)
# dev.off()
# 
# # PART 2: How has hospitalization changed?
# patient <- xtabs(rep(1,nrow(male))~male[,11]+male[,37], data=male)
# # first <- sum(cumsum(rowSums(patient))<20)-3 # Determine the first item where cummaleitive cases are >10
# first <- 1 # start at day 1
# last <- nrow(patient)-finalcut # Determine the last item (the past 8 days are not useful but 3 days prior to our cutoff us requiered for the mean)
# patient <- patient[first:last,]
# roll.p <- sapply(4:(nrow(patient)-3),function(y) {apply(patient,2, function(x) mean(as.numeric(x[(y-3):(y+3)])))}) # Create rolling average for 7 days (2 nested functions, one for calculation of the average, one for advancing within matrix limits)
# colnames(roll.p) <- row.names(patient)[4:(nrow(patient)-3)]
# roll.p <- t(roll.p) # Just aesthetic, as I'd rather used multiple rows than columns
# lroll.p <- log10(roll.p) # Create an additional set for log scale
# lroll.p[!is.finite(lroll.p)] <- 0
# lroll.p[lroll.p<0]=0
# # Finally, calculate the per 100K people
# norm.p_day <- apply(roll.p, 2, function(x) x/rowSums(roll.p)*100) # This is now expressed as relative to dialy totals
# norm.p_age <- t(apply(roll.p, 1, function(x) x/colSums(roll.p)*100))
# 
# 
# ########## Plot type of patient ################
# # Create combinations for line color and width
# lty <- c("solid","dashed","dotted","dotdash","longdash","twodash")
# # col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","wheat1","magenta1","limegreen","darkorange2","darkgray")
# col <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3','mediumorchid2', 'turquoise3', 'wheat4','slategray2',"black","coral1","aquamarine2","blue2","violetred2","palegreen3","purple3","magenta1","limegreen","darkorange2","darkgray")
# lwd <- c(1.3,2)
# pars <- expand.grid(col = col, lty = lty, lwd = lwd, stringsAsFactors = FALSE) # This will create all
# # First the raw rolling average
# cex.X=0.53
# dates <- unique(sort(c(grep("01$",rownames(roll.p)),nrow(roll.p),1)))
# if((dates[length(dates)]-dates[length(dates)-1])<15){dates <- dates[-(length(dates)-1)]} # Fix dates showing
# if((dates[2]-dates[1])<10){dates <- dates[-2]}
# name="Tipo de paciente"
# colnames(roll.p) <- c("Amb Sobr","Amb Def","Hosp Sobr","Hosp Def","Int Sobr","Int Def")
# # Plot by type of patient
# pdf(paste0(prefix,"-",name,"-total_Tipo Paciente_male.pdf"))
# par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
# with(pars[1:ncol(roll.p),],matplot(roll.p[,1:ncol(roll.p)], type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales Sobrevive/Defuncion"), ylab = "Promedio diario", xlab = "", las = 2, xaxt = 'n'))
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(roll.p)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", legend = colnames(roll.p), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(roll.p)], lty = pars$lty[1:ncol(roll.p)], lwd = pars$lwd[1:ncol(roll.p)-1], cex = cex.X)
# dev.off()
# 
# # Next, a log10 visualization
# pdf(paste0(prefix,"-",name,"-total_log_edad_male.pdf"))
# par(oma = c(1, 1, 1, 2)) # This is just a creative fix to plot the legend outside
# with(pars[1:ncol(lroll.p),],matplot(lroll.p, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Totales (log10) por tipo de paciente"), ylab = "Promedio diario (log10)", xlab = "", las = 2, xaxt = 'n'))
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(lroll.p)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", legend = gsub("\"","",colnames(lroll.p)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(lroll.p)], lty = pars$lty[1:ncol(lroll.p)], lwd = pars$lwd[1:ncol(lroll.p)], cex = cex.X)
# dev.off()
# 
# # Next, same thing but normalized male
# pdf(paste0(prefix,"-",name,"-total_norm_por_dia_male.pdf"))
# par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
# with(pars[1:ncol(norm.p_day),],matplot(norm.p_day, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total diario"), ylab = "Porcentaje del promedio diario", xlab = "", las = 2, xaxt = 'n'))
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(norm.p_day)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", legend = gsub("\"","",colnames(norm.p_day)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(norm.p_day)], lty = pars$lty[1:ncol(norm.p_day)], lwd = pars$lwd[1:ncol(norm.p_day)], cex = cex.X)
# dev.off()
# 
# # Next, same thing but normalized male
# pdf(paste0(prefix,"-",name,"-total_norm_por_rango_male.pdf"))
# par(oma = c(1, 1, 1, 2)) #This is just a creative fix to plot the legend outside, i
# with(pars[1:ncol(norm.p_age),],matplot(norm.p_age, type = 'l', col = col, lty = lty, lwd = lwd, main = paste0(name," Porcentaje del total por rango"), ylab = "", xlab = "", las = 2, xaxt = 'n'))
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(norm.p_age)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", legend = gsub("\"","",colnames(norm.p_age)), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", col = pars$col[1:ncol(norm.p_age)], lty = pars$lty[1:ncol(norm.p_age)], lwd = pars$lwd[1:ncol(norm.p_age)], cex = cex.X)
# dev.off()
# 
# # By day with areaplot
# library(areaplot)
# pdf(paste0(prefix,"-",name,"-por_dia_cummulative_male.pdf"))
# par(oma = c(1, 1, 1, 3)) #This is just a creative fix to plot the legend outside, i
# cols <- c("chartreuse3","darkgreen","bisque2","darkgoldenrod3","brown1","firebrick")
# areaplot(norm.p_day,col=cols,ylim=c(50,100),border=NA, las=2, xlab=NA, main="Porcentaje de tipo de paciente y supervivencia", ylab="Porcentaje de pacientes diario (Amb siempre >50%)", xaxt='n')
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(norm.p_day)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", pch=15, col=rev(cols),legend=rev(c("Amb sobr","Amb def","Hosp sobr","Hosp def","Int sobr","Int def")), bty = "n", cex=0.8) # Plot legend here (to the right)
# dev.off()
# 
# pdf(paste0(prefix,"-",name,"-total_norm_por_rango_male.pdf"))
# par(oma = c(1, 1, 1, 3)) #This is just a creative fix to plot the legend outside, i
# matplot(norm.p_age, type = 'l', col = cols, lty = 1, lwd = 1.5, main = paste0(name," normalizado por grupo"), ylab = "", xlab = "", las = 2, xaxt = 'n')
# mtext("Promedio 7 días - a partir de 20 casos - día de registro")
# axis(1, las=2, at = dates, labels = rownames(norm.p_age)[dates], cex.axis = 0.75)
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0.5), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("right", pch=15, col=rev(cols),legend=rev(c("Amb sobr","Amb def","Hosp sobr","Hosp def","Int sobr","Int def")), bty = "n", cex=0.8) # Plot legend here (to the right)
# dev.off()

###################### Week and age x state (2022-07-05) #############################
week <- data.frame("week"=c(paste0("19W",sprintf('%0.2d', rep(52,each=7))),paste0("20W",sprintf('%0.2d', rep(1:52,each=7))),paste0("21W",sprintf('%0.2d', rep(1:52,each=7))),paste0("22W",sprintf('%0.2d', rep(1:52,each=7))))) # Create a dictionary
# Now rename the rows to use them as a hash (dictionary)
rownames(week) <- seq(as.Date("2019/12/29"), as.Date("2022/12/31"), by="day")[1:nrow(week)]
# Now, append a week column to the table
all_dates <- as.Date(data[,11])
data[,"Week"] <- week[as.character(all_dates),]


colnames(vent_estados) <- entidades[colnames(vent_estados),1]

states <- date_vs_X(data, "ENTIDAD_RES", 7)
colnames(states) <- entidades[colnames(states),1]
states <- states[1:(nrow(states)-4),]
params <- define_plot_scheme()
printDat <- printable_dates(rownames(states))
dir.create("Estados", showWarnings=FALSE)
for(i in 1:32){
	print(i)
	nam <- colnames(states)[i]
	pdf(paste0("Estados/",nam,".pdf"),width=20)
	plot(las=2, states[,i], col=params[i,1], type='l', lwd=2, main=paste0("State: ", nam, " Total N: ",round(sum(states[,i]))), ylab="Average Daily Confirmed Cases", xaxt='n', xlab='')
	axis(1, las=2, at=printDat, labels=rownames(states)[printDat], cex.axis=0.8)
	dev.off()
}
write.table(states, "States_cases.tsv", sep="\t", quote=FALSE, row.names=TRUE, col.names=NA)

states2 <- week_vs_X(data, "ENTIDAD_RES")
colnames(states2) <- entidades[colnames(states2),1]
states2 <- states2[1:(nrow(states2)-1),]
params <- define_plot_scheme()
dir.create("Estados_week", showWarnings=FALSE)
for(i in 1:32){
	print(i)
	nam <- colnames(states2)[i]
	pdf(paste0("Estados_week/",nam,".pdf"),width=20)
	plot(las=2, states2[,i], col=params[i,1], type='l', lwd=2, main=paste0("State: ", nam, " Total N: ",round(sum(states[,i]))), ylab="Weekly cases", xlab='', xaxt='n')
 	axis(1, las=2, at=1:nrow(states2), labels=rownames(states2), cex.axis=0.8)
	dev.off()
}
write.table(states2, "States_cases-week.tsv", sep="\t", quote=FALSE, row.names=TRUE, col.names=NA)

###################### Region_5 (2022-07-07) #############################
edos <- read.table("EstadoRegion_v5.tsv",header=T, sep='\t', skip=0, comment.char='',fill=FALSE, check.names=FALSE, stringsAsFactors = FALSE, row.names=1)
edos <- edos["Region_4"]
temp <- states2
colnames(temp) <- edos[colnames(temp),]
temp <- t(temp)
temp <- t(rowsum(temp, group=rownames(temp)))
write.table(temp, "Region_4-week-epi.tsv", sep="\t", quote=FALSE, row.names=TRUE, col.names=NA)

temp <- states
colnames(temp) <- edos[colnames(temp),]
temp <- t(temp)
temp <- t(rowsum(temp, group=rownames(temp)))
write.table(temp, "Region_4-day-epi.tsv", sep="\t", quote=FALSE, row.names=TRUE, col.names=NA)

