# filename="220821COVID19MEXICO.csv"

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) { # at least, two arguments should be included: <min_nonNAs> <prefix_output>  <name_of_alpha_metric>
	stop("A minimum of 2 arguments are mandatory: cat table.tsv|Rscript PreProcess_DGE.R <prefix> <file>", call.=FALSE)
}
filename <- as.character(args[])  # and the name of the metric (free strings)

# partA <- read.table("data_2020-2021.tsv", sep="\t",header=T, skip=0, comment.char='',quote="\"",fill=F, check.names=F) #
data_all <- read.table(unz("datos_abiertos_covid19.zip", filename), header=T, quote="\"", sep=",")
partA <- read.table(unz("data_2020-2021.zip", "data_2020-2021.tsv"), sep="\t",header=T, skip=0, comment.char='',quote="\"",fill=F, check.names=F) #
# data_all <- read.table(filename, header=T, quote="\"", sep=",")
dim(partA)
# [1] 12634194       40
dim(data_all)
# [1] 5361794      40
# colnames(partA) <- colnames(data_all)
write.table(rbind(partA,data_all), "out.tsv", sep='\t', col.names = TRUE, row.names = FALSE, quote=FALSE)
