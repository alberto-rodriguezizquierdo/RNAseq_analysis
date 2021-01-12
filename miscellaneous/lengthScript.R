#first get the gtf file from, lets say, ensembl
library(GenomicFeatures)
#the function above makeTranscriptDbFromGFF is now have different name, make sure that you type the correct one
txdb <- makeTxDbFromGFF("Danio_rerio.GRCz11.93.gtf",format="gtf")
head(txdb)
View(txdb)
# then collect the exons per gene id
exons.list.per.gene <- exonsBy(txdb,by="gene")
head(exons.list.per.gene)
# then for each gene, reduce all the exons to a set of non overlapping exons, calculate their lengths (widths) and sum then
exonic.gene.sizes <- bplapply(exons.list.per.gene,function(x){sum(width(reduce(x)))})
head(exonic.gene.sizes)
#to see them in a table format, you should unlist them
unlist_geneLength<-unlist(exonic.gene.sizes)
write.table(unlist_geneLength,"geneLength2.txt")
