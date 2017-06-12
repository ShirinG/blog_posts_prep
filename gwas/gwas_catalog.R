library(gwascat)

browseVignettes("gwascat")

objects("package:gwascat")

data(ebicat38)

gwas38 <- as.data.frame(ebicat38)
head(gwas38)
table(as.factor(gwas38$DISEASE.TRAIT))


topTraits(ebicat38)
topTraits(ebicat38, n=20, tag="DISEASE/TRAIT")

locs4trait(ebicat38, trait = "Bipolar disorder", tag="DISEASE/TRAIT")

library(SNPlocs.Hsapiens.dbSNP144.GRCh38)

snps <- SNPlocs.Hsapiens.dbSNP144.GRCh38

## Count and load all the SNPs for a given chromosome:
snpcount(snps)

seqlevels(snps)
snpsBySeqname(snps, seqnames = "chMT", drop.rs.prefix=FALSE)
head(snplocs(snps, seqname = "chMT", as.GRanges=FALSE, caching=TRUE))

?snpid2loc
?snpid2alleles
?snpid2grange

## ---------------------------------------------------------------------
## A. BASIC USAGE
## ---------------------------------------------------------------------

## Get the locations and alleles of all SNPs on chromosome 22:
ch22_snps <- snpsBySeqname(snps, "ch22")
ch22_snps
## Get the locations and alleles of all SNPs on chromosomes 22 and MT:
snpsBySeqname(snps, c("ch22", "chMT"))
## ---------------------------------------------------------------------
## B. EXTRACT SNP INFORMATION FOR A SET OF RS IDS
## ---------------------------------------------------------------------
my_rsids <- c("rs2639606", "rs75264089", "rs73396229", "rs55871206",
              "rs10932221", "rs56219727", "rs73709730", "rs55838886",
              "rs3734153", "rs79381275", "rs1516535")
my_snps <- snpsById(snps, my_rsids)
my_snps
## Translate the IUPAC ambiguity codes used to represent the alleles
## into nucleotides:
IUPAC_CODE_MAP[mcols(my_snps)$alleles_as_ambig]

## ---------------------------------------------------------------------
## C. INJECTION IN THE REFERENCE GENOME
## ---------------------------------------------------------------------
library(BSgenome.Hsapiens.UCSC.hg38)
genome <- BSgenome.Hsapiens.UCSC.hg38
genome
genome2 <- injectSNPs(genome, "SNPlocs.Hsapiens.dbSNP144.GRCh38")
genome2 # note the additional line "with SNPs injected from..."
alphabetFrequency(genome$chr22)
alphabetFrequency(genome2$chr22)
## Get the number of nucleotides that were modified by this injection:
neditAt(genome$chr22, genome2$chr22) # 1819314

## ---------------------------------------------------------------------
## D. SOME BASIC QUALITY CONTROL (WITH SURPRISING RESULTS!)
## ---------------------------------------------------------------------
## Note that dbSNP can assign distinct ids to SNPs located at the same
## position:
any(duplicated(mcols(ch22_snps)$RefSNP_id)) # rs ids are all distinct...
any(duplicated(ch22_snps)) # but some locations are repeated!
ch22_snps <- sort(ch22_snps) # sort by location
which(duplicated(ch22_snps))[1:2] # 409, 411
ch22_snps[407:412] # rs9617595 and rs752928383 share the same location
# (11123377) and alleles (S, i.e. C/G)
## Also note that not all SNP alleles are consistent with the GRCh38
## genomic sequences i.e. the alleles reported for a given SNP are not
## always compatible with the nucleotide found at the SNP location in
## GRCh38. For example, to get the number of inconsistent SNPs in chr1:
ch1_snps <- snpsBySeqname(snps, "ch1")
ch1_alleles <- DNAString(paste(mcols(ch1_snps)$alleles_as_ambig, collapse=""))
nchar(ch1_alleles) # 10352408 SNPs on chr1
neditAt(genome$chr1[start(ch1_snps)], ch1_alleles, fixed=FALSE)
## ==> 5561 SNPs (0.054%) are inconsistent with GRCh38 chr1!


subsetByTraits(ebicat38, tr="LDL cholesterol")[1:3]

gwtrunc = ebicat38
mlpv = mcols(ebicat38)$PVALUE_MLOG
mlpv = ifelse(mlpv > 25, 25, mlpv)
mcols(gwtrunc)$PVALUE_MLOG = mlpv

library(GenomeInfoDb)
seqlevelsStyle(gwtrunc) = "UCSC"
gwlit = gwtrunc[ which(as.character(seqnames(gwtrunc)) %in% c("chr4", "chr5", "chr6")) ]

library(ggbio)
mlpv = mcols(gwlit)$PVALUE_MLOG
mlpv = ifelse(mlpv > 25, 25, mlpv)
mcols(gwlit)$PVALUE_MLOG = mlpv
methods:::bind_activation(FALSE)
autoplot(gwlit, geom="point", aes(y=PVALUE_MLOG), xlab="chr4-chr6")


args(traitsManh)
traitsManh(gwtrunc)

traitsManh(gwtrunc, selr = GRanges(seqnames = "chr18", IRanges(0, 10e+08)), traits = "Post-traumatic stress disorder", truncmlp = 25)
traitsManh(gwtrunc, selr = GRanges(seqnames = "chr18", IRanges(0, 10e+08)), truncmlp = 25)

###

library(ggplot2)

snps_data_weight <- as.data.frame(locs4trait(ebicat38, trait = "Weight loss (gastric bypass surgery)", tag="DISEASE/TRAIT"))
snps_data_waist <- as.data.frame(locs4trait(ebicat38, trait = "Waist circumference", tag="DISEASE/TRAIT"))

snps_data_weight <- data.frame(Chr = snps_data_weight$seqnames, Start = snps_data_weight$CHR_POS, SNPid = snps_data_weight$SNPS, Trait = rep("Weight loss (gastric bypass surgery)", nrow(snps_data_weight)), PVALUE_MLOG = snps_data_weight$PVALUE_MLOG, OR.or.BETA = snps_data_weight$OR.or.BETA)
snps_data_waist <- data.frame(Chr = snps_data_waist$seqnames, Start = snps_data_waist$CHR_POS, SNPid = snps_data_waist$SNPS, Trait = rep("Waist circumference", nrow(snps_data_waist)), PVALUE_MLOG = snps_data_waist$PVALUE_MLOG, OR.or.BETA = snps_data_waist$OR.or.BETA)

snps_data <- rbind(snps_data_weight, snps_data_waist)

ggplot(snps_data) + geom_point(aes(x=Start, y=PVALUE_MLOG, color = Trait)) + facet_grid(Chr ~ .)
ggplot(snps_data) + geom_point(aes(x=Start, y=OR.or.BETA, color = Trait)) + facet_grid(Chr ~ .)

ggplot(snps_data) + geom_histogram(aes(x=Start),binwidth=1e6) + facet_grid(Chr ~ Trait)
ggplot(snps_data) + geom_point(aes(x=Start, y=0)) + facet_grid(Chr ~ Trait)


library(AnnotationDbi)
library(org.Hs.eg.db)

library(EnsDb.Hsapiens.v79)
edb <- EnsDb.Hsapiens.v79


keys <- keys(edb, keytype="SEQNAME")
chromosome_length <- select(edb, keys=keys, columns=c("SEQLENGTH", "SEQNAME"), keytype="SEQNAME")
chromosome_length <- chromosome_length[-grep("CHR", chromosome_length$SEQNAME), ]
chromosome_length <- chromosome_length[-grep("LRG", chromosome_length$SEQNAME), ]
chromosome_length <- chromosome_length[-grep("KI", chromosome_length$SEQNAME), ]
chromosome_length <- chromosome_length[-grep("GL", chromosome_length$SEQNAME), ]

#snps_data <- merge(snps_data, chromosome_length, by.x = "Chr", by.y = "SEQNAME", all.x = TRUE)
#chr_data <- unique(snps_data[,c(1,7)])

chr_data <- chromosome_length
chr_data$SEQNAME <- as.factor(chr_data$SEQNAME)
f=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "X", "Y", "MT")
chr_data <- within(chr_data, SEQNAME <- factor(SEQNAME, levels=f))

p <- ggplot(data=chr_data, aes(x=SEQNAME, y=as.numeric(SEQLENGTH))) + geom_bar(stat="identity", fill="grey70")
p + geom_segment(data=snps_data, aes(x=as.numeric(as.character(Chr))-0.45, xend=as.numeric(as.character(Chr))+0.45, y=Start, yend=Start, colour=Trait), size=1)


p <- ggplot(data = snps_data, aes(x=Start, y=PVALUE_MLOG)) + geom_area(aes(fill=Trait))
p + facet_wrap(~ Chr, ncol=1)

###

library(AnnotationDbi)
library(org.Hs.eg.db)

library(EnsDb.Hsapiens.v79)
edb <- EnsDb.Hsapiens.v79

keys <- keys(edb, keytype="SEQNAME")
chromosome_length <- select(edb, keys=keys, columns=c("SEQLENGTH", "SEQNAME"), keytype="SEQNAME")
chromosome_length <- chromosome_length[-grep("CHR", chromosome_length$SEQNAME), ]
chromosome_length <- chromosome_length[-grep("LRG", chromosome_length$SEQNAME), ]
chromosome_length <- chromosome_length[-grep("KI", chromosome_length$SEQNAME), ]
chromosome_length <- chromosome_length[-grep("GL", chromosome_length$SEQNAME), ]

chr_data <- chromosome_length
chr_data$SEQNAME <- as.factor(chr_data$SEQNAME)
f=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "X", "Y", "MT")
chr_data <- within(chr_data, SEQNAME <- factor(SEQNAME, levels=f))

p <- ggplot(data = chr_data, aes(x = SEQNAME, y = as.numeric(SEQLENGTH))) + geom_bar(stat = "identity", fill = "grey90", color = "black") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position="bottom"
  ) +
  labs(title="SNP locations") +
  labs(x="Chromosome", y="Position")


head(gwas38)
gwas38_traits <- as.data.frame(table(gwas38$DISEASE.TRAIT))
head(gwas38_traits)

snps_data_1 <- as.data.frame(locs4trait(ebicat38, trait = "Weight loss (gastric bypass surgery)", tag="DISEASE/TRAIT"))
snps_data_2 <- as.data.frame(locs4trait(ebicat38, trait = "Waist circumference", tag="DISEASE/TRAIT"))

snps_data_1 <- data.frame(Chr = snps_data_1$seqnames, Start = snps_data_1$CHR_POS, SNPid = snps_data_1$SNPS,
                          Trait = rep("Weight loss (gastric bypass surgery)", nrow(snps_data_1)),
                          PVALUE_MLOG = snps_data_1$PVALUE_MLOG, OR.or.BETA = snps_data_1$OR.or.BETA)

snps_data_2 <- data.frame(Chr = snps_data_2$seqnames, Start = snps_data_2$CHR_POS, SNPid = snps_data_2$SNPS,
                          Trait = rep("Waist circumference", nrow(snps_data_2)),
                          PVALUE_MLOG = snps_data_2$PVALUE_MLOG, OR.or.BETA = snps_data_2$OR.or.BETA)

snps_data <- rbind(snps_data_1, snps_data_2)


p + geom_segment(data=snps_data, aes(x=as.numeric(as.character(Chr))-0.45, xend=as.numeric(as.character(Chr))+0.45,
                                     y=Start, yend=Start, colour=Trait), size=2, alpha = 0.5) +
  scale_colour_brewer(palette="Set1")


gwas38_cancer <- gwas38_traits[grep("cancer$", gwas38_traits$Var1),]
gwas38_cancer <- gwas38_cancer[c(2,3,14,16,22),]

for (i in 1:nrow(gwas38_cancer)){
  snps_data <- as.data.frame(locs4trait(ebicat38, trait = paste(gwas38_cancer$Var1[i]), tag="DISEASE/TRAIT"))

  snps_data <- data.frame(Chr = snps_data$seqnames, Start = snps_data$CHR_POS, SNPid = snps_data$SNPS,
                            Trait = rep(paste(gwas38_cancer$Var1[i]), nrow(snps_data)),
                            PVALUE_MLOG = snps_data$PVALUE_MLOG, OR.or.BETA = snps_data$OR.or.BETA)

  if (i==1){
    snps_data_table <- snps_data
  } else {
    snps_data_table <- rbind(snps_data_table, snps_data)
  }
}

snps_data_table$Chr_num <- ifelse(snps_data_table$Chr == "X", "23",
                                  ifelse(snps_data_table$Chr == "Y", "24",
                                         ifelse(snps_data_table$Chr == "MT", "25", as.character(snps_data_table$Chr))))
snps_data_table$Chr_num <- as.numeric(snps_data_table$Chr_num)
snps_data_table <- snps_data_table[order(snps_data_table$Chr_num),]
head(snps_data_table)
str(snps_data_table)

p + geom_segment(data=snps_data_table, aes(x=Chr_num-0.45, xend=Chr_num+0.45, y=Start, yend=Start, colour=Trait), size=2, alpha = 0.3) +
  scale_colour_brewer(palette="Set1")

#facet_wrap(~Trait, ncol = 2)


####################################################################################

gffpath = system.file("gff3/chr17_43000000_45000000.gff3", package="gwascat")
library(rtracklayer)
c17tg = import(gffpath)

c17td = c17tg[ which(mcols(c17tg)$type == "Degner_dsQTL") ]
library(Gviz)
dsqs = DataTrack( c17td, chrom="chr17", genome="hg38", data="score",
                   name="dsQTL")

g2 = GRanges(seqnames="chr17", IRanges(start=4.3e7, width=2e6))
seqlevelsStyle(ebicat38) = "UCSC"
basic = gwcex2gviz(basegr = ebicat38, contextGR=g2, plot.it=FALSE)

c17ts = c17tg[ which(mcols(c17tg)$type == "Stranger_eqtl") ]
eqloc = AnnotationTrack(c17ts, chrom="chr17", genome="hg19", name="Str eQTL")
displayPars(eqloc)$col = "black"
displayPars(dsqs)$col = "red"
integ = list(basic[[1]], eqloc, dsqs, basic[[2]], basic[[3]])
plotTracks(integ)


library(pd.genomewidesnp.6)
con = pd.genomewidesnp.6@getdb()
locon6 = dbGetQuery(con,
                     "select dbsnp_rs_id, chrom, physical_pos from featureSet limit 10000")

data(locon6)
rson6 = as.character(locon6[[1]])
rson6[1:5]

intr = ebicat38[ intersect(getRsids(ebicat38), rson6) ]
sort(table(getTraits(intr)), decreasing=TRUE)[1:10]


gr6.0 = GRanges(seqnames=ifelse(is.na(locon6$chrom),0,locon6$chrom),
                 IRanges(ifelse(is.na(locon6$phys),1,locon6$phys), width=1))

mcols(gr6.0)$rsid = as.character(locon6$dbsnp_rs_id)
seqlevels(gr6.0) = paste("chr", seqlevels(gr6.0), sep="")


ag = function(x) as(x, "GRanges")
ovraw = suppressWarnings(subsetByOverlaps(ag(ebicat38), gr6.0))
length(ovraw)

ovaug = suppressWarnings(subsetByOverlaps(ag(ebicat38+500), gr6.0))
length(ovaug)

rawrs = mcols(ovraw)$SNPS
augrs = mcols(ovaug)$SNPS
ebicat38[augrs]

setdiff( getTraits(ebicat38[augrs]), getTraits(ebicat38[rawrs]) )


data(gg17N) # translated from GGdata chr 17 calls using ABmat2nuc
gg17N[1:5,1:5]

h17 = riskyAlleleCount(gg17N, matIsAB=FALSE, chr="ch17",
                       gwwl = ebicat38)
h17[1:5,1:5]

gwr = ebicat38
gwr = gwr[colnames(h17),]
mcols(gwr) = cbind(mcols(gwr), DataFrame(t(h17)))
sn = rownames(h17)
gwr[,c("DISEASE.TRAIT", sn[1:4])]



library(DO.db)
DO()

alltob = unlist(mget(mappedkeys(DOTERM), DOTERM))
allt = sapply(alltob, Term)
allt[1:5]

hpobo = gzfile(dir(system.file("obo", package="gwascat"), pattern="hpo", full=TRUE))
HPOgraph = obo2graphNEL(hpobo)
close(hpobo)

hpoterms = unlist(nodeData(HPOgraph, nodes(HPOgraph), "name"))
hpoterms[1:10]


library(gwascat)
data(efo.obo.g)
efo.obo.g

sn = head(nodes(efo.obo.g))
sn


nd = nodeData(efo.obo.g)
alldef = sapply(nd, function(x) unlist(x[["def"]]))
allnames = sapply(nd, function(x) unlist(x[["name"]]))
alld2 = sapply(alldef, function(x) if(is.null(x)) return(" ") else x[1])
mydf = data.frame(id = names(allnames), concept=as.character(allnames), def=unlist(alld2))

limdf = mydf[ grep("autoimm", mydf$def, ignore.case=TRUE), ]
library(DT)
suppressWarnings({
  datatable(limdf, rownames=FALSE, options=list(pageLength=5))
})

nodeData(efo.obo.g, "EFO:0000540")

ue = ugraph(efo.obo.g)
neighISD = adj(ue, "EFO:0000540")[[1]]
sapply(nodeData(subGraph(neighISD, efo.obo.g)), "[[", "name")

library(RBGL)

p = sp.between( efo.obo.g, "EFO:0000685", "EFO:0000001")
sapply(nodeData(subGraph(p[[1]]$path_detail, efo.obo.g)), "[[", "name")


data(ebicat38)
names(mcols(ebicat38))

adinds = grep("autoimmu", ebicat38$MAPPED_TRAIT)
adgr = ebicat38[adinds]
adgr

mcols(adgr)[, c("MAPPED_TRAIT", "MAPPED_TRAIT_URI")]

