# RNAseq_analysis

Please check this file carefully. This repository is developed to realise analysis of Differential Expression Genes (DEG) from RNA-seq paired-end data, from raw data of RNA seq sequencies. Before start analysis with there, please check system capacity to reduce time analysis.

# Specifications:

-UNIX server with slurm queue system required (SBATCH files to start analysis)
->200GB ram recommended to improve analysis time.
-Annotations and correspondences depending on the organism (Please include annotations (.GFF3 or .GTF files in a specific folder)
-Please check what adapters (ILLUMINA, e.g.) are used to do RNA-seq analysis.

# Required packages:

- FastQC
- Trimmomatic
- HISAT2
- SAMTOOLS
- featureCounts
- DESeq2
- NOISeq

***** PLEASE CHECK SERVER SPECIFICATIONS BEFORE START

# Specific disposition of raw data samples inputs:

-FORWARD: SAMPLE/fw/SAMPLE.fq or SAMPLE/fw/SAMPLE.fastq
-REVERSE: SAMPLE/rv/SAMPLE.fq or SAMPLE/rv/SAMPLE.fastq

# Outputs architecture:

- SAMPLE/results

# Workflow:

QC --> Trimming --> Alignment --> Counts --> DEG

# API architecture:

-rnaseq_analysis/(NAME_PACKAGE)/app
-rnaseq_analysis/(NAME_PACKAGE)/batch

# Starting with FastQC analysis of data and solving possible problems of overrepresentations.

To start RNA seq analysis, the package will provide you a little script to check the quality of the sequencies.

Check always that all paths are specified in SBATCH file:

*nano SBATCH_FASTQC.sh
(check all paths and save with Ctrl+O and exit file with Ctrl+X)

Order:

*sbatch SBATCH_FASTQC.sh

Check manually all results to detect overrepresentations and quality of RNA-sequencies.

# Trimming sequencies with Trimmomatic 0.38

Raw RNA-seq data requires a trimming to reduce interferences with overrepresented sequencies, sequencing adapters and other products to sequence them. Some specifications are presented in original package, but the easiest way to realise the trimming and reduce losses of information is used in this package. (OPTIONALLY: Use CROP order and specify the number of bp to save).

*nano SBATCH_Trimmomatic.sh
(check all paths and save with Ctrl+O and exit file with Ctrl+X)

Order:

*sbatch SBATCH_trimmomatic.sh

# Alignment with HISAT2 2.1.0

Alignment all sequencies to prove that the sequencies of RNA-seq analysis can found out in the genome of the organism object is the most important part. This program uses index genome sequencies to localise and align the sequencies (if you do not have index files, HISAT can produce these files (to produce them, HISAT requires whole genome of the organism in FASTA (.fasta or fa) format).
*****IMPORTANT: Please pay attention with Annotation file version and .fasta/.fa genome version, they must be concordant).

*nano SBATCH_hisat2.sh
(check all paths and save with Ctrl+O and exit file with Ctrl+X)

Order:

*sbatch SBATCH_hisat2.sh

IMPORTANT: output files of HISAT2 are .sam files. To avoid space problems, it is highly recommended to convert .sam to .bam files (program does this convertion with the package samtools and it is implemented. If you want to have only the .sam files, please check run_hisat2.sh file and silence the order).

# Counting gene expressions with featureCounts.

featureCounts is a R package that allow to count expressed gene sequencies in .sam or .bam files. To use featureCounts, the generation of all files is also implemented.

order:

*sbatch SBATCH_featurecounts.sh

# Differential expression genes with NOISeq and DESeq2 powered in R.

API to develope this DEG requires some specifications of performed experiment, related to check differences between two conditions. For example:

Samples 1, 2 and 3: Drought
Samples 4, 5 and 6: Control

In configFile, it is mandatory to specify the samples to be compared in two conditions, defined by a factor (specify factor_db.csv with all information of the samples and the conditions):

-<samples_1>1,2,3<samples_1> (specifically in that format)
-<samples_2>4,5,6<samples_2> (specifically in that format)
-<factor>drought<factor>
  
Check file paths and store.

This API provides a solution using two different packages to analyse DEG, sorting and selecting the common genes differentially expressed in NOISeq and DESeq2 results.

To start DEG analysis, please run main.R script.

After that, the output results will have the assembly of the two results and the list of genes differentially expressed among them.

To continue, you can use tools like Mapman or Cytoscape to show the pathways involved in these DEG, and Panther website to conclude all results.
