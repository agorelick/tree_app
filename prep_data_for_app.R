

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for shiny app, create named-list of distance matrices
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(ape)
library(rds)
library(stringr)
library(ggplot2)
library(ggbeeswarm)
library(ggrepel)
library(ggpubr)

rm(list=ls())

setwd('~/HMS Dropbox/Naxerova_Lab/Lisa/Peritoneal Revision')
source('WES/func_wes.R')
val_sample_info <- fread('data_analysis/validation_cohort/sample_info_annotated.csv')
setnames(val_sample_info,'patient','Patient_ID')
setnames(val_sample_info,'public_sample_name','Real_Sample_ID')
val_sample_info <- val_sample_info[data_type=='WES']
val_sample_info$Real_Sample_ID <- gsub('PTX','PT0',val_sample_info$Real_Sample_ID)
val_patients <- unique(val_sample_info$Patient_ID)

## sample type color scheme
group_cols <- c("#000000", "#008C45", "#EB5B2B", "#FAB31D", "#4C86C6", "#4C86C6", "#4C86C6")
names(group_cols) <- c('Normal', 'Primary', 'Locoregional', 'Peritoneum', 'Lung', 'Liver', 'Distant (other)')


load_tree <- function(this_patient, sample_info, min_seg_length=5e6, max_informative_tcn=6) {
    message(this_patient)
    sinfo <- sample_info[Patient_ID==this_patient,]
    normal_sample <- grep('^N',sinfo$Real_Sample_ID,value=T)
    data_type <- unique(sinfo$data_type)
    base_dir <- unique(sinfo$base_dir)
    #groups <- sinfo[,c('public_sample_name','group'),with=F]
    #setnames(groups,'public_sample_name','sample')
    dm <- tryCatch({
        if(data_type=='WES') {
            maf <- fread(file.path(base_dir,'public_data',paste0(this_patient,'_filtered_norm_passed_multisample_ccf_oncokb_cleaned.txt')))
            segs <- fread(file.path(base_dir,'public_data',paste0(this_patient,'_copynumber_segments.tsv')))
            fits <- fread(file.path(base_dir,'public_data',paste0(this_patient,'_purity_ploidy_fits.tsv')))
            
            mat1 <- data.table::dcast(tm ~ Tumor_Sample_Barcode, value.var='ccf', data=maf[!is.na(ccf) | Tumor_Sample_Barcode==normal_sample])
            mat1 <- t(d2m(mat1))
            mat1[normal_sample,] <- 0
            
            fits <- fits[!is.na(pu) & pu >= 0.1,]
            truncalWGD <- ifelse(all(fits$pl >= 3),T,F)
            if(truncalWGD) {
                message('Truncal WGD event detected')
                segs[sample==normal_sample,tcn:=2*tcn]
            }
            segs <- segs[seg_length >= min_seg_length,]
            segs[tcn >= max_informative_tcn, tcn:=max_informative_tcn]
            mat2 <- data.table::dcast(segment ~ sample, value.var='tcn', data=segs)
            mat2 <- t(d2m(mat2))
            
            mat1 <- mat1[order(rownames(mat1)),]
            mat2 <- mat2[rownames(mat1),]
            mat <- cbind(mat1, mat2)
            dm <- as.matrix(dist(mat,method='manhattan'))
        } else {
            seg_file <- file.path(base_dir,'processed_data/copy_number_segs.txt')       
            segs <- fread(seg_file)
            tcn <- data.table::dcast(sample ~ segment, value.var='tcn', data=segs[seg_length >= min_seg_length,])
            tcn <- d2m(tcn)
            dm <- as.matrix(dist(tcn, method='manhattan'))
        }
        dm
    },error=function(e) {
        NULL
    })
    rownames(dm) <- gsub('PTX','PT0',rownames(dm))
    colnames(dm) <- gsub('PTX','PT0',colnames(dm))
    dm 
}

read_distance_matrix <- function (file, return.as.matrix = T) {
    distance_matrix <- fread(file)
    rows <- distance_matrix[[1]]
    distance_matrix <- distance_matrix[, (2:ncol(distance_matrix)), with = F]
    m <- as.matrix(distance_matrix)
    rownames(m) <- rows
    if (return.as.matrix == F) {
        as.dist(m, diag = T)
    }
    else {
        m
    }
}

dm_list <- lapply(val_patients, load_tree, val_sample_info)
names(dm_list) <- val_patients
saveRDS(dm_list, file='~/lab_repos/tree_app/data.rds')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# save sample_info for tags
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

val_sample_info[met_timing=='primary/normal', met_timing:='Primary/Normal']
val_sample_info[met_timing=='synchronous', met_timing:='Synchronous']
val_sample_info[met_timing=='metachronous', met_timing:='Metachronous']

val_sample_info[group %in% c('Normal','Primary'), met_treated:='Primary/Normal']
val_sample_info[met_treated %in% c('neoadj sys chemo','adj sys chemo','neoadj sys chemo + adj sys chemo'), met_treated:='Systemic chemo']
val_sample_info[met_treated=='untreated', met_treated:='Untreated']
val_sample_info[met_treated=='hipec', met_treated:='HIPEC']


# add Metachronous after Synchronous
sync_patients <- val_sample_info[met_timing=='Synchronous',(Patient_ID)]
meta_patients <- val_sample_info[met_timing=='Metachronous',(Patient_ID)]
both_patients <- intersect(sync_patients, meta_patients)
val_sample_info[Patient_ID %in% both_patients & met_timing=='Metachronous', met_timing:='Meta after sync']

# add treated after untreated
untr_patients <- val_sample_info[met_treated=='Untreated',(Patient_ID)]
tr_patients <- val_sample_info[met_treated=='Systemic chemo',(Patient_ID)]
both_patients <- intersect(untr_patients, tr_patients)
val_sample_info[Patient_ID %in% both_patients & !group %in% c('Normal','Primary') & met_treated=='Systemic chemo', met_treated:='Sys-chemo after untreated']
saveRDS(val_sample_info, file='~/lab_repos/tree_app/sample_info_annotated.txt')




