

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for shiny app, create named-list of distance matrices
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(phangorn)

load_tree <- function(this_patient, info, B=1e3, min_seg_length=5e6) {
    message(this_patient)
    sinfo <- info[patient==this_patient,]
    data_type <- unique(sinfo$data_type)
    base_dir <- unique(sinfo$base_dir)
    groups <- sinfo[,c('sample','group'),with=F]
    if(data_type=='wes') {
        rds_file <- file.path(base_dir,paste0(this_patient,'_combo_njtree_public.rds'))
        tree <- readRDS(rds_file)
    } else {
        seg_file <- file.path(base_dir,paste0('copy_number_segs.txt'))       
        segs <- fread(seg_file)
        set.seed(42)
        tcn <- data.table::dcast(sample ~ segment, value.var='tcn', data=segs[seg_length >= min_seg_length,])
        tcn <- d2m(tcn)
        dm0 <- dist(tcn, method='manhattan')
        tree0 <- nj(dm0)
        
        # hacky way to add bootstrap vals for offtarget cases for now        
        resample <- function(i, tcn) {
            selected_cols <- sample(1:ncol(tcn), replace=T)
            tcn <- tcn[,selected_cols]
            dm <- dist(tcn, method='manhattan')
            tree <- nj(dm)
            tree
        }
        bstrees <- lapply(1:B, resample, tcn)
        class(bstrees) <- 'multiPhylo'
        tree <- plotBS(tree0,bstrees,type="phylogram",p=0)
        normal_sample <- grep('^N',tree$tip.label,value=T)
        tree <- phytools::reroot(tree, which(tree$tip.label==normal_sample))
    }
    
    tree
}

data_list <- lapply(patients, load_tree, info)
names(data_list) <- patients
saveRDS(data_list, file='data_analysis/data.rds')

cohort1 <- sample(patients, 10, replace=F)
cohort2 <- sample(patients, 15, replace=F)
cohort3 <- sample(patients, 7, replace=F)
cohort4 <- sample(patients, 8, replace=F)
cohorts <- list(cohort1, cohort2, cohort3, cohort4)#, c(), c(), c())
names(cohorts) <- c('Random Cohort 1','Random Cohort 2','Random Cohort 3','Random Cohort 4')#,'Empty Cohort 1','Empty Cohort 2','Empty Cohort 3')
saveRDS(cohorts, file='data_analysis/cohorts.rds')



## sample type color scheme
group_cols <- c("#000000", "#008C45", "#EB5B2B", "#FAB31D", "#4C86C6", "#4C86C6", "#4C86C6")
names(group_cols) <- c('Normal', 'Primary', 'Locoregional', 'Peritoneum', 'Lung', 'Liver', 'Distant (other)')

p <- ggtree(tree, linewidth=0.5) + theme_tree2()
#if(!is.null(xbreaks)) p <- p + scale_x_continuous(breaks=xbreaks,limits=range(xbreaks))
p <- p %<+% groups
p <- p + geom_tiplab(aes(color=group), hjust=-0.2, size=3) 
p <- p + geom_nodelab(color='blue', size=2.5, hjust=-0.1)
p <- p + scale_color_manual(values=group_cols,name='Sample type') + labs(title=this_patient)
p <- p + guides(fill='none') + theme(legend.position='none')#legend.position)
if(!is.na(xbuffer)) p <- p + xlim(0, max(p$data$x+xbuffer))
p 

