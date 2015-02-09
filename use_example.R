source("autoclass.R")
akl = autoclass(pksc, scale=T, delog=F, rel_error=0.1, single_normal_cn=3:ncol(pksc), interactive=F)

source("plot_CRM_heatmap.R")
plot_CRM_heatmap(akl, tfrange=6:11, mycol=rainbow(length(unique(akl$V3))))

source("plot_class_boxplots.R")
res = plot_class_boxplots(cqbm=akl, tfrange=6:11, idcol=1, classcol=3,
                          vertical=F, median.marker="dot", ylim=c(0,20), class.order=as.character(0:12))

