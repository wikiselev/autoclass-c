## (c) Mikhail Spivakov 2014 

library(ggplot2)
library(RColorBrewer)

# Plot boxplots of a cqbm (with one or many classes - for a non-cqbm dataframe need to have a column with constant values to specify as classcol) 
#
# tfrange - if not continuous or doesn't have the format TF_time or the number of times differs from TF to TF, will plot one boxplot per cell,
#     otherwise the number of boxplots per cell will be equal to the number of times and the boxplot colours will correspond each to a different time
# ntimes - the number of timepoints per tf
# vertical - if TRUE, put TFs in rows and classes in columns; if FALSE, do the opposite;
#     defaults to TRUE for >1 classes and FALSE for one.
# median.marker - how to mark the cqbm-wide median for the given TF_time: with a line, with a dot or not at all
# ylim - the (common) Y axis limits for all boxplots. If not set, calculated automatically, but remain common for all 
# class.order, tf.order - the ordering of class and TF names. Note: times order is preserved from the columns' order in cqbm!
# refactor.classes (def TRUE) - if set to FALSE _and_ the class column has the type factor, the class order will be defined by the levels of the factor
# col - boxplot colour(s); one for each "time point". 
# Returns res - the reshaped cqbm - that can be provided as the argument res instead of cqbm

plot_class_boxplots = function (cqbm, tfrange, res=NULL, idcol=1, classcol=3, vertical="default", median.marker=c("line","dot", "none")[1], 
                                ylim=NULL, title="", class.order = NULL, refactor.classes=TRUE, tf.order=NULL, 
                                col=NULL, plot=TRUE){
  
  if (is.null(res) ){
    nms = NULL
    contrange = TRUE
    
    nms = names(cqbm)[tfrange]	
    tfstcol = min(tfrange)
    tfendcol = max(tfrange)
    
    if(! all(min(tfrange):max(tfrange) %in% tfrange)){
      contrange=FALSE
    }
    
    tfs = NULL 
    ntfs = NULL
    times = NULL
    timesOrder = NULL
    
    if (contrange==TRUE){ # if all names have a TF_time structure
      
      if (length(grep(".+_.+", nms))==length(nms)){
        
        tfs = gsub("(.+)_.+","\\1",nms)
        tfs = unique(tfs)
        ntfs = length(tfs)	
        
        times = gsub(".+_(.+)","\\1",nms) 
        times = unique(times)
        
        timesOrder = as.integer(gsub("(\\d+)\\.\\d+", "\\1", times))
        
        ntimes = length(times)
        
        if (ntimes*ntfs != length(nms)){
          contrange = FALSE
        }
        
      }
      else{
        contrange = FALSE
      }    
      
    }
    

    if (!contrange){
      
      cat("TF time mode off (if this is unexpected, double-check tfrange)\n")		
      nms = names(cqbm)[tfrange]  
      tfs = nms
      ntfs = length(tfs)
      times = 1
      timesOrder = 1
      ntimes = 1
      
    }
    
    if (ntimes>length(col) & !is.null(col)){
      stop("col has fewer elements than times")
    }
    
    tflen = length(tfrange)
    total.len = length(cqbm[,1])*tflen
    
    id = vector(mode="numeric", length = total.len)
    cl = vector(mode="character", length = total.len)
    tf = vector(mode="character", length = total.len)
    time = vector(mode="numeric", length = total.len)
    val = vector(mode="numeric", length = total.len)
    
    cat("Reshaping dataframe...")
    
    count = 1
    for (crmid in cqbm[,idcol]){
      id[count:(count+tflen-1)] = rep(crmid, tflen)	
      cl[count:(count+tflen-1)] = rep(cqbm[cqbm[,idcol]==crmid, classcol], tflen)
      tf[count:(count+tflen-1)] = sapply(tfs, function(x,ntimes)rep(x,ntimes), ntimes)	
      time[count:(count+tflen-1)] = times 
      val[count:(count+tflen-1)] = cqbm[cqbm[,idcol]==crmid, tfrange] 
      count = count + tflen
    }
    val = unlist(val)
    res = data.frame(CRMID=id, CLASS=cl, TF=factor(tf), TIME=factor(time, levels = times[order(timesOrder)]), VAL=val)      
    
    if (is.null(class.order)){
      if (refactor.classes == FALSE & is.factor(cqbm[,classcol])){
        class.order = levels(cqbm[,classcol])
      }
      else{
        class.order = sort(as.character(unique(res$CLASS)), na.last = TRUE)	    	}
    }
    cat("unique(res$CLASS)\n")
    print (unique(res$CLASS))
    cat("levels(res$CLASS)\n")
    print (levels(res$CLASS))
    res$CLASS = factor(res$CLASS, levels=class.order)
    
    if (is.null(tf.order)){
      tf.order = sort(as.character(unique(res$TF)), na.last = TRUE)
    }
    res$TF = factor(as.character(res$TF), levels=tf.order)
    
    if (contrange==FALSE){
      res$MEDIAN = apply(res, 1, function(x)median(cqbm[, which(names(cqbm)==x[3])]))
    }
    else{	
      res$MEDIAN = apply(res, 1, function(x)median(cqbm[, which(names(cqbm)==paste(x[3],x[4],sep="_"))]))
    }
    
  }
  
  cat("Plotting (this may take a while)...\n")
  print (head(res)) 
  print (levels(res$TIME))
  
  if (vertical=="default"){ 
    if (length(levels(res$CLASS)) > 1){
      vertical = TRUE
    }	
    else{
      vertical = FALSE
    }
  }
  
  if (!is.null(col)){
    time.palette = col[1:length(levels(res$TIME))]
  }
  else{
    time.palette = gg_colour_hue(length(levels(res$TIME)))
  }
  attr(time.palette, "names") <- levels(res$TIME)
  
  gridFormula=NULL
  if (vertical==TRUE){
    gridFormula=as.formula("TF~CLASS")
  }
  else{
    gridFormula=as.formula("CLASS~TF")
  }
  
  if (is.null(ylim)){
    ylim[1]=min(res$VAL)-abs(min(res$VAL))*0.2
    ylim[2]=max(res$VAL)+abs(max(res$VAL))*0.2    
  }
  
  if (plot==TRUE){
    p=NULL
    p = ggplot(data=res, aes(TIME))+ 
      geom_boxplot(aes(y=VAL, fill=TIME))+
      scale_y_continuous(limits=ylim) 

           
    if (median.marker=="line"){
      p = p+geom_hline(aes(yintercept=MEDIAN, colour=TIME), size=1.1, linetype="dashed")
    }
    else if (median.marker=="dot"|median.marker=="point"){
      p = p+geom_point(aes(y=MEDIAN), colour="blue", size=2.5)
    }
    
    p = p + scale_fill_manual(values=time.palette) +
        scale_colour_manual(values=c(time.palette, "blue")) +  # blue goes for the colour of the geom_point. Otherwise median.marker="dot" will fail; but the colour is still needed to be defined explicitly in geom_point as well.
        facet_grid(gridFormula) + 
        theme_bw() + 
        theme(axis.title.y=element_blank(), legend.title=element_blank(), 
              axis.title.x=element_blank(), title=element_text(title))
    
    suppressWarnings(print(p))
    
  }
  
  invisible(res)
  
}

# ggplot's way of defining default colours: http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_colour_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

