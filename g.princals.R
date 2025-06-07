g.princals <- function(x,
                       plot.dim = c(1, 2),
                       var.subset = "all",
                       max.plot.array = 2,
                       stepvec = NA,
                       col.lines = "black",
                       main = NULL,
                       show = TRUE,
                       save = FALSE,
                       name = NULL,
                       width = 300,
                       height = 300,
                       units = "mm",
                       res = 480,
                       dispersion = 3,
                       colour.group ="red",
                       point.size.loadplot = 1,
                       point.alpha.loadplot = 0.1,
                       language = "english",
                       legend.position = "right",
                       legend.group = FALSE
                       ){
  ##################################### 
  # License: GNU
  # Author: José Solís, June 2025
  # email: solisbenites.jose@gmail.com
  ####################################
  #Begining time
  begin <- Sys.time()
  
  required_pkgs <- c("Gifi","plyr","tidyr","tidyverse","ggplot2","grid","dplyr","gridExtra","scales", "ggrepel", "ggh4x", "Hmisc", "RColorBrewer","beepr")

  for(i in 1:length(required_pkgs)){
  if (!requireNamespace(required_pkgs[i], quietly = TRUE)) install.packages(required_pkgs[i])
  }
  
  invisible(lapply(required_pkgs, require, character.only = TRUE))
  #invisible(lapply(required_pkgs, library, character.only = TRUE))

  ##############################################################################
  ##############################################################################
  
  cat(paste0("To begin to g.princals of ",deparse(substitute(x)), sep="\n\n"))
  
  if (missing(name)) name=as.character(x$call[2])
  if (missing(show)) show=1
  if (missing(save)) save=0
  if (missing(legend.group)) legend.group=0
  if (missing(main)) main=colnames(x$data)
  if (missing(var.subset)) var.subset="all"
  if (missing(width)) width = 300
  if (missing(height)) height =300 
  if (missing(units)) units = 'mm'
  if (missing(res)) res =480
  if (missing(dispersion)) dispersion =3
  if (missing(language)) language="english"
  if (missing(legend.position)) legend.position="right"
  if (missing(point.size.loadplot)) point.size.loadplot=1
  if (missing(point.alpha.loadplot)) point.alpha.loadplot=.1

  ss=c(0,1,T,F,TRUE,FALSE)
  
  if(length(show)!=1){
    beepr::beep(7)
    stop("'show' should be one item: TRUE,FALSE,T,F,1 or 0")
  }else{
    if(sum(ss==show)!=3){
      beepr::beep(7)
      stop("'show'don´t allowed, just to be TRUE,FALSE,T,F,1 or 0")
    }
  }
  
  if(length(save)!=1){
    beepr::beep(7)
    stop("'save' should be one item: TRUE,FALSE,T,F,1 or 0")
  }else{
    if(sum(ss==save)!=3){
      beepr::beep(7)
      stop("'save' don´t allowed, just to be TRUE,FALSE,T,F,1 or 0")
    }
  }
  
  if(length(legend.group)!=1){
    beepr::beep(7)
    stop("'legend.group' should be one item:TRUE,FALSE,T,F,1 or 0")
  }else{
    if(sum(ss==legend.group)!=3){
      beepr::beep(7)
      stop("'legend.group' don´t allowed, just to be TRUE,FALSE,T,F,1 or 0")
    }
  } 
  
  if(!is.numeric(dispersion)){beepr::beep(7);stop("Dispersion of loadplot´s points should a number between 0.5 to 6")}
  if(!is.numeric(point.size.loadplot)){beepr::beep(7);stop("point.size.loadplot of loadplot´s points should a number between 0.01 to 3")}
  if(!is.numeric(point.alpha.loadplot)){beepr::beep(7);stop("point.alpha.loadplot of loadplot´s points should a number between 0.01 to 1")}
  if((dispersion>=.5&dispersion<=6)==0){beepr::beep(7);stop("Dispersion of loadplot´s points should a number between 0.5 to 6")}
  if((point.size.loadplot>=.01&point.size.loadplot<=3)==0) {beepr::beep(7);stop("point.size.loadplot of loadplot´s points should a number between 0.01 to 3")}
  if((point.alpha.loadplot>=.01&point.alpha.loadplot<=1)==0) {beepr::beep(7);stop("point.alpha.loadplot of loadplot´s points should a number between 0.01 to 1")}
  
  if (missing(colour.group)){
    col="red";legend.group=0
  }else{
    if(ncol(x$data)==length(colour.group)){
      col=factor(colour.group,levels = sort(unique(colour.group)))
    }else{
      beepr::beep(7);stop("Number of groups shoud be the same numbers of columns")
    }
  }
  
  if(length(language)!=1){
    beepr::beep(7)
    stop("'language' should be one item: english or spanish")
  }else{
    if(sum(c("english","spanish")==language)!=1){
      beepr::beep(7)
      stop("'language'don´t allowed, just to be english or spanish")
    }
  }
  
  if(language=="spanish"){
    label.language=c("Grupo de\n variables","CP ","Número de componentes","Autovalores","Observado","Transformado"," de ")
    title.biplot <- "Gráfico biplot "
    title.loadplot <- "Gráfico de cargas "
    title.scree <- "Gráfico de sedimentación "
  }
  
  if(language=="english"){
    label.language=c("Group of\nvariables","PC ","Number of Components","Eigenvalues","Observed","Transformed"," of ")
    title.biplot <- "Biplot "
    title.loadplot <- "Loadplot "
    title.scree <- "Scree Plot "
  }
  
  beepr::beep(2)
  beepr::beep(2)#mario bros sound
  
  ################################################################################
  #biplot
  main=paste0(title.biplot,name)
  nvar <- dim(x$data)[2]  
  VAF=x$evals[1:nvar]/sum(x$evals[1:nvar])
  vaf=VAF[plot.dim]
  xycoor=x$loadings
  vPC1 <- xycoor[,1]
  vPC2 <- xycoor[,2]
  vlabs <- rownames(xycoor)
  vPCs <- data.frame(vPC1=vPC1,vPC2=vPC2)
  rownames(vPCs) <- vlabs
  vPCs$col=col
  
  # put a faint circle there, as is customary
  angle <- seq(-pi, pi, length = 100) 
  df <- data.frame(x = sin(angle), y = cos(angle))
  
  p1=ggplot(aes(x, y), data = df)+ 
    theme_classic()+
    geom_path(colour="grey70")+
    geom_vline(aes(xintercept=0), colour="grey70", linetype="dashed")+
    geom_hline(aes(yintercept=0), colour="grey70", linetype="dashed")+
    scale_x_continuous(breaks =scales::pretty_breaks(n = 5),guide = "axis_minor")+
    scale_y_continuous(breaks =scales::pretty_breaks(n = 5),guide = "axis_minor")+
    xlab(paste0("Dim ", plot.dim[1]," (",round(vaf[1]*100,2),"%)") ) + 
    ylab(paste0("Dim ", plot.dim[2]," (",round(vaf[2]*100,2),"%)") )+
    ggtitle(main)+
    geom_segment(data=vPCs, aes(x = 0, y = 0, xend = vPC1, yend = vPC2,colour=col), arrow = arrow(length = unit(.75, 'picas')))+
    scale_colour_brewer(name=label.language[1],palette="Set1")+
    geom_text_repel(data= subset(vPCs,vPC1>0), aes(x=vPC1,y=vPC2,label=rownames(subset(vPCs,vPC1>0)),colour=col,segment.colour="black"),
                    force   = .1,
                    nudge_x = 1.25-subset(vPCs,vPC1>0)$vPC1*.5,
                    direction = "y",
                    vjust = .5,
                    hjust = 0,
                    segment.size      = .2,
                    segment.curvature = -.1,
                    min.segment.length = unit(0, 'lines'),
                    size=4,
                    fontface = 'bold',
                    point.padding = unit(1, 'lines'),
                    segment.ncp = 3,
                    segment.angle =45,
                    box.padding = unit(1, 'lines'),
                    max.time = 10)+
    scale_colour_brewer(name=label.language[1],palette="Set1")+
    geom_text_repel(data= subset(vPCs,vPC1<0), aes(x=vPC1,y=vPC2,label=rownames(subset(vPCs,vPC1<0)),segment.colour="black",colour=col),
                    force   = .1,
                    nudge_x = -1.25-subset(vPCs,vPC1<0)$vPC1*.5,
                    direction = "y",
                    vjust = .5,
                    hjust = 1,
                    segment.size      = .2,
                    segment.curvature = -.1,
                    min.segment.length = unit(0, 'lines'),
                    size=4,
                    fontface = 'bold',
                    point.padding = unit(1, 'lines'),
                    segment.ncp = 3,
                    segment.angle =45,
                    box.padding =unit(1, 'lines'),
                    max.time = 10)+
    scale_colour_brewer(name=label.language[1],palette="Set1")+
    coord_fixed(ratio = 1)+
    theme(plot.title = element_text(size = 18, face = "bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          legend.position=legend.position,
          legend.title=element_text(size=14, face = "bold"), 
          legend.text=element_text(size=14))+
    if(legend.group==0){
      theme(legend.position="none")
    }else{
      guides(colour = guide_legend(override.aes = aes(label ="")))
    }
  ################################################################################
  #loadplot
  main=paste0(title.loadplot,name)
  pxy=data.frame(x$objectscores[,1:2])
  
  mx=mean(pxy[,1],na.rm = T)
  sx=sd(pxy[,1],na.rm = T)
  lx1=mx-dispersion*sx
  lx2=mx+dispersion*sx
  
  my=mean(pxy[,2],na.rm = T)
  sy=sd(pxy[,2],na.rm = T)
  ly1=my-dispersion*sy
  ly2=my+dispersion*sy
  
  pxy1=subset(pxy,pxy[,1]>lx1)
  pxy2=subset(pxy1,pxy1[,1]<lx2)
  pxy3=subset(pxy2,pxy2[,2]>ly1)
  pxy4=subset(pxy3,pxy3[,2]<ly2)
  l1=min(-1*max(abs(pxy4[,1]),na.rm = T)-sd(pxy4[,1],na.rm = T),-1*max(abs(pxy4[,2]),na.rm = T)-sd(pxy4[,2],na.rm = T),na.rm=T)*(1+.1/(.5*dispersion**3+dispersion))
  l2=max(max(abs(pxy4[,1]),na.rm = T)+sd(pxy4[,1],na.rm = T),max(abs(pxy4[,2]),na.rm = T)+sd(pxy4[,2],na.rm = T),na.rm=T)*(1+1/(.5*dispersion**3+dispersion))
  colnames(pxy4)=c("D1","D2")
  
  p2=ggplot(data=pxy4,aes(x=D1,y=D2))+ 
    theme_classic()+
    geom_vline(aes(xintercept=0), colour="grey70", linetype="dashed")+
    geom_hline(aes(yintercept=0), colour="grey70", linetype="dashed")+
    geom_point(size =point.size.loadplot,alpha =point.alpha.loadplot)+
    xlab(paste0(label.language[2], plot.dim[1]," (",round(vaf[1]*100,2),"%)"))+ 
    ylab(paste0(label.language[2], plot.dim[2]," (",round(vaf[2]*100,2),"%)"))+
    ggtitle(main)+
    geom_segment(data=vPCs, aes(x = 0, y = 0, xend = vPC1, yend = vPC2,colour=col),arrow = arrow(length = unit(.75, 'picas')))+
    scale_colour_brewer(name=label.language[1],palette="Set1")+
    geom_text_repel(data= subset(vPCs,vPC1>0), aes(x=vPC1,y=vPC2,label=rownames(subset(vPCs,vPC1>0)),segment.colour="black",colour=col),
                    force   = .1,
                    nudge_x =1.5-subset(vPCs,vPC1>0)$vPC1*.5,
                    direction = "y",
                    vjust = .5,
                    hjust = 0,
                    segment.size      = .2,
                    segment.curvature = -.1,
                    min.segment.length = unit(0, 'lines'),
                    size=4,
                    fontface = 'bold',
                    point.padding = unit(1, 'lines'),
                    segment.ncp = 3,
                    segment.angle =45,
                    box.padding = unit(1, 'lines'),
                    max.time = 100)+
    scale_colour_brewer(name=label.language[1],palette="Set1")+
    geom_text_repel(data= subset(vPCs,vPC1<0), aes(x=vPC1,y=vPC2,label=rownames(subset(vPCs,vPC1<0)),segment.colour="black",colour=col),
                    force   = .1,
                    nudge_x = -1.5-subset(vPCs,vPC1<0)$vPC1*.5,
                    direction = "y",
                    vjust = .5,
                    hjust = 1,
                    segment.size      = .2,
                    segment.curvature = -.1,
                    min.segment.length = unit(0, 'lines'),
                    size=4,
                    fontface = 'bold',
                    point.padding = unit(1, 'lines'),
                    segment.ncp = 3,
                    segment.angle =45,
                    box.padding = unit(1, 'lines'),
                    max.time = 100)+
    scale_x_continuous(limits=c(l1,l2),breaks =scales::pretty_breaks(n = 4),guide = "axis_minor")+
    scale_y_continuous(limits=c(l1,l2),breaks =scales::pretty_breaks(n = 4),guide = "axis_minor")+
    scale_colour_brewer(name=label.language[1],palette="Set1")+
    coord_fixed(ratio = 1)+
    theme(plot.title = element_text(size = 18, face = "bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          legend.position=legend.position,
          legend.title=element_text(size=14, face = "bold"), 
          legend.text=element_text(size=14))+
    if(legend.group==0){
      theme(legend.position="none")
    }else{
      guides(colour = guide_legend(override.aes = aes(label = "")))
    }
  
  ################################################################################
  #screeplot
  nd <-data.frame(x=1:length(x$evals),y=x$evals)
  main <- paste0(title.scree,name)
  xlab <- label.language[3]
  ylab <- label.language[4]
  ylim <- c(0, max(x$evals))
  
  p3=ggplot(nd,aes(x=x,y=y)) + 
    theme_classic(base_size = 20)+
    geom_line(colour="blue")+geom_point(colour="blue")+
    scale_x_continuous(breaks =scales::pretty_breaks(n = length(x$evals)),guide = "axis_minor")+
    scale_y_continuous(limits=ylim,breaks =scales::pretty_breaks(n = round(x$evals[1]+.5)),guide = "axis_minor")+
    ggtitle(main)+
    xlab(xlab)+
    ylab(ylab)+
    theme(plot.title = element_text(size = 18, face = "bold"))
  
  ################################################################################
  #transplot
  xlab <-  label.language[5]
  ylab <-  label.language[6]
  main <- colnames(x$data)
  
  if (var.subset[1] == "all") var.subset <- colnames(x$data)       ## extract variables and scores to be plotted
  if (is.numeric(var.subset)) var.subset <- colnames(x$data)[var.subset]
  if (missing(main)) main <- var.subset
  if (missing(max.plot.array)) max.plot.array=2
  nvars <- length(var.subset)
  vars=length(var.subset)
  g=ceiling(nvars/(max.plot.array**2)) 
  
  if(show %in% c(TRUE, 1)){
    cat(paste0("Ok, it just shows ",name,"-transplots"))
  }else{
    cat(paste0("Ok, just ",name,"´s transplots did not show"))
  }
  
  vx=NA
  vy=NA
  
  if(nvars%%max.plot.array**2==0){
    vy=seq(max.plot.array**2,nvars,by=max.plot.array**2)
    vx=c(1,vy+1%%max.plot.array**2)
  }else{
    vy=c(seq(max.plot.array**2,nvars,by=max.plot.array**2),nvars)
    vx=c(1,vy+1%%max.plot.array**2)
  }
  nn=data.frame(vx[1:as.numeric(g)],vy[1:as.numeric(g)]) 
  
  for(k in 1:as.numeric(g)){
    if(sum(save==TRUE,save==T,save==1,na.rm=T)==3){
      png(filename = paste0(name,".transplot.",k,".png"),res=res,width=width,height=height,units=units)
    }else{
      cat(paste0(name,"´s Transplots, it were not saved"))
    }
    if (class(x$transform[,var.subset])[1] == "list") {           ## homals-type transformation plot if copies are present
      nvars <- length(var.subset)                                 ## number of variables to be plotted
      plotvars <- as.matrix(x$datanum[,var.subset])   
      xlabels <- as.data.frame(x$data[,var.subset])
      ploty <- x$transform[,var.subset]   ## list
      ploty <- lapply(ploty, function(cc) if (ncol(cc) < length(plot.dim)) as.matrix(cc) else as.matrix(cc[, plot.dim]))
      knotsv <- x$knots[var.subset]
      ordv <- x$ordinal[var.subset]
      
      if (missing(max.plot.array)) {
        npanv <- ceiling(sqrt(nvars))
        npanh <- floor(sqrt(nvars))
        if (npanv * npanh < nvars) npanv <- npanv + 1
        if (npanv == 1 && npanh == 1) parop <- FALSE else parop <- TRUE
      } else {
        if (length(max.plot.array) < 2){
          npanv <- max.plot.array[1]
          npanh <- max.plot.array[1]
        } else {
          npanv <- max.plot.array[1]
          npanh <- max.plot.array[2]
        }
        npanv <- max(npanv, 1)
        npanh <- max(npanh, 1)
        if (npanv == 1 && npanh == 1) parop <- FALSE else parop <- TRUE
      }
      if (parop) op <- par(mfrow = c(npanv, npanh))
      
      for (i in (nn[k,1]:nn[k,2])) {
        img=paste0(i,label.language[7],vars)
        ylims <- range(c(ploty[[i]]))*1.05
        for (j in 1:ncol(ploty[[i]])) {
          if (length(col.lines) < ncol(ploty[[i]])) col.lines <- 1:ncol(ploty[[i]])
          x1 <- plotvars[,i]
          y1 <- ploty[[i]][,j]
          xy <- cbind(x1, y1)
          ord <- order(xy[,1])
          
          if (!is.factor(xlabels[,i])) xlabels[,i] <- round(xlabels[,i], 2)
          if (is.na(stepvec[1])) crit <- (length(knotsv[[i]]) == (length(unique(plotvars[,i]))-2)) else crit <- stepvec[i]
          if (crit) {    ## plot step function
            sfun0  <- stepfun(xy[ord,1][-1], xy[ord,2], f = 0)    
            if (ordv[i]) vert <- TRUE else vert <- FALSE
            if (j == 1) {
              #plot(sfun0, xlab = xlab, ylab = ylab, main = main[i], xaxt = "n", col = col.lines[j], do.points = FALSE, verticals = vert, ylim = ylims, lwd = 2, ...)
              plot(sfun0, xlab = xlab, ylab = ylab, main = main[i], xaxt = "n", col = col.lines[j], do.points = FALSE, verticals = vert, ylim = ylims, lwd = 2)
              abline(v = x1, col = "gray", lwd = 0.8)
              axis(1, labels = xlabels[,i], at = x1) 
              axis(2, col = col.lines[j])                                   # formatted Y-axis
              
              mtext(img, side = 1, line =3 , cex = 0.8, adj =.9)
              minor.tick( nx = 2, ny = 2, tick.ratio = 0.5,
                          x.args = list(lwd = 1),     # X-minor tick format argumnets
                          y.args = list(col = col.lines[j]))
            } else {
              plot(sfun0, xlab = xlab, ylab = ylab, main = main[i], xaxt = "n", col = col.lines[j], do.points = FALSE, 
                   add = TRUE, verticals = vert, lwd = 2)
            }
          } else {               
            if (j == 1) {
              plot(xy[ord,1], xy[ord,2], type = "l", xlab = xlab, ylab = ylab, main = main[i], xaxt = "n", col = col.lines[j],ylim = ylims, lwd = 2)
              axis(1, labels = xlabels[,i], at = x1) 
              axis(2, col = col.lines[j])                                   # formatted Y-axis
              minor.tick( nx = 2, ny = 2, tick.ratio = 0.5,
                          x.args = list(lwd = 1),     # X-minor tick format argumnets
                          y.args = list(col = col.lines[j]))
              mtext(img, side = 1, line =3 , cex = 0.8, adj =.9)
            } else {
              points(xy[ord,1], xy[ord,2], type = "l", col = col.lines[j], lwd = 2)
            }
          }
          
        }
      }
      if (parop) on.exit(par(op))
    } else {                                                      ## standard transformation plot with no copies
      nvars <- length(var.subset)                                 ## number of variables to be plotted
      plotvars <- as.matrix(x$datanum[,var.subset])   
      xlabels <- as.data.frame(x$data[,var.subset])
      ploty <- as.matrix(x$transform[,var.subset])
      knotsv <- x$knots[var.subset]
      ordv <- x$ordinal[var.subset]
      
      if (is.na(max.plot.array)) {
        npanv <- ceiling(sqrt(nvars))
        npanh <- floor(sqrt(nvars))
        if (npanv * npanh < nvars) npanv <- npanv + 1
        if (npanv == 1 && npanh == 1) parop <- FALSE else parop <- TRUE
      } else {
        if (length(max.plot.array) < 2){
          npanv <- max.plot.array[1]
          npanh <- max.plot.array[1]
        } else {
          npanv <- max.plot.array[1]
          npanh <- max.plot.array[2]
        }
        npanv <- max(npanv, 1)
        npanh <- max(npanh, 1)
        if (npanv == 1 && npanh == 1) parop <- FALSE else parop <- TRUE
      }
      if (parop) op <- par(mfrow = c(npanv, npanh))
      
      ## begin plotting
      for (i in nn[k,1]:nn[k,2]) {
        img=paste0(i,label.language[7],vars)
        x1 <- plotvars[,i]
        y1 <- ploty[,i]
        xy <- cbind(x1, y1)
        ord <- order(xy[,1])
        
        if (!is.factor(xlabels[,i])) xlabels[,i] <- round(xlabels[,i], 2)
        if (is.na(stepvec[1])) crit <- length(knotsv[[i]]) == (length(unique(plotvars[,i]))-2) else crit <- stepvec[i]
        if (crit) {    ## plot step function
          sfun0  <- stepfun(xy[ord,1][-1], xy[ord,2], f = 0)    
          if (ordv[i]) vert <- TRUE else vert <- FALSE
          plot(sfun0, xlab = xlab, ylab = ylab, main = main[i], xaxt = "n", col = col.lines, do.points = FALSE, verticals = vert)
          #plot(sfun0, xlab = xlab, ylab = ylab, main = main[i], xaxt = "n", col = col.lines, do.points = FALSE, verticals = vert)
          
          axis(1, labels = xlabels[,i], at = x1) 
          axis(2, col = col.lines)
          mtext(img, side = 1, line =3 , cex = 0.8, adj =.9)
          minor.tick( nx = 2, ny = 2, tick.ratio = 0.5,
                      x.args = list(lwd = 1),     # X-minor tick format argumnets
                      y.args = list(col = col.lines))
          
        } else {
          plot(xy[ord,1], xy[ord,2], type = "l", xlab = xlab, ylab = ylab, main = main[i], xaxt = "n", col = col.lines)
          
          axis(1, labels = xlabels[,i], at = x1) 
          axis(2, col = col.lines)
          mtext(img, side = 1, line =3 , cex = 0.8, adj =.9)
          minor.tick( nx = 2, ny = 2, tick.ratio = 0.5,
                      x.args = list(lwd = 1),     # X-minor tick format argumnets
                      y.args = list(col = col.lines))
        }
      }
      if (parop) on.exit(par(op))
    } 
    
    if(sum(save==TRUE,save==T,save==1,na.rm=T)==3){
      dev.off()
      cat(paste0(name,"´s Transplots were saved,but you do not see plots\n"))
    }else{
      cat(paste0("Watch out, ",name,"-Transplot was not save\n"))
    }
  }
  
  if(sum(show==TRUE,show==T,show==1,na.rm=T)==3){
    cat(paste0(name,"´s Biplot, loadplot and scree  plots has just showed\n"))
    #return(list(p3,p2,p1))
    }else{
    cat(paste0("Ok, ",name,"´s Biplot, loadplot and scree  plots did not show\n"))
  }
    if (sum(save==TRUE,save==T,save==1,na.rm=T)==3){
      ggsave(paste0(name, ".biplot.png"), plot = p1, dpi = res, width = width, height = height, units = units)
      ggsave(paste0(name, ".loadplot.png"), plot = p2, dpi = res, width = width, height = height, units = units)
      ggsave(paste0(name, ".screeplot.png"), plot = p3, dpi = res, width = width, height = height, units = units)
      cat(paste0(name,"´s Biplot, loadplot and scree  plots were saving\n"))
    }else{
      cat(paste0("Ok, ",name,"´s Biplot, loadplot and scree  plots were not save\n"))
  }
  
################################################################################
################################################################################
end <- Sys.time()#ending time
tem <- round(difftime(end, begin, units = "secs"),2)
cat(paste0("Working time was ",tem, " seconds",sep="\n\n"))
cat(paste0(name,"´s g.princals finished successfully!!", sep="\n\n"))
beepr::beep(8)#mario bros sound

if(sum(show==TRUE,show==T,show==1,na.rm=T)==3){
  return(list(p3,p2,p1))
}else{
  return(invisible())
}

}