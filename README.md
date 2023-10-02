# g.princals
The best aesthetic plotting of princals function of Gifi package

#function
g.princals=function(x, plot.dim = c(1, 2),var.subset= "all", max.plot.array =2,
                    stepvec = NA, col.lines = "black",main,
                    show=c(TRUE,FALSE,T,F,1,0),
                    save=c(TRUE,FALSE,T,F,1,0),name,
                    width = 250, height =250, units = 'mm', res =300){
  require("Gifi")
  require("ggplot2")
  require("scales")
  require("grid")
  require("plyr")
  require("gridExtra")
  require("ggrepel")
  require("ggh4x")
  require("Hmisc")
  
  ################################################################################
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  } 
################################################################################
  
  if (missing(name)) name=as.character(x$call[2])
  if (missing(show)) show=1
  if (missing(save)) save=0
  if (missing(main)) main=colnames(x$data)
  if (missing(var.subset)) var.subset="all"
  if (missing(width)) width = 250
  if (missing(height)) height =250 
  if (missing(units)) units = 'mm'
  if (missing(res)) res =300
  ss=c(0,1,T,F,TRUE,FALSE)
  
  if(length(show)!=1){
    stop("Value should be one item")
  }else{
    if(sum(ss==show)!=3){
      stop("Value don´t allowed, just to be TRUE,FALSE,T,F,1 and 0")
    }
  } 
      
  if(length(save)!=1){
    stop("Value should be one item")
  }else{
    if(sum(ss==save)!=3){
      stop("Value don´t allowed, just to be TRUE,FALSE,T,F,1 and 0")
    }
  }
        
################################################################################
#biplot
main=paste0("Biplot ",x$call[2])
nvar <- dim(x$data)[2]  
VAF=x$evals[1:nvar]/sum(x$evals[1:nvar])
vaf=VAF[plot.dim]
xycoor=x$loadings
vPC1 <- xycoor[,1]*.9
vPC2 <- xycoor[,2]*.9
vlabs <- rownames(xycoor)
vPCs <- data.frame(vPC1=vPC1,vPC2=vPC2)
rownames(vPCs) <- vlabs
# put a faint circle there, as is customary
angle <- seq(-pi, pi, length = 100) 
df <- data.frame(x = sin(angle), y = cos(angle)) 
p1=ggplot(aes(x, y), data = df)+ 
coord_fixed(ratio = 1)+
theme_classic()+
geom_path(colour="grey70")+
geom_vline(aes(xintercept=0), colour="grey70", linetype="dashed")+
geom_hline(aes(yintercept=0), colour="grey70", linetype="dashed")+
scale_x_continuous(breaks =scales::pretty_breaks(n = 5),guide = "axis_minor")+
scale_y_continuous(breaks =scales::pretty_breaks(n = 5),guide = "axis_minor")+
xlab(paste0("Dim ", plot.dim[1]," (",round(vaf[1]*100,2),"%)") ) + 
ylab(paste0("Dim ", plot.dim[2]," (",round(vaf[2]*100,2),"%)") )+
ggtitle(main)+
geom_segment(data=vPCs, aes(x = 0, y = 0, xend = vPC1, yend = vPC2), arrow = arrow(length = unit(.75, 'picas')), colour = "red")+
geom_text_repel(data=vPCs, aes(x=vPC1,y=vPC2,label=rownames(vPCs),segment.colour="gray"),
                force             = 2,
                nudge_x           = .4,
                direction         = "y",
                hjust             = 0,
                segment.size      = .2,
                segment.curvature = -.1,
                min.segment.length = unit(0, 'lines'),
                colour="red",
                size=4,
                fontface = 'bold',
                point.padding = unit(1, 'lines'),
                segment.ncp = 0,
                segment.angle =0,
                box.padding = .3,
                max.time = 20
                )
################################################################################
#loadplot
main=paste0("Loadplot ",x$call[2])
pxy=data.frame(x$objectscores[,1:2]*.9)
colnames(pxy)=c("D1","D2")
p2=ggplot(data=pxy,aes(x=D1,y=D2))+ 
  coord_fixed(ratio = 1)+
  theme_classic()+
  geom_vline(aes(xintercept=0), colour="grey70", linetype="dashed")+
  geom_hline(aes(yintercept=0), colour="grey70", linetype="dashed")+
  geom_point()+
  scale_x_continuous(breaks =scales::pretty_breaks(n = 3),guide = "axis_minor")+
  scale_y_continuous(breaks =scales::pretty_breaks(n = 3),guide = "axis_minor")+
  xlab(paste0("PC ", plot.dim[1]," (",round(vaf[1]*100,2),"%)")) + 
  ylab(paste0("PC ", plot.dim[2]," (",round(vaf[2]*100,2),"%)"))+
  ggtitle(main)+
  geom_segment(data=vPCs, aes(x = 0, y = 0, xend = vPC1, yend = vPC2), arrow = arrow(length = unit(.75, 'picas')), colour = "red")+
  geom_text_repel(data=vPCs, aes(x=vPC1,y=vPC2,label=rownames(vPCs),segment.colour="gray"),
                  force             = 2,
                  nudge_x           = 2,
                  direction         = "y",
                  hjust             = 0,
                  segment.size      = .2,
                  segment.curvature = -.1,
                  min.segment.length = unit(0, 'lines'),
                  colour="red",
                  size=4,
                  fontface = 'bold',
                  point.padding = unit(1, 'lines'),
                  segment.ncp = 0,
                  segment.angle =0,
                  box.padding = .3,
                  max.time = 20
                  )
################################################################################
#screeplot
nd <-data.frame(x=1:length(x$evals),y=x$evals)
main <- paste0("Scree Plot ",x$call[2])
xlab <- "Number of Components"
ylab <- "Eigenvalues"
ylim <- c(0, max(x$evals))

p3=ggplot(nd,aes(x=x,y=y)) + 
  theme_classic(base_size = 20)+
  geom_line(colour="blue")+geom_point(colour="blue")+
  scale_x_continuous(breaks =scales::pretty_breaks(n = length(x$evals)),guide = "axis_minor")+
  scale_y_continuous(limits=ylim,breaks =scales::pretty_breaks(n = round(x$evals[1]+.5)),guide = "axis_minor")+
  ggtitle(main)+
  xlab(xlab)+
  ylab(ylab)

################################################################################
#transplot
  xlab <- "Observed"
  ylab <- "Transformed"
  main <- colnames(x$data)
  
  if (var.subset[1] == "all") var.subset <- colnames(x$data)       ## extract variables and scores to be plotted
  if (is.numeric(var.subset)) var.subset <- colnames(x$data)[var.subset]
  if (missing(main)) main <- var.subset
  if (missing(max.plot.array)) max.plot.array=2
  nvars <- length(var.subset)
  vars=length(var.subset)
  g=ceiling(nvars/(max.plot.array**2)) 
  
  if(sum(show==TRUE,show==T,show==1,na.rm=T)==3){
    print("Ok, it just shows plots")
  }else{
    print("Ok, just transplots did not show")
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
      png(paste0(name,".transplot.",k,".png"), width = width, height =height, units = units, res =res)}
    else{
      print("Transplots, it were not saved")
    }

  if (class(x$transform[,var.subset])[1] == "list") {                        ## homals-type transformation plot if copies are present
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
      img=paste0(i," of ",vars)
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
      img=paste0(i," of ",vars)
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
    print("Transplots were saved,but you dont see plots")
  }else{
    print("Watch out, Transplot was not save")
  }
}
  
  if(sum(show==TRUE,show==T,show==1,na.rm=T)==3){
    return(list(p3,p2,p1))
    print("Biplot, loadplot and scree  plots just shows")
  }else{
    print("Ok, Biplot, loadplot and scree  plots did not show")
  }
  
  if(sum(save==TRUE,save==T,save==1,na.rm=T)==3){
    ggsave(paste0(name,".biplot.png"), dpi = res,   width = width,height = height,unit=units,plot =p1)
    ggsave(paste0(name,".loadplot.png"), dpi = res,   width = width,height = height,unit=units,plot =p2)
    ggsave(paste0(name,".screeplot.png"), dpi = res,   width = width,height = height,unit=units,plot =p3)
    print("Biplot, loadplot and scree  plots were saving")
  }else{
    print("Ok, Biplot, loadplot and scree  plots were not save")
  }
}
