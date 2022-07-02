plotOverlappingHist <- function(a, b, colors=c("skyblue","steelblue","navyblue"),
                                breaks=NULL, xlim=NULL, ylim=NULL){
  
  ahist=NULL
  bhist=NULL
  
  if(!(is.null(breaks))){
    ahist=hist(a,breaks=breaks,freq=F,plot=F)
    bhist=hist(b,breaks=breaks,freq=F,plot=F)
  } else {
    ahist=hist(a,freq=F,plot=F)
    bhist=hist(b,freq=F,plot=F)
    
    dist = ahist$breaks[2]-ahist$breaks[1]
    breaks = seq(min(ahist$breaks,bhist$breaks),max(ahist$breaks,bhist$breaks),dist)
    
    ahist=hist(a,breaks=breaks,freq=F,plot=F)
    bhist=hist(b,breaks=breaks,freq=F,plot=F)
  }
  
  if(is.null(xlim)){
    xlim = c(min(ahist$breaks,bhist$breaks),max(ahist$breaks,bhist$breaks))
  }
  
  if(is.null(ylim)){
    ylim = c(0,max(ahist$counts,bhist$counts))
  }
  
  overlap = ahist
  for(i in 1:length(overlap$counts)){
    if(ahist$counts[i] > 0 & bhist$counts[i] > 0){
      overlap$counts[i] = min(ahist$counts[i],bhist$counts[i])
    } else {
      overlap$counts[i] = 0
    }
  }
  
  plot(ahist, xlim=xlim, ylim=ylim, col=colors[1])
  plot(bhist, xlim=xlim, ylim=ylim, col=colors[2], add=T)
  plot(overlap, xlim=xlim, ylim=ylim, col=colors[3], add=T)
}