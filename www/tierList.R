tier.list.dc.mtgtop8 <- function(mat, date.d=NULL, date.f=NULL, limit.players=NULL){
  temp <- mat
  
  if(!is.null(date.d)){
    temp <- temp[which(temp$date>=date.d),]
  }
  
  if(!is.null(date.f)){
    temp <- temp[which(temp$date<=date.f),]
  }
  
  if(!is.null(limit.players)){
    temp <- temp[which(temp$players>=limit.players),]
  }
  
  list.arche <- unique(temp$group)
  
  win.rate.arche <- list()
  sd.rate.arche <- list()
  min.rate.arche <- list()
  max.rate.arche <- list()
  taux.player.arch <- list()
  for(ar in list.arche){
    m.w.r <- mean(temp[which(temp$group==ar),"win.rate"])
    t.p.r <- (length(which(temp$group==ar))/dim(temp)[1])*100
    s.w.r <- sd(temp[which(temp$group==ar),"win.rate"])
    min <- min(temp[which(temp$group==ar),"win.rate"])
    max <- max(temp[which(temp$group==ar),"win.rate"])
    
    win.rate.arche[[ar]] <- m.w.r
    taux.player.arch[[ar]] <- t.p.r
    sd.rate.arche[[ar]] <- s.w.r
    min.rate.arche[[ar]] <- min
    max.rate.arche[[ar]] <- max
  }
  nbr.deck <- table(temp$group)
  nbr.deck <- nbr.deck[unlist(list.arche)]
  res <- list(list.arche,
              win.rate.arche,
              sd.rate.arche,
              min.rate.arche,
              max.rate.arche,
              taux.player.arch,
              nbr.deck)
  
  tierlist <- data.frame(do.call(cbind, res))[-1]
  tierlist <- cbind(tierlist, row.names(tierlist))
  #row.names(tierlist) <- c(1:dim(tierlist)[1])
  
  
  tierlist[,dim(tierlist)[2]] <- as.factor(tierlist[,dim(tierlist)[2]])
  tierlist[,1] <- as.numeric(tierlist[,1])
  tierlist[,2] <- as.numeric(tierlist[,2])
  tierlist[,3] <- as.numeric(tierlist[,3])
  tierlist[,4] <- as.numeric(tierlist[,4])
  tierlist[,5] <- as.numeric(tierlist[,5])
  tierlist[,6] <- as.numeric(tierlist[,6])
  colnames(tierlist) <- c("win.rate","sd.win.rate","min.win.rate","max.win.rate",
                          "percent","nbr.deck","group")
  
  return(tierlist)  
}

compute.tier.list <- function(tierlist){
  
  x <- scale(tierlist$win.rate, center = T, scale = T)
  y <- scale(tierlist$percent, center=T, scale=T)
  score <- unlist(x+y)[,1]
  tiers <- cbind("decks"=row.names(tierlist),"score"=as.numeric(score))
  #tiers <- tiers[order(as.numeric(tiers[,2])),]
  m <- mean(score)
  sd <- sd(score)
  tier.th <- list(
    "t0"=m+(3*sd),"t0.5"=m+(2*sd),"t1"=m+sd,"t1.5"=m,
    "t2"=m-sd,"t2.5"=m-(2*sd),"t3"=m-(3*sd)
  )

  tier <- unlist(lapply(c(1:dim(tiers)[1]),function(i){
    return(names(tier.th)[which(tier.th<as.numeric(tiers[i,2]))[1]])
  }))
  
  #browser()
  tierlist <- cbind(tierlist, tier, score)
  
  return(list(tierlist,tier.th))
}
