library("shiny")
library("ggrepel")
library("fpc")
library("dendextend")
library("uwot")

load(file = "www/mtgtop8DC_computeTable.Rdata")
load(file = "www/mtgtop8DC_scdata.Rdata")

source("www/tierList.R")
set.seed(42)

server <- function(input, output, session){
  
  values <- reactiveValues(
    tierlist = NULL
  )
  
  updateDateInput(session, "date1", value=max(mat$date)-61)
  updateDateInput(session, "date2", value=max(mat$date))
  
  updateDateInput(session, "date1.2", value=max(mat$date)-61)
  updateDateInput(session, "date2.2", value=max(mat$date))
  
  output$dataBaseSetting <- renderText({
    str <- paste0("Tools version : 0.03. \n",
    "Data downloaded from ",min(mat$date)," to ",max(mat$date),"\n",
    "We have ",dim(mat)[1]," entry and ",length(unique(mat$group))-1," Archetype"
    )
    return(str)
  })

  observe({
    if(is.null(input$date1)){return(NULL)}
    if(is.null(input$date2)){return(NULL)}
    if(is.null(input$players)){return(NULL)}
    #require(input$minPercent)
    
    tierlist <- tier.list.dc.mtgtop8(mat,date.d=input$date1, 
                  date.f = input$date2, limit.players=input$players)
    values$tierlist <- tierlist
    
    th.percent <- mean(tierlist$percent)
    updateSliderInput(session,"minPercent",value=round(th.percent,1))
  })
  
  
  observe({
    if(is.null(values$tierlist)){return(NULL)}
    if(is.null(input$minPercent)){return(NULL)}
    tierlist <- values$tierlist
    
    if(dim(tierlist)[1]==0){
      output$plotTierList <- renderPlot({plot(1, pch=" ");text(1,1,"No data found")})
      output$plotTierList2  <- renderPlot({plot(1, pch=" ");text(1,1,"No data found")})
      output$plotTierList3  <- renderPlot({plot(1, pch=" ");text(1,1,"No data found")})
      return(NULL)
    }
    # browser()
    th.percent <- input$minPercent
    #th.percent <- mean(tierlist$percent)#-(sd(tierlist$percent)/2)
    
    tierlist <- tierlist[which(tierlist$percent>th.percent),]
    tierlist$sd.win.rate <- 100*(1/tierlist$sd.win.rate)
    
    tierlist <- compute.tier.list(tierlist)
    tier.th <- tierlist[[2]]
    tierlist <- tierlist[[1]]
    tierlist$tier <- as.factor(tierlist$tier)
    
    x <- "percent"
    y <- "win.rate"
    
    output$plotTierList <- renderPlot({
      p <- ggplot(tierlist,aes_string(x=x,y=y,label="group",fill="tier")) +
        geom_point(show.legend = FALSE)+ 
        geom_hline(yintercept = mean(tierlist$win.rate),linetype = "dashed")+
        geom_label_repel()+
        scale_size_continuous(guide = "none")+
        xlab("Percent of presence %")+
        ylab("Estimate Win Rate %")+
        labs(title ="Presence vs Win rate Duel commander",
             subtitle=paste0("From ",input$date1," to ",input$date2,", with ",sum(tierlist$nbr.deck),
                             " decks. Precense thresold ",min(round(tierlist$percent,2))),
             caption=paste0("This archerytpe cover ",round(sum(tierlist$percent),2),"% of all decks"))
      return(p)
    })
    
    output$plotTierList2 <- renderPlot({
      order.group <- tierlist$group[order(tierlist$percent,decreasing = F)]
      p <- ggplot(tierlist, aes(x=factor(group,level=order.group),y=nbr.deck,fill=tier))+
                    geom_col() + coord_flip()+
      xlab(NULL)+ylab(NULL)
      
      return(p)
    })
    
    tierlist <- tierlist[order(as.numeric(tierlist$score)),]
    output$plotTierList3 <- renderPlot({
      p <- ggplot(tierlist,aes(x=factor(group,levels=tierlist$group),y=score,color=tier))+
        geom_point()+
        xlab("Archetype")+
        ylab("Sum of normalize value")+
        geom_hline(yintercept = unlist(tier.th),linetype = "dashed")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      return(p)
    })
  })
  
  
  output$deckClustering <- renderPlot({
    if(is.null(input$deckColor)){return(NULL)}
    df <- df[-c(grep("^Island$|^Plains$|^Mountain$|^Swamp$|^Forest$",row.names(df))),]
    colnames(df) <- gsub(" decks","",colnames(df))
    df <- df[,-grep("Other ",colnames(df))]
    df <- apply(df, 2, scale)
    set.seed(42)
    dt <- uwot::umap(t(df))
    colnames(dt) <- c("UMAP1","UMAP2")
    dt <- cbind(dt, "decks"=row.names(dt))
    
    date.d <- input$date1.2
    date.f <- input$date2.2
    
    tierlist <- tier.list.dc.mtgtop8(mat,date.d=min(mat$date),
                                     date.f = max(mat$date),
                                     limit.players=12)
    tierlist <- compute.tier.list(tierlist)[[1]]

    Presence <- tierlist[row.names(dt),"percent"]
    Win.rate <- tierlist[row.names(dt),"win.rate"]
    Score <- tierlist[row.names(dt),"score"]
    Tierlist <- tierlist[row.names(dt),"tier"]
    dt <- cbind(dt, "Presence"=Presence, "Win.rate" = Win.rate,
                "Score"=Score, "Tierlist"=Tierlist)

    dt <- data.frame(dt[-which(is.na(dt[,input$deckColor])),])
    dt$UMAP1 <- as.numeric(unlist(dt$UMAP1))
    dt$UMAP2 <- as.numeric(unlist(dt$UMAP2))
    dt$Presence <- as.numeric(unlist(dt$Presence))
    dt$Win.rate <- as.numeric(unlist(dt$Win.rate))
    dt$Score <- as.numeric(unlist(dt$Score))
    
    if(!is.null(input$minPercent2)){
      dt <- dt[which(dt$Presence>as.numeric(input$minPercent2)),]
    }
    
    p <- ggplot(dt,aes_string(x="UMAP1",y="UMAP2",label="decks",color=input$deckColor))+
      geom_label_repel()+
      geom_point(show.legend = FALSE)+
      scale_color_gradient(low="blue",mid="yellow", high="red")
    
    return(p)
  }) 
  
}
