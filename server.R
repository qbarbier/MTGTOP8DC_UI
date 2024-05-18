library("shiny")
library("ggrepel")
library("fpc")
library("dendextend")
library("uwot")
library("plotly")
library("ggplot2")
library("png")
library("gridExtra")
library("grid")

load(file = "www/mtgtop8DC_computeTable.Rdata")
load(file = "www/mtgtop8DC_scdata.Rdata")

source("www/tierList.R")
set.seed(42)

server <- function(input, output, session){
  
  values <- reactiveValues(
    tierlist = NULL
  )
  
  updateDateInput(session, "date1", value="2024-03-25")#max(mat$date)-61)
  updateDateInput(session, "date2", value=max(mat$date))
  
  updateDateInput(session, "date1.2", value="2024-02-26")
  updateDateInput(session, "date2.2", value=max(mat$date))
  
  updateSelectInput(session,"selectCard",choices=row.names(df),selected=1)
  
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
    
    tierlist <- compute.tier.list(tierlist,input$y.value)
    tier.th <- tierlist[[2]]
    tierlist <- tierlist[[1]]
    tierlist$tier <- as.factor(tierlist$tier)
    
    x <- "percent"
    y <- input$y.value
    
    y.lab <- "MTGTOP8 Score"
    # img <- readPNG(system.file("img", "Rlogo.png", package="png"))
    
    # browser()
    
    output$plotTierList <- renderPlot({
      p <- ggplot(tierlist,aes_string(x=x,y=y,label="group",fill="tier",color="tier")) +
        geom_point(show.legend = FALSE)+
        geom_hline(yintercept = mean(tierlist[,input$y.value]),linetype = "dashed")+
        # geom_text()+
        geom_label_repel(aes_string(label="group"),colour="black")+
        # annotation_custom(rasterGrob(img), xmin = 0, xmax = 0, ymin = -5, ymax = -10)+
        scale_size_continuous(guide = "none")+
        xlab("Percent of presence %")+
        ylab(y.lab)+
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
    tierlist <- compute.tier.list(tierlist,input$y.value)[[1]]

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
    
    p <- ggplot2::ggplot(dt,aes_string(x="UMAP1",y="UMAP2",label="decks",color=input$deckColor))+
      geom_label_repel()+
      geom_point(show.legend = FALSE)+
      scale_color_gradient(low="blue", high="red")
    
    return(p)
  })
  
  output$scardCamenbert <- renderPlotly({
    if(is.null(input$selectCard)){return(NULL)}
    if(input$selectCard==""){return(NULL)}
    
    val <- df[input$selectCard,]
    val <- val[which(val>0)]
    
    val.max <- sum(val)
    all.max <- sum(apply(df, 2, function(x){max(x)}))
    
    data <- data.frame("Percent"=c(val.max,c(all.max-val.max)),
                         "Categorie"=c("In Deck","Not in Deck"))
    
    fig <- plot_ly(data, labels = ~Categorie, values = ~Percent, type = 'pie')
    fig <- fig %>% layout(title = paste0('% of deck which play ',input$selectCard),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    return(fig)
    
  })
  
  output$scardBarplot <- renderPlotly({
    if(is.null(input$selectCard)){return(NULL)}
    if(input$selectCard==""){return(NULL)}
    
    val <- df[input$selectCard,]
    val <- val[which(val>0)]
    
    df.max <- df[,names(val)]
    max <- apply(df.max, 2, function(x){max(x)})
    
    percent <- unlist((val/max)*100)
    names(percent) <- colnames(val)
    names(percent) <- gsub(" decks","",names(percent))
    
    # percent <- sort(percent, decreasing = T)
    percent.name <- factor(names(percent), level=names(sort(percent, decreasing = F)))
    
    tierlist <- compute.tier.list(values$tierlist,input$y.value)[[1]][,c("group","tier")]
    colors <- rep("tx",length(percent))
    names(colors) <- percent.name
    
    for(i in c(1:length(colors))){
      if(names(colors)[i]%in%row.names(tierlist)){
        colors[i] <- tierlist[names(colors)[i],"tier"]
      }
    }

    fig <- plot_ly(x = percent, 
                   y = percent.name, color = colors,
                   type = 'bar', orientation = 'h',
                   height=100+(20*length(percent)),
                   xaxis = list(categoryorder = "total descending")
                   )
    
    return(fig)
  })
  
}
