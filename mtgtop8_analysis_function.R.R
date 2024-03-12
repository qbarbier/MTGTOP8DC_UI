############### MTGTOP8 R scarping functions ################
#'@title MTGTOP8 Scaping
#'@description 
#'
#'@import rvest
#'
#'@param db.deck Un objet R contenant la sortie de la fonction de scraping. Si cet argument est pas NULL alors imin est pas pris en compte
#'@param db.card
#'@param imax 
#'@param form Chaine de caractere qui va indiquer quelle format seras enregistrer lors du parsing du site mtgtop8 parmis 'Duel Commander','Modern','Pauper','Leagcy' ou  'Standard
#'@param imin First event id use in php get function in mtgtop8 url
#'
#'@examples
#'
#'@author Quentin Barbier
#'
mtgtop8.scraping <- function(db.deck=NULL,db.card=NULL,imax=41200,
                             form="Duel Commander",imin=NULL){
  if(!form%in%c("Duel Commander","Modern","Pauper","Legacy","Standard","Pioneer")){
    return("Error format please select in 'Duel Commander', 
           'Modern', 'Pauper', 'Legacy','Pioneer' or 'Standard'")
  }
  if(is.null(db.deck)){
    db.deck <- list()
    if(is.null(imin)){
      imin <- 1 
    }
  } else {
    if(is.null(imin)){
      imin <- db.deck[[length(db.deck)]]$events+1
    }
  }
  
  if(is.null(db.card)){
    db.card <- list()
  }
  
  for(i in c(imin:imax)){
    print(i)
    
    tryCatch({
      # page <- read_html(i)
      
      html<-rvest::read_html(paste0("https://www.mtgtop8.com/event?e=",i))
      format<-rvest::html_text(rvest::html_nodes(html,".meta_arch"))
      format <- gsub(" *$","",format)
      if(!rlang::is_empty(format) && format == form){
        html.decks <- rvest::html_text(rvest::html_nodes(html, ".S14"))
        decks.id <- grep("(^{1}[0-8]$)|(^{1}[0-8]-{1}[0-8]$)",html.decks)
        decks <- list()
        names.decks <- list()
        
        html_deck_link <- rvest::html_attr(rvest::html_nodes(html,"a")[grep("d=",rvest::html_nodes(html,"a"))],"href")
        html_deck_link <- unique(html_deck_link)
        html_deck_link <- html_deck_link[-grep("(switch|mtgo|dec)",html_deck_link)]
        decks.list.id <- list()
        for(l in c(1:length(html_deck_link))){
          dl <- html_deck_link[[l]]
          card_html <- rvest::read_html(paste0("https://www.mtgtop8.com/event",dl))
          decks.name <- rvest::html_text(rvest::html_nodes(card_html,"a"))[grep("archetype",rvest::html_nodes(card_html,"a"))]
          print(decks.name)
          str.pos <- rvest::html_text(rvest::html_nodes(card_html,"div.event_title"))[grep("#",rvest::html_text(rvest::html_nodes(card_html,"div.event_title")))]
          str.pos <- stringr::str_extract(str.pos,"#[^ ]+")
          pos <- gsub("#","",str.pos)
          names.decks[[length(names.decks)+1]] <- pos
          decks[[length(decks)+1]] <- decks.name
          decks.id <- gsub("&f","",strsplit(dl,"=")[[1]][3])
          cardlist <- rvest::html_text(rvest::html_nodes(card_html,".L14"))
          db.card[[length(db.card)+1]] <- list("cardlist"=cardlist,"decks.name"=decks.name,
                                               "events"=i,"decks.id"=decks.id,
                                               "position"=pos)
          decks.list.id[[length(decks.list.id)+1]] <- decks.id
        }
        
        names(decks) <- unlist(names.decks)
        print(decks)
        
        if(!rlang::is_empty(decks)){
          html.date <- rvest::html_text(rvest::html_nodes(html,".S14"))
          date <- html.date[grep("players - ",html.date)[length(grep("players - ",html.date))]]
          players <- stringr::str_extract(date,"\n\t.* players -")
          players <- gsub("\n\t","",players)
          players <- gsub(" players -","",players)
          date <- gsub(".* - ","",date)
          date <- gsub("\n.*","",date)
          print(date)
          print(players)
          if(!rlang::is_empty(date)){
            db.deck[[length(db.deck)+1]] <- list("date"=date,
                                                 "format"=gsub(" *$","",format),
                                                 "decks"=decks,
                                                 "players"=as.numeric(players),
                                                 "events"=i,
                                                 "decks.id"=decks.list.id)
          }
        }
      }
      
    }, error = function(e) {
      cat("Erreur : La page", i, "n'existe pas ou n'est pas accessible.\n")
    })
    
  }
  return(list(db.deck,db.card))
}


########## CONVERT MTGTOP8 DATA #######################################
#'@title Convert MTGtop8 Rdata output to Dataframe
#'@description Convert output of scraping function to daframe for computing
#'@author Quentin Barbier
#'@exemple
#'
convert.mtgtop8.to.df <- function(db){
  db.list <- list()
  for(i in db){
    res <- lapply(c(1:length(i$decks)),function(j){
      return(list(
        "decks"=i$decks[[j]],
        "date"=i$date,
        "position"=names(i$decks)[[j]],
        "players"=i$players,
        "top.players"=length(i$decks),
        "events"=i$events
      ))
    })
    db.list[[length(db.list)+1]]<-do.call(rbind, res)
  }
  
  mat <- data.frame(do.call(rbind, db.list))
  return(mat)
}

########## MTGTOP8 ESTIMATE WINRATE #######################################
#'@title Estime win rate of mtgtop8 data
#'@author Quentin Barbier
#'@description  
#'
mtgtop8.estimate.winrate <- function(mat){
  
  ronde.suisse <- list( #Nombre de ronde par rapport au nombre de joueur
    "0-8"=3,
    "9-16"=4,
    "17-32"=5,
    "33-64"=6,
    "65-128"=7,
    "129-226"=8,
    "227-999"=9
  )
  
  pretop8.score <- list( #Nombre de win estimé d'après le nombre de ronde
    "3"=2.5,"4"=3,"5"=3.5,"6"=4,"7"=5,"8"=6,"9"=7
  )
  
  posttop8.score <- data.frame( #Nbr de pt bonus par rapport classement top/top player
    "1"  = c(0,1,2,2,2,2,3,3,4),
    "2"  = c(0,0,1,1,2,2,2,2,2),
    "3-4"= c(0,0,0,1,1,1,1,1,1),
    "3"  = c(0,0,0,1,1,1,1,1,1),
    "4"  = c(0,0,0,0,1,1,1,1,1),
    "5-8"= c(0,0,0,0,0,1,1,1,1),
    "5" =  c(0,0,0,0,0,1,1,1,1),
    "6" =  c(0,0,0,0,0,0,1,1,1),
    "7" =  c(0,0,0,0,0,0,0,1,1),
    "8" =  c(0,0,0,0,0,0,0,0,1),
    "9-16"=c(0,0,0,0,0,0,0,0,0)
  )
  posttop8.score <- t(posttop8.score)
  row.names(posttop8.score) <- c("1","2","3-4","3","4","5-8","5","6","7","8","9-16")
  colnames(posttop8.score) <- c("1","2","3","4","5","6","7","8","16")
  
  win.rate <- function(players,top.players,ronde.suisse,position){
    res <- lapply(c(1:length(ronde.suisse)),function(i){
      a <- as.numeric(stringr::str_split(names(ronde.suisse)[[i]],"-")[[1]][1])
      b <- as.numeric(stringr::str_split(names(ronde.suisse)[[i]],"-")[[1]][2])
      if(players>=a && players <=b){
        return(ronde.suisse[[i]])
      }
    })
    ronde <- unlist(res)
    score1 <- pretop8.score[[as.character(ronde)]]
    score2 <- posttop8.score[as.character(position),as.character(top.players)]
    return(score1+score2)
  }
  
  match.jouer <- function(players,top.players,ronde.suisse,position){
    res <- lapply(c(1:length(ronde.suisse)),function(i){
      a <- as.numeric(stringr::str_split(names(ronde.suisse)[[i]],"-")[[1]][1])
      b <- as.numeric(stringr::str_split(names(ronde.suisse)[[i]],"-")[[1]][2])
      if(players>=a && players <=b){
        return(ronde.suisse[[i]])
      }
    })
    ronde <- unlist(res)
    return(round(top.players/2)+ronde) #a ameliorer
  }
  
  mat <- mat[-which(!mat$position%in%rownames(posttop8.score)),]
  
  score <- unlist(lapply(c(1:dim(mat)[1]),function(i){
    players <- as.numeric(mat[i,"players"])
    position <- mat[i,"position"]
    top.players <- as.numeric(mat[i,"top.players"])
    sc <- win.rate(players,top.players,ronde.suisse,position)
    return(sc)
  }))
  
  match.jouer <- unlist(lapply(c(1:dim(mat)[1]),function(i){
    players <- as.numeric(mat[i,"players"])
    position <- mat[i,"position"]
    top.players <- as.numeric(mat[i,"top.players"])
    sc <- match.jouer(players,top.players,ronde.suisse,position)
    return(sc)
  }))
  
  mat <- cbind(mat, score, match.jouer, win.rate=(score/match.jouer)*100)
  return(mat)
}

########## MTGTOP8 ARCHEYTPE #############################
#'@title
#'
#'
#'
mtgtop8.archetype <- function(mat, type=NULL){
  group <- list()
  for(i in c(1:dim(mat)[1])){
    d.name <- mat[i,'decks']
    g <- gsub(" decks","",d.name)
    group[[length(group)+1]] <- g
  }
  group <- unlist(group)
  mat <- cbind(mat, group)
  
  # win.rate <- apply(mat[,c(6,7)],1, function(x){
  #   return((x[[1]]/x[[2]])*100)
  # })
  # mat <- cbind(mat, win.rate)
  
  mat$date <- as.Date(unlist(mat$date),"%d/%m/%y")
  mat$players <- unlist(mat$players)
  return(mat)
}

########## MTGTOP8 SINGLECARD ANALYSIS ####################
#'@title
#'
#'
#'
#'
mtgtop8.sc <- function(card){
  
  archetype <- lapply(c(1:length(card)),function(i){
    return(card[[i]]$decks.name)
  })
  
  card.archetype <- card[which(!is.na(unlist(archetype)))]
  card.archetype <- lapply(card.archetype, function(i){
    return(i$cardlist)
  })
  
  names.card.archetype <- as.vector(unlist(archetype[which(!is.na(unlist(archetype)))]))
  
  unique.card <- unique(unlist(card.archetype))
  unique.archetype <- unique(names.card.archetype)
  
  df <- data.frame(matrix(0, nrow=length(unique.card), ncol=length(unique.archetype)))
  colnames(df) <- unique.archetype
  row.names(df) <- unique.card
  
  for(x in c(1:dim(df)[1])){
    print(x)
    for(y in c(1:dim(df)[2])){
      #print(y)
      a <- colnames(df)[y]
      c <- row.names(df)[x]
      value <- 0
      for(l in which(names.card.archetype==a)){
        for(d in card.archetype[[l]]){
          if(length(which(c%in%d))>0){
            value <- value+1
          }
        }
      }
      #print(value)
      df[x,y] <- value
    }
  }
  
  return(df)
}
