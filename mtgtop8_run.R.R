source("mtgtop8_analysis_function.R.R")

############## FOUND IMAX ###################
html<-rvest::read_html("https://www.mtgtop8.com/format?f=EDH")
link<-as.character(rvest::html_nodes(rvest::html_nodes(html,"table")[[2]],"a")[[1]])
res <- regmatches(link, regexpr("e=(.*?)&amp", link))
imax <- as.numeric(gsub("&amp","",gsub("e=","",res)))
############### DUEL COMMANDER UPDATE #######
if(file.exists("mtgtop8DC.Rdata")){
  load("mtgtop8DC.Rdata")
} else {
  db <- NULL
  imax <- imax
  imin <- imax-500
}

if(file.exists("mtgtop8DCard.Rdata")){
  load("mtgtop8DCard.Rdata")
} else {
  card <- NULL
}

####### COMPARE IMAX AND EVENT MAX IN DB OBJECT

if(!is.null(db)){
  if(db[[length(db)]]$events<imax){
    run <- TRUE 
  } else {
    run <- FALSE
  }
}

if(run){

  res <- mtgtop8.scraping(db.deck=db,db.card = card,imax=imax,
                          form="Duel Commander",imin=NULL)
  db <- res[[1]]
  save(db, file = "mtgtop8DC.Rdata")
  card <- res[[2]]
  save(card, file="mtgtop8DCard.Rdata")
  
  ############## COMPUTE DECK ################
  load("mtgtop8DC.Rdata")
  mat <- convert.mtgtop8.to.df(db)
  mat <- mtgtop8.estimate.winrate(mat)
  mat <- mtgtop8.archetype(mat, type=NULL)
  save(mat, file="../mtg_top8_dc_ui/www/mtgtop8DC_computeTable.Rdata")
  
  load("mtgtop8DCard.Rdata")
  df <- mtgtop8.sc(card)
  save(df,file="../mtg_top8_dc_ui/www/mtgtop8DC_scdata.Rdata")
  
} else {
  print("No Update not necessary")
}

