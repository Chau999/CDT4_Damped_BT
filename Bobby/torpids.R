# get data from online

dataGather <- function(years=1,torpids=TRUE){
  df <- data.frame(Boat_1=character(),
                   Boat_2=character(), 
                   outcome=character(), 
                   stringsAsFactors=FALSE) 
  
  tmpdf <- data.frame(Boat=character(),
                       Day_1=numeric(), 
                       Day_2=numeric(),
                       Day_3=numeric(), 
                       Day_4=numeric(), 
                       stringsAsFactors=FALSE)  
  for (year in seq_len(years)){

    yeardf <- data.frame(Boat=character(),
                     Day_1=numeric(), 
                     Day_2=numeric(),
                     Day_3=numeric(), 
                     Day_4=numeric(), 
                     stringsAsFactors=FALSE)            
    print(paste0('Currently downloading data from the year: ', 2019-year))
    # make sure the year being tried actually has data
    if (class(try(readLines(paste0('http://eodg.atm.ox.ac.uk/user/dudhia/rowing/bumps/e', 2019-year,'/e', 2019-year,'m.txt'))
                  ))!='try-error'){
      eightsFile <-readLines(paste0('http://eodg.atm.ox.ac.uk/user/dudhia/rowing/bumps/e', 2019-year,'/e', 2019-year,'m.txt'))
      # remove the first line
      for (ii in seq(2, length(eightsFile))){
        line <- eightsFile[ii]
        # remove the lines without college races
        if ((!grepl('=',line)) &&  (!grepl('\\(', line)) ){
          line <- unlist(strsplit(line,'  '))
          
          line <- line[which(line != '')]
          
          if (year==8 && line[1]=="St Antony's II"){
            line<- c(line[seq_len(length(line)-1)],'-1','-12')
          }
         
          
          yeardf[nrow(df)+1,] <- line
          
        }
      }
    }

  yeardf$Day_1 <- as.numeric(yeardf$Day_1) 
  
  yeardf$Day_2 <- as.numeric(yeardf$Day_2)
  
  yeardf$Day_3 <- as.numeric(yeardf$Day_3)
  
  yeardf$Day_4 <- as.numeric(yeardf$Day_4) 
  
  tmpdf <- rbind.data.frame(tmpdf,yeardf)
  
  boatVec <- yeardf$Boats
  # need to iterate through the boats on different days to update who's competing
  for (day in 1:4){
    dayString <- paste0('Day_',day)
    newBoatVec <- c()
    
    # vector of boats who have changed position from the day's racing
    bumpBoats <- df$Boats[df[dayString] != 0]
    
    boringBoats <- df$Boats[df[dayString] == 0]
    
    #while (length(bumpBoats)>0){
      #loseBoat <- bumpBoats[1]
      #bumperB
    #}
    
      #for (jj in nrow(df)){
       # boat <- df$Boat[jj]
        #rankChange <- as.numeric(df[dayString][jj,1])
        ## update Boat listings for the next day
        #newBoatVec[jj-rankChange] <- boat
        #result <- c(boat, df$Boat) 
        
      }
    }
  }
  }

  
  return(yeardf)
}

n=2018-1835
df=data.frame()
for (year in seq_len(n)){
  eightsFile = readLines(paste0('http://eodg.atm.ox.ac.uk/user/dudhia/rowing/bumps/e', 2019-year,'/e', 2019-year,'m.txt'))
  for (line in eightsFile){
    splitLine <- unlist(strsplit(line, "  "))
    if (all(sapply(splitLine),grep, '-10'))
  }
year=1
eightsFile = scan(paste0('http://eodg.atm.ox.ac.uk/user/dudhia/rowing/bumps/e', 2019-year,'/e', 2019-year,'m.txt'),what='character')
  