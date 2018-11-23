# get data from online http://eodg.atm.ox.ac.uk/user/dudhia/rowing/bumps/
# don't count bumps from lower divisions using cutoff, 65 corresponds to top 5 divisions
dataGather <- function(years=1,from=2019,torpids=FALSE,draw=TRUE, cutoff=65){
  df <- data.frame(Boat_1=character(),
                   Boat_2=character(), 
                   outcome=character(), 
                   stringsAsFactors=FALSE) 
  
  for (year in seq_len(years)){
    # organise the data from each year before adding to df
    yeardf <- data.frame(Day_1=numeric(), 
                     Day_2=numeric(),
                     Day_3=numeric(), 
                     Day_4=numeric(), 
                     stringsAsFactors=FALSE)            
    print(paste0('Currently downloading data from the year: ', from-year))
    # make sure the year being tried actually has data
    if (class(try(readLines(paste0('http://eodg.atm.ox.ac.uk/user/dudhia/rowing/bumps/e', from-year,'/e', from-year,'m.txt'))
                  ))!='try-error'){
      eightsFile <-readLines(paste0('http://eodg.atm.ox.ac.uk/user/dudhia/rowing/bumps/e', from-year,'/e', from-year,'m.txt'))
      # remove the first line
      for (ii in seq(2, length(eightsFile))){
        line <- eightsFile[ii]
        # remove the lines without college races
        if ((!grepl('=',line)) &&  (!grepl('\\(', line)) ){
          line <- unlist(strsplit(line,'  '))
          
          line <- line[which(line != '')]
          
          # a particularly bad anomaly
          if (from-year==2011 && line[1]=="St Antony's II"){
            line<- c(line[seq_len(length(line)-1)],'-1','-1')
          }
          boatName <-line[1]
          
          results <- tail(line,4)
          
          yeardf[nrow(yeardf)+1,] <- results
          
         rownames(yeardf)[nrow(yeardf)] <- boatName
          }
      }
    }

  yeardf$Day_1 <- as.numeric(yeardf$Day_1) 
  
  yeardf$Day_2 <- as.numeric(yeardf$Day_2)
  
  yeardf$Day_3 <- as.numeric(yeardf$Day_3)
  
  yeardf$Day_4 <- as.numeric(yeardf$Day_4) 
  
  boatVec <- rownames(yeardf)
  
  # need to iterate through the boats on different days to update who's competing
  for (day in 1:4){
    dayString <- paste0('Day_',day)
    newBoatVec <- c()
    
    bumpBoatsdf <- yeardf[yeardf[dayString]!=0,]
    
    boringBoatsdf <- yeardf[yeardf[dayString] ==0,]

    boringBoats <- rownames(boringBoatsdf)
    
    # sort out the bumps first
    while(nrow(bumpBoatsdf)>0){
      
      # for changing ii, remove the vanilla bumps (or overbumps for ii>1) that occur in the same division
      for (ii in 1:10){
        # check if a normal bump remains in bumpBoatsdf
        while(grepl(paste(c(-ii,ii),collapse=";"),paste(bumpBoatsdf[,dayString],collapse=";"))){
          
          # vector of boats who have changed position from the day's racing
          bumpBoats <- rownames(bumpBoatsdf)
          
          # find the first time we have -ii followed by ii in the race
          bumpIndex <- which((bumpBoatsdf[,dayString]== -ii)& (c(bumpBoatsdf[seq(2,nrow(bumpBoatsdf)),dayString],0)==ii))[1]
          
          loseBoat <- bumpBoats[bumpIndex]
          
          winBoat <- bumpBoats[bumpIndex+1]
          
          position <- which(rownames(yeardf)==loseBoat)
          
          newBoatVec[position+ii] <- loseBoat

          newBoatVec[position] <- winBoat
          # add ii rows to the df in order to give overbumps more weighting
          if (position < cutoff){
            df[seq(nrow(df)+1,nrow(df)+ii),] <- matrix(rep(c(loseBoat, winBoat, 'W2'),each=ii, nrow=ii))
          }  
          
          
          bumpBoatsdf <- bumpBoatsdf[-c(bumpIndex, bumpIndex+1),]
        }
      }
      # now to deal with the dodgy cases
      if (day==1 & from-year==2018 & torpids == FALSE){
        mert <- 'Merton II'
        hild <- "St Hilda's"
        hert <- 'Hertford II'
        linc <- 'Lincoln II'
        df[nrow(df)+1,] <- c(mert,hild,'W2')
        df[nrow(df)+1,] <- c(hert, linc, 'W2')
        position <- which(rownames(yeardf)==mert)
        newBoatVec[position] <- hild
        newBoatVec[position + 1] <- linc
        newBoatVec[position + 2] <- mert
        newBoatVec[position + 3] <- hert
        bumpBoatsdf <- bumpBoatsdf[!rownames(bumpBoatsdf) %in% c(hild,linc,mert,hert),]
      }
      # hopefully, bumpBoatsdf contains only the mess that is cross divisional bumps now
      for (boat in rownames(bumpBoatsdf)){
        change <- bumpBoatsdf[boat,dayString]
        position <- which(rownames(yeardf)==boat)
        newBoatVec[position-change] <- boat
        # count all encounters from the loser's perspective
        if (change<0){
          index <-  which(rownames(bumpBoatsdf)==boat)
          # find the bumper, this will be by definition the next boat in bumpBoatsdf that has >0 change
          bumperIndex <- which(bumpBoatsdf[seq(index+1,nrow(bumpBoatsdf)),dayString]>0)[1]
          bumper <- rownames(bumpBoatsdf)[bumperIndex + index]
          position <- which(rownames(yeardf)==boat)
          if (position < cutoff){
            df[seq(nrow(df)+1,nrow(df)-change),] <- matrix(rep(c(boat, bumper,'W2'), each=-change), nrow= -change)
          }
          
        }
      }
      
      bumpBoatsdf <- data.frame()
      
    }
    
    # update the positions that haven't changed
    newBoatVec[which(rownames(yeardf) %in% boringBoats)] <- boringBoats
    
    # add draws to the dataframe if parameter selected
    if (draw==TRUE){
      boringPlaces=which(rownames(yeardf) %in% boringBoats)
      
      drawIndices <- which(boringPlaces - c(boringPlaces[seq(2,length(boringPlaces))] ,-1) ==-1)
      
      for (drawIndex in drawIndices){
        boat1 <- rownames(yeardf)[boringPlaces[drawIndex]]
        boat2 <- rownames(yeardf)[boringPlaces[drawIndex+1]]
        position <- which(rownames(yeardf)==boat1)
        if (position < cutoff){
          df[nrow(df)+1,] <- c(boat1,boat2, 'D')
        }
      
      }
    }
    #rearrange in the new order
    yeardf <- yeardf[newBoatVec,]
  }
  
  }

  
  return(df)
}
