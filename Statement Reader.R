# Rohan Balakrishna
# for reference, x %>% y is equivalent to y(x) (i.e you evaluate x and then plug it into y)
# How to use: put a file with a .pdf extension into the folder with this program. Hit Command+Enter until you reach the bottom. Copy / Paste into excel
install.packages("pdftools")
install.packages("tm")
library(pdftools)
library(tm) 
library(dplyr)
library(stringr)

# files <- list.files(pattern="pdf$")

pdf <- pdf_text("June Statement.pdf") %>% 
  strsplit(split = "\n")
# ^^^ INSERT APPLICABLE FILE NAME HERE


#Check that it worked
pdf

#substrings are inclusive on both ends

#This allows us to view the whole statement as one string, and not several pages
stitchDoc <- function(){
  doc <- ""
  for(i in 1:length(pdf)){
    doc <- paste(doc, pdf[i])
  }
  return(doc)
}

pdf <- stitchDoc()

parseDoc <- function(key, start){
  i <- start
  done <- FALSE
  while(!done){
    if(substring(pdf, i, i+str_length(key)-1)==key){
      done <- TRUE
    }
    else{
      i <- i+1
    }
  }
  return(i)
}


# Returns the word/phrase starting at a given index
processWord <- function(start){
  j <- start
  res <- ""
  while((!substring(pdf, j, j+10)=="           ")&&(!substring(pdf, j, j)=="\"")){
    res <- paste(res, substring(pdf, j, j), sep="")
    j <- j+1
  }
  return(res)
}
# Moves the index key to the start of the next word from the END of a given word
findNextWord <- function(start){
  i <- start + 1
  while(substring(pdf, i, i)==" "){
    i <- i+1
  }
  return(i)
}

#find the start of the next line by finding a number in date format
findStart <- function(index){
  i <- index
  while(!substring(pdf, i+2, i+2) == "/"){
    i <- i + 1
  }
  return(i)
}



readSummary <- function(){
  headers <- c("Beginning Balance", "ATM & Debit Card Withdrawals", "Electronic Withdrawals","Deposits and Additions", "Ending Balance")
  items <- c()
  numbers <- c()
  numCharges <- c()
  for(i in 1:5){  
    index <- parseDoc(headers[i], 0)
    #get first item
    items <- c(items, processWord(index))
    index <- index + str_length(processWord(index))
    
    index <- findNextWord(index)
    if(headers[i] == "Beginning Balance"){
      numCharges <- c(numCharges, "N/A")
    }
    else{
      numCharges <- c(numCharges, processWord(index))
      index <- findNextWord(index + str_length(processWord(index)))
    }
    
    numbers <- c(numbers, processWord(index))
  }
  return(data.frame("Items" = items,"Number of charges"=numCharges, "Charges" = numbers))
  
}

depositsAndAdditions <- function(){
  index <- parseDoc("DEPOSITS AND ADDITIONS", 0)
  #get the cursor to the start of the first line
  
  index <- findStart(index)
  
  dates <- c()
  descriptions <- c()
  amounts <- c()
  done <- FALSE
  #each loop goes through a line
  while(!done){
    #get date (always first)
    dates <- c(dates, processWord(index))
    #move to next word
    index <- index + str_length(processWord(index))
    index <- findNextWord(index)
    
    #if this is the case then we just have to advance it one word further
    if(processWord(index) == ", "){
      index <- findNextWord(index + str_length(processWord(index)))
      index <- findNextWord(index + str_length(processWord(index)))
      index <- findNextWord(index + str_length(processWord(index)))
      
    }
    
    # read description item
    descriptions <- c(descriptions, processWord(index))
    index <- index + str_length(processWord(index)) 
    index <- findNextWord(index)
    # read amount
    amounts <- c(amounts, processWord(index))

    index <- index + str_length(processWord(index))
    #check if at the end 
    temp <- index
    temp <- findNextWord(temp)
    temp <- str_length(processWord(temp)) + temp
    if(substring(processWord(findNextWord(temp)), 0, 5)=="Total"){
      done <- TRUE
    }
    else{      
    index <- findStart(index)
    }
  }
  return(data.frame("Dates" = dates, "Description" = descriptions, "Amount" = amounts))
  
}

electronicWithdrawals <- function(){
  index <- parseDoc("ELECTRONIC WITHDRAWALS", 0)
  #get the cursor to the start of the first line
  
  index <- findStart(index)
  
  dates <- c()
  descriptions <- c()
  amounts <- c()
  done <- FALSE
  #each loop goes through a line
  while(!done){
    #get date (always first)
    dates <- c(dates, processWord(index))
    #move to next word
    index <- index + str_length(processWord(index))
    index <- findNextWord(index)
    
    #if this is the case then we just have to advance it one word further
    if(processWord(index) == ", "){
      index <- findNextWord(index + str_length(processWord(index)))
      index <- findNextWord(index + str_length(processWord(index)))
      index <- findNextWord(index + str_length(processWord(index)))
      
    }
    
    # read description item
    descriptions <- c(descriptions, processWord(index))
    index <- index + str_length(processWord(index)) 
    index <- findNextWord(index)
    # read amount
    amounts <- c(amounts, processWord(index))
    
    index <- index + str_length(processWord(index))
    #check if at the end 
    temp <- index
    temp <- findNextWord(temp)
    temp <- str_length(processWord(temp)) + temp
    if(substring(processWord(findNextWord(temp)), 0, 5)=="Total"){
      done <- TRUE
    }
    else{      
      index <- findStart(index)
    }
  }
  return(data.frame("Dates" = dates, "Description" = descriptions, "Amount" = amounts))
  
}

AtmAndDebitCardWithdrawals <- function(){
  index <- parseDoc("ATM & DEBIT CARD WITHDRAWALS", 0)
  #get the cursor to the start of the first line
  
  index <- findStart(index)
  
  dates <- c()
  descriptions <- c()
  amounts <- c()
  done <- FALSE
  #each loop goes through a line
  while(!done){
    #get date (always first)
    dates <- c(dates, processWord(index))
    #move to next word
    index <- index + str_length(processWord(index))
    index <- findNextWord(index)
    
    #if this is the case then we just have to advance it one word further
    if(processWord(index) == ", "){
      index <- findNextWord(index + str_length(processWord(index)))
      index <- findNextWord(index + str_length(processWord(index)))
      index <- findNextWord(index + str_length(processWord(index)))
      
    }
    
    # read description item
    descriptions <- c(descriptions, processWord(index))
    index <- index + str_length(processWord(index)) 
    index <- findNextWord(index)
    # read amount
    amounts <- c(amounts, processWord(index))
    
    index <- index + str_length(processWord(index))
    #check if at the end 
    temp <- index
    temp <- findNextWord(temp)
    temp <- str_length(processWord(temp)) + temp
    if(substring(processWord(findNextWord(temp)), 0, 5)=="Total"){
      done <- TRUE
    }
    else{      
      index <- findStart(index)
    }
  }
  return(data.frame("Dates" = dates, "Description" = descriptions, "Amount" = amounts))
  
}

findPreviousWord <- function(start){
  i <- start
  while(substring(pdf, i, i)==" "){
    i <- i-1
  }
  while(!substring(pdf, i, i)==" "){
    i <- i-1
  }
  return(i)
}

checksPaid <- function(){
  index <- parseDoc("CHECKS PAID", 0)
  #get the cursor to the start of the first line
  
  index <- findStart(index)
  
  dates <- c()
  numbers <- c()
  amounts <- c()
  done <- FALSE
  i <- 1
  #each loop goes through a line
  while(i < 5){
      #get date (always first)
      dates <- c(dates, processWord(index))
      #move backwards (index pointed at start of date)
      temp <- findPreviousWord(index)
      if(processWord(temp)=="^"){
        temp <- findPreviousWord(temp)
      }
      numbers <- c(numbers, processWord(temp))
      
      
      index <- index + str_length(processWord(index))
      index <- findNextWord(index)
      # read amount
      amounts <- c(amounts, processWord(index))
      
      index <- index + str_length(processWord(index))
      #check if at the end 
      temp <- index
      temp <- findNextWord(temp)
      temp <- str_length(processWord(temp)) + temp
      if(substring(processWord(findNextWord(temp)), 0, 5)=="Total"){
        done <- TRUE
      }
      else{      
        index <- findNextCheck(index)
      }
      i <- i+1
  }
  return(data.frame("Dates" = dates, "Numbers" = numbers, "Amount" = amounts))
  
}

View(depositsAndAdditions())
View(AtmAndDebitCardWithdrawals())
View(electronicWithdrawals())





