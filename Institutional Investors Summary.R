
Institutional_Investors_Summary <- function(filepath){
  #date
  date_hyphen <- toString(substring(Sys.time(),0,10))
  date <- gsub("-" , "",date_hyphen,fixed = FALSE)
  url <- paste("http://www.twse.com.tw/fund/BFI82U?response=csv&dayDate=",date,"&type=day",sep = "")
  #url <- paste("http://www.twse.com.tw/fund/BFI82U?response=csv&dayDate=","20180420","&type=day",sep = "")
  destfile <- paste(filepath,date,".csv",sep = '')
  download.file(url, destfile, mode="wb")
  #read file
  raw_data <- read.csv(destfile, head = FALSE, sep = ",")
  #去除多於欄位
    #後8列
    tail_row <- 9
    tail_col <- 5
    for(x in 1:8){
      raw_data <- raw_data[-tail_row,]
    }
    #前2列
    for(x in 1:1){
      raw_data <- raw_data[-1,]
    }
    #後1行
    for(x in 1:1){
      raw_data <- raw_data[,-5]
    }
    #delete first raw
    raw_data <- raw_data[-1,]
  #rowname
  row.names(raw_data) <- raw_data[,1]
  #delete first col
  raw_data <- raw_data[,-1]
  #colname
  col_name <- c("買進金額","賣出金額","買賣差額(億")
  colnames(raw_data) <- col_name
  #以億為單位
  for (a in 1:3) {
    raw_data[,a] <- floor(as.numeric(gsub(",", "",as.character(raw_data[,a])))) 
  }
  raw_data <- raw_data/100000000
  #不顯示小數點後
  write.csv(format(raw_data,digits=2),paste(filepath,date,"_Institutional_Investors_Summary.csv",sep = ''))
  #delete data
  if (file.exists(destfile)) file.remove(destfile)
}
Institutional_Investors_Summary("C://000//")

