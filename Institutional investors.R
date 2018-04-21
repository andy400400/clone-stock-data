#selectType : 17金融股,ALL所有股票
#filepath : 檔案位置
#display : 是否只顯示部分
#display_list : 顯示部分資料
Institutional_investors_data<- function(selectType,filepath,display,display_list){
	#組網址
	date_hyphen <- toString(substring(Sys.time(),0,10))
	date <- gsub("-" , "",date_hyphen,fixed = FALSE)
	#下載資料
	url <- paste("http://www.twse.com.tw/fund/T86?response=csv&date=",date,"&selectType=",selectType,sep = '')
	#url <- "http://www.twse.com.tw/fund/T86?response=csv&date=20180420&selectType=ALL"
	destfile <- paste(filepath,date,".csv",sep = '')
	download.file(url, destfile, mode="wb")
	#讀取CSV
	extraction <- read.csv(destfile, head = FALSE, sep = ",")
	#去除多於欄位
		#後8列
		tail_row <- dim(extraction)[1]-7
		for(x in 1:8){
		  extraction <- extraction[-tail_row,]
		}
		#前2列
		for(x in 1:2){
		  extraction <- extraction[-1,]
		}
	names(extraction) <- c("代號","名稱","外資買","外資賣","外資買賣超","外資自營買","外資自營賣","外資自營買賣超",
						   "投信買","投信賣","投信買賣超","自營商買賣超","自營商(自行買賣)買","自營商(自行買賣)賣",
						   "自營商(自行買賣)買賣超","自營商(避險)買","自營商(避險)賣","自營商(避險)買賣超","三大法人買賣超")
	#取1-19並轉換型態
	for (a in 3:19) {
	  extraction[,a] <- floor(as.numeric(gsub(",", "",as.character(extraction[,a])))) 
	}
	#整理後
	clean_data <- cbind(extraction[,1:2],extraction[,3:19]/1000)
	#不顯示小數點後
	options(digits = 0)
	#避免變成科學符號
	options(scipen=999)
	#是否只顯示部分資料
	if(display){
		clean_data_only_display <- NULL
		for(x in 1:length(display_list)){
			clean_data_only_display <- rbind(clean_data_only_display,subset(clean_data, 代號 == display_list[x]))
		}
		clean_data <- clean_data_only_display
	}
	#取要得欄位
	Institutional_investors_data<- cbind(clean_data[,1:5],clean_data[,9:11],clean_data[,13]+clean_data[,16],clean_data[,14]+clean_data[,17],clean_data[,12],clean_data[,19])
	names(Institutional_investors_data) <- c("代號","名稱","外資買入","外資賣出","外資買賣超","投信買入","投信賣出","投信買賣超",
								  "自營商買入","自營商賣出","自營商買賣超","三大法人買賣超")
	write.csv(format(Institutional_investors_data,digits=1),paste(filepath,date,"_",selectType,".csv",sep = ''))
	#delete data
	if (file.exists(destfile)) file.remove(destfile)
}
display_list <- c(2903,2834,2883)
Institutional_investors_data("ALL","C://000/",FALSE,display_list)
Institutional_investors_data("17","C://000/",FALSE,display_list)