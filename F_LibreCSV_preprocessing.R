
##########################################
## LibreData_transformation 
## ---------------------------------------
## input 
## . inFileName : 'NAME_glucose_YYYYMMDD.xlsx' 
## . FinalDate : 리브레 종료일  YYYY-MM-DD 의 형태
## . mod : 1=1기간 분석 2 = 2기간 이상 분석 
## .... ver 2.0 에서는 mod = 2 에서 최대 2개 기간만 출력하도록 제한해둠 #### 
## ---------------------------------------
## ver 3.0 ( 220502 ) 

LibreData_transformation = function( inFileName, FinalDate ) {
	errCode.sub = c()

	### step0 =============================================================================##
	## 데이터 불러오기    

	lines = readLines(inFileName,encoding='UTF-8')[-c(1:2)]
    lIDX = grep('FreeStyle LibreLink',lines)
    data = matrix(NA, nrow=length(lIDX),ncol=19)

    for ( l in 1:length(lIDX) ) {

        if ( l==length(lIDX) ) {
            lines.tmp = lines[lIDX[l]]
        } else {
            lines.tmp = paste(lines[lIDX[l]:(lIDX[l+1]-1)],collapse=',')
        }        
        line1 = unlist(strsplit(lines.tmp,split="\""))
        if ( length(line1)==1 ) {
            line2 = strsplit(line1,split=',')[[1]]
        } else if ( length(line1)==2 ) {           
            line2 = c(strsplit(line1,split=',')[[1]],line1[2],rep('',5))
        } else if ( length(line1)==3 ) {           
            line2 = c(strsplit(line1,split=',')[[1]],line1[2],strsplit(line1,split=',')[[3]])
        }
        if ( length(line2)!=19 ) {
            for ( j in (length(line2)+1):19 ) line2[j] = ''
        }
        data[l,] = line2

    }
	colnames(data) = unlist(strsplit(lines[[1]],split=','))
	data = as.data.frame(data)
	data = data[!is.na(data[,1]),]

	data$dateandtime = as.POSIXct(data[,3],tz='GMT')
	data$date = as.Date(data$dateandtime)
	data$time = format(data$dateandtime,format='%H:%M:%S')


	### step1 =============================================================================##
	## 분석기간 분류 
	subNum = 0
	End.s1 = T
	End.s2 = F 
	Datelist = as.character(unique(as.Date(data[which(data[,4]%in%c(0,1)),]$date)))

	date.tmp = c(as.Date(FinalDate,'%Y-%m-%d')-13, as.Date(FinalDate,'%Y-%m-%d'))
	data$sub = NA
	ndays = length(Datelist[Datelist>=date.tmp[1] & Datelist<=date.tmp[2]])

	while ( 1 ) {

		subNum = ( subNum+1 )

		## part1 ## 
		if ( ndays==14 ) {
			data[which(data$date>=date.tmp[1] & data$date<=date.tmp[2]),]$sub = subNum

		} else {

			ndays.max = ndays
			date.max = date.tmp
			FinalDate.tmp = ifelse( any(!is.na(data$sub)), max(Datelist[Datelist<min(data[!is.na(data$sub),]$date,na.rm=T)]), max(Datelist,na.rm=T) )
			FinalDate.tmp = as.Date(FinalDate.tmp)

			date.tmp = c(as.Date(FinalDate.tmp,'%Y-%m-%d')-13,as.Date(FinalDate.tmp,'%Y-%m-%d'))

			while ( End.s1==T ) {
	
				ndays.tmp = length(Datelist[Datelist>=date.tmp[1] & Datelist<=date.tmp[2]])
				if ( ndays.tmp > ndays.max ) {
					ndays.max = ndays.tmp 
					date.max = date.tmp 	
				}
				date.tmp = (date.tmp-1)
				if ( date.tmp[2] < (FinalDate.tmp-1) ) {
					End.s1=F
				}
			}

			while ( End.s2==T ) {

				ndays.tmp = length(Datelist[Datelist>=date.tmp[1] & Datelist<=date.tmp[2]])
				if ( ndays.tmp > ndays.max ) {
					ndays.max = ndays.tmp 
					date.max = date.tmp 
				}
				date.tmp = (date.tmp-1)
				if ( ndays.tmp==0 | ndays.tmp==14 ) {
					End.s2=F
				}
				if ( date.tmp[1] < min(Datelist) ) {
					End.s2=F
				}
			}

			data[which(data$date>=date.max[1] & data$date<=date.max[2]),]$sub = subNum

		}


		## part2 ## 
		## 과거 데이터 자동 탐색 ###
		Datelist = Datelist[Datelist<min(data[!is.na(data$sub),]$date,na.rm=T)]
#		Datelist = setdiff(as.character(Datelist), unique(as.character(data[!is.na(data$sub),]$date)))
		if ( length(Datelist)<2 ) {
			break

		} else {
			FinalDate = max(Datelist[Datelist<min(data[!is.na(data$sub),]$date,na.rm=T)],na.rm=T)
			
			if ( is.na(FinalDate) ) {
				break 

			} else {
				date.tmp = c(as.Date(FinalDate,'%Y-%m-%d')-13, as.Date(FinalDate,'%Y-%m-%d'))
				ndays = length(Datelist[Datelist>=date.tmp[1] & Datelist<=date.tmp[2]])
				date.max = NULL
				ndays.max = NULL

				End.s2 = T
			}
		}
	}

	mod = ifelse( all(is.na(data$sub)), NA, min(max(data$sub,na.rm=T),2,na.rm=T) )


	### step2 =============================================================================##
	## 기록유형 분류 (자동,스캐닝,식사 등)  

	### 기록유형 = 0 -> 자동 혈당값 
	data$log = ifelse(data[,4]==0,1,0)

	## S2-1. 동일시점기록 요약 

	### 자동 혈당 ---
	data_type0 = data[which(data[,4]==0),c(1:3,20:24,5)]
	colnames(data_type0)[9]='glucose'

	## 스캔 혈당 ---
	data_type1 = data[which(data[,4]==1),c(1:3,20:24,6)]
	colnames(data_type1)[9]='glucose'

	## 음식 기록 ---
	# 아침, 점심, 저녁 분류할 필요 있음 
	data_type5 = data[which(data[,4]==5),c(1:3,20:24,9)]
	colnames(data_type5)[9]='event_eat'

	## 운동 기록 ---
	data_type6_1 = data[which(data[,4]==6),c(1:3,20:24,14)]
	data_type6_1 = data_type6_1[which(data_type6_1[,9]=='운동'),]## 메모로 '운동'이라고 기록한 경우도 포함시켜야하지 않을까 ? ## data_type6_1[grepl('운동',data_type6_1[,9][[1]]),]
	if(dim(data_type6_1)[1]!=0){
		data_type6_1[,9]=1
	}
	colnames(data_type6_1)[9]='event_exercise'

	## 모든 메모 ---
	data_type6_2 = data[which(data[,4]==6),c(1:3,20:24,14)]
	if(dim(data_type6_2[which(data_type6_2[,9]!=''),])[1]!=0){
		data_type6_2 = data_type6_2[which(data_type6_2[,9]!=''),]
	}
	colnames(data_type6_2)[9]='memo'

	## S2-2. merge

	data_type1to5 = merge(data_type1[,-c(1:3)],data_type5[,-c(1:3)],by=c('dateandtime','date','time','sub','log'),all=T)
	data_type6 = merge(data_type6_1[,-c(1:3)],data_type6_2[,-c(1:3)],by=c('dateandtime','date','time','sub','log'),all=T)
	data_type1to6 = merge(data_type1to5,data_type6,by=c('dateandtime','date','time','sub','log'),all=T)

	data_type0to6 = merge(data_type0[,-c(1:3)],data_type1to6,by=c('dateandtime','date','time','sub','log','glucose'),all=T)
	AGPdata = data.frame(data_type0to6[!is.na(data_type0to6$dateandtime),])
	AGPdata$glucose = as.numeric(as.vector(AGPdata$glucose))


	### output =============================================================================##
#	out.sub = 1 # ver1.0 #

	mod.ndays = tapply(AGPdata$date,AGPdata$sub,function(x){length(c(min(x,na.rm=T):max(x,na.rm=T)))})
	
	if ( any(mod.ndays>3) ) {
		## 
		RM.Sub = as.numeric(names(which(mod.ndays<3)))
		AGPdata$sub[which(AGPdata$sub%in%RM.Sub)] = NA
		NEW.Sub = unique(AGPdata$sub[!is.na(AGPdata$sub)])

		sub.tmp = AGPdata$sub
		for ( s in 1:length(NEW.Sub) ) {
    		sub.tmp = ifelse(AGPdata$sub==NEW.Sub[s],order(NEW.Sub)[s],sub.tmp)
		}
		AGPdata$sub = sub.tmp

		mod = ifelse( all(is.na(AGPdata$sub)), NA, min(max(AGPdata$sub,na.rm=T),2,na.rm=T) )
		

	} else {
		## 1개 기간만 있으면 
		mod = NA 
		AGPdata$sub = NA 
	#	errCode.sub = c(errCode.sub,'ERR')

	}

	
	if ( mod !=1 ) {
		out.sub = c(1,2) # ver2.0 # //최대 2개 기간까지만 출력하도록 제한 ## 
	} else {
		out.sub = 1 
	}

	if ( any(AGPdata$sub==1,na.rm=T)==F ) {
#		cat('[경고] 리브레 데이터 변환 완료하였으나, 분석기간에 해당되는 데이터가 없습니다.\n')
		errCode.sub = c(errCode.sub, 'Warn_104')
	}

	return(list(AGPdata = AGPdata[which(AGPdata$sub%in%out.sub),], errCode.sub = errCode.sub, mod=mod))


}