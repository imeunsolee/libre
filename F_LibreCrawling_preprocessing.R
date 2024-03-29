
##########################################
## LibreCrawling_preprocessing 
## ---------------------------------------
## input 
## . inFileName : 'NAME_glucose_YYYYMMDD.json' 
## . FinalDate : 리브레 종료일  YYYY-MM-DD 의 형태
## . mod : 1=1기간 분석 2 = 2기간 이상 분석 
## .... ver 2.0 에서는 mod = 2 에서 최대 2개 기간만 출력하도록 제한해둠 #### 
## ---------------------------------------
## ver 0.0 ( 230713 ) 
# head(AGPdata$AGPdata,2)
#             dateandtime       date     time sub log glucose event_eat
#4411 2022-07-13 12:08:00 2022-07-13 12:08:00   2   0      65         1
#4412 2022-07-13 12:08:00 2022-07-13 12:08:00   2   1      83      <NA>
#     event_exercise                               memo
#4411             NA 식사중 바르다김선생 묵은지참치김밥
#4412             NA                               <NA>
#> dim(AGPdata$AGPdata)
#[1] 2595    9


LibreCrawling_preprocessing = function( data, FinalDate ) {
	errCode.sub = c()

	### step0 =============================================================================##
	## 데이터 불러오기    

#    data = fromJSON(inFileName)
#    class(data) # data.frame
#    colnames(data) #[1] "_id"   "birth" "data"  "email"

#    class(data$data) # list
#    length(data$data) # 1;1명의 데이터
    
#    class(data$data[[1]]) # data.frame
#    dim(data$data[[1]]) # 15*3
#    colnames(data$data[[1]]) #[1] "graphTime"   "glucoseData" "foodData"
#    class(data$data[[1]]$graphTime) # character
#    class(data$data[[1]]$glucoseData) # list
#    length(data$data[[1]]$glucoseData) # 15 (15일치의 데이터)
#    class(data$data[[1]]$foodData) # data.frame

#    class(data$data[[1]]$glucoseData[[1]]) # matrix array
#    dim(data$data[[1]]$glucoseData[[1]]) # 16*4 ;하루간 16pt 가 있었음
    #1열: timestamp 2: 혈당값 3: 스파이크값 4: 1이면 다음포인트랑 이어지고 0이면 안이어지고 null이면 끝
    
#    data$data[[1]]$glucoseData[[1]]
#    sum(unlist(lapply(data$data[[1]]$glucoseData,nrow)))

#    dim(data$data[[1]]$foodData) # 15*2
#    class(data$data[[1]]$foodData$foodMemoData) #list
#    class(data$data[[1]]$foodData$foodNutriData) #list
#    head(data$data[[1]]$foodData$foodMemoData)

	### step1 =============================================================================##
	## 혈당데이터 생성     

	AGPdata = as.data.frame(matrix(NA,nrow=1,ncol=3))

	ndays = length(data$data[[1]]$graphTime)
	for ( d in 1:ndays ) {
		AGPdata.tmp = data$data[[1]]$glucoseData[[d]][,1:3]
		AGPdata = rbind(AGPdata,AGPdata.tmp)
	}
	AGPdata = AGPdata[!is.na(AGPdata[,1]),]
	colnames(AGPdata) = c('dateandtime','glucose','spikeValue')
	AGPdata$dateandtime = (as.POSIXct(AGPdata$dateandtime/1000,origin='1970-01-01',tz='GMT')+9*60*60)


	### step2 =============================================================================##
	## 음식데이터 생성

	Fdata = as.data.frame(matrix(NA,nrow=1,ncol=3))
	for ( d in 1:ndays ) {
		Fdata.tmp = data$data[[1]]$foodData$foodMemoData[[d]]
		Fdata = rbind(Fdata,Fdata.tmp)
	}
	Fdata = Fdata[!is.na(Fdata[,1]),]
	colnames(Fdata) = c('dateandtime','memo','spikeValue')
	Fdata$dateandtime = (as.POSIXct(as.numeric(as.vector(Fdata$dateandtime))/1000,origin='1970-01-01',tz='GMT')+9*60*60)


	### step3 =============================================================================##
	## 병합

	AGPdata$spikeValue = NULL #todo 
#	AGPdata$memo = Fdata$memo[match(AGPdata$dateandtime,Fdata$dateandtime)]
	AGPdata = merge(x=AGPdata,y=Fdata,by=c('dateandtime'),all=T)
	AGPdata$spikeValue = as.numeric(as.vector(AGPdata$spikeValue))
	AGPdata$date = as.Date(AGPdata$dateandtime)
	AGPdata$time = format(AGPdata$dateandtime,format='%H:%M:%S')
	AGPdata$spikeYN = !is.na(AGPdata$spikeValue)
	AGPdata$sub = NA
	AGPdata$log = 1 #자동기록혈당 (크롤링데이터는 0,1의 구분이 없어서 모두 자동기록값으로 부여)
	AGPdata$event_eat = NA 
	AGPdata$event_exercise = NA

	# 날짜순 정렬 todo
	AGPdata = AGPdata[order(AGPdata$dateandtime),]

	# %시간 활성화비율 높은 일자로 1일차,15일차 중 선택
	daysAZ = unique(AGPdata$date)

	CGMactive = rep(NA, length(daysAZ))
	AGPdata$time96 = NA
	for ( d in 1:length(daysAZ) ) {
		alltimecut = seq.POSIXt(as.POSIXct(strptime(paste(unique(AGPdata$date)[d],'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT')),as.POSIXct(strptime(paste(as.Date(unique(AGPdata$date)[d])+1,'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT')), by='15 min')

		for ( i in which(AGPdata$date==daysAZ[d]) ) {
			AGPdata[i,]$time96 = sum(alltimecut<=AGPdata$dateandtime[i],na.rm=T)
		}
		CGMactive[d] = length(unique(AGPdata[which(AGPdata$date==daysAZ[d]),]$time96))

	}
	names(CGMactive) = daysAZ

	# 1970-01-01
	if ( length(grep('1970-01-01',daysAZ))>0 ) {
		CGMactive = CGMactive[-grep('1970-01-01',daysAZ)]
		daysAZ = daysAZ[-grep('1970-01-01',daysAZ)]
	}

	## mod 
	ndays.all = as.numeric(range(daysAZ)[2]-range(daysAZ)[1]+1)
	if ( ndays.all > 20 ) {
		mod = 2
	} else {
		mod = 1
	}
	if ( mod==1 ) {
		while ( as.numeric(range(daysAZ)[2]-range(daysAZ)[1]+1)>14 ) {
			if ( CGMactive[1] < CGMactive[length(daysAZ)] ) {
				daysAZ = daysAZ[-1]
				CGMactive = CGMactive[-1]
			} else {
				daysAZ = daysAZ[-length(daysAZ)]
				CGMactive = CGMactive[-length(CGMactive)]
			}
		}
		strtDt.tmp = range(daysAZ)[1]
		endDt.tmp = strtDt.tmp+14-1 
		AGPdata[which(AGPdata$date>=strtDt.tmp & AGPdata$date<=endDt.tmp),]$sub = 1

	} else {
		subN = 0
		daysAZ.tmp = daysAZ
		while ( as.numeric(range(daysAZ.tmp)[2]-range(daysAZ.tmp)[1]+1)>5 ) {
			subN = subN+1
			endDt.tmp = range(daysAZ.tmp)[2]
			strtDt.tmp = daysAZ[daysAZ.tmp>=(endDt.tmp-14)][1]
			if ( as.numeric(endDt.tmp-strtDt.tmp+1)>14 ) {
				CGMactive.tmp = CGMactive[names(CGMactive)>=strtDt.tmp & names(CGMactive)<=endDt.tmp]
				if ( CGMactive.tmp[1] < CGMactive.tmp[length(CGMactive.tmp)] ) {
					strtDt.tmp = endDt.tmp-14+1
				} else {
					endDt.tmp = strtDt.tmp+14-1
				}
			}
			AGPdata[which(AGPdata$date>=strtDt.tmp & AGPdata$date<=endDt.tmp),]$sub = subN
			daysAZ.tmp = daysAZ.tmp[(daysAZ.tmp<strtDt.tmp)]
		}
		daysAZ = unique(AGPdata[!is.na(AGPdata$sub),]$date)
		CGMactive = CGMactive[grep(paste(daysAZ,collapse='|'),names(CGMactive))]
	}
	AGPdata = AGPdata[which(AGPdata$date%in%daysAZ),]


	### step4 =============================================================================##
	## 누락시점 확인
    subN = ifelse(mod==1,1,subN)
	daysAZ.seq = vector('list',subN)
	for ( k in 1:subN ) {
		daysAZ.seq = seq.Date(range(AGPdata[which(AGPdata$sub==k),]$date)[1],range(AGPdata[which(AGPdata$sub==k),]$date)[2],1)
		for ( d in 1:length(daysAZ.seq) ) {
			if ( length(setdiff(c(1:96),AGPdata[which(AGPdata$date==daysAZ.seq[d]),]$time96))>0 ) {
				addIdx.time = as.POSIXct(paste(daysAZ.seq[d],format(alltimecut[setdiff(c(1:96),AGPdata[which(AGPdata$date==daysAZ.seq[d]),]$time96)],'%H:%M:%S')),format='%Y-%m-%d %H:%M:%S',tz='GMT')
				addline.tmp = AGPdata[1:length(addIdx.time),]

				addline.tmp$dateandtime = addIdx.time
				addline.tmp$glucose = NA
				addline.tmp$memo = NA
				addline.tmp$spikeValue = NA
				addline.tmp$date = as.Date(addline.tmp$dateandtime)
				addline.tmp$time = NA
				addline.tmp$spikeYN = NA
				addline.tmp$sub = k #todo
				addline.tmp$log = 1
				addline.tmp$event_eat = NA 
				addline.tmp$event_exercise = NA
				addline.tmp$time96 = setdiff(c(1:96),AGPdata[which(AGPdata$date==daysAZ.seq[d]),]$time96)

				AGPdata = rbind(AGPdata,addline.tmp)
			}
		}
	}
    
	# 날짜순 정렬
	AGPdata = AGPdata[order(AGPdata$dateandtime),]

	### output =============================================================================##
	mod = mod # 1:1개 기간 2:k개기간

	# 최대 14일까지 todo 
	return(list(AGPdata=AGPdata, CGMactive=CGMactive, errCode.sub = errCode.sub, mod=mod))

}

