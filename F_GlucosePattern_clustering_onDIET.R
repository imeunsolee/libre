
##########################################
## glucosePattern_clustering
## ---------------------------------------
## ---------------------------------------

GlucosePattern_clustering_DIET = function( daysAZ, data, mealtime, spikeLog, method=method, PPG.L=PPG.L, PPG.U=PPG.U, Target=Target, IncCut=IncCut ) {

	time.event=list(.POSIXct(NA,tz='GMT',cl='POSIXct'))
	time.start=list(.POSIXct(NA,tz='GMT',cl='POSIXct'))
	time.end=list(.POSIXct(NA,tz='GMT',cl='POSIXct'))

	errCode.sub = c()
		  
	### step1 =============================================================================##
	## 분석기간의 식사 정보 추출                   -------------------## 

	HMScut=strptime(format(seq.POSIXt(strptime('04:00:00',format='%H:%M:%S'),strptime('23:59:59',format='%H:%M:%S'),by='6 hours'),format='%H:%M:%S'),format='%H:%M:%S',tz='GMT') ## 리브레뷰와 통일
	
	data$timef = as.POSIXct(strptime(data$time, format = '%H:%M:%S'),tz='GMT')
	data$timeCut2 = NA 
	for(i in 1:nrow(data)) data$timeCut2[i] = sum(data$timef[i]>=HMScut)
	data$timeCut2 = ifelse(data$timeCut2==0,4,data$timeCut2)
    
    if ( !is.na(mealtime) ) {
        data = data[which(data$timeCut2==mealtime),]
    }
	data$event = as.numeric(data$event_spike>0)

	for ( i in 1:length(daysAZ) ) {

		## 이벤트시간
		time.event[[i]] = data[which(data$date==daysAZ[i] & data$event==1),]$dateandtime

		# 조건. time.event  사이 간격이 3시간보다 작으면, 하나의 식사로 간주 ##
		if ( length(time.event[[i]])>1 ) {
			rm.event=c()
			for(j in 1:(length(time.event[[i]])-1)){
				if(as.numeric(difftime(time.event[[i]][j+1],time.event[[i]][j],unit='hours'))<3){
					rm.event = c(rm.event,j+1)
				}
			}
			if(!is.null(rm.event)){
				time.event[[i]] = time.event[[i]][-rm.event]
			}
		}
		time.start[[i]] = time.event[[i]] - (1*60*60)
		time.end[[i]] = time.event[[i]] + (3*60*60)
	}


	### step2 =============================================================================##

	glucose.bef = data.frame()
	glucose.aft = data.frame()
	avgIncValue = vector('list',length(daysAZ))
	speedIncValue = vector('list',length(daysAZ));speedIncValue.warnings = vector('list',length(daysAZ))
	recoveryTime = vector('list',length(daysAZ))
	peakValue = vector('list',length(daysAZ))
	spikeandcrash = vector('list',length(daysAZ))
	overallspike = vector('list',length(daysAZ))
	memo = vector('list',length(daysAZ))

	for ( i in 1:length(daysAZ) ) {

		if ( length(time.event[[i]])==0 ) next

		for ( j in 1:length(time.event[[i]]) ) {

			if ( time.event[[i]][j]==max(data$dateandtime) ) { # 식사기록 후 식후혈당 측정 없음
				errCode.sub = c(errCode.sub, 'Warn_004')
				next
			}

			## 식전 60분간 glucose
			bef.x1 = data[which(data$dateandtime >= time.start[[i]][j] & data$dateandtime<time.event[[i]][j]),c('dateandtime','glucose','memo','timeCut2')]
			bef.x2 = as.numeric(difftime(bef.x1$dateandtime,time.event[[i]][j],units='mins'))
			bef.x3 = rep(0,length(bef.x2)); bef.x3[which.max(bef.x1$dateandtime)]=1 # 식전 마지막 혈당
			bef.x4 = j
			bef.x5 = NA

			if ( dim(bef.x1)[1]!=0 ) {
				glucose.bef = rbind(glucose.bef,cbind(bef.x1,bef.x2,bef.x3,bef.x4,bef.x5,time.event[[i]][j]))

			} else if ( dim(bef.x1)[1]==0 ) { # 식사기록 후 식후혈당 측정 없음
				errCode.sub = c(errCode.sub, 'Warn_004')
				avgIncValue[[i]][j] = NA
				speedIncValue[[i]][j] = NA
				speedIncValue.warnings[[i]][j] = NA
				recoveryTime[[i]][j] = NA
				peakValue[[i]][j] = NA
				spikeandcrash[[i]][j] = NA
				memo[[i]][j] = paste(as.character(data[which(data$sub==1 & data$dateandtime==time.event[[i]][j] & !is.na(data$memo)),'memo']),collapse=',')
				next
			}

			## 식사후 3시간내 glucose
			aft.x1 = data[which(data$dateandtime >= time.event[[i]][j] & data$dateandtime<time.end[[i]][j]),c('dateandtime','glucose','memo','timeCut2')]
			aft.x2 = as.numeric(difftime(aft.x1$dateandtime,time.event[[i]][j],units='mins'))
			aft.x3 = rep(0,length(aft.x2)); aft.x3[which.max(aft.x1$glucose)]=1 # 식후 최고 혈당
			aft.x4 = j
			aft.x5 = rep(0,length(aft.x2)); aft.x5[which.min(aft.x1$glucose)]=1 # 식후 최저 혈당


			if ( dim(aft.x1)[1]!=0 ) {
				glucose.aft = rbind(glucose.aft,cbind(aft.x1,aft.x2,aft.x3,aft.x4,aft.x5,time.event[[i]][j]))

			} else if ( dim(aft.x1)[1]==0 ) { # 식사기록 후 식후혈당 측정 없음
				errCode.sub = c(errCode.sub, 'Warn_004')
				avgIncValue[[i]][j] = NA
				speedIncValue[[i]][j] = NA
				speedIncValue.warnings[[i]][j] = NA
				recoveryTime[[i]][j] = NA
				peakValue[[i]][j] = NA
				spikeandcrash[[i]][j] = NA
				memo[[i]][j] = paste(as.character(data[which(data$sub==1 & data$dateandtime==time.event[[i]][j] & !is.na(data$memo)),'memo']),collapse=',')
				next
			}

			## 식전대비식후 혈당상승 평균 ---
#			glucose.inc = (aft.x1$glucose[as.logical(aft.x3)]-min(bef.x1$glucose[as.logical(bef.x3)],aft.x1$glucose[1],na.rm=T))
			glucose.inc = spikeLog[which(spikeLog$time_event==time.event[[i]][j]),]$peak - data[which(data$dateandtime==time.event[[i]][j]),]$glucose
			avgIncValue[[i]][j] = glucose.inc

			## 식전대비식후 혈당상승 속도 ---
			time.inc = (aft.x2[as.logical(aft.x3)]-bef.x2[as.logical(bef.x3)])
			speedIncValue[[i]][j] = glucose.inc/time.inc			
			speedIncValue.warnings[[i]][j] = ifelse(glucose.inc/time.inc <1,0,1) + ifelse(glucose.inc/time.inc <2,0,1) + ifelse(glucose.inc/time.inc <3,0,1)
			# 0: 유지(30분이내 30mgdL, 1분이내 1mgdL) 
			# 1: 서서히(30분이내 30-60mgdL, 1분이내 1-2mgdL) 
			# 2: 그냥(30분이내 60-90mgdL, 1분이내 2-3mgdL) 
			# 3: 급격히(30분이내 90mgdL이상, 1분이내 3mgdL이상)

			## 식사메모
				spikeNo = data[which(data$dateandtime==time.event[[i]][j]),]$SpikeIdx[1]
                memo[[i]][j] = as.character(spikeLog[which(spikeLog$SpikeIdx==spikeNo),'memoCol'])
				if ( is.na(memo[[i]][j]) ) memo[[i]][j] = ''

            #    memo[[i]][j] = as.character(spikeLog[which(spikeLog$SpikeIdx%in%spikeNo & !is.na(spikeLog$memoCol)),'memoCol'])


			## 식전대비식후 혈당상승 유지시간
			# aft.x3 이후이면서 식전혈당 *1.2 보다 낮아지는 시점까지의 시간 (ref. 맞춤혁명식단)
			recoveryTime[[i]][j] = aft.x2[setdiff(which(aft.x1$glucose<(bef.x1$glucose[as.logical(bef.x3)]*1.2)),c(1:which(as.logical(aft.x3))))[1]]

			## 식후 혈당 피크값 
			# aft.x3 이후에 aft.x5가 나타나며 식전혈당*0.9보다 낮아짐 & 낮아진 이후에 혈당재상승하며 식전혈당 *1.2 보다 높아짐
			peakValue[[i]][j] = aft.x1$glucose[as.logical(aft.x3)]

			## 식전대비식후 혈당 급하락여부 (식후혈당 급하락 후 재상승)
			# aft.x3 이후에 aft.x5가 나타나며 식전혈당*0.9보다 낮아짐 & 낮아진 이후에 혈당재상승하며 식전혈당 *1.2 보다 높아짐
			spikeandcrash[[i]][j] = ifelse(aft.x2[as.logical(aft.x3)]<aft.x2[as.logical(aft.x5)] & aft.x1$glucose[as.logical(aft.x5)]<bef.x1$glucose[as.logical(bef.x3)]*0.9,
				ifelse(!is.na(setdiff(which(aft.x1$glucose >(bef.x1$glucose[as.logical(bef.x3)]*1.2)),c(1:which(as.logical(aft.x5))))[1]),1,0),0) 

			rm(bef.x1,bef.x2,bef.x3,bef.x4,aft.x1,aft.x2,aft.x3)

		}
	}
	colnames(glucose.bef) = gsub('bef.','',colnames(glucose.bef))
	colnames(glucose.aft) = gsub('aft.','',colnames(glucose.aft))
	colnames(glucose.bef)[ncol(glucose.bef)] = 'event'
	colnames(glucose.aft)[ncol(glucose.aft)] = 'event'
	data.glucose = rbind(glucose.bef,glucose.aft)


	### step3 =============================================================================##
	## 정상 이상패턴 분류  -------------------## 

	## 방법1 TargetValue--------------------
	## 1. 권장 : 180 이내로 증가하는 경우 
	## 2. 주의 : 180보다 높게 증가하나, 3시간내 회복되는 경우 (식전혈당의 1.2배 이내로 돌아옴)
	## 3. 이상 : 180보다 높게 증가하고, 3시간내에도 회복되지 않는 경우 
	## 4. 이상 : 180보다 높게 증가하고, 식전혈당보다 낮아졌다 다시 증가하는 형태로 3시간내에도 회복되지 않는 경우 

	## 방법2 IncValue--------------------
	## 1. 정상 : +alpha 이내로 증가하는 경우 
	## 2. 주의 : +alpha보다 높게 증가하나, 3시간내 회복되는 경우 (식전혈당의 1.2배 이내로 돌아옴)
	## 3. 이상 : +alpha보다 높게 증가하고, 3시간내에도 회복되지 않는 경우 
	## 4. 이상 : +alpha보다 높게 증가하고, 식전혈당보다 낮아졌다 다시 증가하는 형태로 3시간내 회복되지 않는 경우 

	out_value1 = data.frame()
	out_value2 = data.frame()
	out_value3 = data.frame()
	out_value4 = data.frame()
	out_value5 = data.frame() # 스파이크 

	peakValue2 = unlist(peakValue)
	recoveryTime2 = unlist(recoveryTime)
	spikeandcrash2 = unlist(spikeandcrash)
	time.event2 = as.POSIXct(unlist(time.event),origin='1970-01-01',tz='GMT')
	avgIncValue2 = unlist(avgIncValue)
	memo2 = unlist(memo)

	## 조건
	cont1a = !is.na(peakValue2) & peakValue2<=PPG.U
	cont1b = !is.na(peakValue2) & peakValue2>PPG.U
	cont2a = !is.na(recoveryTime2) & recoveryTime2<=(60*3)
	cont2b = is.na(recoveryTime2) | (!is.na(recoveryTime2) & recoveryTime2>(60*3))
	cont3a = !as.logical(spikeandcrash2)
	cont3b = as.logical(spikeandcrash2)
	cont4a = !is.na(avgIncValue2) & avgIncValue2<=IncCut
	cont4b = !is.na(avgIncValue2) & avgIncValue2>IncCut

	## 패턴분류
	# method = 'IncValue' # temp 20230925 #todo 
	ptrn1 = cont4a & cont2a
	ptrn2 = cont4b & cont2a & cont3a
	ptrn3 = cont4b & cont2b & cont3a
	ptrn4 = cont4b & cont2a & cont3b
#	ptrn1 = ptrn2 = ptrn3 = ptrn4 = is.na(time.event2)
	ptrn5 = !is.na(time.event2)
	
	## 메모
	out_value1 = data.frame(x=format(time.event2[which(ptrn1)],format='%m월%d일,%H시'),y=memo2[which(ptrn1)],z=avgIncValue2[which(ptrn1)],k=time.event2[which(ptrn1)],p=peakValue2[which(ptrn1)])

	out_value2 = data.frame(x=format(time.event2[which(ptrn2)],format='%m월%d일,%H시'),y=memo2[which(ptrn2)],z=avgIncValue2[which(ptrn2)],k=time.event2[which(ptrn2)],p=peakValue2[which(ptrn2)])

	out_value3 = data.frame(x=format(time.event2[which(ptrn3)],format='%m월%d일,%H시'),y=memo2[which(ptrn3)],z=avgIncValue2[which(ptrn3)],k=time.event2[which(ptrn3)],p=peakValue2[which(ptrn3)])

	out_value4 = data.frame(x=format(time.event2[which(ptrn4)],format='%m월%d일,%H시'),y=memo2[which(ptrn4)],z=avgIncValue2[which(ptrn4)],k=time.event2[which(ptrn4)],p=peakValue2[which(ptrn4)])

	out_value5 = data.frame(x=format(time.event2[which(ptrn5)],format='%m월%d일,%H시'),y=memo2[which(ptrn5)],z=avgIncValue2[which(ptrn5)],k=time.event2[which(ptrn5)],p=peakValue2[which(ptrn5)])
	out_value5 = out_value5[which(out_value5$z>=30),] #실제30이상 상승폭 가진 경우만 


	### step4 =============================================================================## 
	## 시각화   -------------------##

	## 고민. 상승폭기준일경우, 식전혈당값이 서로 다르기떄문에, 식후 혈당값도 모두 다르고-> 그럼 그림의 시각화 회색명암을 어디로 둬야할지..? #
	## - 배경은 우선 그대로 가져가자 -> 설득: 배경색이 원래 일반적인 목표값인데, 너의 식전혈당 수준이 이렇기에 차이가 있는거다 라고 설명할 수 있을 듯 # 

	## 권장식사   -------------------##
	out.Plot1 = PPGGlucosePlot_onW(out_value=out_value1, data.glucose=data.glucose, Target, PPG.L, PPG.U)

	## 주의식사   -------------------##
	out.Plot2 = PPGGlucosePlot_onW(out_value=out_value2, data.glucose=data.glucose, Target, PPG.L, PPG.U)

	## 지양1식사   -------------------##
	out.Plot3 = PPGGlucosePlot_onW(out_value=out_value3, data.glucose=data.glucose, Target, PPG.L, PPG.U)

	## 지양2식사   -------------------##
	out.Plot4 = PPGGlucosePlot_onW(out_value=out_value4, data.glucose=data.glucose, Target, PPG.L, PPG.U)

	## 혈당스파이크식사   -------------------##
	out.Plot5 = PPGGlucosePlot_onW(out_value=out_value5, data.glucose=data.glucose, Target, PPG.L, PPG.U)


	### step5 =============================================================================## 
	## 식사내역(메모)   -------------------##

	## 식후 평균 혈당 상승분의 크기로 sorting ---------------
	# out_value1 : 혈당상승 작은 순서로
	# out_value2,3,4 : 혈당상승 큰 순서로 
	# out_value5 : 혈당상승 큰 순서로 

	avgInc_byPtrn = c(mean(out_value1$z,na.rm=T),mean(out_value2$z,na.rm=T),mean(out_value3$z,na.rm=T),mean(out_value4$z,na.rm=T), mean(out_value5$z,na.rm=T))

#	out_value1 = try(out_value1[order(out_value1$z,decreasing=F),1:3],silent=T)
#	out_value2 = try(out_value2[order(out_value2$z,decreasing=T),1:3],silent=T)
#	out_value3 = try(out_value3[order(out_value3$z,decreasing=T),1:3],silent=T)
#	out_value4 = try(out_value4[order(out_value4$z,decreasing=T),1:3],silent=T)
	out_value1 = try(out_value1[order(out_value1$z,decreasing=F),c('x','y','z','p')],silent=T)
	out_value2 = try(out_value2[order(out_value2$z,decreasing=T),c('x','y','z','p')],silent=T)
	out_value3 = try(out_value3[order(out_value3$z,decreasing=T),c('x','y','z','p')],silent=T)
	out_value4 = try(out_value4[order(out_value4$z,decreasing=T),c('x','y','z','p')],silent=T)
	out_value5 = try(out_value5[order(out_value5$z,decreasing=T),c('x','y','z','p')],silent=T) # 

	if ( class(out_value1)=='try-error' || dim(out_value1)[1]==0 ) {
		out_value1 = data.frame(date='',menu='해당 없음',z='',p='')
	}
	if ( class(out_value2)=='try-error' || dim(out_value2)[1]==0 ) {
		out_value2 = data.frame(date='',menu='해당 없음',z='',p='')
	}
	if ( class(out_value3)=='try-error' || dim(out_value3)[1]==0 ) {
		out_value3 = data.frame(date='',menu='해당 없음',z='',p='')
	}
	if ( class(out_value4)=='try-error' || dim(out_value4)[1]==0 ) {
		out_value4 = data.frame(date='',menu='해당 없음',z='',p='')
	}
	if ( class(out_value5)=='try-error' || dim(out_value5)[1]==0 ) {
		out_value5 = data.frame(date='',menu='해당 없음',z='',p='')
	}

	colnames(out_value1) = paste(c('date','menu','z','p'),mealtime,sep='')
	out_value1$rowNum = seq(1,nrow(out_value1),by=1)#rownames(out_value1)
	colnames(out_value2) = paste(c('date','menu','z','p'),mealtime,sep='')
	out_value2$rowNum = seq(1,nrow(out_value2),by=1)#rownames(out_value2)
	colnames(out_value3) = paste(c('date','menu','z','p'),mealtime,sep='')
	out_value3$rowNum = seq(1,nrow(out_value3),by=1)#rownames(out_value3)
	colnames(out_value4) = paste(c('date','menu','z','p'),mealtime,sep='')
	out_value4$rowNum = seq(1,nrow(out_value4),by=1)#rownames(out_value4)
	colnames(out_value5) = paste(c('date','menu','z','p'),mealtime,sep='')
	out_value5$rowNum = seq(1,nrow(out_value5),by=1)


	### output =============================================================================##

	out.Result =  list(out_value1=out_value1,out_value2=out_value2,out_value3=out_value3,out_value4=out_value4,out_value5=out_value5,
			avgInc_Total=mean(unlist(avgIncValue),na.rm=T), avgInc_byPtrn=avgInc_byPtrn,
			data=data.glucose,
			out.Plot1_forApp=out.Plot1$out.Plot_forApp,out.Plot2_forApp=out.Plot2$out.Plot_forApp,out.Plot3_forApp=out.Plot3$out.Plot_forApp,out.Plot4_forApp=out.Plot4$out.Plot_forApp,out.Plot5_forApp=out.Plot5$out.Plot_forApp,
			out.Plot1_forWeb=out.Plot1$out.Plot_forWeb,out.Plot2_forWeb=out.Plot2$out.Plot_forWeb,out.Plot3_forWeb=out.Plot3$out.Plot_forWeb,out.Plot4_forWeb=out.Plot4$out.Plot_forWeb,out.Plot5_forWeb=out.Plot5$out.Plot_forWeb,
			errCode.sub = errCode.sub)	

	return(out.Result)

}