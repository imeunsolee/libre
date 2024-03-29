##########################################
## statisticGlucose
## ---------------------------------------
## input 
## . data : AGPdata
## . unit.glucose : 'mg.dl'
## . StartDate : YYYY-MM-DD
## . EndDate : YYYY-MM-DD
## ---------------------------------------
## ver ( 1.0 230824 )

statisticGlucose = function( data, unit.glucose='mg.dl', stat=T, tirstat=T,
TIR.Goal,TBR_lev2.Cut,TBR_lev2.Goal,TBR_lev1.Cut,TBR_lev1.Goal,TAR_lev1.Cut,TAR_lev1.Goal,TAR_lev2.Cut,TAR_lev2.Goal ) {


	### step1 =============================================================================##
	## 일반 통계  
	#자동로그기록만 통계 #todo

	## 분석기간 ---
	periodDate = range(data$date)

	## 분석일수 --- 
	periodDays = as.numeric(diff.Date(periodDate)+1)

	if ( stat==T ) {

		## %시간 CGM활성화비율 ---
		CGMactive.tmp = sum(tapply(data$glucose,data$time96,function(x){any(!is.na(x))}))
		CGMactivePer = CGMactive.tmp/96*100
		
		if ( F) {
		AllTimeCut = as.POSIXct(seq.POSIXt(min(data$dateandtime),max(data$dateandtime),by='15 min'),format='%Y-%m-%d %H:%M:%S')
		data.tmp = data
		CGMactive = rep(NA,length(AllTimeCut))
		for ( i in 1:length(AllTimeCut) ) {
			if ( i!=length(AllTimeCut) ) {
				CGMactive[i] = ifelse(any(AllTimeCut[i]<= round(data.tmp$dateandtime,unit='min') & AllTimeCut[i+1]> round(data.tmp$dateandtime,unit='min')),1,0)
			} else {
				CGMactive[i] = ifelse(any(AllTimeCut[i]<= round(data.tmp$dateandtime,unit='min')),1,0)
			}
		}
		CGMactivePer = sum(CGMactive)/length(CGMactive)*100
		rm(data.tmp)}

		## 평균 ---
		avgGlu = mean(data$glucose,na.rm=T) 

		## GMI or eA1c ---
		eA1ceqn = function(x,unit.glucose) {
			if ( unit.glucose=='mg.dl' ) {
				y = (x+46.7)/28.7
			} else if ( unit.glucose=='mmol.mol' ) {
				y = (x+2.59)/1.59
			}

			return(y)
		}
		GMIeqn = function(x,unit.glucose) {
			if ( unit.glucose=='mg.dl' ) {
				y = 3.31 + 0.02392*x
			} else if ( unit.glucose=='mmol.mol' ) {
				y = 12.71 + 4.70587*x
			}

			return(y)
		}
		
		gmiGlu = GMIeqn(x=avgGlu,unit.glucose=unit.glucose)

		## 혈당변동성 ---
		glycemicVariability = function(x,method){
			if ( method=='%cv' ) {
				GV = (sd(x,na.rm=T)/mean(x,na.rm=T))*100
				GV.assesment = 36
			} else if ( method=='sd' ) {
				GV = sd(x,na.rm=T)
				GV.assesment = mean(x,na.rm=T)/3
			}

			return(list(GV=GV,GV.assesment=GV.assesment))
		}

		cvGlu = glycemicVariability(x=data$glucose,method='%cv')

	}


	### step2 =============================================================================##
	## 혈당목표수준 TBR, TIR, TAR 

	if ( F ) {

		GlucoseCut.in = rep(NA,length(GlucoseCut))
		for ( k in 1:length(GlucoseCut) ) {
			GlucoseCut.in[k] = sum(data$glucose<GlucoseCut[k],na.rm=T)
			if ( grepl('TAR',names(GlucoseCut)[k]) ) {
				GlucoseCut.in[k] = GlucoseCut.in[k]-sum(data$glucose<GlucoseCut[k-1],na.rm=T)
			}
			if ( names(GlucoseCut)[k]=='TAR1' ) {
				names(GlucoseCut.in)[k] = 'TIR'
			} else {
				names(GlucoseCut.in)[k] = setdiff(names(GlucoseCut),names(GlucoseCut.in))[1]
			}
		}
		GlucoseCut.in = c(GlucoseCut.in,sum(data$glucose>=max(GlucoseCut),na.rm=T))
		names(GlucoseCut.in)[length(GlucoseCut.in)] = setdiff(names(GlucoseCut),names(GlucoseCut.in))[1]
	#	GlucoseCut.in = GlucoseCut.in/length(data$glucose)
	#	GlucoseCut.in = GlucoseCut.in/sum(!is.na(data$glucose)) # length(data$glucose)

	}
	if ( tirstat==T ) {
		TBR2.value = TBR1.value = TIR.value = TAR1.value = TAR2.value = NA
		if (!is.na(TBR_lev2.Cut)) TBR2.value = sum(data$glucose<TBR_lev2.Cut,na.rm=T)
		if (!is.na(TBR_lev1.Cut)) TBR1.value = sum(data$glucose<TBR_lev1.Cut,na.rm=T)
		if (!is.na(TAR_lev1.Cut)) TIR.value = sum(data$glucose<=TAR_lev1.Cut,na.rm=T) - sum(data$glucose<TBR_lev1.Cut,na.rm=T)
		if (!is.na(TAR_lev2.Cut)) {
			TAR1.value = sum(data$glucose<=TAR_lev2.Cut,na.rm=T) - sum(data$glucose<=TAR_lev1.Cut,na.rm=T)
			TAR2.value = sum(data$glucose>TAR_lev2.Cut,na.rm=T)
		} else {
			TAR1.value = sum(data$glucose>TAR_lev1.Cut,na.rm=T)
		}

		TBR2.value = TBR2.value/sum(!is.na(data$glucose))*100
		TBR1.value = TBR1.value/sum(!is.na(data$glucose))*100
		TIR.value = TIR.value/sum(!is.na(data$glucose))*100
		TAR1.value = TAR1.value/sum(!is.na(data$glucose))*100
		TAR2.value = TAR2.value/sum(!is.na(data$glucose))*100
	}


	### step3 =============================================================================##
	## result

	if ( stat==T ) {
		out.result = list( periodDate=periodDate, periodDays=periodDays, CGMactivePer=CGMactivePer, avgGlu=avgGlu, gmiGlu=gmiGlu, cvGlu=cvGlu$GV )
	} else {
		out.result = list()
	}
	if ( tirstat==T ) {
		out.result$TBR2.value = TBR2.value
		out.result$TBR1.value = TBR1.value
		out.result$TIR.value = TIR.value
		out.result$TAR1.value = TAR1.value
		out.result$TAR2.value = TAR2.value
	}

	return(out.result)

}
