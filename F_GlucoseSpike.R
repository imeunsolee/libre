##########################################
## GlucoseSpike
## ---------------------------------------
## input 
## . daysAZ : 분석기간 Date %Y-%m-%d 
## . data : 혈당데이터 AGPdata data.frame() 
## . baseGlu : 기저혈당
## . IncCut : 상승폭기준 상승폭 
## ---------------------------------------

GlucoseSpike = function( daysAZ, data, baseGlu, IncCut ) {

    errCode.sub = c()

	### step1 ======================================================================##
	## 혈당변곡점찾기 in data$log==1                        -------------------##
    data_log1 = data[which(data$log==1),c('dateandtime','glucose','date')]
    data_log1$x = NA
    data_log1$x = 1:nrow(data_log1)#as.numeric(rownames(data_log1))
    data_log1$turnPt = NA
    data_log1$turnSign = NA
    for ( d in 1:length(daysAZ) ) {
        span.v = 0.05
        while(1) {
            # fit.tmp = loess(max_glucose ~ x, data=data_tmp[which(data_tmp$date==daysAZ[d]),],span=span.v,control=loess.control(surface='interpolate'))
            fit.tmp = loess(glucose ~ x, data=data_log1[which(data_log1$date==daysAZ[d]),],span=span.v,control=loess.control(surface='interpolate'))
            pred.tmp = try(predict(fit.tmp,data_log1[which(data_log1$date==daysAZ[d]),]$x),silent=T)
            if ( class(pred.tmp)!='try-error' ) {
                break
            }
            span.v = (span.v+0.01)
            if ( span.v>0.1 ) {
                #errcode
                break
            }
        }
        data_log1$turnPt[setdiff(data_log1$x[c(FALSE,diff(diff(predict(fit.tmp,data_log1$x))>0)!=0)],NA)] = 1
        tmp1 = which(data_log1$date==daysAZ[d])[1]
        tmp2 = which(data_log1$date==daysAZ[d] & data_log1$turnPt==1)[1]
        if ( d>1 && (daysAZ[d]-1 == daysAZ[d-1]) ) {
            tmp1 = max(which(data_log1$date==daysAZ[d-1] & data_log1$turnPt==1))
        }
        if ( which.min(data_log1[tmp1:tmp2,]$glucose)!=1 && which.min(data_log1[tmp1:tmp2,]$glucose!=length(tmp1:tmp2)) ) {
            data_log1[c(tmp1:tmp2)[which.min(data_log1[tmp1:tmp2,]$glucose)],]$turnPt = 1
        }
        if ( which.max(data_log1[tmp1:tmp2,]$glucose)!=1 && which.max(data_log1[tmp1:tmp2,]$glucose)!=length(tmp1:tmp2) ) {
            data_log1[c(tmp1:tmp2)[which.max(data_log1[tmp1:tmp2,]$glucose)],]$turnPt = 1
        }

    }
    data_log1$turnSign = ifelse(is.na(data_log1$turnPt),NA,c(NA,diff(data_log1$glucose)))

    data_log0 = data[which(data$log==0),c('dateandtime','glucose')]
    data_tmp = try(sqldf('select *, max(glucose) as max_glucose, max(memo) as max_memo from data group by dateandtime'),silent=T)
    if ( class(data_tmp)=='try-error' ) {
        errCode.sub = c(errCode.sub,'sqldfFail')
    }
    data_tmp$dateandtime = as.POSIXct(as.numeric(data_tmp$dateandtime),origin='1970-01-01',tz='GMT')
    data_tmp$glucose_log1 = data_log1$glucose[match(data_tmp$dateandtime,data_log1$dateandtime)]
    data_tmp$glucose_log0 = data_log0$glucose[match(data_tmp$dateandtime,data_log0$dateandtime)]
    data_tmp$SpikeIdx = NA 
    data_tmp$high = NA
    data_tmp$low = NA
    data_tmp$turnPt = data_log1$turnPt[match(data_tmp$dateandtime,data_log1$dateandtime)]
    data_tmp$turnSign = data_log1$turnSign[match(data_tmp$dateandtime,data_log1$dateandtime)]

    # 그래프 # 
    # fit.tmp = loess(glucose ~ x, data=data_tmp[!is.na(data_tmp$max_glucose),],span=0.01,control=loess.control(surface='interpolate'))
    # x11();plot(data_log1[which(data_log1$date==daysAZ[d]),]$x,data_log1[which(data_log1$date==daysAZ[d]),]$glucose,type='l')
    # lines(data_log1[which(data_log1$date==daysAZ[d]),]$x,predict(fit.tmp,data_log1[which(data_log1$date==daysAZ[d]),]$x),col='red')
    # points(data_log1[which(data_log1$date==daysAZ[d]),]$x[c(FALSE,diff(diff(predict(fit.tmp,data_log1[which(data_log1$date==daysAZ[d]),]$x))>0)!=0)],predict(fit.tmp,data_log1[which(data_log1$date==daysAZ[d]),]$x)[c(FALSE,diff(diff(predict(fit.tmp,data_log1[which(data_log1$date==daysAZ[d]),]$x))>0)!=0)],col='blue')

	## 음식식사기록                            -------------------##
    for ( e in which(data_tmp$event_eat==1) ) {
        # e2 = max(which(data_tmp[1:max(which(data_tmp$dateandtime<=(data_tmp$dateandtime[e]+15*60))),]$turnPt==1 & data_tmp[1:max(which(data_tmp$dateandtime<=(data_tmp$dateandtime[e]+15*60))),]$turnSign<=0))
        e2 = which(data_tmp$dateandtime>=(data_tmp$dateandtime[e]-2*60*60) & data_tmp$dateandtime<=(data_tmp$dateandtime[e]+15*60))[max(which(data_tmp[which(data_tmp$dateandtime>=(data_tmp$dateandtime[e]-2*60*60) & data_tmp$dateandtime<=(data_tmp$dateandtime[e]+15*60)),]$turnPt==1 & data_tmp[which(data_tmp$dateandtime>=(data_tmp$dateandtime[e]-2*60*60) & data_tmp$dateandtime<=(data_tmp$dateandtime[e]+15*60)),]$turnSign<=0))]
        if ( is.na(e2) ) {
            next
        } else if ( !is.na(e2) && e2=='-Inf' ) {
            e1 = which(data_tmp$dateandtime>=data_tmp$dateandtime[e]-2*60*60)[1]
            e2 = c(e1:e)[which(data_tmp[e1:max(which(data_tmp$dateandtime<=(data_tmp$dateandtime[e]+15*60))),]$glucose == min(data_tmp[e1:max(which(data_tmp$dateandtime<=(data_tmp$dateandtime[e]+15*60))),]$glucose,na.rm=T))]
            if ( identical(e2,integer(0)) ) {
                next
            }
        }
        # e->e2 로 기록 이동 #
        data_tmp[e2,]$event_eat = 1
        data_tmp[e,]$event_eat = NA
        if( !is.na(data_tmp[e2,]$memo) ) {
            data_tmp[e2,]$memo = paste(data_tmp[c(e2,e),]$memo,collapse=';') #음식외 다른 메모일 수 있음
            data_tmp[e2,]$max_memo = paste(data_tmp[c(e2,e),]$max_memo,collapse=';') #음식외 다른 메모일 수 있음
        } else{
            data_tmp[e2,]$memo = data_tmp[e,]$memo
            data_tmp[e2,]$max_memo = data_tmp[e2,]$memo
        }
        data_tmp[e,]$memo = NA 
        data_tmp[e,]$max_memo = NA 
    }

	## 혈당스파이크                            -------------------##

    if ( is.na(baseGlu) ) {
        # baseGlu = round(tapply(data_tmp$max_glucose,data_tmp$date,mean,na.rm=T),1) #fix: round 1-> round 0 
        for ( d in 1:length(daysAZ) ) {
            avg.tmp = tapply(data_tmp[which(data_tmp$date==daysAZ[d]),]$glucose_log1,data_tmp[which(data_tmp$date==daysAZ[d]),]$time96,mean,na.rm=T)
            baseGlu[d] = round(mean(avg.tmp,na.rm=T),1) 
            names(baseGlu)[d] = as.character(daysAZ[d])

            data_tmp[which(data_tmp$date==names(baseGlu)[d]),]$high = ifelse(data_tmp[which(data_tmp$date==names(baseGlu)[d]),]$max_glucose>=(baseGlu[d]+IncCut),1,0) 
            data_tmp[which(data_tmp$date==names(baseGlu)[d]),]$low = ifelse(data_tmp[which(data_tmp$date==names(baseGlu)[d]),]$glucose_log1<=(baseGlu[d]),1,0) 
        }

    } else {
        # baseGlu = round(mean(data_tmp$max_glucose,na.rm=T),1) #fix: round 1-> round 0 
        baseGlu = round(mean(data_tmp$glucose_log1,na.rm=T),1)
        data_tmp$high = ifelse(data_tmp$max_glucose>=(baseGlu+IncCut),1,0)
        data_tmp$low = ifelse(data_tmp$max_glucose<=(baseGlu),1,0)
    }

    spikeNo = 0
    highidx = which(data_tmp$high==1)
    lowidx = which(data_tmp$low==1) 
    spikeIdx.tmp = rep(NA,dim(data_tmp)[1])

    if ( length(highidx)>0 ) {
        for ( i in 1:length(highidx) ) {

            h1 = highidx[i]
            h0 = max(0,highidx[i-1]) # 직전의 스파이크
            # h0 = max(h0,which(data_tmp$dateandtime>=(data_tmp$dateandtime[h1]-2*60*60))[1]) # max(직전의 스파이크, 2hr전)
    #        if ( h1==1 || h0==0 ) { # fix
            if ( i==1 & h1==1 ) {
                next
            } else if ( any(h0<lowidx&lowidx<h1) ) {
                spikeNo = spikeNo+1
                spikeIdx.tmp[h1] = spikeNo
            } else if ( any(h0<lowidx&lowidx<h1)==F ) {
                if ( any(diff(data_tmp[c(h0:h1)[!is.na(data_tmp[h0:h1,]$glucose)],]$dateandtime,unit='min') > 30) ) {
                    spikeNo = spikeNo+1
                }
                spikeIdx.tmp[h1] = spikeNo
            }
            # } else if ( h0!=0 && !is.na(spikeIdx.tmp[h0]) & (h1-h0)==1 ) {
            #     if ( difftime(data_tmp[h1,]$dateandtime,data_tmp[h0,]$dateandtime,unit='min')>30 ) { # fix: 20min -> 30min
            #         spikeNo = spikeNo+1
            #     }
            #     spikeIdx.tmp[h1] = spikeNo
            # }
        }
    }
    data_tmp$SpikeIdx = spikeIdx.tmp

    ## 스파이크요약 ------
    statE = try(sqldf('select SpikeIdx, max(max_glucose) as peak, min(dateandtime) as bpSpike_strt, max(dateandtime) as bpSpike_end, (max(dateandtime)-min(dateandtime))/60 as duration from data_tmp where SpikeIdx>0 group by SpikeIdx'),silent=T)
    statE$bpSpike_strt = as.POSIXct(statE$bpSpike_strt,origin='1970-01-01',tz='GMT')
    statE$bpSpike_end = as.POSIXct(statE$bpSpike_end,origin='1970-01-01',tz='GMT')
    
    if ( dim(statE)[1]>0 ) {
        ## 혈당상승 이벤트 시작 시점 찾기
        statE$time_event = (.POSIXct(NA,tz='GMT',cl='POSIXct'))
        statE$time_peak = (.POSIXct(NA,tz='GMT',cl='POSIXct'))
        ## 이벤트 구간내 메모 가져오기
        statE$memoCol = NA
        s3.past = min(data_tmp$dateandtime)

        for ( e in 1:dim(statE)[1] ) {

            # s1 = data_tmp[max(which(data_tmp[1:max(which(data_tmp$SpikeIdx==e)),]$low==1)),]$dateandtime
            s1 = data_tmp[max(which(data_tmp[1:max(which(data_tmp$SpikeIdx==e)),]$low==1 & data_tmp[1:max(which(data_tmp$SpikeIdx==e)),]$turnPt==1)),]$dateandtime
            s2 = data_tmp[min(intersect(max(which(data_tmp$SpikeIdx==e)):nrow(data_tmp), which(data_tmp$low==1))),]$dateandtime

            # if ( !is.na(s1) && s1 < statE[e,]$bpSpike_strt-2*60*60 ) {
            #     next 
            # }
            if ( is.na(s1) ) {
                s0.idx = which(data_tmp$dateandtime>=min(data_tmp[which(data_tmp$SpikeIdx==e),]$dateandtime)-2*60*60)[1]
                s1.idx = c(s0.idx:max(which(data_tmp$SpikeIdx==e)))[which(data_tmp[s0.idx:max(which(data_tmp$SpikeIdx==e)),]$glucose==min(data_tmp[s0.idx:max(which(data_tmp$SpikeIdx==e)),]$glucose,na.rm=T))]
                time.event1 = data_tmp$dateandtime[s1.idx]
            
            } else if ( !is.na(s1) ) {
                s1.idx = max(which(data_tmp[1:min(which(data_tmp$SpikeIdx==e)),]$turnPt==1 & data_tmp[1:min(which(data_tmp$SpikeIdx==e)),]$turnSign<=0))
                time.event1 = data_tmp$dateandtime[s1.idx]
            } 

            statE$time_event[e] = time.event1

            s3 = min(s2, time.event1+(3*60*60),na.rm=T)

            # 피크값 대체 
            # from: time.event1 ~ to:s3(+3시간) or s2(혈당이 다시 낮아지는 시점)
            statE[which(statE$SpikeIdx==e),]$peak = max(data_tmp[which(data_tmp$dateandtime>=time.event1 & data_tmp$dateandtime<=s2),]$max_glucose,na.rm=T)#max(data_tmp[which(data_tmp$SpikeIdx==e),]$max_glucose,na.rm=T)#
            statE$time_peak[e] = data_tmp[which(data_tmp$dateandtime>=time.event1 & data_tmp$dateandtime<=s2 & data_tmp$max_glucose==statE[which(statE$SpikeIdx==e),]$peak)[1],]$dateandtime#data_tmp[which(data_tmp$SpikeIdx==e & data_tmp$max_glucose==statE[which(statE$SpikeIdx==e),]$peak)[1],]$dateandtime#
            # 스파이크 구간 변경 있을 수 있음 bpSpike_strt ~ bpSpike_end, duration 
            statE[which(statE$SpikeIdx==e),]$bpSpike_strt = min(data_tmp[which(data_tmp$dateandtime>=time.event1 & data_tmp$dateandtime<=s2 & data_tmp$high==1),]$dateandtime)
            statE[which(statE$SpikeIdx==e),]$bpSpike_end = max(data_tmp[which(data_tmp$dateandtime>=time.event1 & data_tmp$dateandtime<=s2 & data_tmp$high==1),]$dateandtime)
            statE[which(statE$SpikeIdx==e),]$duration = difftime(statE[which(statE$SpikeIdx==e),]$bpSpike_end,statE[which(statE$SpikeIdx==e),]$bpSpike_strt,unit='min')

    #    }

    #    ## 이벤트 구간내 메모 가져오기
            time_tmp1 = max(statE[which(statE$SpikeIdx==e),]$time_event - (25*60), s3.past)
            time_tmp2 = statE[which(statE$SpikeIdx==e),]$bpSpike_end 

            memo_tmp = data_tmp[which(data_tmp$dateandtime>=time_tmp1 & data_tmp$dateandtime<=time_tmp2),]$max_memo
            if ( any(!is.na(memo_tmp)) ) {
                statE$memoCol[e] = paste(memo_tmp[!is.na(memo_tmp)],collapse=';')
                # next # ???이게 왜 있는지 모르겠음 
            }
            s3.past = s3
        }
    }
    ## time.event 찾을 수 없는 경우 삭제 ##
    statE = statE[!is.na(statE$time_event),]
    ## time.peak 찾을 수 없는 경우 삭제 ##
    statE = statE[!is.na(statE$time_peak),]

    if ( dim(statE)[1]>=1 ) { 
        statE$SpikeIdx = 1:nrow(statE)
    }
    ## 스파이크 이벤트
    data$SpikeIdx = NA
    data$event_spike = NA
    if ( dim(statE)[1]>0 ) {
        for ( e in 1:dim(statE)[1] ) {
            time_tmp1 = statE[which(statE$SpikeIdx==e),]$time_event
            time_tmp2 = min(statE[which(statE$SpikeIdx==e),]$bpSpike_end,statE[which(statE$SpikeIdx==(e+1)),]$time_event,na.rm=T)
            if ( time_tmp1==time_tmp2 ) {
                next
            }
            data[which(data$dateandtime>=time_tmp1 & data$dateandtime<time_tmp2),]$SpikeIdx = e 
        }
        data$event_spike = statE[match(data$dateandtime,statE$time_event),]$SpikeIdx
    }
    data$event_eat = data_tmp$event_eat[match(data$dateandtime,data_tmp$dateandtime)]
    data$memo = data_tmp$max_memo[match(data$dateandtime,data_tmp$dateandtime)] 



	### output =============================================================================##
    out.Result = list(data=data,stat=statE,baseGlu=baseGlu)
    return(out.Result)

} 