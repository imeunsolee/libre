##########################################
## GlucoseSpike_onW
## ---------------------------------------
## input 
## . daysAZ : 분석기간 Date %Y-%m-%d 
## . data : 혈당데이터 AGPdata data.frame() 
## . baseGlu : 기저혈당
## . IncCut : 상승폭기준 상승폭 
## ---------------------------------------

GlucoseSpike_onW = function( daysAZ, data, baseGlu, IncCut ) {

    errCode.sub = c()

	### step1 ======================================================================##
	## 혈당스파이크                            -------------------##
    data_log1 = data[which(data$log==1),c('dateandtime','glucose')]
    data_log0 = data[which(data$log==0),c('dateandtime','glucose')]
    data_tmp = try(sqldf('select *, max(glucose) as max_glucose, max(memo) as max_memo from data group by dateandtime'),silent=T)
    if ( class(data_tmp)=='try-error' ) {
        errCode.sub = c(errCode.sub,'sqldfFail')
    }
    data_tmp$dateandtime = as.POSIXct(as.numeric(data_tmp$dateandtime),origin='1970-01-01',tz='GMT')
    data_tmp$glucose_log1 = data_log1$glucose[match(data_tmp$dateandtime,data_log1$dateandtime)]
    data_tmp$glucose_log0 = data_log0$glucose[match(data_tmp$dateandtime,data_log0$dateandtime)]
    data_tmp$SpikeIdx = NA 
    data_tmp$high = ifelse(data_tmp$max_glucose>=(baseGlu+IncCut),1,0)
    data_tmp$low = ifelse(data_tmp$max_glucose<=(baseGlu),1,0)

    spikeNo = 0
    highidx = which(data_tmp$high==1)
    lowidx = which(data_tmp$low==1) 
    spikeIdx.tmp = rep(NA,dim(data_tmp)[1])

    for ( i in 1:length(highidx) ) {

        h0 = max(0,highidx[i-1])
        h1 = highidx[i]
        if ( h1==1 || h0==0 ) {
            next
        } else if ( any(h0<lowidx&lowidx<h1) ) {
            spikeNo = spikeNo+1
            spikeIdx.tmp[h1] = spikeNo
        } else if ( !is.na(spikeIdx.tmp[h0]) & (h1-h0)==1 ) {
            if ( round(difftime(data_tmp[h1,]$dateandtime,data_tmp[h0,]$dateandtime,unit='min')>20) ) {
                spikeNo = spikeNo+1
            }
            spikeIdx.tmp[h1] = spikeNo
        }
    }
    data_tmp$SpikeIdx = spikeIdx.tmp

    ## 스파이크요약 ------
    statE = try(sqldf('select SpikeIdx, max(max_glucose) as peak, min(dateandtime) as bpSpike_strt, max(dateandtime) as bpSpike_end, (max(dateandtime)-min(dateandtime))/60 as duration from data_tmp where SpikeIdx>0 group by SpikeIdx'),silent=T)
    statE$bpSpike_strt = as.POSIXct(statE$bpSpike_strt,origin='1970-01-01',tz='GMT')
    statE$bpSpike_end = as.POSIXct(statE$bpSpike_end,origin='1970-01-01',tz='GMT')
    
    ## 혈당상승 이벤트 시작 시점 찾기
    statE$time_event = (.POSIXct(NA,tz='GMT',cl='POSIXct'))
    statE$time_peak = (.POSIXct(NA,tz='GMT',cl='POSIXct'))
    for ( e in 1:dim(statE)[1] ) {

        statE$time_peak[e] = data_tmp[which(data_tmp$SpikeIdx==e & data_tmp$max_glucose==statE[which(statE$SpikeIdx==e),]$peak),]$dateandtime
        #
        time_tmp1 = statE[which(statE$SpikeIdx==e),]$time_peak - (3*60*60) 
        time_tmp2 = statE[which(statE$SpikeIdx==(e-1)),]$bpSpike_end

        set_tmp = data_tmp[which(data_tmp$dateandtime>=max(time_tmp1,time_tmp2,na.rm=T) & data_tmp$dateandtime<=statE[which(statE$SpikeIdx==e),]$bpSpike_strt & data_tmp$log==1),]

        if ( dim(set_tmp)[1]>1 ) {
            set_tmp$glucose1 = NA
            for ( i in 2:dim(set_tmp)[1] ) set_tmp$glucose1[i] = (set_tmp$glucose_log1[i]-set_tmp$glucose_log1[i-1])

            jIdx = sort(which(set_tmp$glucose1<=0),decreasing=T)
            if ( length(jIdx)>0 ) {
                time.event1 = set_tmp$dateandtime[jIdx[1]]
                for ( j in jIdx ) {
                    if ( !is.na(set_tmp[j+1,'glucose1']) && set_tmp[j+1,'glucose1'] > 0 ) {
                        time.event1 = set_tmp$dateandtime[j]
                        break

                    } else if ( !is.na(set_tmp[j+1,'glucose1']) && set_tmp[j+1,'glucose1']==0 ) {
                        if ( !is.na(set_tmp[j+2,'glucose1']) && set_tmp[j+2,'glucose1'] > 0 ) {
                            time.event1 = set_tmp$dateandtime[j]
                            break
                        }
                    } else {
                        next
                    }
                }
            } else {
                time.event1 = set_tmp$dateandtime[1]
            }
        } else {
            time.event1 = statE[which(statE$SpikeIdx==e),]$bpSpike_strt
        }
        statE$time_event[e] = time.event1
    }

    ## 이벤트 구간내 메모 가져오기
    statE$memoCol = NA
    for ( e in 1:dim(statE)[1] ) {

        # 피크 전
        time_tmp1 = statE[which(statE$SpikeIdx==e),]$time_event - (20*60)
        time_tmp2 = statE[which(statE$SpikeIdx==e),]$time_peak + (20*60)

        memo_tmp = data_tmp[which(data_tmp$dateandtime>=time_tmp1 & data_tmp$dateandtime<time_tmp2),]$max_memo
        if ( any(!is.na(memo_tmp)) ) {
            statE$memoCol[e] = paste(memo_tmp[!is.na(memo_tmp)],collapse=';')
            next            
        } else {
            # 피크 후 
            time_tmp3 = statE[which(statE$SpikeIdx==e),]$time_event + (3*60*60)
            time_tmp4 = statE[which(statE$SpikeIdx==(e+1)),]$time_event

            memo_tmp = data_tmp[which(data_tmp$dateandtime>=time_tmp2 & data_tmp$dateandtime<min(time_tmp3,time_tmp4,na.rm=T)),]$max_memo
            statE$memoCol[e] = paste(memo_tmp[!is.na(memo_tmp)],collapse=';')

        }
    }

    ## 스파이크 이벤트
    data$SpikeIdx = NA
    for ( e in 1:dim(statE)[1] ) {
        time_tmp1 = statE[which(statE$SpikeIdx==e),]$time_event
        time_tmp2 = min(statE[which(statE$SpikeIdx==e),]$time_event+(3*60*60),statE[which(statE$SpikeIdx==(e+1)),]$time_event,na.rm=T)
        data[which(data$dateandtime>=time_tmp1 & data$dateandtime<time_tmp2),]$SpikeIdx = e 
    }

    data$event_spike = NA
    data$event_spike = statE[match(data$dateandtime,statE$time_event),]$SpikeIdx


	### output =============================================================================##
    out.Result = list(data=data,stat=statE)
    return(out.Result)

} 