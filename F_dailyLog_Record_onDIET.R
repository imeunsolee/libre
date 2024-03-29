##########################################
## dailyLog_Record_DIET
## ---------------------------------------
## input 
## . daysAZ : 분석기간 Date %Y-%m-%d  
## . data : 혈당데이터 AGPdata data.frame() 
## . baseGlu : 기저혈당
## . IncCut : 상승폭기준 상승폭 
## ex
# daysAZ = unique(AGPdata$date)
# data = AGPdata
# stat = page3.tmp$statE
# baseGlu = summaryValue$meanGlu
## ---------------------------------------

dailyLog_Record_DIET = function( daysAZ, data, stat, baseGlu, FPG.L=FPG.L, PPG.U=PPG.U ) {

    errCode.sub = c()

    # output
    out.Plot = vector('list',length(daysAZ))
    out.SpikeN = vector('list',length(daysAZ))
    out.Memo = vector('list',length(daysAZ))

	### step1 ======================================================================##
	## 일일 혈당                            -------------------## 
    data_log1 = data[which(data$log==1),]
    data_log0 = data[which(data$log==0),] 
    data = try(sqldf('select *, max(glucose) as max_glucose, max(memo) as max_memo from data group by dateandtime'),silent=T)
    if ( class(data)=='try-error' ) {
        errCode.sub = c(errCode.sub,'sqldfFail')
    }
    data$dateandtime1 = as.POSIXct(as.numeric(data$dateandtime),origin='1970-01-01',tz='GMT')
    data$dateandtime = data$dateandtime1
    data$dateandtime1 = NULL
    data$glucose_log1 = data_log1$glucose[match(data$dateandtime,data_log1$dateandtime)]
    data$glucose_log0 = data_log0$glucose[match(data$dateandtime,data_log0$dateandtime)]
    ## 15분간격
    data$timef = as.POSIXct(strptime(data$time,format='%H:%M:%S'),tz='GMT')
    HMScut = strptime(format(seq.POSIXt(strptime('00:00:00',format='%H:%M:%S'),strptime('23:59:59',format='%H:%M:%S'),by='15 min'),format='%H:%M:%S'),format='%H:%M:%S',tz='GMT')
    data$timeCut1 = NA
    for ( i in 1:nrow(data) ) data$timeCut1[i] = sum(round(data$timef[i],unit='min')>=HMScut)
    #
    ylim.tmp = ifelse(max(data$max_glucose,na.rm=T)>350,ifelse(max(data$max_glucose,na.rm=T)>400,ifelse(max(data$max_glucose,na.rm=T)>450,500,450),400),350)

    if ( is.na(baseGlu) ) {
        baseGlu = round(tapply(data$max_glucose,data$date,mean,na.rm=T),0)
    }

    for ( d in 1:length(daysAZ) ) {

        # 그래프
        data_tmp = data[which(data$date==daysAZ[d]),]
        xlim.tmp = as.POSIXct(c(paste(daysAZ[d],'00:00:00'),paste(daysAZ[d]+1,'00:00:00')),format='%Y-%m-%d %H:%M:%S',tz='GMT')

        # 스파이크 정보
        stat_event = stat[which(as.Date(stat$time_event)==daysAZ[d]),] # 스파이크횟수는 이벤트발생시작(추정) 기준
        out.SpikeN[[d]] = dim(stat_event)[1]
        stat_peak_nextday = stat[which(as.Date(stat$time_peak)==daysAZ[d] & as.Date(stat$time_event)!=daysAZ[d]),] 

        if ( all(is.na(data_tmp$max_glucose)) ) {
            next
        }
        # 메모
        out.Memo[[d]] = data_tmp[!is.na(data_tmp$max_memo),c('dateandtime','max_memo','SpikeIdx')]
        if ( dim(out.Memo[[d]])[1]>=1 ) {
            for ( l in 1:dim(out.Memo[[d]])[1] ) {
                spikeidx.tmp = stat[which(as.Date(stat$time_event)==daysAZ[d]),]$SpikeIdx[grep(out.Memo[[d]]$max_memo[l],stat[which(as.Date(stat$time_event)==daysAZ[d]),]$memoCol)]
                if (length(spikeidx.tmp)>0) out.Memo[[d]]$SpikeIdx[l] = spikeidx.tmp
            }
        }
        
        out.Plot[[d]] = ggplot(data_tmp)+ 
            scale_y_continuous(name='',limits=c(0,ylim.tmp),breaks=c(FPG.L,PPG.U,ylim.tmp),labels=c(FPG.L,PPG.U,ylim.tmp))+
            scale_x_datetime(name='',date_labels='%H:%M',breaks=as.POSIXct(seq.POSIXt(xlim.tmp[1],xlim.tmp[2],by='3 hours')))+
            coord_cartesian(ylim=c(0,ylim.tmp),xlim=xlim.tmp,expand=F,clip='off')+
            annotate('rect',xmin=xlim.tmp[1],xmax=xlim.tmp[2],ymin=FPG.L,ymax=PPG.U,fill='#e3e3e3')+
            geom_vline(xintercept=as.POSIXct(paste(daysAZ[d],'12:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),color='#bcbec0',size=0.2)+
            geom_vline(xintercept=as.POSIXct(paste(daysAZ[d],c('03:00:00','06:00:00','09:00:00','15:00:00','18:00:00','21:00:00')),format='%Y-%m-%d %H:%M:%S',tz='GMT'),color='#bcbec0',lty=2,size=0.2)+
            geom_hline(yintercept=baseGlu[d],color='#40ac49',size=0.3)+
            annotation_custom(grob=textGrob(baseGlu[d],gp=gpar(fontsize=11,col='#231f20',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=(xlim.tmp[1]-2.5*60*60),xmax=(xlim.tmp[1]-2.5*60*60),ymin=baseGlu[d],ymax=baseGlu[d])+
            annotation_custom(grob=linesGrob(gp=gpar(col='#40ac49',lwd=1)),xmin=(xlim.tmp[1]-1.8*60*60),xmax=xlim.tmp[1],ymin=baseGlu[d],ymax=baseGlu[d])

        out.Plot[[d]] = out.Plot[[d]]+
            geom_line(mapping=aes(x=dateandtime,y=max_glucose),col='#1f50ce',size=1.5)

        out.Plot[[d]] = out.Plot[[d]]+
            geom_label(data=stat_event,mapping=aes(x=time_peak,y=peak,label=peak),fill='#FEC449',colour='#FF6600',vjust=-0.5,fontface='bold',label.size=0,size=5,alpha=0.4)+ # 스파이크횟수용
            geom_label(data=stat_peak_nextday,mapping=aes(x=time_peak,y=peak,label=peak),fill='#A5A6A7',colour='#5D5F60',vjust=-0.5,fontface='bold',label.size=0,size=5,alpha=0.4) # 실제 피크 

        out.Plot[[d]] = out.Plot[[d]]+
            theme(panel.background=element_rect(fill=NA,color=NA),
                panel.border=element_rect(colour='#122747',fill=NA,size=0.3),
                axis.text.y=element_text(color='#231f20',size=11),
			    axis.text.x=element_text(color=c('#231f20',rep('#808285',3),'#231f20',rep('#808285',3),'#231f20'),size=11,hjust=0.5,vjust=-3,face='plain'),
                axis.ticks.x=element_blank(),
                plot.margin=margin(10,25,10,10),
                text=element_text(family='NotoSansCJKkrR'))   

    }

    
	### output =============================================================================##
    out.Result = list(plot=out.Plot,spikeN=out.SpikeN,memo=out.Memo)

}