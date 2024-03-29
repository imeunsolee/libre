##########################################
## dailyGP_onW
## ---------------------------------------
## input 
## . data : 상위함수에서 가져옴 # page1.tmp$out.Plot_31_source
## . bestday
## . worstday
## ---------------------------------------

dailyGP_onW = function( data, bestday, worstday, TBR_lev1.Cut, TAR_lev1.Cut, method, spikeStat=spikeStat ) {

	errCode.sub = c()

	if ( missing(bestday) ) bestday = NULL
	if ( missing(worstday) ) worstday = NULL


	### step4 =============================================================================##
	## (3단, 전체(중앙))                   -------------------## 
	## 일일 혈당 프로필 그래프 생성

	## 7일 간격으로 그래프 생성 ---
	days.tmp = seq.Date(min(unique(data$date)),min(unique(data$date))+13,1)
	wk.text = weekdays(days.tmp)[1:7] ## temp
	out.Plot = list()
	out.Plot[[1]] = list()
	out.Plot[[2]] = list()
	ylim.tmp = ifelse(max(data$glucose,na.rm=T)>350,ifelse(max(data$glucose,na.rm=T)>400,ifelse(max(data$glucose,na.rm=T)>450,500,450),400),350)

	## Target별 ymin, ymax
	ymin.Target = TBR_lev1.Cut # <- 70
	ymax.Target = TAR_lev1.Cut # <- 180

	for ( i in 1:2 ) {
        # 주차별 그래프
		wk.tmp = days.tmp[which(c(1:length(days.tmp))<=7*i & c(1:length(days.tmp))>7*(i-1))]
		data.tmp = data[which(data$date %in% wk.tmp),]

		data.tmp$glucose.hi = ifelse(data.tmp$glucose>ymax.Target,data.tmp$glucose,NA)
		data.tmp$glucose.lo = ifelse(data.tmp$glucose<ymin.Target,data.tmp$glucose,NA)
		xlim.tmp = as.POSIXct(c(paste(min(wk.tmp),'00:00:00'),paste(min(wk.tmp)+7,'00:00:00')),format='%Y-%m-%d %H:%M:%S',tz='GMT')

		out.Plot[[1]][[i]] = ggplot(data.tmp) +
			scale_y_continuous(name='',limits=c(0,ylim.tmp),breaks=c(TBR_lev1.Cut,TAR_lev1.Cut,ylim.tmp),labels=c(TBR_lev1.Cut,TAR_lev1.Cut,ylim.tmp)) +
			annotate('rect',xmin=xlim.tmp[1],xmax=xlim.tmp[2],ymin=ymin.Target,ymax=ymax.Target,fill='#e3e3e3') +
			geom_vline(xintercept=as.POSIXct(paste(wk.tmp,'12:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),color='#8a8b8a',lty=2,size=0.2) +
			geom_vline(xintercept=as.POSIXct(paste(wk.tmp,'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),color='#231f20',size=0.2) +
			geom_vline(xintercept=as.POSIXct(paste(wk.tmp,'03:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),color='#231f20',lty=3,size=0.2) +
			geom_vline(xintercept=as.POSIXct(paste(wk.tmp,'06:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),color='#231f20',lty=3,size=0.2) +
			geom_vline(xintercept=as.POSIXct(paste(wk.tmp,'09:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),color='#231f20',lty=3,size=0.2) +
			geom_vline(xintercept=as.POSIXct(paste(wk.tmp,'15:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),color='#231f20',lty=3,size=0.2) +
			geom_vline(xintercept=as.POSIXct(paste(wk.tmp,'18:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),color='#231f20',lty=3,size=0.2) +
			geom_vline(xintercept=as.POSIXct(paste(wk.tmp,'21:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),color='#231f20',lty=3,size=0.2)


		if (any(!is.na(data.tmp$glucose.lo))) out.Plot[[1]][[i]] = out.Plot[[1]][[i]]+geom_ribbon(aes(x=dateandtime,ymin=glucose.lo,ymax=ymin.Target),fill='#d71920')
		if (any(!is.na(data.tmp$glucose.hi))) out.Plot[[1]][[i]] = out.Plot[[1]][[i]]+geom_ribbon(aes(x=dateandtime,ymin=ymax.Target,ymax=glucose.hi),fill='#fff100')

		out.Plot[[1]][[i]] = out.Plot[[1]][[i]] + geom_line(mapping=aes(x=dateandtime,y=glucose),col='#1f50ce',size=0.8) ##0b538f 

		if ( i==1 ) {
			out.Plot[[1]][[i]] = out.Plot[[1]][[i]] +
				scale_x_datetime(name='',date_labels='%A',limits=c(xlim.tmp[1],xlim.tmp[2]),position='top',
					breaks=as.POSIXct(seq.POSIXt(xlim.tmp[1]+12*60*60,xlim.tmp[2],by='24 hours'),format='%A',tz='GMT')) +
				coord_cartesian(expand=F,clip='off') +

				theme(axis.text.x=element_text(size=12,color='#122747',face='bold'))

			for ( j in 1:length(wk.tmp) ) {
				out.Plot[[1]][[i]] = out.Plot[[1]][[i]] +
					geom_text(x=as.POSIXct(strptime(paste(wk.tmp[j],'02:20:00'),format='%Y-%m-%d %H:%M:%S'),tz='GMT'),y=(ylim.tmp*0.92),label=as.POSIXlt(wk.tmp[j])$mday,size=5,family='NotoSansCJKkrR')
			}

			out.Plot[[1]][[i]] = out.Plot[[1]][[i]] +
				theme(panel.background=element_rect(fill=NA,color=NA),
					panel.border=element_rect(colour='#231f20',fill=NA,size=0.3),
					axis.text.y=element_text(size=11),axis.ticks.x=element_blank(),
					plot.margin=unit(c(-5,20,0,10),'points'),
					text=element_text(family='NotoSansCJKkrR'))

		}
		if ( i==2 ) {

			break.tmp = as.POSIXct(seq.POSIXt(xlim.tmp[1],xlim.tmp[2],by='3 hours'),format='%H:%M',tz='GMT')
			lab.tmp = format(break.tmp,'%k')
			lab.tmp = gsub('0','0시',lab.tmp)
			lab.tmp = gsub('12','12시',lab.tmp)

			out.Plot[[1]][[i]] = out.Plot[[1]][[i]] +
				scale_x_datetime(name='',date_labels=lab.tmp,position='top',breaks=break.tmp) +
				coord_cartesian(expand=F,clip='off') +

				theme(axis.text.x=element_text(size=c(9,9,9,9),hjust=0.5,color=c('#122747','#4d5262','#4d5262','#4d5262'),face=c('bold','plain','plain','plain')))

			for ( j in 1:length(wk.tmp) ) {
				out.Plot[[1]][[i]] = out.Plot[[1]][[i]]+
					geom_text(x=as.POSIXct(strptime(paste(wk.tmp[j],'02:20:00'),format='%Y-%m-%d %H:%M:%S'),tz='GMT'),y=(ylim.tmp*0.92),label=as.POSIXlt(wk.tmp[j])$mday,size=5,family='NotoSansCJKkrR')
			}

			out.Plot[[1]][[i]] = out.Plot[[1]][[i]]+
				theme(panel.background=element_rect(fill=NA,color=NA),
					panel.border=element_rect(colour='#122747',fill=NA,size=0.3),
					axis.text.y=element_text(size=11),axis.ticks.x=element_blank(),
					plot.margin=unit(c(-5,20,0,10),'points'),
					text=element_text(family='NotoSansCJKkrR'))

		}
        
        if ( any(wk.tmp%in%bestday) ) {
			bestNum = spikeStat[which(spikeStat$date_event==bestday),]$count_peak
            out.Plot[[1]][[i]] = out.Plot[[1]][[i]] +
                geom_rect(data=data.tmp,mapping=aes(xmin=as.POSIXct(paste(bestday,'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),xmax=as.POSIXct(paste(bestday+1,'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),ymin=0,ymax=ylim.tmp),color='#12c29c',size=3,alpha=0)
            out.Plot[[1]][[i]] = out.Plot[[1]][[i]] +
                geom_label(aes(label=paste('BEST:',bestNum),x=as.POSIXct(paste(bestday,'18:30:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),y=ylim.tmp-30),size=5,family='NotoSansCJKkrB',fill='#12c29c',color='#FFFFFF',alpha=0.02)
        }
        if ( any(wk.tmp%in%worstday) ) {
			worstNum = spikeStat[which(spikeStat$date_event==worstday),]$count_peak
            out.Plot[[1]][[i]] = out.Plot[[1]][[i]] +
                geom_rect(data=data.tmp,mapping=aes(xmin=as.POSIXct(paste(worstday,'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),xmax=as.POSIXct(paste(worstday+1,'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),ymin=0,ymax=ylim.tmp),color='#EF4559',size=3,alpha=0)
			out.Plot[[1]][[i]] = out.Plot[[1]][[i]] + 
                geom_label(aes(label=paste('WORST:',worstNum),x=as.POSIXct(paste(worstday,'17:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),y=ylim.tmp-30),size=5,family='NotoSansCJKkrB',fill='#EF4559',color='#FFFFFF',alpha=0.02)
        }

	}


	### step5 =============================================================================##
	## 일일 혈당 프로필 달력 생성

    if ( method=='Spike' ) {

		data = data[!is.na(data$glucose),]    
        dates = seq.Date(range(data$date)[1],range(data$date)[2],by=1)
        while ( format(min(dates),'%w')!=0 ) {
            dates = c(min(dates)-1,dates)
        }
        while ( format(max(dates),'%w')!=6 ) {
            dates = c(dates,max(dates)+1)
        }

        ##
        marker.tmp = c()
        for ( i in 1:length(dates) ) marker.tmp[i] = as.numeric(!(dates[i] %in% unique(data$date)))+1

        ## 
        calendarA.plt = list()
        for ( w in 1:7 ) {
            
            calendarA.plt[[w]] = ggplot() +
                scale_y_continuous(name='',limits=c(0.75,1.25))+
                scale_x_continuous(name='',limits=c(0.75,3.25))+
                theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),plot.margin=unit(c(-1,0.5,-1.5,-2.5),'point'))
            
            calendarA.plt[[w]] = calendarA.plt[[w]] +
                theme(panel.background=element_rect(fill='#EAF0FF',colour='#E9EDF3'))+
                geom_text(mapping=aes(x=2,y=1),label=format(dates[w],'%a'),color='#113694',size=16/3,family='NotoSansCJKkrB')

        }


        calendarD.plt=list()
        for ( d in 1:length(dates) ) {

            dates.tmp = dates[d]

            calendarD.plt[[d]] = ggplot() +
                scale_y_continuous(name='',limits=c(0.75,3.25))+
                scale_x_continuous(name='',limits=c(0.75,3.25))+
                theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),plot.margin=unit(c(-1,0.5,-1.5,-2.5),'point'))
            
            if ( bestday %in% dates.tmp ) {
                calendarD.plt[[d]] = calendarD.plt[[d]] +
                    theme(panel.background=element_rect(fill='#DBF6E6',colour='#E9EDF3'))+
                    geom_text(mapping=aes(x=1,y=3),label=as.POSIXlt(dates.tmp)$mday,color='#20C4A0',size=16/3,family='NotoSansCJKkrB')+
                    geom_text(mapping=aes(x=2,y=2),label='BEST',color='#20C4A0',size=20/3,family='NotoSansCJKkrB')

            } else if ( worstday %in% dates.tmp ) {
                calendarD.plt[[d]] = calendarD.plt[[d]] +
                    theme(panel.background=element_rect(fill='#FFC7CB',colour='#E9EDF3'))+
                    geom_text(mapping=aes(x=1,y=3),label=as.POSIXlt(dates.tmp)$mday,color='#EF4559',size=16/3,family='NotoSansCJKkrB')+
                    geom_text(mapping=aes(x=2,y=2),label='WORST',color='#EF4559',size=20/3,family='NotoSansCJKkrB')

            } else {
                calendarD.plt[[d]] = calendarD.plt[[d]] +
                    theme(panel.background=element_rect(fill=c('#FFFFFF','#F7F7F7')[marker.tmp[d]],colour='#E9EDF3')) +
                    geom_text(mapping=aes(x=1,y=3),label=as.POSIXlt(dates.tmp)$mday,color=c('#113694','#7E8695')[marker.tmp[d]],size=16/3,family='NotoSansCJKkrR')
            }

            if ( min(which(marker.tmp==1))==d ) {
                calendarD.plt[[d]] = calendarD.plt[[d]] +
                    geom_text(mapping=aes(x=1.5,y=1),label='← 시작',color='#3E71F0',size=16/3,family='NotoSansCJKkrR')
            }
            if ( max(which(marker.tmp==1))==d ) {
                calendarD.plt[[d]] = calendarD.plt[[d]] +
                    geom_text(mapping=aes(x=2.5,y=1),label='끝 →',color='#3E71F0',size=16/3,family='NotoSansCJKkrR')
            }
        }


        calendarA_forApp = try(grid.arrange(grobs=calendarA.plt,
            nrow=1,ncol=7,widths=unit(rep(640/7,7)*0.75,'points'),heights=unit(36*0.75,'points')),silent=T)

        calendarD_forApp = try(grid.arrange(grobs=calendarD.plt,
            nrow=(length(dates)%/%7),ncol=7,widths=unit(rep(640/7,7)*0.75,'points'),heights=unit(rep(100,(length(dates)%/%7))*0.75,'points')),silent=T)

        out.Plot[[2]][[1]] = try(grid.arrange(grobs=list(calendarA_forApp,calendarD_forApp),
            nrow=2,ncol=1,widths=unit(640*0.75,'points'),heights=unit(c(36,sum(rep(100,(length(dates)%/%7))))*0.75,'points')),silent=T)
		if ( all(class(out.Plot[[2]][[1]])!='try-error') ) {
			out.Plot[[2]][[2]] = sum(c(36,sum(rep(100,(length(dates)%/%7)))))
		} 


    }

	### output =============================================================================##
    
    return(list(out.Plot=out.Plot))

}