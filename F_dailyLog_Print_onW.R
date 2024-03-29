##########################################
## dailyLog_Record_onW
## ---------------------------------------
## input 
## . daysAZ : 분석기간 Date %Y-%m-%d  
## . plot
## . spikeN
## . memo
## ---------------------------------------

dailyLog_Print_onW = function( reportIdx, daysAZ, plot, spikeN, memo ) {

    errCode.sub = c()
    fileName.tmp = c()

	### step1 ======================================================================##
	## 스타일설정                           -------------------##
    pagetitle.theme_forWeb = ttheme_minimal(
        base_family=c('NotoSansCJKkrB','NotoSansCJKkrR'),
        core=list(bg_params=list(fill=NA,col=NA),
                fg_params=list(hjust=0,x=0.01,vjust=0.5,col=c('#122747','#4d5262'),fontsize=c(13.5,12),fontface=c('plain','plain')))
    )
    pagetitle.theme_forApp = ttheme_minimal(
        base_family=c('NotoSansCJKkrB','NotoSansCJKkrR'),
        core=list(bg_params=list(fill=NA,col=NA),
                fg_params=list(hjust=0,x=0.01,vjust=0.5,col=c('#122747','#4d5262'),fontsize=c(25,17),fontface=c('plain','plain')))
    )

    subtitle.theme_forWeb = ttheme_minimal(
        base_family='NotoSansCJKkrB',
        core=list(bg_params=list(fill='#000000',col=NA),
                fg_params=list(hjust=0,x=0.01,col='#ffffff',fontsize=13.5,fontface='plain'))
    )
    subtitle.theme_forApp = ttheme_minimal(
        base_family='NotoSansCJKkrB',
        core=list(bg_params=list(fill='#000000',col=NA),
                fg_params=list(hjust=0,x=0.01,col='#ffffff',fontsize=25,fontface='plain'))
    )

    remark.theme_forWeb = ttheme_minimal(
        base_family='NotoSansCJKkrR',
        core=list(bg_params=list(fill=NA,col=NA),
                fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=12,fontface='plain'))
    )
    remark.theme_forApp = ttheme_minimal(
        base_family='NotoSansCJKkrR',
        core=list(bg_params=list(fill=NA,col=NA),
                fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=17,fontface='plain'))
    )

    colhead.theme_forWeb = ttheme_minimal( 
        base_family='NotoSansCJKkrB',
        core=list(bg_params=list(fill=NA,col=NA),
                fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#122747',fontsize=13,fontface='plain'))
    )
    colmain.theme_forWeb = ttheme_minimal( 
        base_family='NotoSansCJKkrR',
        core=list(bg_params=list(fill=NA,col=NA),
                fg_params=list(hjust=0,x=0.02,vjust=1,y=0.85,col='#122747',fontsize=28,fontface='plain'))
    )
    colhead.theme_forApp = ttheme_minimal( 
        base_family='NotoSansCJKkrB',
        core=list(bg_params=list(fill='#F5F5F5',col=NA),
                fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#122747',fontsize=22,fontface='plain'))
    )
    colmain.theme_forApp = ttheme_minimal( 
        base_family='NotoSansCJKkrR',
        core=list(bg_params=list(fill='#F5F5F5',col=NA),
                fg_params=list(hjust=0,x=0.02,vjust=1,y=0.85,col='#122747',fontsize=28,fontface='plain'))
    )
  
	### step2 ======================================================================##
	## 데일리로그파일                           -------------------##

    pagetitle.tmp = rbind('일일 요약','분석 기간 동안 일일 혈당패턴과 식사에 의해 혈당 스파이크를 보였던 기록을 확인해보세요.')
    subtitle.tmp = '일일 혈당 스파이크 기록'
    remark.tmp = '혈당 스파이크는 평균혈당 대비 30 mg/dL이상 상승된 기록입니다. 혈당 스파이크를 일으킨 식사 종류와 방법을 되짚어 보고, 다음 식사에는 식사 순서를 바꾸는 등의 방법을 활용해 보세요.'

    pageTitle_forWeb = tableGrob(pagetitle.tmp,theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,width=unit(876,'points'),height=unit(c(28.5,28.5),'points'))
    subTitle_forWeb = tableGrob(subtitle.tmp,theme=subtitle.theme_forWeb,cols=NULL,rows=NULL,width=unit(876,'points'),height=unit(28.5,'points'))
    remark_forWeb = tableGrob(paste(strwrap(remark.tmp,width=120),collapse='\n'),theme=remark.theme_forWeb,cols=NULL,rows=NULL,width=unit(876,'points'),height=unit(36,'points'))

    pagetitle.tmp[2,1] = paste(strwrap(pagetitle.tmp[2,1],width=63),collapse='\n')
    pageTitle_forApp = tableGrob(pagetitle.tmp,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,width=unit(640*0.75,'points'),height=unit(c(67,72)*0.75,'points'))
    subTitle_forApp = tableGrob(subtitle.tmp,theme=subtitle.theme_forApp,cols=NULL,rows=NULL,width=unit(640*0.75,'points'),height=unit(67*0.75,'points'))
    remark_forApp = tableGrob(paste(strwrap(remark.tmp,width=63),collapse='\n'),theme=remark.theme_forApp,cols=NULL,rows=NULL,width=unit(640*0.75,'points'),height=unit(100*0.75,'points'))

    nullMain = tableGrob('',theme=colmain.theme_forWeb,cols=NULL,rows=NULL,width=unit(18,'points'),heights=unit(1,'points'))

    tmpH.W = tmpH.A = vector('list',length(daysAZ))
    daily1.W = daily1.A = vector('list',length(daysAZ))
    for ( d in 1:length(daysAZ) ) {
        
        plotHead_forWeb = tableGrob(format(daysAZ[d],format='%Y년 %m월 %d일 (%A)'),theme=colhead.theme_forWeb,cols=NULL,rows=NULL,width=unit(420,'points'),height=unit(33,'points'))
        spikeHead_forWeb = tableGrob('혈당 스파이크',theme=colhead.theme_forWeb,cols=NULL,rows=NULL,width=unit(75,'points'),height=unit(33,'points'))
        memoHead_forWeb = tableGrob('식사기록',theme=colhead.theme_forWeb,cols=NULL,rows=NULL,width=unit(345,'points'),height=unit(33,'points'))

        colhead.theme_forApp$core$bg_params$fill = '#F5F5F5'
        colhead.theme_forApp$core$fg_params$fontfamily='NotoSansCJKkrB'
        colhead.theme_forApp$core$fg_params$fontsize = 22
        
        plotHead_forApp = tableGrob(c(format(daysAZ[d],format=' %Y년 %m월 %d일 (%A)'),''),theme=colhead.theme_forApp,cols=NULL,rows=NULL,width=unit(428*0.75,'points'),height=unit(c(50,40),'points'))

        colhead.theme_forApp$core$fg_params$fontfamily='NotoSansCJKkrR'
        spikeHead_forApp = tableGrob('혈당 스파이크',theme=colhead.theme_forApp,cols=NULL,rows=NULL,width=unit(212*0.75,'points'),height=unit(50,'points'))

        colhead.theme_forApp$core$bg_params$fill=NA
        colhead.theme_forApp$core$fg_params$fontfamily='NotoSansCJKkrB'
        colhead.theme_forApp$core$fg_params$fontsize = 18

        memoHead_forApp = tableGrob('식사기록',theme=colhead.theme_forApp,cols=NULL,rows=NULL,width=unit(640*0.75,'points'),height=unit(50,'points'))

        if ( !is.null(memo[[d]]) && dim(memo[[d]])[1]!=0 ) {
            memo.tmp = memo[[d]]$max_memo
            for ( l in 1:length(memo.tmp) ) memo.tmp[l] = paste(strwrap(memo[[d]]$max_memo[l],width=50),collapse='\n')
            memo[[d]]$max_memo = memo.tmp
            memo.nrows = str_count(memo.tmp,'\n')+1
            memo[[d]]$time = format(memo[[d]]$dateandtime,format='%H시%M분',tz='GMT')
            memo.cols=rep('#122747',length(memo.tmp))
            memo.cols[!is.na(memo[[d]]$SpikeIdx)] = '#FF6600'

        } else {
            memo.nrows = 1
            memo.cols='#122747'
            memo[[d]] = data.frame(dateandtime='',max_memo='기록이 없습니다.',SpikeIdx=NA,time='')
        }

        tmpH.tmp = tabHtFt.W(memo.nrows)
        if ( sum(tmpH.tmp)<200 ) {
            tmpH.tmp[length(tmpH.tmp)] = tmpH.tmp[length(tmpH.tmp)]+(200-sum(tmpH.tmp))
        }

        ## for spike
        colmain.theme_forWeb$core$fg_params$fontsize = 28
        colmain.theme_forWeb$core$fg_params$col = '#122747'
        colmain.theme_forWeb$core$fg_params$x = 0.2

        spikeMain_forWeb = tableGrob(c(paste(spikeN[[d]],'회'),rep('',(length(memo.nrows)-1))),theme=colmain.theme_forWeb,cols=NULL,rows=NULL,width=unit(75,'points'),heights=unit(tmpH.tmp,'points'))

        colmain.theme_forApp$core$bg_params$fill = '#F5F5F5'
        colmain.theme_forApp$core$fg_params$fontfamily = 'NotoSansCJKkrB'
        colmain.theme_forApp$core$fg_params$fontsize = 28
        colmain.theme_forApp$core$fg_params$col = '#122747'
        colmain.theme_forApp$core$fg_params$x = 0.2

        spikeMain_forApp = tableGrob(paste(spikeN[[d]],'회'),theme=colmain.theme_forApp,cols=NULL,rows=NULL,width=unit(212*0.75,'points'),heights=unit(40,'points'))

        ## for memo
        colmain.theme_forWeb$core$fg_params$fontsize = 12
        colmain.theme_forWeb$core$fg_params$col=memo.cols
        colmain.theme_forWeb$core$fg_params$x = 0.02

        memoMain_forWeb = tableGrob(memo[[d]][,c('time','max_memo')],theme=colmain.theme_forWeb,cols=NULL,rows=NULL,width=unit(c(55,290),'points'),heights=unit(tmpH.tmp,'points'))

        colmain.theme_forApp$core$bg_params$fill=NA
        colmain.theme_forApp$core$fg_params$fontsize = 18
        colmain.theme_forApp$core$fg_params$x = 0.02
        colmain.theme_forApp$core$fg_params$col=memo.cols
        colmain.theme_forApp$core$fg_params$fontfamily = 'NotoSansCJKkrR'

        memoMain_forApp = tableGrob(memo[[d]][,c('time','max_memo')],theme=colmain.theme_forApp,cols=NULL,rows=NULL,width=unit(c(108,532)*0.75,'points'),heights=unit(tabHtFt.A(memo.nrows),'points'))

        ##
        if ( !is.null(plot[[d]]) ) {
            daily.tmp = gtable_combine(spikeMain_forWeb,nullMain,memoMain_forWeb,along=1)
            tmpH.W[[d]] = c(33,200,(sum(tmpH.tmp)-200),8)
            daily1.W[[d]] = grid.arrange(
                grobs=list(plotHead_forWeb,spikeHead_forWeb,memoHead_forWeb,plot[[d]],daily.tmp,nullMain),
                nrow=4, ncol=5, layout_matrix=rbind(c(1,NA,2,NA,3),c(4,NA,5,5,5),c(6,NA,5,5,5),NA),
                widths=unit(c(420,18,75,18,345),'points'),heights=unit(tmpH.W[[d]],'points')
            )
            tmpH.A[[d]] = c(50,40,5,285,10,50,sum(tabHtFt.A(memo.nrows)),15)
            daily1.A[[d]] = grid.arrange(
                grobs=list(plotHead_forApp,spikeHead_forApp,spikeMain_forApp,plot[[d]],memoHead_forApp,memoMain_forApp),
                nrow=8, ncol=2, layout_matrix=rbind(c(1,2),c(1,3),NA,4,NA,5,6,NA),
                widths=unit(c(428,212)*0.75,'points'),heights=unit(tmpH.A[[d]],'points')
            )
        }
    }

    
    ## for Web ==== ##
    daily2_forWeb = grid.arrange(
        grobs=daily1.W,
        nrow=length(daily1.W),ncol=1,widths=unit(876,'points'),heights=unit(unlist(lapply(tmpH.W,sum)),'points')
    )
    ##
    CairoPNG(filename=paste(reportIdx,'Web_DailyLog.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168,height=sum(28.5*3,36,27,sum(unlist(tmpH.W)))/0.75,units='px',dpi=96)
    LibreReport_DailyLog_forWeb = try(grid.arrange(
        grobs=list(pageTitle_forWeb,subTitle_forWeb,remark_forWeb,daily2_forWeb),
        nrow=5, ncol=1, layout_matrix=rbind(1,2,3,NA,4),widths=unit(876,'points'),heights=unit(c(28.5*2,28.5,36,27,sum(unlist(tmpH.W))),'points')),silent=T)
    dev.off()

    if ( all(class(LibreReport_DailyLog_forWeb)!='try-error') ) fileName.tmp = c(fileName.tmp,paste(reportIdx,'Web_DailyLog.png',sep='_'))

    ## for App ==== ##
    daily2_forApp = grid.arrange(
        grobs=daily1.A,
        nrow=length(daily1.A),ncol=1,widths=unit(512,'points'),heights=unit(unlist(lapply(tmpH.A,sum)),'points')
    )
    ##
    if ( F ) { # 임시 (230407, 임시 방편)
    CairoPNG(filename=paste(reportIdx,'App_DailyLog.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720,height=sum(c(c((67+72),14,67,14,100,48)*0.75,sum(unlist(tmpH.A))))/0.75,units='px',dpi=96)
    }
    LibreReport_DailyLog_forApp = try(grid.arrange(
        grobs=list(
            pageTitle_forApp,#1
            subTitle_forApp,#2
            remark_forApp,#3
            daily2_forApp),#4
        nrow=7, ncol=1, layout_matrix=rbind(1,NA,2,NA,3,NA,4),widths=unit(640*0.75,'points'),heights=unit(c(c((67+72),14,67,14,100,48)*0.75,sum(unlist(tmpH.A))),'points')),silent=T)
    if ( F ) { # 임시 (230407, 임시 방편)
    dev.off()

    if ( all(class(LibreReport_DailyLog_forApp)!='try-error') ) fileName.tmp = c(fileName.tmp,paste(reportIdx,'App_DailyLog.png',sep='_'))


	### output =============================================================================##
    out.Result = list(fileName.tmp=fileName.tmp)
    }
    ## 임시 (230407, 임시 방편 )
    out.Result = list(fileName.tmp=fileName.tmp,LibreReport_DailyLog_forApp=LibreReport_DailyLog_forApp, out.H.A=sum(c(c((67+72),14,67,14,100,48)*0.75,sum(unlist(tmpH.A)))))

    return(out.Result)

}

#############################################################

	tabHtFt.W = function(nline){
		return( 14*nline+5*(nline-1)+20 )
	}
	tabHtFt.A = function(nline){
		return( 21*nline+5*(nline-1)+20 )
	}