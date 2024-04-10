##########################################
## Libre_x_Diet_report
## ---------------------------------------
## input 
## . Target : DIET
## . StartDate : YYYY-MM-DD
## . EndDate : YYYY-MM-DD
## . inFileName : 리브레뷰에서 다운받은 혈당 데이터 파일명 (홍길동_glucose_2020-8-5.csv) 
## . method : 식후혈당분석방법 TargetValue IncValue Spike
## . path0 : 결과저장 directory
## ---------------------------------------
## ver ( 1.1 240329 )

Libre_x_Diet_report_multi = function( Target='DIET', StartDate, EndDate, inFileName, method, path0, memberKey, memberKeyId ) {
	
	strt.tmp = Sys.time() 
	errCode = c()
	outFileNames = c()
	### step0 =============================================================================##
	### Install Package

	getos = Sys.info()['sysname']
	if ( getos=='Windows' ) {
		my.libPaths = .libPaths()[1]
	#	my.libPaths = c(my.libPaths[2],my.libPaths[1])
		.libPaths(my.libPaths)
	}
	
	needto_packages = c('ggplot2','gridExtra','grid','gtable','readxl','jsonlite','Cairo','stringr','jsonlite','png','sysfonts','showtextdb','showtext','sqldf') #
	if ( getos=='Windows' ) {
		for ( i in 1:length(needto_packages) ) {
			if ( !needto_packages[i] %in% installed.packages(lib.loc=my.libPaths) ) {
				install.packages( needto_packages[i] , repos='http://cran.r-project.org')
			}
		}
	}

	for ( i in 1:length(needto_packages) ) {
		for ( j in 1:length(.libPaths()) ) {
			libPath.tmp = .libPaths()[j]
			libPath.err = try(library(needto_packages[i],lib.loc=libPath.tmp,character.only=T,quietly=T),silent=T)
			if ( class(libPath.err)!='try-error' ) {
				next
			}
		}
	}

	# font 설정 # 
	if ( getos=='Windows' ) {
		font_add('NotoSansCJKkrB','NOTOSANSKR-BLACK.otf') # window
		font_add('NotoSansCJKkrR','NOTOSANSKR-REGULAR.otf') # window
	} else {
		font_add('NotoSansCJKkrB','NotoSansCJKkr-Bold_TTF.ttf')
		font_add('NotoSansCJKkrR','NotoSansCJKkr-Regular_TTF.ttf')
	}
	showtext_auto()

	# style 설정 # 
	source('S_Libre_x_Diet_report.R')

	if ( Target=='DIET' ) {
		Target.text = '다이어트(일반인)'
		method = 'Spike'#ifelse(method=='IncValue','Spike',method)
		IncCut = 30
		PPG.L = 100; PPG.U = 130
		TIR.Goal = 90;
		TAR_lev2.Cut = 250; TAR_lev1.Cut = 180; TBR_lev1.Cut = 70; TBR_lev2.Cut = 54
		TAR_lev2.Goal = 0; TAR_lev1.Goal = 6; TBR_lev1.Goal=4; TBR_lev2.Goal = 0 
	}

	### step1 =============================================================================##
	### 데이터전처리

	if ( any(grepl('.csv',inFileName)) ) {
		
		## find data 
		num = 1 
		while( 1 ) {
			userInfo = try(read.table(paste('C:/Users/User/Documents/A_리브레/2_결과지알고리즘/9_혈당데이터/',inFileName[num],sep=''),col.names=1:2,flush=T,nrow=2,sep=',',fileEncoding='UTF-8'),silent=T)
			if ( class(userInfo)=='try-error' ) {

			}
			userName = paste(strsplit(userInfo[2,1],' ')[[1]][2],strsplit(userInfo[2,1],' ')[[1]][1],sep='')
			if ( memberKey == userName ) {
				break
			}
			num = num+1
		}

		## data preprocessing
		AGPdata = try(LibreCSV_preprocessing(inFileName1=paste('C:/Users/User/Documents/A_리브레/2_결과지알고리즘/9_혈당데이터/',inFileName[num],sep=''),FinalDate=EndDate,maxmod=2),silent=T)
		# 에러 확인용 ***************************************************************************
		if ( class(AGPdata)=='try-error' ) {
			errCode = c(errCode,'11900')
			break
		} else {
			errCode = c(errCode,AGPdata$errCode.sub)
			mod = AGPdata$mod
			CGMactive = AGPdata$CGMactive/96*100#(%) #todo 
	#		AGPdata = AGPdata$AGPdata[which(AGPdata$AGPdata$sub<=mod),]
			AGPdata = AGPdata$AGPdata[!is.na(AGPdata$AGPdata$sub),]
		}

	} else if ( any(grepl('.json',inFileName)) ) {

		## find data
		userDB = try(fromJSON(inFileName),silent=T)
		if ( class(userDB)=='try-error' ) {
			errCode = c(errCode,'01400')
			break
		}

		if ( (memberKeyId %in% userDB$email)==F ) {
			break # 리브레 없음
		}
		u = grep(memberKeyId,userDB$email)

		## data preprocessing
		AGPdata = try(LibreCrawling_preprocessing(data=userDB[u,],FinalDate=EndDate,maxmod=2),silent=T)
		# 에러 확인용 ***************************************************************************
		if ( class(AGPdata)=='try-error' ) {
			errCode = c(errCode,'11900')
			break
		} else {
			errCode = c(errCode,AGPdata$errCode.sub)
			mod = AGPdata$mod
			CGMactive = AGPdata$CGMactive/96*100#(%) 
#			AGPdata = AGPdata$AGPdata[which(AGPdata$AGPdata$sub<=mod),]
			AGPdata = AGPdata$AGPdata[!is.na(AGPdata$AGPdata$sub),]
		}

	}


	### step2 =============================================================================##
	### 다이어트용 혈당스파이크 찾기
	if ( method=='Spike' ) {
		spikeData = try(GlucoseSpike(daysAZ=unique(AGPdata$date),data=AGPdata,baseGlu=NA,IncCut=IncCut),silent=T)
		if ( class(spikeData)=='try-error' ) {
			errCode = c(errCode,'21900')
			break
		} else {
			errCode = c(errCode,spikeData$errCode.sub)
			AGPdata = spikeData$data
		}
	}


	### step3 =============================================================================##
	### 일일혈당프로필
	if ( method=='Spike' ) {
		## best day, worst day 선정
		## day별 spikeN, peak, tir 
		spikeStat = spikeData$stat
		spikeStat$date_event = as.Date(spikeStat$time_event)
		spikeStat = try(sqldf('select date_event, max(peak) as max_peak, count(peak) as count_peak from spikeStat group by date_event'),silent=T)
		if ( class(spikeStat)=='try-error' ) {
			errCode = c(errCode,'31900')
		}
		## 0 번 추가 ## 
		add.tmp = as.Date(setdiff(unique(spikeData$data$date),spikeStat$date_event),origin='1970-01-01')
		if ( length(add.tmp)>0 ) {
			spikeStat = rbind(spikeStat,data.frame(date_event=add.tmp,max_peak=NA,count_peak=0))
		}
		gluStat = data.frame(max_glu=tapply(AGPdata[!is.na(AGPdata$glucose),]$glucose,AGPdata[!is.na(AGPdata$glucose),]$date,max,na.rm=T))
		gluStat$date = as.Date(rownames(gluStat))
		spikeStat$max_glu = NA 
		spikeStat$max_glu = gluStat$max_glu[match(spikeStat$date_event,gluStat$date)]

		spikeStat = spikeStat[which(spikeStat$date_event %in% AGPdata[which(AGPdata$sub==1),]$date),]

		## worst day
		cond1.w = which(spikeStat$count_peak==max(spikeStat$count_peak))
		if ( all(spikeStat[cond1.w,]$count_peak==0) ) {
			cond2.w = (spikeStat[cond1.w,]$max_glu > TBR_lev1.Cut)
			cond3.w = cond1.w[cond2.w]
			worstday = spikeStat[cond3.w[which.max(spikeStat[cond3.w,]$max_glu)],]$date_event
		} else {
			cond2.w = (spikeStat[cond1.w,]$max_peak > TBR_lev1.Cut)
			cond3.w = cond1.w[cond2.w]
			worstday = spikeStat[cond3.w[which.max(spikeStat[cond3.w,]$max_peak)],]$date_event
		}

		## best day
		# CGM 활성도가 Q1수준 또는 50% 이상인 날 중에 best
		spikeStat$CGMactive = NA
		spikeStat$CGMactive = CGMactive[match(as.character(spikeStat$date_event),names(CGMactive))]
		CGMactive.cut = min(50,quantile(CGMactive,prob=0.25))
		spikeStat1 = spikeStat[which(spikeStat$CGMactive>=CGMactive.cut),]

		cond1.b = which(spikeStat1$count_peak==min(spikeStat1$count_peak))
		if ( all(spikeStat1[cond1.b,]$count_peak==0) ) {
			cond2.b = (spikeStat1[cond1.b,]$max_glu > TBR_lev1.Cut)
			cond3.b = cond1.b[cond2.b]
			bestday = spikeStat1[cond3.b[which.min(spikeStat1[cond3.b,]$max_glu)],]$date_event
		} else {
			cond2.b = (spikeStat1[cond1.b,]$max_peak > TBR_lev1.Cut)
			cond3.b = cond1.b[cond2.b]
			bestday = spikeStat1[cond3.b[which.min(spikeStat1[cond3.b,]$max_peak)],]$date_event
		}

	} else {
		bestday = NULL
		worstday = NULL
	}

	### 일일혈당프로필 시각화
	dailygp = try(dailyGP_onW(data=AGPdata[which(AGPdata$log==1 & AGPdata$sub==1),],bestday=bestday,worstday=worstday,TBR_lev1.Cut=TBR_lev1.Cut,TAR_lev1.Cut=TAR_lev1.Cut, method=method,spikeStat=spikeStat),silent=T)
	# 에러 확인용 ****************************************************************************
	if ( class(dailygp)=='try-error' ) {
		errCode = c(errCode,'32900')
		break
	}

	#### daliyGP graph - print out - ####################
	dailygp.subtitle = data.frame(x='일일 혈당 프로필')
	dailygp.anno = data.frame(x='혈당 관리가 가장 잘 된 날은 BEST DAY, 혈당 관리를 가장 못 한 날은 WORST DAY 로 표기됩니다.')
	dailygp.anno_forApp = data.frame(x='혈당 관리가 가장 잘 된 날은 BEST DAY,\n혈당 관리를 가장 못 한 날은 WORST DAY 로 표기됩니다.')

	## forWeb ==== ## 
	LibreReport_DailyGP_forWeb = try(grid.arrange(
		grobs=list(
			tableGrob(dailygp.subtitle,theme=subtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#1
			tableGrob(dailygp.anno,theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(21.5,'points')),#2
			dailygp$out.Plot[[1]][[1]],#3
			dailygp$out.Plot[[1]][[2]]#4
		),
		nrow=7, ncol=1, layout_matrix=rbind(c(1),NA,c(2),NA,c(3),c(4),NA),widths=unit(c(876),'points'),heights=unit(c(28.5,6,21.5,3,120,120,4),'points')
	),silent=T)
	dev.off()
	dailygp.h = LibreReport_DailyGP_forWeb$heights

	if ( class(LibreReport_DailyGP_forWeb)[1]!='try-error' ) {
	} else {
		errCode = c(errCode,'33801')
	}

	if ( F ) {#Target=='DIET' ) { #temp (20230407, 모바일개발 전 임시방편)

		## forApp ==== ## 
		CairoPNG(filename=paste(reportIdx,'App_DailyGP.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(67,14,72,48,dailygp$out.Plot[[2]][[2]])), units='px',dpi=96)
	
		LibreReport_DailyGP_forApp = try(grid.arrange(grobs=list(
			tableGrob(data.frame(x='BEST / WORST DAY'),theme=subtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(67*0.75,'points')),#1
			tableGrob(dailygp.anno_forApp,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(72*0.75,'points')),#2
			dailygp$out.Plot[[2]][[1]]), #3
			nrow=5, ncol=1, layout_matrix=rbind(c(1),NA,c(2),NA,c(3)),
			widths=unit(640*0.75,'points'),heights=unit(c(67,14,72,48,dailygp$out.Plot[[2]][[2]])*0.75,'points')),silent=T) 

		dev.off()

		if ( class(LibreReport_DailyGP_forApp)[1]!='try-error' ) {
#			outFileNames = c(outFileNames,paste(reportIdx,'App_DailyGP.png',sep='_'))
#		} else {
			errCode = c(errCode,'34801')
		}
	}


	### step4 =============================================================================##
	### 식후혈당분석
	# dietplan.alltime = try(GlucosePattern_clustering_DIET(daysAZ=unique(AGPdata$date),data=AGPdata,mealtime=NA,spikeLog=spikeData$stat,method=method,PPG.L=PPG.L,PPG.U=PPG.U,Target=Target,IncCut=IncCut),silent=T)
	# # 에러 확인용 ****************************************************************************
	# if ( class(dietplan.alltime)=='try-error' ) {
	# 	errCode = c(errCode,'41900')
	# } else {
	# 	errCode = c(errCode,dietplan.alltime$errCode.sub)
	# }

	dietplan.alltime1 = try(GlucosePattern_clustering_DIET(daysAZ=unique(AGPdata[which(AGPdata$sub==1),]$date),data=AGPdata[which(AGPdata$sub==1),],mealtime=NA,spikeLog=spikeData$stat,baseGlu=spikeData$baseGlu,method=method,PPG.L=PPG.L,PPG.U=PPG.U,Target=Target,IncCut=IncCut),silent=T)
	# 에러 확인용 alltime1 ****************************************************************************
	if ( class(dietplan.alltime1)=='try-error' ) {
		errCode = c(errCode,'41900')
	} else {
		errCode = c(errCode,dietplan.alltime1$errCode.sub)
	}
	if ( mod>1 ) {

		dietplan.alltime2 = try(GlucosePattern_clustering_DIET(daysAZ=unique(AGPdata[which(AGPdata$sub!=1),]$date),data=AGPdata[which(AGPdata$sub!=1),],mealtime=NA,spikeLog=spikeData$stat,baseGlu=spikeData$baseGlu,method=method,PPG.L=PPG.L,PPG.U=PPG.U,Target=Target,IncCut=IncCut),silent=T)
		# 에러 확인용 ****************************************************************************
		if ( class(dietplan.alltime2)=='try-error' ) {
			errCode = c(errCode,'41900')
		} else {
			errCode = c(errCode,dietplan.alltime2$errCode.sub)
			dietplan.tmp = rbind(cbind(dietplan.alltime1$out_value5,sub=1),cbind(dietplan.alltime2$out_value5,sub=2))
		}
	} else {
		dietplan.tmp = cbind(dietplan.alltime1$out_value5,sub=1)
	}

	#### dietplan table - print out - ####################
	dietplan.subtitle = data.frame('주의가 필요한 식사 리스트(추정)')
	dietplan.anno = data.frame(x=paste('식후 혈당의 최고값이 평균 혈당 대비 ', IncCut,' mg/dL 이상으로 상승된 식사 기록을 보여줍니다.', sep=''))

	subtitle.theme_forWeb$core$bg_params$fill = '#ff6600'

	espike_null_msg = function(time) { paste('분석기간 ', c('오전','정오','저녁')[time],' 식사 중\n혈당을 높인 식사 기록이 없었어요 ^_^',sep='') }

#	dietplanT_spike.all = f_dietPlanTableBody(invalueLs=list(dietplan.alltime1$out_value5),msg='해당되는 식사기록이 없습니다.',all=T)
	dietplanT_spike.all = f_dietPlanTableBody(invalueLs=list(dietplan.tmp),msg='해당되는 식사기록이 없습니다.',all=T,hist=T)

	## forApp ==== ##
	if ( F ) { #temp
    if ( !is.null(dietplanT_spike.all$nrows.A) ) {

        dietplanT_spike.head.Af = tableGrob(data.frame('순위','식사기록','혈당상승폭','mg/dL'),theme=dietPlanTableHead.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(64,384,128,64)*0.75,'points'),heights=unit(71*0.75,'points'))

		Spike.all = try(grid.arrange(
            grobs=list(dietplanT_spike.head.Af,
			dietplanT_spike.all$tab.Af),
			nrow=2, ncol=1, layout_matrix=rbind(1,2),
			widths=unit(640*0.75,'points'),heights=unit(c(71*0.75,sum(tabHtFt2.A(dietplanT_spike.all$nrows.A))),'points')),silent=T)

		tmpH.all = sum(c(71*0.75,sum(tabHtFt2.A(dietplanT_spike.all$nrows.A))))

	} else {

		dietplanT_spike.head.Af = tableGrob(espike_null_msg(time=0),theme=dietPlanTableHead.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(100*0.75,'points'))

		Spike.all = try(grid.arrange(
			grobs=list(dietplanT_spike.head.Af),#,
			nrow=1, ncol=1, layout_matrix=rbind(1),
			widths=unit(640*0.75,'points'),heights=unit(c(100*0.75),'points')),silent=T)
		tmpH.all = sum(c(100)*0.75) # 0-> 

	}

		## for App ==== ##
		CairoPNG(filename=paste(reportIdx,'App_DietPlanSpike.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720,height=sum(c(c(110,14,67,14,72,48),tmpH.all/0.75)), units='px', dpi=96)

		LibreReport_DietPlanSpike_forApp = try(grid.arrange(
			grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(c(67,43)*0.75,'points')),#1
			tableGrob(dietplan.subtitle5,theme=subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(67*0.75,'points')),#2
			tableGrob(dietplan.tabtitle5.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(72*0.75,'points')),#3
			## 통합 ##
			Spike.all),#4
			nrow=7, ncol=1, layout_matrix=rbind(1,NA,2,NA,3,NA,4),
			widths=unit(640*0.75,'points'),heights=unit(c(c(110,14,67,14,72,48)*0.75,tmpH.all),'points')),silent=T)
		dev.off() 

		if ( all(class(LibreReport_DietPlanSpike_forApp)!='try-error') )  outFileNames = c(outFileNames,paste(reportIdx,'App_DietPlanSpike.png',sep='_'))
	}

	## for Web ==== ##
	if ( !is.null(dietplanT_spike.all$nrows.W) ) {
				
		dietPlanTableHead.theme_forWeb = ttheme_minimal( 
			base_family=c('NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrR'),  
			core = list(bg_params=list(fill='#EAF0FF',col=NA),
					fg_params=list(hjust=0,x=c(0.02,0.02,0.02,0,0,0),vjust=0.5,col='#113694',fontsize=c(13,13,13,13,13,10),fontface='plain'))
		)

		tmpH = sum(tabHtFt.W(dietplanT_spike.all$nrows.W))

		dietplanT_spike.head.Wf = tableGrob(data.frame('순위','일시','식사기록','최고혈당','(상승폭)','mg/dL'),theme=dietPlanTableHead.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(54,108,416,60,60,36),'points'),heights=unit(28,'points'))

		LibreReport_DietPlanSpike_forWeb = try(grid.arrange(
			grobs=list(
				tableGrob(dietplan.subtitle,theme=subtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#1
				tableGrob(dietplan.anno,theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#2
				dietplanT_spike.head.Wf, #3
				dietplanT_spike.all$tab.Wf #4
			), nrow=5, ncol=6, layout_matrix=rbind(c(1,1,1,1,1,1),c(2,2,2,2,2,2),NA,c(3,3,3,3,3,NA),c(4,4,4,4,4,NA)),
			widths=unit(c(280,18,280,108,48,142),'points'),heights=unit(c(28.5,21.5,29,28,tmpH),'points')
		),silent=T)
		dev.off()
		dietplan.h = LibreReport_DietPlanSpike_forWeb$height
		
		if ( all(class(LibreReport_DietPlanSpike_forWeb)!='try-error') ) {
#			outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanSpike.png',sep='_'))
#		} else {
			errCode = c(errCode,'42801')
		}

	} else {

		LibreReport_DietPlanSpike_forWeb = try(grid.arrange(
			grobs=list(
				tableGrob(dietplan.subtitle,theme=subtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#1
				tableGrob(dietplan.anno,theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#2
				textGrob('분석기간 중 혈당을 높인 식사기록이 없습니다.', gp=gpar(fontsize=12,fontface='plain',col='#333333',fontfamily='NotoSansCJKkrB'),hjust=0.5,vjust=0.5) #3
			), nrow=4, ncol=5, layout_matrix=rbind(c(1,1,1,1,1),c(2,2,2,2,2),NA,c(3,3,3,3,3)),
			widths=unit(c(280,18,280,18,280),'points'),heights=unit(c(28.5,21.5,30,30),'points')
		),silent=T)
		dev.off()
		dietplan.h = LibreReport_DietPlanSpike_forWeb$height
		
		if ( all(class(LibreReport_DietPlanSpike_forWeb)!='try-error') ) {
#			outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanSpike.png',sep='_'))
#		} else {
			errCode = c(errCode,'42801')

		}
	}


	stat.this = try(statisticGlucose(data=AGPdata[which(AGPdata$log==1 & AGPdata$sub==1),],stat=T,tirstat=F,TIR.Goal=TIR.Goal,TBR_lev2.Cut=TBR_lev2.Cut,TBR_lev2.Goal=TBR_lev2.Goal,TBR_lev1.Cut=TBR_lev1.Cut,TBR_lev1.Goal=TBR_lev1.Goal,TAR_lev1.Cut=TAR_lev1.Cut,TAR_lev1.Goal=TAR_lev1.Goal,TAR_lev2.Cut=TAR_lev2.Cut,TAR_lev2.Goal=TAR_lev2.Goal),silent=T)
	stat.this$spikeN = ifelse(dietplan.tmp[which(dietplan.tmp$sub==1),]$zNA>=30,1,0)+ifelse(dietplan.tmp[which(dietplan.tmp$sub==1),]$zNA>=60,1,0)+ifelse(dietplan.tmp[which(dietplan.tmp$sub==1),]$zNA>=100,1,0)
	spikeN.this = table(stat.this$spikeN)
	spikeN.this[1] = ifelse(is.na(spikeN.this['1']),0,spikeN.this['1'])
	spikeN.this[2] = ifelse(is.na(spikeN.this['2']),0,spikeN.this['2'])
	spikeN.this[3] = ifelse(is.na(spikeN.this['3']),0,spikeN.this['3'])
	if ( class(stat.this)!='try-error' ) {
		if ( mod>1 ) {
			stat.last = statisticGlucose(data=AGPdata[which(AGPdata$log==1 & AGPdata$sub==2),],stat=T,tirstat=F,TIR.Goal=TIR.Goal,TBR_lev2.Cut=TBR_lev2.Cut,TBR_lev2.Goal=TBR_lev2.Goal,TBR_lev1.Cut=TBR_lev1.Cut,TBR_lev1.Goal=TBR_lev1.Goal,TAR_lev1.Cut=TAR_lev1.Cut,TAR_lev1.Goal=TAR_lev1.Goal,TAR_lev2.Cut=TAR_lev2.Cut,TAR_lev2.Goal=TAR_lev2.Goal)
			if ( class(stat.last)!='try-error' ) {
				stat.last$spikeN = ifelse(dietplan.tmp[which(dietplan.tmp$sub==2),]$zNA>=30,1,0)+ifelse(dietplan.tmp[which(dietplan.tmp$sub==2),]$zNA>=60,1,0)+ifelse(dietplan.tmp[which(dietplan.tmp$sub==2),]$zNA>=100,1,0)
				spikeN.last = table(stat.last$spikeN)
				spikeN.last[1] = ifelse(is.na(spikeN.last[1]),0,spikeN.last[1])
				spikeN.last[2] = ifelse(is.na(spikeN.last[2]),0,spikeN.last[2])
				spikeN.last[3] = ifelse(is.na(spikeN.last[3]),0,spikeN.last[3])
			}
		}
	}


	stat.r1 = paste('이번 분석기간: ',paste(stat.this$periodDate,collapse=' ~ '),' (',stat.this$periodDays,'일)',sep='')
	stat.r2 = ifelse(mod==1,'',paste('지난 분석기간: ',paste(stat.last$periodDate,collapse=' ~ '),' (',stat.last$periodDays,'일)',sep=''))

	stat.r3 = c('> 평균 혈당', paste(round(stat.this$avgGlu,1),'mg/dL',sep=''),'')
	stat.r4 = c('',ifelse(mod==1,'',paste(round(stat.last$avgGlu,1),'mg/dL',sep='')),'')
	stat.r5 = c('> 예상되는 당화혈색소',paste(round(stat.this$gmiGlu,1),'%',sep=''),'')
	stat.r6 = c('',ifelse(mod==1,'',paste(round(stat.last$gmiGlu,1),'%',sep='')),'')
	stat.r7 = c('> 혈당스파이크 발생 횟수',paste(length(stat.this$spikeN),'회',sep=''),paste(c('+30▲:','+60▲:','+100▲:'),paste(spikeN.this,'회',sep=''),collapse=' /'))
	stat.r8 = c('',ifelse(mod==1,'',paste(length(stat.last$spikeN),'회',sep='')),ifelse(mod==1,'',paste(c('+30▲:','+60▲:','+100▲:'),paste(spikeN.last,'회',sep=''),collapse=' /')))

	statr1.theme_forWeb = ttheme_minimal(
		base_family=c('NotoSansCJKkrR','NotoSansCJKkrB','NotoSansCJKkrR'),
		core=list(bg_params=list(fill=rep('#ffffff',3),col=NA),
			fg_params=list(hjust=0,x=0.01,col='#000000',fontsize=c(12,12,11),fontface='plain'))
	)
	statr2.theme_forWeb = statr1.theme_forWeb
	statr2.theme_forWeb$core$fg_params$fontfamily = 'NotoSansCJKkrR'
	statr2.theme_forWeb$core$fg_params$fontsize = 11
	statr2.theme_forWeb$core$bg_params$fill[2:3]=ifelse(mod>1,'#e3e3e3','#ffffff')

	tabtitle2.theme_forWeb = tabtitle.theme_forWeb
	tabtitle2.theme_forWeb$core$bg_params$fill = ifelse(mod>1,'#e3e3e3',tabtitle2.theme_forWeb$core$bg_params$fill)

if ( mod==2 ) {
	LibreReport_Stat_forWeb = try(grid.arrange(
		grobs=list(
			tableGrob(stat.r1,theme=tabtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(24,'points')),
			tableGrob(stat.r2,theme=tabtitle2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(24,'points')),
			
			tableGrob(t(stat.r3),theme=statr1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(24,'points')),
			tableGrob(t(stat.r4),theme=statr2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(24,'points')),
			tableGrob(t(stat.r5),theme=statr1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(24,'points')),
			tableGrob(t(stat.r6),theme=statr2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(24,'points')),
			tableGrob(t(stat.r7),theme=statr1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(24,'points')),
			tableGrob(t(stat.r8),theme=statr2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(24,'points'))
		), nrow=9, ncol=1,layout_matrix=rbind(1,2,NA,3,4,5,6,7,8),widths=unit(876,'points'),heights=unit(rep(24,9),'points')
	),silent=T)
	stat.h = LibreReport_Stat_forWeb$height
} else if ( mod==1 ) {
   	LibreReport_Stat_forWeb = try(grid.arrange(
		grobs=list(
			tableGrob(stat.r1,theme=tabtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(24,'points')),
			tableGrob(stat.r2,theme=tabtitle2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(4,'points')),
			
			tableGrob(t(stat.r3),theme=statr1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(24,'points')),
			tableGrob(t(stat.r4),theme=statr2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(4,'points')),
			tableGrob(t(stat.r5),theme=statr1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(24,'points')),
			tableGrob(t(stat.r6),theme=statr2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(4,'points')),
			tableGrob(t(stat.r7),theme=statr1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(24,'points')),
			tableGrob(t(stat.r8),theme=statr2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(146,73,657),'points'),heights=unit(4,'points'))
		), nrow=9, ncol=1,layout_matrix=rbind(1,2,NA,3,4,5,6,7,8),widths=unit(876,'points'),heights=unit(c(24,4,24,24,4,24,4,24,4),'points')
	),silent=T)
	stat.h = LibreReport_Stat_forWeb$height 
}


	#### LibreReport - overall - print out - ####################
if ( F ) {
	setwd(path0)
	createdtime = as.integer(Sys.time(),format='%Y-%m-%d_%H:%M:%S.%OS')
	reportIdx = paste(memberKey,createdtime,sep='_')
	report.h = c(28.5,30,sum(dailygp.h),30,sum(dietplan.h))


	CairoPNG(filename=paste(reportIdx,'Web_DietReport.png',sep='_'), family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=sum(report.h)/0.75, units='px', dpi=96)

	LibreReport_forWeb = try(grid.arrange(
		grobs=list(
			tableGrob(data.frame(paste(memberKey,'님의 혈당분석리포트')),theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#1
			LibreReport_DailyGP_forWeb,#2
			LibreReport_DietPlanSpike_forWeb#3			
		),nrow=5,ncol=1,layout_matrix=rbind(c(1),NA,c(2),NA,c(3)),widths=unit(876,'points'),heights=unit(report.h,'points')
	),silent=T)
	dev.off()
}

	setwd(path0)
	createdtime = as.integer(Sys.time(),format='%Y-%m-%d_%H:%M:%S.%OS')
	reportIdx = paste(memberKey,createdtime,sep='_')
    report.h = c(28.5,10,sum(stat.h),30,sum(dailygp.h),30,sum(dietplan.h))

	CairoPNG(filename=paste(reportIdx,'Web_DietReport.png',sep='_'), family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=sum(report.h)/0.75, units='px', dpi=96)

	LibreReport_forWeb = try(grid.arrange(
		grobs=list(
			tableGrob(data.frame(paste(memberKey,'님의 혈당분석리포트')),theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#1
			LibreReport_Stat_forWeb,#2
			LibreReport_DailyGP_forWeb,#3
			LibreReport_DietPlanSpike_forWeb#4			
		),nrow=7,ncol=1,layout_matrix=rbind(c(1),NA,c(2),NA,c(3),NA,c(4)),widths=unit(876,'points'),heights=unit(report.h,'points')
	),silent=T)
	dev.off()

	if ( class(LibreReport_forWeb)[1]!='try-error' ) {
		outFileNames = c(outFileNames,paste(reportIdx,'Web_DietReport.png',sep='_'))
	} else {
		errCode = c(errCode,'99801')
	}

}