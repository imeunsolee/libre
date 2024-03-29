##########################################
## Libre_x_DW_report
## ---------------------------------------
## input 
## . Target : 당뇨유형 T2DM T1DM T1GDM T2GDM GDM HighRiskDM preDM DIET
## . FinalDate : YYYY-MM-DD
## . inFileName : 리브레뷰에서 다운받은 혈당 데이터 파일명 (홍길동_glucose_2020-8-5.csv) 
## . method : 식후혈당분석방법 TargetValue IncValue Spike
## ---------------------------------------
## ver ( 230407 )

Libre_x_DW_report_onW = function( Target, FinalDate, inFileName, method, path0, memberKey ){
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
	source('S_reportStyle_onW.R')

	### 기본값 세팅 
	IncCut = 60 # 기본 식전대비 식후 최고 상승폭 제한값 
	if(Target == 'T1DM') {
		Target.text = '1형 당뇨'		
		PPG.L = 100; PPG.U = 180
		TIR.Goal = 70; 
		TAR_lev2.Cut = 250; TAR_lev1.Cut = 180; TBR_lev1.Cut = 70; TBR_lev2.Cut = 54
		TAR_lev2.Goal = 5; TAR_lev1.Goal = 25; TBR_lev1.Goal = 4; TBR_lev2.Goal = 1	

	} else if(Target == 'T2DM') {
		Target.text = '2형 당뇨'
		PPG.L = 100; PPG.U = 180
		TIR.Goal = 70; 
		TAR_lev2.Cut = 250; TAR_lev1.Cut = 180; TBR_lev1.Cut = 70; TBR_lev2.Cut = 54
		TAR_lev2.Goal = 5; TAR_lev1.Goal = 25; TBR_lev1.Goal = 4; TBR_lev2.Goal = 1	

	} else if(Target == 'T1GDM') {
		Target.text = '1형 당뇨 임산부'
		PPG.L = 100; PPG.U = 120
		TIR.Goal = 70; 
		TAR_lev2.Cut = NA; TAR_lev1.Cut = 140; TBR_lev1.Cut = 63; TBR_lev2.Cut = 54
		TAR_lev2.Goal = NA; TAR_lev1.Goal = 25; TBR_lev1.Goal = 4; TBR_lev2.Goal = 1	

	} else if(Target == 'T2GDM') {
		Target.text = '2형 당뇨 임산부'
		PPG.L = 100; PPG.U = 120
		TIR.Goal = 85; 
		TAR_lev2.Cut = NA; TAR_lev1.Cut = 140; TBR_lev1.Cut = 63; TBR_lev2.Cut = 54
		TAR_lev2.Goal = NA; TAR_lev1.Goal = 10; TBR_lev1.Goal = 4; TBR_lev2.Goal = 0	

	} else if(Target == 'GDM') {
		Target.text = '임신성 당뇨'
		PPG.L = 100; PPG.U = 120
		TIR.Goal = 85; 
		TAR_lev2.Cut = NA; TAR_lev1.Cut = 140; TBR_lev1.Cut = 63; TBR_lev2.Cut = 54
		TAR_lev2.Goal = NA; TAR_lev1.Goal = 10; TBR_lev1.Goal = 4; TBR_lev2.Goal = 0	

	} else if(Target == 'HighRiskDM') {
		Target.text = '당뇨 고위험군'
		PPG.L = 100; PPG.U = 180
		TIR.Goal = 50; 
		TAR_lev2.Cut = 250; TAR_lev1.Cut = 180; TBR_lev1.Cut = 70; TBR_lev2.Cut = NA
		TAR_lev2.Goal = 10; TAR_lev1.Goal = 50; TBR_lev1.Goal = 1; TBR_lev2.Goal = NA	

	} else if(Target == 'preDM') {
		Target.text = '당뇨전단계'
		PPG.L = 100; PPG.U = 140
		TIR.Goal = 90; 
		TAR_lev2.Cut = 250; TAR_lev1.Cut = 180; TBR_lev1.Cut = 70; TBR_lev2.Cut = 54
		TAR_lev2.Goal = 0; TAR_lev1.Goal = 6; TBR_lev1.Goal = 4; TBR_lev2.Goal = 0	

	} else if(Target == 'DIET') { # todo :  tir 등(현재 당뇨전단계값으로 임의지정함)
		Target.text = '다이어트(일반인)'
		method = 'Spike'#ifelse(method=='IncValue','Spike',method)
		IncCut = 30
		PPG.L = 100; PPG.U = 130
		TIR.Goal = 90;
		TAR_lev2.Cut = 250; TAR_lev1.Cut = 180; TBR_lev1.Cut = 70; TBR_lev2.Cut = 54
		TAR_lev2.Goal = 0; TAR_lev1.Goal = 6; TBR_lev1.Goal=4; TBR_lev2.Goal = 0 

	} else {
		Target.text = Target
		PPG.L = 100; PPG.U = 180
		TIR.Goal = 70; 
		TAR_lev2.Cut = 250; TAR_lev1.Cut = 180; TBR_lev1.Cut = 70; TBR_lev2.Cut = 54
		TAR_lev2.Goal = 5; TAR_lev1.Goal = 25; TBR_lev1.Goal = 4; TBR_lev2.Goal = 1	
		
	}


	### read Source
	userInfo = try(read.table(inFileName,col.names=1:2,flush=T,nrow=2,sep=',',fileEncoding='UTF-8'),silent=T)
	if ( class(userInfo)=='try-error' ) {
		errCode = c(errCode,'01100')
		break
	}
	userInfo = c(as.character(userInfo[2,1]),as.character(userInfo[2,2]))

	bad_emoji = readPNG('bad_emoji.png')
	good_emoji = readPNG('good_emoji.png')
	
	### step1 =============================================================================##
	### Build Dataset
	AGPdata = try(LibreData_transformation(inFileName,FinalDate),silent=T)
	# 에러 확인용 ***************************************************************************
	if(class(AGPdata)=='try-error') {
#		cat('[에러] 알 수 없는 이유로 리브레 데이터 변환이 중지되었습니다.\n')
		errCode = c(errCode,'10900')
		break
	} else {
		errCode = c(errCode,AGPdata$errCode.sub)
		mod = 1
		AGPdata = AGPdata$AGPdata[which(AGPdata$AGPdata$sub<=mod),]
	}


	### step2 =============================================================================##
	### analysis Glucose
	page1.tmp = try(create_AGPreportItem_onW(data=AGPdata, Target=Target, Target.text=Target.text,
 	   TIR.Goal=TIR.Goal,TBR_lev2.Cut=TBR_lev2.Cut,TBR_lev2.Goal=TBR_lev2.Goal,TBR_lev1.Cut=TBR_lev1.Cut,TBR_lev1.Goal=TBR_lev1.Goal,TAR_lev1.Cut=TAR_lev1.Cut,TAR_lev1.Goal=TAR_lev1.Goal,TAR_lev2.Cut=TAR_lev2.Cut,TAR_lev2.Goal=TAR_lev2.Goal), silent=T)
	# 에러 확인용 ****************************************************************************
	if(class(page1.tmp)=='try-error') {
#		cat('[에러] 알 수 없는 이유로 리브레 연속혈당 분석이 종료되었습니다.\n')
		errCode = c(errCode,'20900')
		break
	}

	## summaryValue ##
	summaryValue = data.frame(meanGlu=NA, GMI=NA, cvGlu=NA)
	summaryValue$meanGlu = as.numeric(as.vector(gsub(' mg/dL','',page1.tmp$out.Tab_11_source3[3,2])))
	summaryValue$GMI = as.numeric(as.vector(gsub('%','',page1.tmp$out.Tab_11_source3[4,2])))
	summaryValue$cvGlu = as.numeric(as.vector(gsub('%','',page1.tmp$out.Tab_11_source5[1,2])))

	tirValueSrc = page1.tmp$out.Plot_12_source
	tirValueSrc$ynum = as.numeric(as.vector(gsub('%','',tirValueSrc$ytext)))

    summaryValue$tirValue[[1]] = unbox(data.frame(TBR2=NA,TBR1=NA,TIR=NA,TAR1=NA,TAR2=NA))
    summaryValue$tirValue[[1]]$TBR2 = ifelse(any(grepl('TBR2',tirValueSrc$gr)),tirValueSrc$ynum[tirValueSrc$gr=='TBR2'],NA)
    summaryValue$tirValue[[1]]$TBR1 = ifelse(any(grepl('TBR1',tirValueSrc$gr)),tirValueSrc$ynum[tirValueSrc$gr=='TBR1'],NA)
    summaryValue$tirValue[[1]]$TIR = ifelse(any(grepl('TIR',tirValueSrc$gr)),tirValueSrc$ynum[tirValueSrc$gr=='TIR'],NA)
    summaryValue$tirValue[[1]]$TAR1 = ifelse(any(grepl('TAR1',tirValueSrc$gr)),tirValueSrc$ynum[tirValueSrc$gr=='TAR1'],NA)
    summaryValue$tirValue[[1]]$TAR2 = ifelse(any(grepl('TAR2',tirValueSrc$gr)),tirValueSrc$ynum[tirValueSrc$gr=='TAR2'],NA)
	rm(tirValueSrc)


	## 1page SubTitle +++++++++++++
	page1.subtitle_11 = data.frame(x='혈당 통계 및 목표값') 
	page1.subtitle_12 = data.frame(x='범위 내 시간') 
	page1.subtitle_21 = data.frame(x='24시간 연속 혈당 프로필(AGP)')  
	page1.subtitle_31 = data.frame(x='일일 혈당 프로필')


	## 1page Remark +++++++++++++
	page1.remark_21_forApp = paste(strwrap(data.frame(x='AGP는 보고서 기간의 혈당 값을 요약한 것으로, 중앙값(50%) 및 기타 백분위수가 하루에 발생한 것처럼 함께 표시됩니다.'),width=63),collapse='\n')
	page1.remark_21_forWeb = data.frame(x='AGP는 보고서 기간의 혈당 값을 요약한 것으로, 중앙값(50%) 및 기타 백분위수가 하루에 발생한 것처럼 함께 표시됩니다.')
	page1.remark_22 = data.frame(c1='■',c2='25~75백분위수 면적은 일상적인 생활패턴에 의해 나타납니다.')
	page1.remark_23 = data.frame(c1='■',c2='5~95백분위수 면적은 어느 특별한 이벤트에 의해 나타납니다.')
	
	if ( Target=='DIET' ) {
		page1.remark_31 = data.frame(x='혈당 관리가 가장 잘 된 날은 BEST DAY, 혈당 관리를 가장 못 한 날은 WORST DAY 로 표기됩니다.')
		page1.remark_31_forApp = data.frame(x='혈당 관리가 가장 잘 된 날은 BEST DAY,\n혈당 관리를 가장 못 한 날은 WORST DAY 로 표기됩니다.')
	} else {
		page1.remark_31 = data.frame(x='각 일일 프로필은 자정에서 자정까지의 기간을 나타내며 왼쪽 상단 모서리에 날짜가 표시됩니다.')
	}

	## theme
	remark025.theme_forWeb = remark.theme_forWeb
	remark025.theme_forWeb$core$fg_params$x = c(0.3,0)
	remark025.theme_forWeb$core$fg_params$col = c('#a2b5d4','#767e89')
	
	remark005.theme_forWeb = remark.theme_forWeb
	remark005.theme_forWeb$core$fg_params$x = c(0.3,0)
	remark005.theme_forWeb$core$fg_params$col = c('#dfe3ed','#767e89')
	
	remark025.theme_forApp = remark.theme_forApp
	remark025.theme_forApp$core$fg_params$x = c(0.03,0)
	remark025.theme_forApp$core$fg_params$col = c('#a2b5d4','#767e89')

	remark005.theme_forApp = remark.theme_forApp
	remark005.theme_forApp$core$fg_params$x = c(0.03,0)
	remark005.theme_forApp$core$fg_params$col = c('#dfe3ed','#767e89')

	## 1page. Table format +++++++++++++ 

	page1.Tab_111a_forApp = tableGrob(page1.tmp$out.Tab_11_source1a,theme=page1.tmp$out.Tab_11_source1a.theme,cols=NULL,rows=NULL,
		widths=unit(c(341,171),'points'),heights=unit(c(7.5,38.4,38.4,7.5),'points'))
	page1.Tab_111a_forApp.f = gtable_add_grob(page1.Tab_111a_forApp,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=4,b=4,l=1,r=2)

	page1.Tab_11_source1a.theme_forWeb = page1.tmp$out.Tab_11_source1a.theme
	page1.Tab_11_source1a.theme_forWeb$core$fg_params$fontsize = c(1,12,12,1)
	page1.Tab_111a_forWeb = tableGrob(page1.tmp$out.Tab_11_source1a,theme=page1.Tab_11_source1a.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(300,183),'points'),heights=unit(c(2,28,28,2),'points'))
	page1.Tab_111a_forWeb.f = gtable_add_grob(page1.Tab_111a_forWeb,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=4,b=4,l=1,r=2)

	page1.Tab_111b_forApp = tableGrob(page1.tmp$out.Tab_11_source1b,theme=page1.tmp$out.Tab_11_source1b.theme,cols=NULL,rows=NULL,
		widths=unit(512,'points'),heights=unit(45.5,'points'))

	page1.Tab_11_source1b.theme_forWeb = page1.tmp$out.Tab_11_source1b.theme
	page1.Tab_11_source1b.theme_forWeb$core$fg_params$fontsize = 12
	page1.Tab_111b_forWeb = tableGrob(page1.tmp$out.Tab_11_source1b,theme=page1.Tab_11_source1b.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(483,'points'),heights=unit(28.5,'points'))

	page1.Tab_111c_forApp = tableGrob(page1.tmp$out.Tab_11_source1c,theme=page1.tmp$out.Tab_11_source1c.theme,cols=NULL,rows=NULL,
		widths=unit(c(256,256),'points'),heights=unit(c(39.5,rep(28.5,5)),'points'))
	page1.Tab_111c_forApp.f = gtable_add_grob(page1.Tab_111c_forApp,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=1,b=1,l=1,r=2)
	page1.Tab_111c_forApp.f = gtable_add_grob(page1.Tab_111c_forApp.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=6,b=6,l=1,r=2) 

	page1.Tab_11_source1c.theme_forWeb = page1.tmp$out.Tab_11_source1c.theme
	page1.Tab_11_source1c.theme_forWeb$core$fg_params$fontsize = rep(12,6)
	page1.Tab_111c_forWeb = tableGrob(page1.tmp$out.Tab_11_source1c,theme=page1.Tab_11_source1c.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(300,183),'points'),heights=unit(c(21,rep(19.2,5)),'points'))
	page1.Tab_111c_forWeb.f = gtable_add_grob(page1.Tab_111c_forWeb,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=1,b=1,l=1,r=2)
	page1.Tab_111c_forWeb.f = gtable_add_grob(page1.Tab_111c_forWeb.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc'),gp=gpar(col='#4d5262')),t=6,b=6,l=1,r=2)  

	page1.Tab_112_forApp = tableGrob(page1.tmp$out.Tab_11_source2,theme=page1.tmp$out.Tab_11_source2.theme,cols=NULL,rows=NULL,
		widths=unit(c(512),'points'),heights=unit(c(38.4),'points'))

	page1.Tab_11_source2.theme_forWeb = page1.tmp$out.Tab_11_source2.theme
	page1.Tab_11_source2.theme_forWeb$core$fg_params$fontsize = 12
	page1.Tab_112_forWeb = tableGrob(page1.tmp$out.Tab_11_source2,theme=page1.Tab_11_source2.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(483),'points'),heights=unit(c(19),'points'))

	page1.Tab_113_forApp = tableGrob(page1.tmp$out.Tab_11_source3,theme=page1.tmp$out.Tab_11_source3.theme,cols=NULL,rows=NULL,
		widths=unit(c(256,256),'points'),heights=unit(c(2.8,5.7,rep(38.4,2)),'points'))
	page1.Tab_113_forApp.f = gtable_add_grob(page1.Tab_113_forApp,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=1,b=1,l=1,r=2)

	page1.Tab_11_source3.theme_forWeb = page1.tmp$out.Tab_11_source3.theme
	page1.Tab_11_source3.theme_forWeb$core$fg_params$fontsize = c(1,1,13,13)
	page1.Tab_113_forWeb = tableGrob(page1.tmp$out.Tab_11_source3,theme=page1.Tab_11_source3.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(300,183),'points'),heights=unit(c(2,6,rep(26,2)),'points'))
	page1.Tab_113_forWeb.f = gtable_add_grob(page1.Tab_113_forWeb,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=1,b=1,l=1,r=2)

	page1.Tab_114_forApp = tableGrob(page1.tmp$out.Tab_11_source4,theme=page1.tmp$out.Tab_11_source4.theme,cols=NULL,rows=NULL,
		widths=unit(c(512),'points'),heights=unit(c(37),'points'))

	page1.Tab_11_source4.theme_forWeb = page1.tmp$out.Tab_11_source4.theme
	page1.Tab_11_source4.theme_forWeb$core$fg_params$fontsize = 12
	page1.Tab_114_forWeb = tableGrob(page1.tmp$out.Tab_11_source4,theme=page1.Tab_11_source4.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(483),'points'),heights=unit(c(21),'points'))

	page1.Tab_115_forApp = tableGrob(page1.tmp$out.Tab_11_source5,theme=page1.tmp$out.Tab_11_source5.theme,cols=NULL,rows=NULL,
		widths=unit(c(256,256),'points'),heights=unit(c(37),'points'))

	page1.Tab_11_source5.theme_forWeb = page1.tmp$out.Tab_11_source5.theme
	page1.Tab_11_source5.theme_forWeb$core$fg_params$fontsize = 13
	page1.Tab_115_forWeb = tableGrob(page1.tmp$out.Tab_11_source5,theme=page1.Tab_11_source5.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(300,183),'points'),heights=unit(c(26),'points'))

	page1.Tab_116_forApp = tableGrob(page1.tmp$out.Tab_11_source6,theme=page1.tmp$out.Tab_11_source6.theme,cols=NULL,rows=NULL,
		widths=unit(c(512),'points'),heights=unit(rep(28.5,2),'points'))

	page1.Tab_11_source6.theme_forWeb = page1.tmp$out.Tab_11_source6.theme
	page1.Tab_11_source6.theme_forWeb$core$fg_params$fontsize = c(12,12)
	page1.Tab_116_forWeb = tableGrob(page1.tmp$out.Tab_11_source6,theme=page1.Tab_11_source6.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(483),'points'),heights=unit(rep(21,2),'points'))


	## 1page. Graph format +++++++++++++ 

	page1.tmp$out.Plot_21_forWeb$theme$axis.text.y$size = 12 
	page1.tmp$out.Plot_21_forWeb$theme$axis.text.x$size = 12 
	
	#### LibreReport_Stat - overall - print out - ####################

	setwd(path0)
	createdtime = as.integer(Sys.time(),format='%Y-%m-%d_%H:%M:%S.%OS')
	reportIdx = paste(memberKey,createdtime,sep='_')

	if ( Target!='DIET' ) {
		## forApp ==== ##
		CairoPNG(filename=paste(reportIdx,'App_Stat.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=832, units='px',dpi=96)
		LibreReport_Stat_forApp = try(grid.arrange(grobs=list(tableGrob(page1.subtitle_11,theme=subtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										page1.Tab_111a_forApp.f,page1.Tab_111b_forApp,page1.Tab_111c_forApp.f,
										page1.Tab_112_forApp,page1.Tab_113_forApp.f,page1.Tab_114_forApp,page1.Tab_115_forApp,page1.Tab_116_forApp), 
							nrow=9, ncol=1, layout_matrix=rbind(c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9)),
							widths=unit(c(512),'points'),heights=unit(c(50,91.8,45.5,182,38.4,85.3,37,37,57),'points')),silent=T)
		dev.off()

		if(class(LibreReport_Stat_forApp)[1]!='try-error') {
			outFileNames = c(outFileNames,paste(reportIdx,'App_Stat.png',sep='_'))
		} else {
			errCode = c(errCode,'21101')
		}
	}


	#### LibreReport_TIR - print out - ####################

	if ( Target!='DIET' ) {
		## forApp ==== ##
		CairoPNG(filename=paste(reportIdx,'App_TIR.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=832, units='px',dpi=96)
		LibreReport_TIR_forApp = try(grid.arrange(grobs=list(tableGrob(page1.subtitle_12,theme=subtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										page1.tmp$out.Plot_12), 
							nrow=3, ncol=1, layout_matrix=rbind(c(1),NA,c(2)),
							widths=unit(c(512),'points'),heights=unit(c(50,62,512),'points')),silent=T)
		dev.off()

		if(class(LibreReport_TIR_forApp)[1]!='try-error') {
			outFileNames = c(outFileNames,paste(reportIdx,'App_TIR.png',sep='_'))
		} else {
			errCode = c(errCode,'21102')
		}
	}

	if ( Target!='DIET' ) {
		## forWeb ==== ##
		CairoPNG(filename=paste(reportIdx,'Web_StatandTIR.png',sep='_'), family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=544, units='px',dpi=96)
		LibreReport_StatandTIR_forWeb = try(grid.arrange(grobs=list(tableGrob(page1.subtitle_11,theme=subtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(483,'points'),heights=unit(28.5,'points')),
											tableGrob(page1.subtitle_12,theme=subtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(357,'points'),heights=unit(28.5,'points')),
											page1.Tab_111a_forWeb.f,page1.Tab_111b_forWeb,page1.Tab_111c_forWeb.f,page1.Tab_112_forWeb,page1.Tab_113_forWeb.f,page1.Tab_114_forWeb,page1.Tab_115_forWeb,page1.Tab_116_forWeb,
											page1.tmp$out.Plot_12_forWeb),
									nrow=10, ncol=3, layout_matrix=rbind(c(1,NA,2),NA,c(3,NA,11),c(4,NA,11),c(5,NA,11),c(6,NA,11),c(7,NA,11),c(8,NA,11),c(9,NA,11),c(10,NA,11)),
									widths=unit(c(483,36,357),'points'),heights=unit(c(28.5,6,60,28.5,117,19,60,21,26,42),'points')),silent=T)
		dev.off()

		if(class(LibreReport_StatandTIR_forWeb)[1]!='try-error') {
			outFileNames = c(outFileNames,paste(reportIdx,'Web_StatandTIR.png',sep='_'))
		} else {
			errCode = c(errCode,'21201')
		}
	}

	#### item3 - 24HR AGP graph - print out - ####################

	if ( Target!='DIET' ) {
		## forApp ==== ##
		CairoPNG(filename=paste(reportIdx,'App_AGP.png',sep='_'), family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=1112, units='px',dpi=96)
		LibreReport_AGP_forApp = try(grid.arrange(grobs=list(tableGrob(page1.subtitle_21,theme=subtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),#1
										tableGrob(page1.remark_21_forApp,theme=remark.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(58,'points')),#2
										tableGrob(page1.remark_22,theme=remark025.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(24,488),'points'),heights=unit(27,'points')),#3
										tableGrob(page1.remark_23,theme=remark005.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(24,488),'points'),heights=unit(27,'points')),#4
										page1.tmp$out.Plot_21_forApp), #5
							nrow=6, ncol=1, layout_matrix=rbind(c(1),NA,c(2),c(3),c(4),c(5)),
							widths=unit(c(512),'points'),heights=unit(c(50,6,58,27,27,666),'points')),silent=T)

		dev.off()
		
		if(class(LibreReport_AGP_forApp)[1]!='try-error') {
			outFileNames = c(outFileNames,paste(reportIdx,'App_AGP.png',sep='_'))
		} else {
			errCode = c(errCode,'21103')
		}

		## forWeb ==== ##
		CairoPNG(filename=paste(reportIdx,'Web_AGP.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=496, units='px',dpi=96)
		LibreReport_AGP_forWeb = try(grid.arrange(grobs=list(tableGrob(page1.subtitle_21,theme=subtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#1
										tableGrob(page1.remark_21_forWeb,theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(21.5,'points')),#2
										tableGrob(page1.remark_22,theme=remark025.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(24,852),'points'),heights=unit(21,'points')),#3
										tableGrob(page1.remark_23,theme=remark005.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(24,852),'points'),heights=unit(21,'points')),#4
										page1.tmp$out.Plot_21_forWeb), #5
							nrow=6, ncol=3, layout_matrix=rbind(c(1,1,1),NA,c(2,2,2),c(3,3,3),c(4,4,4),c(NA,5,NA)),
							widths=unit(c(25,826,25),'points'),heights=unit(c(28.5,4,21.5,21,21,276),'points')),silent=T)

		dev.off()

		if(class(LibreReport_AGP_forWeb)[1]!='try-error') {
			outFileNames = c(outFileNames,paste(reportIdx,'Web_AGP.png',sep='_'))
		} else {
			errCode = c(errCode,'21203')
		}
	}


	### step3 =============================================================================##
	### 다이어트용 혈당스파이크

	if ( method=='Spike' ) {
		SPIKEdata.tmp = try(GlucoseSpike_onW(daysAZ=unique(AGPdata$date),data=AGPdata,baseGlu=summaryValue$meanGlu,IncCut=IncCut),silent=T)
		if ( class(SPIKEdata.tmp)=='try-error' ) {
			errCode = c(errCode,'30900')
			break
		} else {
			errCode = c(errCode,SPIKEdata.tmp$errCode.sub)
			AGPdata = SPIKEdata.tmp$data
		}
	}


	### step4 =============================================================================##
	### 일일혈당프로필

	if ( method=='Spike' ) {
		spikeStat = SPIKEdata.tmp$stat
		spikeStat$date_event = as.Date(spikeStat$time_event)
		spikeStat = try(sqldf('select date_event, max(peak) as max_peak, count(peak) as count_peak from spikeStat group by date_event'),silent=T)
		if ( class(spikeStat)=='try-error' ) {
			errCode = c(errCode,'41900')
		}
		## 0 번 추가 ## 
		add.tmp = as.Date(setdiff(unique(SPIKEdata.tmp$data$date),spikeStat$date_event),origin='1970-01-01')
		if ( length(add.tmp)>0 ) {
			spikeStat = rbind(spikeStat,data.frame(date_event=add.tmp,max_peak=NA,count_peak=0))
		}
		gluStat = data.frame(max_glu=tapply(AGPdata[!is.na(AGPdata$glucose),]$glucose,AGPdata[!is.na(AGPdata$glucose),]$date,max,na.rm=T))
		gluStat$date = as.Date(rownames(gluStat))
		spikeStat$max_glu = NA 
		spikeStat$max_glu = gluStat[match(gluStat$date,spikeStat$date_event),]$max_glu

		cond1.b = which(spikeStat$count_peak==min(spikeStat$count_peak))
		if ( all(spikeStat[cond1.b,]$count_peak==0) ) {
			cond2.b = (spikeStat[cond1.b,]$max_glu > TBR_lev1.Cut)
			cond3.b = cond1.b[cond2.b]
			bestday = spikeStat[cond3.b[which.min(spikeStat[cond3.b,]$max_glu)],]$date_event
		} else {
			cond2.b = (spikeStat[cond1.b,]$max_peak > TBR_lev1.Cut)
			cond3.b = cond1.b[cond2.b]
			bestday = spikeStat[cond3.b[which.min(spikeStat[cond3.b,]$max_peak)],]$date_event
		}

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

	} else {
		bestday = NULL
		worstday = NULL
	}

	dailygp = try(dailyGP_onW(data=page1.tmp$out.Plot_31_source,bestday=bestday,worstday=worstday,TBR_lev1.Cut=TBR_lev1.Cut,TAR_lev1.Cut=TAR_lev1.Cut, method=method),silent=T)
	# 에러 확인용 ****************************************************************************
	if ( class(dailygp)=='try-error' ) {
		errCode = c(errCode,'42900')
		break
	}

	#### daliyGP graph - print out - ####################

	## forWeb ==== ## 
 	CairoPNG(filename=paste(reportIdx,'Web_DailyGP.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=404, units='px',dpi=96)

	LibreReport_DailyGP_forWeb = try(grid.arrange(grobs=list(
		tableGrob(page1.subtitle_31,theme=subtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#1
		tableGrob(page1.remark_31,theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(21.5,'points')),#2
		dailygp$out.Plot[[1]][[1]],dailygp$out.Plot[[1]][[2]]), #3,4
		nrow=7, ncol=1, layout_matrix=rbind(c(1),NA,c(2),NA,c(3),c(4),NA),
		widths=unit(c(876),'points'),heights=unit(c(28.5,6,21.5,3,120,120,4),'points')),silent=T) 

	dev.off()

	if ( class(LibreReport_DailyGP_forWeb)[1]!='try-error' ) {
		outFileNames = c(outFileNames,paste(reportIdx,'Web_DailyGP.png',sep='_'))
	} else {
		errCode = c(errCode,'21204')
	}


	if ( F ) {#Target=='DIET' ) { #temp (20230407, 모바일개발 전 임시방편)

		## forApp ==== ## 
		CairoPNG(filename=paste(reportIdx,'App_DailyGP.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(67,14,72,48,dailygp$out.Plot[[2]][[2]])), units='px',dpi=96)
	}
		LibreReport_DailyGP_forApp = try(grid.arrange(grobs=list(
			tableGrob(data.frame(x='BEST / WORST DAY'),theme=subtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(67*0.75,'points')),#1
			tableGrob(page1.remark_31_forApp,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(72*0.75,'points')),#2
			dailygp$out.Plot[[2]][[1]]), #3
			nrow=5, ncol=1, layout_matrix=rbind(c(1),NA,c(2),NA,c(3)),
			widths=unit(640*0.75,'points'),heights=unit(c(67,14,72,48,dailygp$out.Plot[[2]][[2]])*0.75,'points')),silent=T) 

		dev.off()
	if ( F ) {#Target=='DIET' ) { #temp (20230407, 모바일개발 전 임시방편)
		if ( class(LibreReport_DailyGP_forApp)[1]!='try-error' ) {
			outFileNames = c(outFileNames,paste(reportIdx,'App_DailyGP.png',sep='_'))
		} else {
			errCode = c(errCode,'21204')
		}
	}


	### step4 =============================================================================##
	### 식후혈당분석

	timelogo1 = data.frame(c1='',c2='오전',c3='04:00-10:00',c4='')
	timelogo2 = data.frame(c1='',c2='정오',c3='10:00-16:00',c4='')
	timelogo3 = data.frame(c1='',c2='저녁',c3='16:00-22:00',c4='')

	#### breakfast
	dietplan.morning = try(GlucosePattern_clustering_onW(daysAZ=unique(AGPdata$date),data=AGPdata,mealtime=1,method=method,PPG.L=PPG.L,PPG.U=PPG.U,Target=Target,IncCut=IncCut),silent=T)
	#### lunch 
	dietplan.noon = try(GlucosePattern_clustering_onW(daysAZ=unique(AGPdata$date),data=AGPdata,mealtime=2,method=method,PPG.L=PPG.L,PPG.U=PPG.U,Target=Target,IncCut=IncCut),silent=T)
	#### dinner 
	dietplan.evening = try(GlucosePattern_clustering_onW(daysAZ=unique(AGPdata$date),data=AGPdata,mealtime=3,method=method,PPG.L=PPG.L,PPG.U=PPG.U,Target=Target,IncCut=IncCut),silent=T)

	if ( any(class(dietplan.morning)=='try-error', class(dietplan.noon)=='try-error', class(dietplan.evening)=='try-error') ) {
		errCode = c(errCode,'40900')
	} else {
		errCode = c(errCode,unique(c(dietplan.morning$errCode.sub,dietplan.noon$errCode.sub,dietplan.evening$errCode.sub)))
	}

	## dietplan title +++++++++++++ 	
	dietplan.title = t(data.frame(r1='혈당 패턴에 따른 식사 리스트',r2='분석 기간 동안 식사에 의한 혈당 변동 패턴을 분석하여 보여줍니다.'))

	## dietplan subTitle +++++++++++++
	dietplan.subtitle1 = data.frame('권장되는 식사 리스트')
	dietplan.subtitle2 = data.frame('주의가 필요한 식사 리스트')
	dietplan.subtitle3 = data.frame('피해야 할 식사 리스트')
	dietplan.subtitle5 = data.frame('주의가 필요한 식사 리스트(추정)')

	## theme 
	subtitle1.theme_forWeb = subtitle.theme_forWeb
	subtitle1.theme_forWeb$core$bg_params$fill = '#00b050'
	subtitle2.theme_forWeb = subtitle1.theme_forWeb
	subtitle2.theme_forWeb$core$bg_params$fill = '#ff6600'
	subtitle3.theme_forWeb = subtitle1.theme_forWeb
	subtitle3.theme_forWeb$core$bg_params$fill = '#ff0000'
    
	subtitle1.theme_forApp = subtitle.theme_forApp 
	subtitle1.theme_forApp$core$bg_params$fill = '#00b050'
	subtitle2.theme_forApp = subtitle1.theme_forApp
	subtitle2.theme_forApp$core$bg_params$fill = '#ff6600'
	subtitle3.theme_forApp = subtitle1.theme_forApp
	subtitle3.theme_forApp$core$bg_params$fill = '#ff0000'

	## dietpaln tabtitle, remark +++++++++++++

	if ( method=='TargetValue' ) {
		dietplan.tabitle1 = data.frame(x=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL 에서 벗어나지 않은 식사 기록을 보여줍니다.',sep=''))
		dietplan.tabitle2 = data.frame(x=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL 보다 상승하나, 식후 3시간 내에 식전 혈당으로 회복되었던 식사 기록을 보여줍니다.',sep=''))
		dietplan.tabtile3l = data.frame(c1='고혈당 유지') 
		dietplan.tabtile3r = data.frame(c2=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL 보다 상승한 후, 식후 3시간 내에 식전 혈당으로 회복되지 못한 식사 기록을 보여줍니다.',sep='')) 
		dietplan.tabtile4l = data.frame(c1='혈당 스파이크')
		dietplan.tabtile4r = data.frame(c2=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL 보다 상승한 후, 식전 혈당보다 낮은 수준으로 하락 후 재상승된 식사 기록을 보여줍니다.',sep=''))

	} else if ( method=='IncValue' ) {
		dietplan.tabitle1 = data.frame(x=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL 이내로 상승된 식사 기록을 보여줍니다.',sep=''))
		dietplan.tabitle2 = data.frame(x=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL 보다 높게 상승하나, 식후 3시간 내에 식전 혈당으로 회복되었던 식사 기록을 보여줍니다.',sep=''))
		dietplan.tabtile3l = data.frame(c1='고혈당 유지')
		dietplan.tabtile3r = data.frame(c2=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL 보다 높게 상승한 후, 식후 3시간 내에 식전 혈당으로 회복되지 못한 식사 기록을 보여줍니다.',sep=''))
		dietplan.tabtile4l = data.frame(c1='혈당 스파이크')
		dietplan.tabtile4r = data.frame(c2=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL 보다 높게 상승한 후, 식전 혈당보다 낮은 수준으로 하락 후 재상승된 식사 기록을 보여줍니다.',sep=''))

	} else if ( method=='Spike' ) {
        dietplan.tabtitle5 = data.frame(x=paste('식후 혈당의 최고값이 평균 혈당 대비 ', IncCut,' mg/dL 이상으로 상승된 식사 기록을 보여줍니다.', sep=''))
	    dietplan.tabtitle5.A = paste(strwrap(dietplan.tabtitle5,width=63),collapse='\n')

	}

	if ( method!='Spike' ) {
		dietplan.tabitle1.A = paste(strwrap(dietplan.tabitle1,width=63),collapse='\n')
		dietplan.tabitle2.A = paste(strwrap(dietplan.tabitle2,width=63),collapse='\n')
		dietplan.tabtile3r.A = paste(strwrap(dietplan.tabtile3r,width=45),collapse='\n')
		dietplan.tabtile4r.A = paste(strwrap(dietplan.tabtile4r,width=45),collapse='\n')
	}

	#### 분류1. 식후혈당 권장 good  # todo 
	avgPPG.A_good = vector('list',length=3)
	avgPPG.A_good.Af = vector('list',length=3)
	avgPPG.A_good.Wf = vector('list',length=3)
	
	avgPPG.A_good[[1]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.morning$avgInc_byPtrn[1]),round(dietplan.morning$avgInc_byPtrn[1]),'-'),c3='mg/dL')
	avgPPG.A_good[[2]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.noon$avgInc_byPtrn[1]),round(dietplan.noon$avgInc_byPtrn[1]),'-'),c3='mg/dL')
	avgPPG.A_good[[3]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.evening$avgInc_byPtrn[1]),round(dietplan.evening$avgInc_byPtrn[1]),'-'),c3='mg/dL')

	good_null_msg = function(time) { paste('분석기간 ', c('오전','정오','저녁')[time],' 식사 중\n혈당을 안정화 시키는 식사 기록이 없었어요 ㅠㅠ',sep='') }

	for ( k in 1:3 ) {
		avgPPG.A_good.Af[[k]] = tableGrob(avgPPG.A_good[[k]],theme=avgPPG.A.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
		avgPPG.A_good.Wf[[k]] = tableGrob(avgPPG.A_good[[k]],theme=avgPPG.A.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	}

	#### 분류2. 식후혈당 주의 warn   # todo 
	avgPPG.A_warn = vector('list',length=3)
	avgPPG.A_warn.Af = vector('list',length=3)
	avgPPG.A_warn.Wf = vector('list',length=3)

	avgPPG.A_warn[[1]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.morning$avgInc_byPtrn[2]),round(dietplan.morning$avgInc_byPtrn[2]),'-'),c3='mg/dL')
	avgPPG.A_warn[[2]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.noon$avgInc_byPtrn[2]),round(dietplan.noon$avgInc_byPtrn[2]),'-'),c3='mg/dL')
	avgPPG.A_warn[[3]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.evening$avgInc_byPtrn[2]),round(dietplan.evening$avgInc_byPtrn[2]),'-'),c3='mg/dL')

	warn_null_msg = function(time) { paste('분석기간 ', c('오전','정오','저녁')[time],' 식사 중\n혈당을 높인 식사 기록이 없었어요 ^_^',sep='') }

	for ( k in 1:3 ) {
		avgPPG.A_warn.Af[[k]] = tableGrob(avgPPG.A_warn[[k]],theme=avgPPG.A.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	    avgPPG.A_warn.Wf[[k]] = tableGrob(avgPPG.A_warn[[k]],theme=avgPPG.A.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	}

	#### 분류3. 식후혈당 지양1 badh   # todo 
	avgPPG.A_badh = vector('list',length=3)
	avgPPG.A_badh.Af = vector('list',length=3)
	avgPPG.A_badh.Wf = vector('list',length=3)

	avgPPG.A_badh[[1]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.morning$avgInc_byPtrn[3]),round(dietplan.morning$avgInc_byPtrn[3]),'-'),c3='mg/dL')
	avgPPG.A_badh[[2]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.noon$avgInc_byPtrn[3]),round(dietplan.noon$avgInc_byPtrn[3]),'-'),c3='mg/dL')
	avgPPG.A_badh[[3]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.evening$avgInc_byPtrn[3]),round(dietplan.evening$avgInc_byPtrn[3]),'-'),c3='mg/dL')

	badh_null_msg = function(time) { paste('분석기간 ', c('오전','정오','저녁')[time],' 식사 중\n고혈당이 지속된 식사 기록이 없었어요 ^_^',sep='') }
	
	for ( k in 1:3 ) {
		avgPPG.A_badh.Af[[k]] = tableGrob(avgPPG.A_badh[[k]],theme=avgPPG.A.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	    avgPPG.A_badh.Wf[[k]] = tableGrob(avgPPG.A_badh[[k]],theme=avgPPG.A.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	}

	#### 분류4. 식후혈당 지양2 bads   # todo 
	avgPPG.A_bads = vector('list',length=3)
	avgPPG.A_bads.Af = vector('list',length=3)
	avgPPG.A_bads.Wf = vector('list',length=3)

	avgPPG.A_bads[[1]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.morning$avgInc_byPtrn[4]),round(dietplan.morning$avgInc_byPtrn[4]),'-'),c3='mg/dL')
	avgPPG.A_bads[[2]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.noon$avgInc_byPtrn[4]),round(dietplan.noon$avgInc_byPtrn[4]),'-'),c3='mg/dL')
	avgPPG.A_bads[[3]] = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(dietplan.evening$avgInc_byPtrn[4]),round(dietplan.evening$avgInc_byPtrn[4]),'-'),c3='mg/dL')

	bads_null_msg = function(time) { paste('분석기간 ', c('오전','정오','저녁')[time],' 식사 중\n혈당스파이크를 일으킨 식사 기록이 없었어요 ^_^',sep='') }

	for ( k in 1:3 ) {
		avgPPG.A_bads.Af[[k]] = tableGrob(avgPPG.A_bads[[k]],theme=avgPPG.A.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	    avgPPG.A_bads.Wf[[k]] = tableGrob(avgPPG.A_bads[[k]],theme=avgPPG.A.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	}

	#### 분류5. 식후혈당 다이어트 스파이크 spike  

	espike_null_msg = function(time) { paste('분석기간 ', c('오전','정오','저녁')[time],' 식사 중\n혈당을 높인 식사 기록이 없었어요 ^_^',sep='') }


	tabHtFt.W = function(nline){
		return( 14*nline+5*(nline-1)+20 )
	}
	tabHtFt.A = function(nline){
		return( 21*nline+5*(nline-1)+20 )
	}
	tabHtFt2.A = function(nline){
		return( (34*nline+32)*0.75 )
	}

	cheer_msg1 = data.frame(x='웰체크는 언제나 응원합니다.\n함께 건강을 잘 관리해 보아요!')
	cheer_msg2 = data.frame(x='앞으로도 웰체크와 함께\n식후 혈당을 잘 관리해 보아요!')
	bads_detl_msg = data.frame(x='혈당 스파이크란\n혈당이 급격히 올라갔다 내려가는 증상을 말해요')
	
	dietPlanTableBody_create = function( invalue, msg ) {

		if ( dim(invalue)[1]==1 && grepl('해당 없음',invalue[1,2]) ) {
			tab.nrows=NULL;tab.nrowsW=NULL
			out.tab.Af = tableGrob(msg,theme=dietPlanTableBody_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(67,'points'))
			out.tab.Wf = dietPlanTableBody_null.Wf
		} else {
			memo.tmp = invalue[,2]
			for ( l in 1:length(memo.tmp) ) memo.tmp[l] = paste(strwrap(invalue[l,2],width=36),collapse='\n')
			invalue[,2] = memo.tmp
			tab.nrows = str_count(invalue[,2],'\n')+1

			out.tab.Af = tableGrob(invalue[,1:2],theme=dietPlanTableBody.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(tab.nrows),'points'))
			for ( g in 1:nrow(out.tab.Af) ) out.tab.Af = gtable_add_grob(out.tab.Af, grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc'),gp=gpar(col='#E9EDF3',lwd=2)),l=1,r=ncol(out.tab.Af),t=g,b=g)
			for ( m in 1:length(invalue[,2]) ) invalue[m,2] = paste(strwrap(gsub('\n',' ',invalue[m,2]),width=32),collapse='\n')
			tab.nrowsW = str_count(invalue[,2],'\n')+1
			out.tab.Wf = tableGrob(invalue[,1:2],theme=dietPlanTableBody.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(tab.nrowsW),'points'))
			for ( g in 1:nrow(out.tab.Wf) ) out.tab.Wf = gtable_add_grob(out.tab.Wf, grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc'),gp=gpar(col='#E9EDF3',lwd=2)),l=1,r=ncol(out.tab.Wf),t=g,b=g)

			if ( length(tab.nrows)==1 ) {
				out.tab.Af$grobs[which(out.tab.Af$layout$t==1 & out.tab.Af$layout$l==2 & out.tab.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
				out.tab.Wf$grobs[which(out.tab.Wf$layout$t==1 & out.tab.Wf$layout$l==2 & out.tab.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			}
		}
    	return(list(tab.Af=out.tab.Af,tab.Wf=out.tab.Wf,nrows.A=tab.nrows,nrows.W=tab.nrowsW))
	}

    All_dietPlanTableBody_create = function( invalueLs, msg) {

        invalue = data.frame(date=NA,menu=NA,z=NA,rowNum=NA)

        for ( k in 1:length(invalueLs) ) {            
            if ( dim(invalueLs[[k]])[1]==1 && grepl('해당 없음',invalueLs[[k]][1,2]) ) {
                next
            } else {
                colnames(invalueLs[[k]]) = colnames(invalue)
                invalue = rbind(invalue,invalueLs[[k]])
            }
        }
        invalue = invalue[which(!is.na(invalue$z) & invalue$menu!=''),]
		invalue = try(invalue[order(invalue$z,decreasing=T),],silent=T)

        if ( dim(invalue)[1]==0 ) {
            tab.nrows=NULL;tab.nrowsW=NULL
            out.tab.Af = tableGrob(msg,theme=dietPlanTableBody_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(71*0.75,'points'))
            out.tab.Wf = dietPlanTableBody_null.Wf
			tab.nrows = 1

        } else {
            invalue$rowNum = 1:dim(invalue)[1]
            memo.tmp = invalue[,2]
            for ( l in 1:length(memo.tmp) ) memo.tmp[l] = paste(strwrap(invalue[l,2],width=36),collapse='\n')
            invalue[,2] = memo.tmp
            tab.nrows = str_count(invalue[,2],'\n')+1

			# theme by invalue
			dietPlanTableBody.theme_forApp$core$fg_params$fontface = rep(c('plain','plain','bold'),each=nrow(invalue))

            out.tab.Af = tableGrob(invalue[,c('rowNum','menu','z')],theme=dietPlanTableBody.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(64,(384+64),(192-64))*0.75,'points'),heights=unit(tabHtFt2.A(tab.nrows),'points'))
			for ( g in 1:nrow(out.tab.Af) ) out.tab.Af = gtable_add_grob(out.tab.Af, grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc'),gp=gpar(col='#E9EDF3',lwd=2)),l=1,r=ncol(out.tab.Af),t=g,b=g)
            for ( m in 1:length(invalue[,2]) ) invalue[m,2] = paste(strwrap(gsub('\n',' ',invalue[m,2]),width=70),collapse='\n')
            tab.nrowsW = str_count(invalue[,2],'\n')+1
            out.tab.Wf = tableGrob(invalue[,c('rowNum','menu','z')],theme=dietPlanTableBody.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(54,(416+27),(108-27)),'points'),heights=unit(tabHtFt.W(tab.nrowsW),'points'))
			for ( g in 1:nrow(out.tab.Wf) ) out.tab.Wf = gtable_add_grob(out.tab.Wf, grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc'),gp=gpar(col='#E9EDF3',lwd=2)),l=1,r=ncol(out.tab.Wf),t=g,b=g)

            if ( length(tab.nrows)==1 ) {
                out.tab.Af$grobs[which(out.tab.Af$layout$t==1 & out.tab.Af$layout$l==2 & out.tab.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
                out.tab.Wf$grobs[which(out.tab.Wf$layout$t==1 & out.tab.Wf$layout$l==2 & out.tab.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
            }
        }
    	return(list(tab.Af=out.tab.Af,tab.Wf=out.tab.Wf,nrows.A=tab.nrows,nrows.W=tab.nrowsW))
    }

	##
	out.msgPlt_bad = ggplot() + annotation_custom(rasterGrob(bad_emoji)) + theme(panel.background=element_rect(fill=NA,color=NA))
	out.msgPlt_good = ggplot() + annotation_custom(rasterGrob(good_emoji)) + theme(panel.background=element_rect(fill=NA,color=NA))


    if ( method!='Spike' ) {   ############# not Spike / TargetValue, IncValue

	#### Dietary Plan (good) - print out - ####################
	dietplanT_good.morning = dietPlanTableBody_create(invalue=dietplan.morning$out_value1,msg=cheer_msg1)
	dietplanT_good.noon = dietPlanTableBody_create(invalue=dietplan.noon$out_value1,msg=cheer_msg1)
	dietplanT_good.evening = dietPlanTableBody_create(invalue=dietplan.evening$out_value1,msg=cheer_msg1)

	if ( !is.null(dietplanT_good.morning$nrows.A) ) {

		timelogo.theme_forApp$core$bg_params$fill = '#b2e7ca'
		timelogo.morning = tableGrob(timelogo1,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		Good.morning = try(grid.arrange(
            grobs=list(timelogo.morning,
			dietplan.morning$out.Plot1_forApp,
			avgPPG.A_good.Af[[1]],
			dietplanT_good.morning$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_good.morning$nrows.A))),'points')),silent=T)

		tmpH.morning = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_good.morning$nrows.A))))

	} else {
		tmpH.morning = 0 
	}

	if ( !is.null(dietplanT_good.noon$nrows.A) ) {

		timelogo.theme_forApp$core$bg_params$fill = '#b2e7ca'
		timelogo.noon = tableGrob(timelogo2,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		Good.noon =  try(grid.arrange(
            grobs=list(timelogo.noon,
			dietplan.noon$out.Plot1_forApp,
			avgPPG.A_good.Af[[2]],
			dietplanT_good.noon$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_good.noon$nrows.A))),'points')),silent=T)

		tmpH.noon = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_good.noon$nrows.A))))

	} else {
		tmpH.noon = 0 
	}

	if ( !is.null(dietplanT_good.evening$nrows.A) ) {

		timelogo.theme_forApp$core$bg_params$fill = '#b2e7ca'
		timelogo.evening = tableGrob(timelogo3,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		Good.evening = try(grid.arrange(
            grobs=list(timelogo.evening,
			dietplan.evening$out.Plot1_forApp,
			avgPPG.A_good.Af[[3]],
			dietplanT_good.evening$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_good.evening$nrows.A))),'points')),silent=T)
		
		tmpH.evening = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_good.evening$nrows.A))))

	} else {
		tmpH.evening = 0 
	}

	
	if ( !is.null(dietplanT_good.morning$nrows.A) | !is.null(dietplanT_good.noon$nrows.A) | !is.null(dietplanT_good.evening$nrows.A) ) {

		## for App ==== ##
		CairoPNG(filename=paste(reportIdx,'App_DietPlanGood.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720,height=sum(c(82,50,55,tmpH.morning,tmpH.noon,tmpH.evening))/0.75, units='px', dpi=96)

		if ( !is.null(dietplanT_good.morning$nrows.A) & is.null(dietplanT_good.noon$nrows.A) & is.null(dietplanT_good.evening$nrows.A) ) {

			LibreReport_DietPlanGood_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle1,theme=subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle1.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
				## 통합 ##
				Good.morning),
				nrow=4, ncol=1, layout_matrix=rbind(1,2,3,4),
				widths=unit(512,'points'),heights=unit(c(82,50,55,tmpH.morning),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_good.morning$nrows.A) & !is.null(dietplanT_good.noon$nrows.A) & is.null(dietplanT_good.evening$nrows.A) ) {

			LibreReport_DietPlanGood_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle1,theme=subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle1.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
				## 통합 ##
				Good.noon),
				nrow=4, ncol=1, layout_matrix=rbind(1,2,3,4),
				widths=unit(512,'points'),heights=unit(c(82,50,55,tmpH.noon),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_good.morning$nrows.A) & is.null(dietplanT_good.noon$nrows.A) & !is.null(dietplanT_good.evening$nrows.A) ) {

			LibreReport_DietPlanGood_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle1,theme=subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle1.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
				## 통합 ##
				Good.evening),
				nrow=4, ncol=1, layout_matrix=rbind(1,2,3,4),
				widths=unit(512,'points'),heights=unit(c(82,50,55,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_good.morning$nrows.A) & !is.null(dietplanT_good.noon$nrows.A) & is.null(dietplanT_good.evening$nrows.A) ) {

			LibreReport_DietPlanGood_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle1,theme=subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle1.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
				## 통합 ##
				Good.morning,Good.noon),
				nrow=5, ncol=1, layout_matrix=rbind(1,2,3,4,5),
				widths=unit(512,'points'),heights=unit(c(82,50,55,tmpH.morning,tmpH.noon),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_good.morning$nrows.A) & is.null(dietplanT_good.noon$nrows.A) & !is.null(dietplanT_good.evening$nrows.A) ) {

			LibreReport_DietPlanGood_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle1,theme=subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle1.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
				## 통합 ##
				Good.morning,Good.evening),
				nrow=5, ncol=1, layout_matrix=rbind(1,2,3,4,5),
				widths=unit(512,'points'),heights=unit(c(82,50,55,tmpH.morning,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_good.morning$nrows.A) & !is.null(dietplanT_good.noon$nrows.A) & !is.null(dietplanT_good.evening$nrows.A) ) {

			LibreReport_DietPlanGood_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle1,theme=subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle1.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
				## 통합 ##
				Good.noon,Good.evening),
				nrow=5, ncol=1, layout_matrix=rbind(1,2,3,4,5),
				widths=unit(512,'points'),heights=unit(c(82,50,55,tmpH.noon,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_good.morning$nrows.A) & !is.null(dietplanT_good.noon$nrows.A) & !is.null(dietplanT_good.evening$nrows.A) ) {

			LibreReport_DietPlanGood_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle1,theme=subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle1.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
				## 통합 ##
				Good.morning,Good.noon,Good.evening),
				nrow=6, ncol=1, layout_matrix=rbind(1,2,3,4,5,6),
				widths=unit(512,'points'),heights=unit(c(82,50,55,tmpH.morning,tmpH.noon,tmpH.evening),'points')),silent=T)
			dev.off()
		} 

		if ( all(class(LibreReport_DietPlanGood_forApp)!='try-error') )  outFileNames = c(outFileNames,paste(reportIdx,'App_DietPlanGood.png',sep='_'))

	} else {

		avgPPG.A_good.Af[[4]] = tableGrob(good_null_msg(time=0),theme=avgPPG.A_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(77,'points'))

		## for App ==== ##
		CairoPNG(filename=paste(reportIdx,'App_DietPlanGood.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720,height=sum(c(82,50,55,5,285,77,67))/0.75, units='px', dpi=96)
		
		LibreReport_DietPlanGood_forApp = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
			tableGrob(dietplan.subtitle1,theme=subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
			tableGrob(dietplan.tabitle1.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
			## 피드백메시지통합 ##
			out.msgPlt_bad,
			avgPPG.A_good.Af[[4]],                                    
			dietplanT_good.morning$tab.Af),
			nrow=7, ncol=1, layout_matrix=rbind(1,2,3,NA,4,5,6),
			widths=unit(512,'points'),heights=unit(c(82,50,55,5,285,77,67),'points')),silent=T)
		dev.off()

		if ( all(class(LibreReport_DietPlanGood_forApp)!='try-error') )  outFileNames = c(outFileNames,paste(reportIdx,'App_DietPlanGood.png',sep='_'))

	}

	## forWeb ==== ##
	if( !is.null(dietplanT_good.morning$nrows.W) | !is.null(dietplanT_good.noon$nrows.W) | !is.null(dietplanT_good.evening$nrows.W) ) {
		tmpL = max(length(dietplanT_good.morning$nrows.W),length(dietplanT_good.noon$nrows.W),length(dietplanT_good.evening$nrows.W))
		tmpH = matrix(NA,nrow=tmpL,ncol=3); 
		if(!is.null(dietplanT_good.morning$nrows.W)) tmpH[1:length(dietplanT_good.morning$nrows.W),1]=dietplanT_good.morning$nrows.W; 
		if(!is.null(dietplanT_good.noon$nrows.W)) tmpH[1:length(dietplanT_good.noon$nrows.W),2]=dietplanT_good.noon$nrows.W; 
		if(!is.null(dietplanT_good.evening$nrows.W)) tmpH[1:length(dietplanT_good.evening$nrows.W),3]=dietplanT_good.evening$nrows.W
		tmpH = sum(tabHtFt.W(apply(tmpH,1,max,na.rm=T)))
		
		CairoPNG(filename=paste(reportIdx,'Web_DietPlanGood.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=sum(c(474,tmpH))/0.75, units='px',dpi=96)
		LibreReport_DietPlanGood_forWeb = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
			tableGrob(dietplan.subtitle1,theme=subtitle1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
			tableGrob(dietplan.tabitle1,theme=tabtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#3
			tableGrob(timelogo1,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#4
			tableGrob(timelogo2,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#5
			tableGrob(timelogo3,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#6
			dietplan.morning$out.Plot1_forWeb,dietplan.noon$out.Plot1_forWeb,dietplan.evening$out.Plot1_forWeb, #7,8,9
			avgPPG.A_good.Wf[[1]],avgPPG.A_good.Wf[[2]],avgPPG.A_good.Wf[[3]], #10,11,12                    
			gtable_combine(dietplanT_good.morning$tab.Wf,out.Tab_spaceitem.Wf,dietplanT_good.noon$tab.Wf,out.Tab_spaceitem.Wf,dietplanT_good.evening$tab.Wf,along=1)), # 13
			nrow=8, ncol=5, layout_matrix=rbind(c(1,1,1,1,1),c(2,2,2,2,2),c(3,3,3,3,3),NA,c(4,NA,5,NA,6),c(7,NA,8,NA,9),c(10,NA,11,NA,12),c(13,13,13,13,13)),
			widths=unit(c(280,18,280,18,280),'points'),heights=unit(c(57,28.5,21.5,29,25,285,28,tmpH),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanGood_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanGood.png',sep='_'))
	} else {
		CairoPNG(filename=paste(reportIdx,'Web_DietPlanGood.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=453/0.75, units='px',dpi=96)
		LibreReport_DietPlanGood_forWeb = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
			tableGrob(dietplan.subtitle1,theme=subtitle1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
			tableGrob(dietplan.tabitle1,theme=tabtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#3
			tableGrob(timelogo1,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#4
			tableGrob(timelogo2,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#5
			tableGrob(timelogo3,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#6
			dietplan.morning$out.Plot1_forWeb,dietplan.noon$out.Plot1_forWeb,dietplan.evening$out.Plot1_forWeb, # 7,8,9
			textGrob('분석기간 중 혈당을 안정화 시키는 식사 기록이 없습니다.',
				gp=gpar(fontsize=12,fontface='plain',col='#333333',fontfamily='NotoSansCJKkrB'),hjust=0.5,vjust=0.5)), # 10
			nrow=7, ncol=5, layout_matrix=rbind(c(1,1,1,1,1),c(2,2,2,2,2),c(3,3,3,3,3),NA,c(4,NA,5,NA,6),c(7,NA,8,NA,9),c(10,10,10,10,10)),
			widths=unit(c(280,18,280,18,280),'points'),heights=unit(c(57,28.5,21.5,6,25,285,30),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanGood_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanGood.png',sep='_'))	
	}


	#### Dietary Plan (warning) - print out - ####################
	dietplanT_warn.morning = dietPlanTableBody_create(invalue=dietplan.morning$out_value2,msg=cheer_msg2)
	dietplanT_warn.noon = dietPlanTableBody_create(invalue=dietplan.noon$out_value2,msg=cheer_msg2)
	dietplanT_warn.evening = dietPlanTableBody_create(invalue=dietplan.evening$out_value2,msg=cheer_msg2)
	
	if ( !is.null(dietplanT_warn.morning$nrows.A) ) {

		timelogo.theme_forApp$core$bg_params$fill = '#ffd1b2'
		timelogo.morning = tableGrob(timelogo1,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		Warn.morning = try(grid.arrange(
            grobs=list(timelogo.morning,
			dietplan.morning$out.Plot2_forApp,
			avgPPG.A_warn.Af[[1]],
			dietplanT_warn.morning$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_warn.morning$nrows.A))),'points')),silent=T)

		tmpH.morning = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_warn.morning$nrows.A))))

	} else {
		tmpH.morning = 0 
	}

	if ( !is.null(dietplanT_warn.noon$nrows.A) ) {

		timelogo.theme_forApp$core$bg_params$fill = '#ffd1b2'
		timelogo.noon = tableGrob(timelogo2,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		Warn.noon =  try(grid.arrange(
            grobs=list(timelogo.noon,
			dietplan.noon$out.Plot2_forApp,
			avgPPG.A_warn.Af[[2]],
			dietplanT_warn.noon$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_warn.noon$nrows.A))),'points')),silent=T)

		tmpH.noon = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_warn.noon$nrows.A))))

	} else {
		tmpH.noon = 0 
	}

	if ( !is.null(dietplanT_warn.evening$nrows.A) ) {

		timelogo.theme_forApp$core$bg_params$fill = '#ffd1b2'
		timelogo.evening = tableGrob(timelogo3,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		Warn.evening = try(grid.arrange(
            grobs=list(timelogo.evening,
			dietplan.evening$out.Plot2_forApp,
			avgPPG.A_warn.Af[[3]],
			dietplanT_warn.evening$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4),c(5,5,5,5)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_warn.evening$nrows.A))),'points')),silent=T)
		
		tmpH.evening = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_warn.evening$nrows.A))))

	} else {
		tmpH.evening = 0 
	}

	if ( !is.null(dietplanT_warn.morning$nrows.A) | !is.null(dietplanT_warn.noon$nrows.A) | !is.null(dietplanT_warn.evening$nrows.A) ) {

		## for App ==== ##
		CairoPNG(filename=paste(reportIdx,'App_DietPlanWarn.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720,height=sum(c(82,50,76,tmpH.morning,tmpH.noon,tmpH.evening))/0.75, units='px', dpi=96)

		if ( !is.null(dietplanT_warn.morning$nrows.A) & is.null(dietplanT_warn.noon$nrows.A) & is.null(dietplanT_warn.evening$nrows.A) ) {

			LibreReport_DietPlanWarn_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle2,theme=subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle2.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(76),'points')),
				## 통합 ##
				Warn.morning),
				nrow=4, ncol=1, layout_matrix=rbind(1,2,3,4),
				widths=unit(512,'points'),heights=unit(c(82,50,76,tmpH.morning),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_warn.morning$nrows.A) & !is.null(dietplanT_warn.noon$nrows.A) & is.null(dietplanT_warn.evening$nrows.A) ) {

			LibreReport_DietPlanWarn_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle2,theme=subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle2.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(76),'points')),
				## 통합 ##
				Warn.noon),
				nrow=4, ncol=1, layout_matrix=rbind(1,2,3,4),
				widths=unit(512,'points'),heights=unit(c(82,50,76,tmpH.noon),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_warn.morning$nrows.A) & is.null(dietplanT_warn.noon$nrows.A) & !is.null(dietplanT_warn.evening$nrows.A) ) {

			LibreReport_DietPlanWarn_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle2,theme=subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle2.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(76),'points')),
				## 통합 ##
				Warn.evening),
				nrow=4, ncol=1, layout_matrix=rbind(1,2,3,4),
				widths=unit(512,'points'),heights=unit(c(82,50,76,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_warn.morning$nrows.A) & !is.null(dietplanT_warn.noon$nrows.A) & is.null(dietplanT_warn.evening$nrows.A) ) {

			LibreReport_DietPlanWarn_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle2,theme=subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle2.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(76),'points')),
				## 통합 ##
				Warn.morning,Warn.noon),
				nrow=5, ncol=1, layout_matrix=rbind(1,2,3,4,5),
				widths=unit(512,'points'),heights=unit(c(82,50,76,tmpH.morning,tmpH.noon),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_warn.morning$nrows.A) & is.null(dietplanT_warn.noon$nrows.A) & !is.null(dietplanT_warn.evening$nrows.A) ) {

			LibreReport_DietPlanWarn_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle2,theme=subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle2.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(76),'points')),
				## 통합 ##
				Warn.morning,Warn.evening),
				nrow=5, ncol=1, layout_matrix=rbind(1,2,3,4,5),
				widths=unit(512,'points'),heights=unit(c(82,50,76,tmpH.morning,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_warn.morning$nrows.A) & !is.null(dietplanT_warn.noon$nrows.A) & !is.null(dietplanT_warn.evening$nrows.A) ) {

			LibreReport_DietPlanWarn_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle2,theme=subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle2.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(76),'points')),
				## 통합 ##
				Warn.noon,Warn.evening),
				nrow=5, ncol=1, layout_matrix=rbind(1,2,3,4,5),
				widths=unit(512,'points'),heights=unit(c(82,50,76,tmpH.noon,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_warn.morning$nrows.A) & !is.null(dietplanT_warn.noon$nrows.A) & !is.null(dietplanT_warn.evening$nrows.A) ) {

			LibreReport_DietPlanWarn_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle2,theme=subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabitle2.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(76),'points')),
				## 통합 ##
				Warn.morning,Warn.noon,Warn.evening),
				nrow=6, ncol=1, layout_matrix=rbind(1,2,3,4,5,6),
				widths=unit(512,'points'),heights=unit(c(82,50,76,tmpH.morning,tmpH.noon,tmpH.evening),'points')),silent=T)
			dev.off()
		} 

		if ( all(class(LibreReport_DietPlanWarn_forApp)!='try-error') )  outFileNames = c(outFileNames,paste(reportIdx,'App_DietPlanWarn.png',sep='_'))

	} else {

		avgPPG.A_warn.Af[[4]] = tableGrob(warn_null_msg(time=0),theme=avgPPG.A_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points'))

		## for App ==== ##
		CairoPNG(filename=paste(reportIdx,'App_DietPlanWarn.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720,height=sum(c(82,50,76,5,285,77,67))/0.75, units='px', dpi=96)
		
		LibreReport_DietPlanWarn_forApp = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
			tableGrob(dietplan.subtitle2,theme=subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
			tableGrob(dietplan.tabitle2.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(76),'points')),
			## 피드백메시지통합 ##
			out.msgPlt_good,
			avgPPG.A_warn.Af[[4]],                                    
			dietplanT_warn.morning$tab.Af),
			nrow=7, ncol=1, layout_matrix=rbind(1,2,3,NA,4,5,6),
			widths=unit(512,'points'),heights=unit(c(82,50,76,5,285,77,67),'points')),silent=T)
		dev.off()

		if ( all(class(LibreReport_DietPlanWarn_forApp)!='try-error') )  outFileNames = c(outFileNames,paste(reportIdx,'App_DietPlanWarn.png',sep='_'))

	}


	## forWeb ==== ##
	if( !is.null(dietplanT_warn.morning$nrows.W) | !is.null(dietplanT_warn.noon$nrows.W) | !is.null(dietplanT_warn.evening$nrows.W) ) {
		tmpL = max(length(dietplanT_warn.morning$nrows.W),length(dietplanT_warn.noon$nrows.W),length(dietplanT_warn.evening$nrows.W))
		tmpH = matrix(NA,nrow=tmpL,ncol=3);
		if(!is.null(dietplanT_warn.morning$nrows.W)) tmpH[1:length(dietplanT_warn.morning$nrows.W),1]=dietplanT_warn.morning$nrows.W; 
		if(!is.null(dietplanT_warn.noon$nrows.W)) tmpH[1:length(dietplanT_warn.noon$nrows.W),2]=dietplanT_warn.noon$nrows.W; 
		if(!is.null(dietplanT_warn.evening$nrows.W)) tmpH[1:length(dietplanT_warn.evening$nrows.W),3]=dietplanT_warn.evening$nrows.W
		tmpH = sum(tabHtFt.W(apply(tmpH,1,max,na.rm=T)))

		CairoPNG(filename=paste(reportIdx,'Web_DietPlanWarn.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=sum(c(474,tmpH))/0.75, units='px',dpi=96)
		LibreReport_DietPlanWarn_forWeb = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
			tableGrob(dietplan.subtitle2,theme=subtitle2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
			tableGrob(dietplan.tabitle2,theme=tabtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#3
			tableGrob(timelogo1,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#4
			tableGrob(timelogo2,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#5
			tableGrob(timelogo3,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#6
			dietplan.morning$out.Plot2_forWeb,dietplan.noon$out.Plot2_forWeb,dietplan.evening$out.Plot2_forWeb, #7,8,9
			avgPPG.A_warn.Wf[[1]],avgPPG.A_warn.Wf[[2]],avgPPG.A_warn.Wf[[3]], #10,11,12
			gtable_combine(dietplanT_warn.morning$tab.Wf,out.Tab_spaceitem.Wf,dietplanT_warn.noon$tab.Wf,out.Tab_spaceitem.Wf,dietplanT_warn.evening$tab.Wf,along=1)), #13
			nrow=8, ncol=5, layout_matrix=rbind(c(1,1,1,1,1),c(2,2,2,2,2),c(3,3,3,3,3),NA,c(4,NA,5,NA,6),c(7,NA,8,NA,9),c(10,NA,11,NA,12),c(13,13,13,13,13)),
			widths=unit(c(280,18,280,18,280),'points'),heights=unit(c(57,28.5,21.5,29,25,285,28,tmpH),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanWarn_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanWarn.png',sep='_'))
	} else {
		CairoPNG(filename=paste(reportIdx,'Web_DietPlanWarn.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=453/0.75, units='px',dpi=96)
		LibreReport_DietPlanWarn_forWeb = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
			tableGrob(dietplan.subtitle2,theme=subtitle2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
			tableGrob(dietplan.tabitle2,theme=tabtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#3
			tableGrob(timelogo1,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#4
			tableGrob(timelogo2,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#5
			tableGrob(timelogo3,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#6
			dietplan.morning$out.Plot2_forWeb,dietplan.noon$out.Plot2_forWeb,dietplan.evening$out.Plot2_forWeb, # 7,8,9
			textGrob('분석기간 중 혈당을 높인 식사기록이 없습니다.',
				gp=gpar(fontsize=12,fontface='plain',col='#333333',fontfamily='NotoSansCJKkrB'),hjust=0.5,vjust=0.5)), #10
			nrow=7, ncol=5, layout_matrix=rbind(c(1,1,1,1,1),c(2,2,2,2,2),c(3,3,3,3,3),NA,c(4,NA,5,NA,6),c(7,NA,8,NA,9),c(10,10,10,10,10)),
			widths=unit(c(280,18,280,18,280),'points'),heights=unit(c(57,28.5,21.5,6,25,285,30),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanWarn_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanWarn.png',sep='_'))		
	}


	#### Dietary Plan (BadH) - print out - ####################
	dietplanT_badh.morning = dietPlanTableBody_create(invalue=dietplan.morning$out_value3,msg=cheer_msg2)
	dietplanT_badh.noon = dietPlanTableBody_create(invalue=dietplan.noon$out_value3,msg=cheer_msg2)
	dietplanT_badh.evening = dietPlanTableBody_create(invalue=dietplan.evening$out_value3,msg=cheer_msg2)

	if ( !is.null(dietplanT_badh.morning$nrows.A) ) {

		timelogo.theme_forApp$core$bg_params$fill = '#ffb2b2'
		timelogo.morning = tableGrob(timelogo1,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		BadH.morning = try(grid.arrange(
            grobs=list(timelogo.morning,
			dietplan.morning$out.Plot3_forApp,
			avgPPG.A_badh.Af[[1]],
			dietplanT_badh.morning$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_badh.morning$nrows.A))),'points')),silent=T)

		tmpH.morning = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_badh.morning$nrows.A))))

	} else {
		tmpH.morning = 0 
	}

	if ( !is.null(dietplanT_badh.noon$nrows.A) ) {

		timelogo.theme_forApp$core$bg_params$fill = '#ffb2b2'
		timelogo.noon = tableGrob(timelogo2,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		BadH.noon =  try(grid.arrange(
            grobs=list(timelogo.noon,
			dietplan.noon$out.Plot3_forApp,
			avgPPG.A_badh.Af[[2]],
			dietplanT_badh.noon$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_badh.noon$nrows.A))),'points')),silent=T)

		tmpH.noon = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_badh.noon$nrows.A))))

	} else {
		tmpH.noon = 0 
	}

	if ( !is.null(dietplanT_badh.evening$nrows.A) ) {

		timelogo.theme_forApp$core$bg_params$fill = '#ffb2b2'
		timelogo.evening = tableGrob(timelogo3,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		BadH.evening = try(grid.arrange(
            grobs=list(timelogo.evening,
			dietplan.evening$out.Plot3_forApp,
			avgPPG.A_badh.Af[[3]],
			dietplanT_badh.evening$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_badh.evening$nrows.A))),'points')),silent=T)
		
		tmpH.evening = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_badh.evening$nrows.A))))

	} else {
		tmpH.evening = 0 
	}

	if ( !is.null(dietplanT_badh.morning$nrows.A) | !is.null(dietplanT_badh.noon$nrows.A) | !is.null(dietplanT_badh.evening$nrows.A) ) {

		## for App ==== ##
		CairoPNG(filename=paste(reportIdx,'App_DietPlanBadH.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720,height=sum(c(82,50,105,tmpH.morning,tmpH.noon,tmpH.evening))/0.75, units='px', dpi=96)

		if ( !is.null(dietplanT_badh.morning$nrows.A) & is.null(dietplanT_badh.noon$nrows.A) & is.null(dietplanT_badh.evening$nrows.A) ) {

			LibreReport_DietPlanBadH_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile3l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile3r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadH.morning),
				nrow=4, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.morning),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_badh.morning$nrows.A) & !is.null(dietplanT_badh.noon$nrows.A) & is.null(dietplanT_badh.evening$nrows.A) ) {

			LibreReport_DietPlanBadH_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile3l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile3r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadH.noon),
				nrow=4, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.noon),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_badh.morning$nrows.A) & is.null(dietplanT_badh.noon$nrows.A) & !is.null(dietplanT_badh.evening$nrows.A) ) {

			LibreReport_DietPlanBadH_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile3l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile3r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadH.evening),
				nrow=4, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_badh.morning$nrows.A) & !is.null(dietplanT_badh.noon$nrows.A) & is.null(dietplanT_badh.evening$nrows.A) ) {

			LibreReport_DietPlanBadH_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile3l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile3r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadH.morning,BadH.noon),
				nrow=5, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5),c(6,6,6,6)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.morning,tmpH.noon),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_badh.morning$nrows.A) & is.null(dietplanT_badh.noon$nrows.A) & !is.null(dietplanT_badh.evening$nrows.A) ) {

			LibreReport_DietPlanBadH_forApp =try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile3l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile3r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadH.morning,BadH.evening),
				nrow=5, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5),c(6,6,6,6)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.morning,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_badh.morning$nrows.A) & !is.null(dietplanT_badh.noon$nrows.A) & !is.null(dietplanT_badh.evening$nrows.A) ) {

			LibreReport_DietPlanBadH_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile3l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile3r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadH.noon,BadH.evening),
				nrow=5, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5),c(6,6,6,6)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.noon,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_badh.morning$nrows.A) & !is.null(dietplanT_badh.noon$nrows.A) & !is.null(dietplanT_badh.evening$nrows.A) ) {

			LibreReport_DietPlanBadH_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile3l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile3r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadH.morning,BadH.noon,BadH.evening),
				nrow=6, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5),c(6,6,6,6),c(7,7,7,7)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.morning,tmpH.noon,tmpH.evening),'points')),silent=T)
			dev.off()
		} 

		if ( all(class(LibreReport_DietPlanBadH_forApp)!='try-error') )  outFileNames = c(outFileNames,paste(reportIdx,'App_DietPlanBadH.png',sep='_'))

	} else {

		avgPPG.A_badh.Af[[4]] = tableGrob(badh_null_msg(time=0),theme=avgPPG.A_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points'))

		## for App ==== ##
		CairoPNG(filename=paste(reportIdx,'App_DietPlanBadH.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720,height=sum(c(82,50,105,5,285,77,67))/0.75, units='px', dpi=96)
		
		LibreReport_DietPlanBadH_forApp = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),#1
			tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),#2
			tableGrob(dietplan.tabtile3l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),#3
			tableGrob(dietplan.tabtile3r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),#4
			## 피드백메시지통합 ##
			out.msgPlt_good,#5
			avgPPG.A_badh.Af[[4]],#6                  
			dietplanT_badh.morning$tab.Af),#7
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(5,5,5,5),c(6,6,6,6),c(7,7,7,7)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,5,285,77,67),'points')),silent=T)
		dev.off()

		if ( all(class(LibreReport_DietPlanBadH_forApp)!='try-error') )  outFileNames = c(outFileNames,paste(reportIdx,'App_DietPlanBadH.png',sep='_'))

	}


	## forWeb ==== ##
	if( !is.null(dietplanT_badh.morning$nrows.W) | !is.null(dietplanT_badh.noon$nrows.W) | !is.null(dietplanT_badh.evening$nrows.W) ) {
		tmpL = max(length(dietplanT_badh.morning$nrows.W),length(dietplanT_badh.noon$nrows.W),length(dietplanT_badh.evening$nrows.W))
		tmpH = matrix(NA,nrow=tmpL,ncol=3);
		if(!is.null(dietplanT_badh.morning$nrows.W)) tmpH[1:length(dietplanT_badh.morning$nrows.W),1]=dietplanT_badh.morning$nrows.W; 
		if(!is.null(dietplanT_badh.noon$nrows.W)) tmpH[1:length(dietplanT_badh.noon$nrows.W),2]=dietplanT_badh.noon$nrows.W; 
		if(!is.null(dietplanT_badh.evening$nrows.W)) tmpH[1:length(dietplanT_badh.evening$nrows.W),3]=dietplanT_badh.evening$nrows.W
		tmpH = sum(tabHtFt.W(apply(tmpH,1,max,na.rm=T))) 
		
		CairoPNG(filename=paste(reportIdx,'Web_DietPlanBadH.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=sum(c(484,tmpH))/0.75, units='px',dpi=96)
		LibreReport_DietPlanBadH_forWeb = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
			tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
			tableGrob(dietplan.tabtile3l,theme=tabtitlel.theme_forWeb,cols=NULL,rows=NULL,widths=unit(84,'points'),heights=unit(c(31.5),'points')),#3
			tableGrob(dietplan.tabtile3r,theme=tabtitler.theme_forWeb,cols=NULL,rows=NULL,widths=unit(792,'points'),heights=unit(31.5,'points')),#4
			tableGrob(timelogo1,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#5
			tableGrob(timelogo2,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#6
			tableGrob(timelogo3,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#7
			dietplan.morning$out.Plot3_forWeb,dietplan.noon$out.Plot3_forWeb,dietplan.evening$out.Plot3_forWeb, #8,9,10
			avgPPG.A_badh.Wf[[1]],avgPPG.A_badh.Wf[[2]],avgPPG.A_badh.Wf[[3]], #11,12,13
			gtable_combine(dietplanT_badh.morning$tab.Wf,out.Tab_spaceitem.Wf,dietplanT_badh.noon$tab.Wf,out.Tab_spaceitem.Wf,dietplanT_badh.evening$tab.Wf,along=1)), #14
			nrow=8, ncol=6, layout_matrix=rbind(c(1,1,1,1,1,1),c(2,2,2,2,2,2),c(3,4,4,4,4,4),NA,c(5,5,NA,6,NA,7),c(8,8,NA,9,NA,10),c(11,11,NA,12,NA,13),c(14,14,14,14,14,14)),
			widths=unit(c(84,196,18,280,18,280),'points'),heights=unit(c(57,28.5,31.5,29,25,285,28,tmpH),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanBadH_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanBadH.png',sep='_'))
	} else {
		CairoPNG(filename=paste(reportIdx,'Web_DietPlanBadH.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=462/0.75, units='px',dpi=96)
		LibreReport_DietPlanBadH_forWeb = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
			tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
			tableGrob(dietplan.tabtile3l,theme=tabtitlel.theme_forWeb,cols=NULL,rows=NULL,widths=unit(84,'points'),heights=unit(c(31.5),'points')),#3
			tableGrob(dietplan.tabtile3r,theme=tabtitler.theme_forWeb,cols=NULL,rows=NULL,widths=unit(792,'points'),heights=unit(31.5,'points')),#4
			tableGrob(timelogo1,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#5
			tableGrob(timelogo2,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#6
			tableGrob(timelogo3,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#7
			dietplan.morning$out.Plot3_forWeb,dietplan.noon$out.Plot3_forWeb,dietplan.evening$out.Plot3_forWeb, # 8,9,10
			textGrob('분석기간 중 고혈당이 지속된 식사기록이 없습니다.',
				gp=gpar(fontsize=12,fontface='plain',col='#333333',fontfamily='NotoSansCJKkrB'),hjust=0.5,vjust=0.5)), # 11
			nrow=7, ncol=6, layout_matrix=rbind(c(1,1,1,1,1,1),c(2,2,2,2,2,2),c(3,4,4,4,4,4),NA,c(5,5,NA,6,NA,7),c(8,8,NA,9,NA,10),c(11,11,11,11,11,11)),
			widths=unit(c(84,196,18,280,18,280),'points'),heights=unit(c(57,28.5,31.5,5,25,285,30),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanBadH_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanBadH.png',sep='_'))
	}


	#### Dietary Plan (BadS) - print out - ####################
	dietplanT_bads.morning = dietPlanTableBody_create(invalue=dietplan.morning$out_value4,msg=bads_detl_msg)
	dietplanT_bads.noon = dietPlanTableBody_create(invalue=dietplan.noon$out_value4,msg=bads_detl_msg)
	dietplanT_bads.evening = dietPlanTableBody_create(invalue=dietplan.evening$out_value4,msg=bads_detl_msg)

	if ( !is.null(dietplanT_bads.morning$nrows.A) ) { 

		timelogo.theme_forApp$core$bg_params$fill = '#ffb2b2'
		timelogo.morning = tableGrob(timelogo1,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		BadS.morning = try(grid.arrange(
            grobs=list(timelogo.morning,
			dietplan.morning$out.Plot4_forApp,
			avgPPG.A_bads.Af[[1]],
			dietplanT_bads.morning$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_bads.morning$nrows.A))),'points')),silent=T)

		tmpH.morning = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_bads.morning$nrows.A))))

	} else {
		tmpH.morning = 0 
	}

	if ( !is.null(dietplanT_bads.noon$nrows.A) ) {

		timelogo.theme_forApp$core$bg_params$fill = '#ffb2b2'
		timelogo.noon = tableGrob(timelogo2,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		BadS.noon =  try(grid.arrange(
            grobs=list(timelogo.noon,
			dietplan.noon$out.Plot4_forApp,
			avgPPG.A_bads.Af[[2]],
			dietplanT_bads.noon$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_bads.noon$nrows.A))),'points')),silent=T)

		tmpH.noon = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_bads.noon$nrows.A))))

	} else {
		tmpH.noon = 0 
	}

	if ( !is.null(dietplanT_bads.evening$nrows.A) ) {

		timelogo.theme_forApp$core$bg_params$fill = '#ffb2b2'
		timelogo.evening = tableGrob(timelogo3,theme=timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(156,60,140,156),'points'),heights=unit(c(31),'points'))

		BadS.evening = try(grid.arrange(
            grobs=list(timelogo.evening,
			dietplan.evening$out.Plot4_forApp,
			avgPPG.A_bads.Af[[3]],
			dietplanT_bads.evening$tab.Af),
			nrow=5, ncol=4, layout_matrix=rbind(NA,c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(29,31,285,50,sum(tabHtFt.A(dietplanT_bads.evening$nrows.A))),'points')),silent=T)
		
		tmpH.evening = sum(c(29,31,285,50,sum(tabHtFt.A(dietplanT_bads.evening$nrows.A))))

	} else {
		tmpH.evening = 0 
	}

	if ( !is.null(dietplanT_bads.morning$nrows.A) | !is.null(dietplanT_bads.noon$nrows.A) | !is.null(dietplanT_bads.evening$nrows.A) ) {

		## for App ==== ##
		CairoPNG(filename=paste(reportIdx,'App_DietPlanBadS.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720,height=sum(c(82,50,105,tmpH.morning,tmpH.noon,tmpH.evening))/0.75, units='px', dpi=96)

		if ( !is.null(dietplanT_bads.morning$nrows.A) & is.null(dietplanT_bads.noon$nrows.A) & is.null(dietplanT_bads.evening$nrows.A) ) {

			LibreReport_DietPlanBadS_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile4l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile4r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadS.morning),
				nrow=4, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.morning),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_bads.morning$nrows.A) & !is.null(dietplanT_bads.noon$nrows.A) & is.null(dietplanT_bads.evening$nrows.A) ) {

			LibreReport_DietPlanBadS_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile4l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile4r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadS.noon),
				nrow=4, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.noon),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_bads.morning$nrows.A) & is.null(dietplanT_bads.noon$nrows.A) & !is.null(dietplanT_bads.evening$nrows.A) ) {

			LibreReport_DietPlanBadS_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile4l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile4r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadS.evening),
				nrow=4, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_bads.morning$nrows.A) & !is.null(dietplanT_bads.noon$nrows.A) & is.null(dietplanT_bads.evening$nrows.A) ) {

			LibreReport_DietPlanBadS_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile4l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile4r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadS.morning,BadS.noon),
				nrow=5, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5),c(6,6,6,6)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.morning,tmpH.noon),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_bads.morning$nrows.A) & is.null(dietplanT_bads.noon$nrows.A) & !is.null(dietplanT_bads.evening$nrows.A) ) {

			LibreReport_DietPlanBadS_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile4l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile4r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadS.morning,BadS.evening),
				nrow=5, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5),c(6,6,6,6)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.morning,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( is.null(dietplanT_bads.morning$nrows.A) & !is.null(dietplanT_bads.noon$nrows.A) & !is.null(dietplanT_bads.evening$nrows.A) ) {

			LibreReport_DietPlanBadS_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile4l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile4r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadS.noon,BadS.evening),
				nrow=5, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5),c(6,6,6,6)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.noon,tmpH.evening),'points')),silent=T)
			dev.off()

		} else if ( !is.null(dietplanT_bads.morning$nrows.A) & !is.null(dietplanT_bads.noon$nrows.A) & !is.null(dietplanT_bads.evening$nrows.A) ) {

			LibreReport_DietPlanBadS_forApp = try(grid.arrange(
                grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
				tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
				tableGrob(dietplan.tabtile4l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
				tableGrob(dietplan.tabtile4r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
				## 통합 ##
				BadS.morning,BadS.noon,BadS.evening),
				nrow=6, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),c(5,5,5,5),c(6,6,6,6),c(7,7,7,7)),
				widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,tmpH.morning,tmpH.noon,tmpH.evening),'points')),silent=T)
			dev.off()
		} 

		if ( all(class(LibreReport_DietPlanBadS_forApp)!='try-error') )  outFileNames = c(outFileNames,paste(reportIdx,'App_DietPlanBadS.png',sep='_'))

	} else {

		avgPPG.A_bads.Af[[4]] = tableGrob(bads_null_msg(time=0),theme=avgPPG.A_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points')) 

		## for App ==== ##
		CairoPNG(filename=paste(reportIdx,'App_DietPlanBadS.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720,height=sum(c(82,50,105,5,285,77,67))/0.75, units='px', dpi=96)
		
		LibreReport_DietPlanBadS_forApp = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),#1
			tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),#2
			tableGrob(dietplan.tabtile4l,theme=tabtitlel.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),#3
			tableGrob(dietplan.tabtile4r.A,theme=tabtitler.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),#4
			## 피드백메시지통합 ##
			out.msgPlt_good,#5
			avgPPG.A_bads.Af[[4]],#6
			dietplanT_bads.morning$tab.Af),#7 #todo 
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(5,5,5,5),c(6,6,6,6),c(7,7,7,7)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,5,285,77,67),'points')),silent=T)
		dev.off()

		if ( all(class(LibreReport_DietPlanBadS_forApp)!='try-error') )  outFileNames = c(outFileNames,paste(reportIdx,'App_DietPlanBadS.png',sep='_'))

	}


	## forWeb ==== ##
	if( !is.null(dietplanT_bads.morning$nrows.W) | !is.null(dietplanT_bads.noon$nrows.W) | !is.null(dietplanT_bads.evening$nrows.W) ) {
		tmpL = max(length(dietplanT_bads.morning$nrows.W),length(dietplanT_bads.noon$nrows.W),length(dietplanT_bads.evening$nrows.W))
		tmpH = matrix(NA,nrow=tmpL,ncol=3); 
		if(!is.null(dietplanT_bads.morning$nrows.W)) tmpH[1:length(dietplanT_bads.morning$nrows.W),1]=dietplanT_bads.morning$nrows.W; 
		if(!is.null(dietplanT_bads.noon$nrows.W)) tmpH[1:length(dietplanT_bads.noon$nrows.W),2]=dietplanT_bads.noon$nrows.W; 
		if(!is.null(dietplanT_bads.evening$nrows.W)) tmpH[1:length(dietplanT_bads.evening$nrows.W),3]=dietplanT_bads.evening$nrows.W
		tmpH = sum(tabHtFt.W(apply(tmpH,1,max,na.rm=T)))
    
		CairoPNG(filename=paste(reportIdx,'Web_DietPlanBadS.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=sum(c(484,tmpH))/0.75, units='px',dpi=96)
		LibreReport_DietPlanBadS_forWeb = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#
			tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
			tableGrob(dietplan.tabtile4l,theme=tabtitlel.theme_forWeb,cols=NULL,rows=NULL,widths=unit(84,'points'),heights=unit(c(31.5),'points')),#3
			tableGrob(dietplan.tabtile4r,theme=tabtitler.theme_forWeb,cols=NULL,rows=NULL,widths=unit(792,'points'),heights=unit(31.5,'points')),#4
			tableGrob(timelogo1,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#5
			tableGrob(timelogo2,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#6
			tableGrob(timelogo3,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#7
			dietplan.morning$out.Plot4_forWeb,dietplan.noon$out.Plot4_forWeb,dietplan.evening$out.Plot4_forWeb, #8,9,10
			avgPPG.A_bads.Wf[[1]],avgPPG.A_bads.Wf[[2]],avgPPG.A_bads.Wf[[3]], #11,12,13
			gtable_combine(dietplanT_bads.morning$tab.Wf,out.Tab_spaceitem.Wf,dietplanT_bads.noon$tab.Wf,out.Tab_spaceitem.Wf,dietplanT_bads.evening$tab.Wf,along=1)), #14
			nrow=8, ncol=6, layout_matrix=rbind(c(1,1,1,1,1,1),c(2,2,2,2,2,2),c(3,4,4,4,4,4),NA,c(5,5,NA,6,NA,7),c(8,8,NA,9,NA,10),c(11,11,NA,12,NA,13),c(14,14,14,14,14,14)),
			widths=unit(c(84,196,18,280,18,280),'points'),heights=unit(c(57,28.5,31.5,29,25,285,28,tmpH),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanBadS_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanBadS.png',sep='_'))
	} else {

		CairoPNG(filename=paste(reportIdx,'Web_DietPlanBadS.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=462/0.75, units='px',dpi=96)
		LibreReport_DietPlanBadS_forWeb = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
			tableGrob(dietplan.subtitle3,theme=subtitle3.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
			tableGrob(dietplan.tabtile4l,theme=tabtitlel.theme_forWeb,cols=NULL,rows=NULL,widths=unit(84,'points'),heights=unit(c(31.5),'points')),#3
			tableGrob(dietplan.tabtile4r,theme=tabtitler.theme_forWeb,cols=NULL,rows=NULL,widths=unit(792,'points'),heights=unit(31.5,'points')),#4
			tableGrob(timelogo1,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#5
			tableGrob(timelogo2,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#6
			tableGrob(timelogo3,theme=timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(1,83,195,1),'points'),heights=unit(c(25),'points')),#7
			dietplan.morning$out.Plot4_forWeb,dietplan.noon$out.Plot4_forWeb,dietplan.evening$out.Plot4_forWeb, # 8,9,10
			textGrob('분석기간 중 혈당 스파이크를 일으킨 식사기록이 없습니다.',
				gp=gpar(fontsize=12,fontface='plain',col='#333333',fontfamily='NotoSansCJKkrB'),hjust=0.5,vjust=0.5)), #11
			nrow=7, ncol=6, layout_matrix=rbind(c(1,1,1,1,1,1),c(2,2,2,2,2,2),c(3,4,4,4,4,4),NA,c(5,5,NA,6,NA,7),c(8,8,NA,9,NA,10),c(11,11,11,11,11,11)),
			widths=unit(c(84,196,18,280,18,280),'points'),heights=unit(c(57,28.5,31.5,5,25,285,30),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanBadS_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanBadS.png',sep='_'))
	}

    }## end of method not spike / targetValue IncValue 


    if ( method=='Spike' ) { ## strt of method Spike 
 
	#### Dietary Plan (Spike) - print out - ####################
    dietplanT_spike.all = All_dietPlanTableBody_create(invalueLs=list(dietplan.morning$out_value5,dietplan.noon$out_value5,dietplan.evening$out_value5),msg='해당되는 식사기록이 없습니다.')


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

	if ( F ) { # 임시 (230407, 임시 방편 )
		## for App ==== ##
		CairoPNG(filename=paste(reportIdx,'App_DietPlanSpike.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720,height=sum(c(c(110,14,67,14,72,48),tmpH.all/0.75)), units='px', dpi=96)
	}
		LibreReport_DietPlanSpike_forApp = try(grid.arrange(
			grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(c(67,43)*0.75,'points')),#1
			tableGrob(dietplan.subtitle5,theme=subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(67*0.75,'points')),#2
			tableGrob(dietplan.tabtitle5.A,theme=tabtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(640*0.75,'points'),heights=unit(72*0.75,'points')),#3
			## 통합 ##
			Spike.all),#4
			nrow=7, ncol=1, layout_matrix=rbind(1,NA,2,NA,3,NA,4),
			widths=unit(640*0.75,'points'),heights=unit(c(c(110,14,67,14,72,48)*0.75,tmpH.all),'points')),silent=T)
		dev.off() 
	if ( F ) { # 임시 (230407, 임시 방편 )

		if ( all(class(LibreReport_DietPlanSpike_forApp)!='try-error') )  outFileNames = c(outFileNames,paste(reportIdx,'App_DietPlanSpike.png',sep='_'))
	}

	## forWeb ==== ##
	if( !is.null(dietplanT_spike.all$nrows.W) ) {

		tmpH = sum(tabHtFt.W(dietplanT_spike.all$nrows.W))
	    dietplanT_spike.head.Wf = tableGrob(data.frame('순위','식사기록','혈당상승폭','mg/dL'),theme=dietPlanTableHead.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(54,416,72,36),'points'),heights=unit(28,'points')) # todo :widths
	
		CairoPNG(filename=paste(reportIdx,'Web_DietPlanSpike.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=sum(c(164,tmpH))/0.75, units='px',dpi=96)

		LibreReport_DietPlanSpike_forWeb = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
			tableGrob(dietplan.subtitle5,theme=subtitle2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
			tableGrob(dietplan.tabtitle5,theme=tabtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#3
			dietplanT_spike.head.Wf, #10,11,12 -> #4
			dietplanT_spike.all$tab.Wf), #13 -> #5
			nrow=6, ncol=5, layout_matrix=rbind(c(1,1,1,1,1),c(2,2,2,2,2),c(3,3,3,3,3),NA,c(4,4,4,NA,NA),c(5,5,5,NA,NA)),
			widths=unit(c(280,18,280,18,280),'points'),heights=unit(c(57,28.5,21.5,29,28,tmpH),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanSpike_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanSpike.png',sep='_'))

	} else {

		CairoPNG(filename=paste(reportIdx,'Web_DietPlanSpike.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=143/0.75, units='px',dpi=96)

		LibreReport_DietPlanSpike_forWeb = try(grid.arrange(
            grobs=list(tableGrob(dietplan.title,theme=pagetitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
			tableGrob(dietplan.subtitle5,theme=subtitle2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
			tableGrob(dietplan.tabtitle5,theme=tabtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#3
			textGrob('분석기간 중 혈당을 높인 식사기록이 없습니다.',
				gp=gpar(fontsize=12,fontface='plain',col='#333333',fontfamily='NotoSansCJKkrB'),hjust=0.5,vjust=0.5)), #4 
			nrow=5, ncol=5, layout_matrix=rbind(c(1,1,1,1,1),c(2,2,2,2,2),c(3,3,3,3,3),NA,c(4,4,4,4,4)),
			widths=unit(c(280,18,280,18,280),'points'),heights=unit(c(57,28.5,21.5,6,30),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanSpike_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(reportIdx,'Web_DietPlanSpike.png',sep='_'))		
	}

		### DAILYLOG
		DAILYlog.tmp = try(dailyLog_Record_onW(daysAZ=unique(AGPdata$date),data=AGPdata,stat=SPIKEdata.tmp$stat,baseGlu=summaryValue$meanGlu,FPG.L=70,PPG.U),silent=T)
		if ( class(DAILYlog.tmp)=='try-error' ) {
			errCode = c(errCode,'50900')
			break
		}
		DAILYlog.out = try(dailyLog_Print_onW(reportIdx,daysAZ=unique(AGPdata$date),plot=DAILYlog.tmp$plot,spikeN=DAILYlog.tmp$spikeN,memo=DAILYlog.tmp$memo),silent=T) # app 용 plot 사이즈 키우기 #todo
		if ( class(DAILYlog.out)=='try-error' ) {
			errCode = c(errCode,'51900')
			break
		} else {
			outFileNames = c(outFileNames,DAILYlog.out$fileName.tmp)
			summaryValue$spikeN = sum(unlist(DAILYlog.tmp$spikeN))
		}	
		## 임시 (230407, 임시 방편 )
		
		CairoPNG(filename=paste(reportIdx,'App_DailyLog.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(sum(c(67,14,72,48,dailygp$out.Plot[[2]][[2]])*0.75),90,DAILYlog.out$out.H.A,90,sum(c(c(110,14,67,14,72,48)*0.75,tmpH.all)))/0.75),units='px',dpi=96)
		
		LibreReport_temp_forApp = try(grid.arrange(
			grobs=list(
				LibreReport_DailyGP_forApp, #1
				DAILYlog.out$LibreReport_DailyLog_forApp, #2
				LibreReport_DietPlanSpike_forApp #3
			),
			nrow=5, ncol=1, layout_matrix=rbind(1,NA,2,NA,3),
			width=unit(640*0.75,'points'), heights=unit(c(sum(c(67,14,72,48,dailygp$out.Plot[[2]][[2]])*0.75),90,DAILYlog.out$out.H.A,90,sum(c(c(110,14,67,14,72,48)*0.75,tmpH.all))),'points')),silent=T)
		dev.off()

		if ( all(class(LibreReport_temp_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(reportIdx,'App_DailyLog.png',sep='_'))

    ##
    } ## end of method spike 


	## summaryPlot ##

	CairoPNG(filename=paste(reportIdx,'summaryPlot.png',sep='_'),family='NotoSansCJKkrR',scale=1/0.32,bg='white',width=700,height=300,units='px',dpi=96)

	AGP_forSum = try(grid.arrange(grobs=list(page1.tmp$out.Plot_21_forWeb),
					nrow=1,ncol=1,layout_matrix=matrix(1),widths=unit(525,'points'),heights=unit(225,'points')),silent=T)
	dev.off()

	if ( all(class(AGP_forSum)!='try-error') ) {
		summaryPlot = paste(reportIdx,'summaryPlot.png',sep='_')
	} else {
		summaryPlot = NULL
	}
	summaryValue$outFileNames = summaryPlot
	
	
#	print(paste('-------- running time: ',round(difftime(Sys.time(),strt.time,units='sec')),' sec --------',sep=''))
	out.Result.tmp = list(errCode=errCode, outFileNames_Web=outFileNames[grep('Web_',outFileNames)], outFileNames_App=outFileNames[grep('App_',outFileNames)],
	summaryValue = unbox(summaryValue))

	out.Result = toJSON(out.Result.tmp, pretty=T)
	end.tmp = Sys.time()
	print(paste('processing Time :',as.numeric(round(difftime(end.tmp,strt.tmp),2),'secs')))
	return( result = out.Result )

}