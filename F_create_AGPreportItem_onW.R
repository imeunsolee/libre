##########################################
## create_AGPreportItem
## ---------------------------------------
## input 
## . data : 상위함수에서 가져옴
## . unit.glucose : 리브레 혈당 측정 단위 'mg.dl' 'mmol.mol'
## . Target='T2DM' option : T2DM T1DM T1GDM T2GDM GDM HighRiskDM preDM
## ---------------------------------------

create_AGPreportItem_onW = function( data, unit.glucose='mg.dl', Target='T2DM', Target.text,
	TIR.Goal=NULL,TBR_lev2.Cut=NULL,TBR_lev2.Goal=NULL,TBR_lev1.Cut=NULL,TBR_lev1.Goal=NULL,TAR_lev1.Cut=NULL,TAR_lev1.Goal=NULL,TAR_lev2.Cut=NULL,TAR_lev2.Goal=NULL){

	### step0 =============================================================================##
	## Target 및 혈당 목표값 설정
	GlucoseCut = rep(NA,4)
	GlucoseCut.goal = rep(NA,5)
	if(!is.null(TBR_lev2.Cut)) GlucoseCut[1] = TBR_lev2.Cut
	if(!is.null(TBR_lev1.Cut)) GlucoseCut[2] = TBR_lev1.Cut
	if(!is.null(TAR_lev1.Cut)) GlucoseCut[3] = (TAR_lev1.Cut+1)
	if(!is.null(TAR_lev2.Cut)) GlucoseCut[4] = (TAR_lev2.Cut+1)

	if(!is.null(TBR_lev2.Goal)) GlucoseCut.goal[1] = TBR_lev2.Goal/100
	if(!is.null(TBR_lev1.Goal)) GlucoseCut.goal[2] = TBR_lev1.Goal/100
	if(!is.null(TIR.Goal)) GlucoseCut.goal[3] = TIR.Goal/100
	if(!is.null(TAR_lev1.Goal)) GlucoseCut.goal[4] = TAR_lev1.Goal/100
	if(!is.null(TAR_lev2.Goal)) GlucoseCut.goal[5] = TAR_lev2.Goal/100

	## GlucoseCut 의 순서가 제대로 배열되었는지 확인 후 경고 문구 출력 
#	if( any(sort(GlucoseCut) != GlucoseCut[!is.na(GlucoseCut)]) ) {
#		cat(paste('[경고] 지정된 혈당 수준값에 오류가 있습니다 :\n      ',
#			paste(GlucoseCut[!is.na(GlucoseCut)][which(sort(GlucoseCut)!=GlucoseCut[!is.na(GlucoseCut)])],collapse=' < '),
#			' 의 로직이 맞도록 수정이 필요합니다.\n',sep=''))
#		if( GlucoseCut[2]==(GlucoseCut[3]-1) ){
#			cat('      ','목표범위 혈당 수준의 하한값과 상한값이 동일하여 수정이 필요합니다.\n')
#		}
#	} else if( GlucoseCut[2]==(GlucoseCut[3]-1) ){
#		cat(paste('[경고] 지정된 혈당 수준값에 오류가 있습니다 :\n      ',
#			'목표범위 혈당 수준의 하한값과 상한값이 동일하여 수정이 필요합니다.\n',sep=''))
#	}


	### step1 =============================================================================##
	## (1단, 좌측)                   -------------------## 
	## 혈당 통계 및 목표값 생성

	## 항목1 분석기간 ---
	out.value1 = range(data$date)

	## 항목2 분석일수 ---
	out.value2 = as.numeric(diff.Date(out.value1)+1)

	## 항목3 %시간 CGM활성화비율 --- ###mod
	## 1-2초씩 엇갈리는 경우 있어 dateandtime 분단위로 조정 (반올림)
	AllTimeCut = as.POSIXct(seq.POSIXt(min(data$dateandtime),max(data$dateandtime),by='15 min'),format='%Y-%m-%d %H:%M:%S')
	data.tmp = data
	CGMactive=rep(NA,length(AllTimeCut))
	for(i in 1:length(AllTimeCut)){
		if(i!=length(AllTimeCut)){
			CGMactive[i] = ifelse(any(AllTimeCut[i]<= round(data.tmp$dateandtime,unit='min') & AllTimeCut[i+1]> round(data.tmp$dateandtime,unit='min')),1,0)
		} else {
			CGMactive[i] = ifelse(any(AllTimeCut[i]<= round(data.tmp$dateandtime,unit='min')),1,0)
		}	
	}
	out.value3 = sum(CGMactive)/length(CGMactive)*100#paste( round( sum(CGMactive)/length(CGMactive)*100,2),'%',sep='' )
	rm(data.tmp)

	## 항목4 평균혈당 ---
	out.value4 = mean(data[which(data$log==1),]$glucose)

	## 항목5 GMI or eA1c ---
	eA1ceqn = function(x,unit.glucose){
		if(unit.glucose=='mg.dl'){
			y = (x+46.7)/28.7 
		} else if(unit.glucose=='mmol.mol'){
			y = (x+2.59)/1.59 
		}
		return(y)
	}
	GMIeqn = function(x,unit.glucose) {
		if(unit.glucose=='mg.dl'){
			y = 3.31 + 0.02392*x 
		} else if(unit.glucose=='mmol.mol'){
			y = 12.71 + 4.70587*x
		}

		return(y)
	}

	out.value5 = GMIeqn(mean(data[which(data$log==1),]$glucose),unit.glucose=unit.glucose)#GMI

	## 항목6 혈당변동성 ---
	glycemicVariability = function(x,method){
		if(method=='%cv'){
			GV = sd(x)/mean(x)
			GV.assesment = 36
		} else if(method=='sd'){
			GV = sd(x)
			GV.assesment = mean(x)/3
		}

		return(list(GV=GV,GV.assesment=GV.assesment))
	}

	out.value6_1 = glycemicVariability(x=data[which(data$log==1),]$glucose,method='%cv')$GV*100
	out.value6_2 = glycemicVariability(x=data[which(data$log==1),]$glucose,method='%cv')$GV.assesment


	## 라벨 생성 ---
	lab.add = c()
	for(i in 1:(max(which(!is.na(GlucoseCut)))+1)){
		if(!is.na(GlucoseCut[i])){
			if(GlucoseCut[i]==min(GlucoseCut,na.rm=T)){
				lab.add[i] = paste('＜',GlucoseCut[i],' mg/dL',sep='')
			} else {
				lab.add[i] = paste(GlucoseCut[i-1],'-',(GlucoseCut[i]-1),'mg/dL')
#				break.tmp[i-1] = GlucoseCut[i-1]
			}
		} else if(is.na(GlucoseCut[i])){
			if((i-1) %in% c(0,which(is.na(GlucoseCut)))){
				next
			}
			if(GlucoseCut[i-1]==max(GlucoseCut,na.rm=T)){
				lab.add[i] = paste('＞',(GlucoseCut[i-1]-1),' mg/dL',sep='')
#				break.tmp[i-1] = GlucoseCut[i-1]-1
			}
		}
	}


	## table 생성 ---

	out.Tab_11_source1a = data.frame(matrix(NA,nrow=4,ncol=2))
	out.Tab_11_source1a[1,1] = ''
	out.Tab_11_source1a[1,2] = ''
	out.Tab_11_source1a[2,1] = paste(format(out.value1,'%Y년 %m월 %d일'),collapse=' - ')
	out.Tab_11_source1a[2,2] = paste(out.value2,'일',sep='')
	out.Tab_11_source1a[3,1] = '%시간 CGM이 활성 상태임'
	out.Tab_11_source1a[3,2] = paste(round(out.value3),'%',sep='') 
	out.Tab_11_source1a[4,1] = ''
	out.Tab_11_source1a[4,2] = ''
	out.Tab_11_source1a.theme = ttheme_minimal( base_family = 'NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#122747',fontsize=c(1,17,17,1),fontface='bold'))
	)
	out.Tab_11_source1b = data.frame(x=paste('※',Target.text,'대상자의 혈당 참고수치',sep=' '))
	out.Tab_11_source1b.theme = ttheme_minimal( base_family = 'NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=18,fontface='bold'))
	)
	out.Tab_11_source1c = data.frame(matrix(NA,nrow=6,ncol=2))
	out.Tab_11_source1c[1,1] = '혈당 범위'
	out.Tab_11_source1c[1,2] = '목표값 수치의%(시간/일)'
	out.Tab_11_source1c[2,1] = paste('목표 범위',gsub(' - ','-',lab.add[3]),sep='')
	out.Tab_11_source1c[2,2] = paste(GlucoseCut.goal[3]*100,'%보다 큼 (',floor(GlucoseCut.goal[3]*24),'시간',round(((GlucoseCut.goal[3]*24)%%1)*60),'분)',sep='')
	out.Tab_11_source1c[3,1] = paste(GlucoseCut[2],' mg/dL 미만',sep='')
	out.Tab_11_source1c[3,2] = paste(GlucoseCut.goal[2]*100,'%보다 작음 (',
		ifelse(floor(GlucoseCut.goal[2]*24)>0,paste(floor(GlucoseCut.goal[2]*24),'시간',sep=''),''),
		round(((GlucoseCut.goal[2]*24)%%1)*60),'분)',sep='')
	out.Tab_11_source1c[4,1] = ifelse(!is.na(GlucoseCut[1]),paste(GlucoseCut[1],' mg/dL 미만',sep=''),'')
	out.Tab_11_source1c[4,2] = ifelse(!is.na(GlucoseCut[1]),paste(GlucoseCut.goal[1]*100,'%보다 작음 (',
		ifelse(floor(GlucoseCut.goal[1]*24)>0,paste(floor(GlucoseCut.goal[1]*24),'시간',sep=''),''),
		round(((GlucoseCut.goal[1]*24)%%1)*60),'분)',sep=''),'')
	out.Tab_11_source1c[5,1] = paste(GlucoseCut[3]-1,' mg/dL 초과',sep='')
	out.Tab_11_source1c[5,2] = paste(GlucoseCut.goal[4]*100,'%보다 작음 (',
		ifelse(floor(GlucoseCut.goal[4]*24)>0,paste(floor(GlucoseCut.goal[4]*24),'시간',sep=''),''),
		round(((GlucoseCut.goal[4]*24)%%1)*60),'분)',sep='')
	out.Tab_11_source1c[6,1] = ifelse(!is.na(GlucoseCut[4]),paste(GlucoseCut[4]-1,' mg/dL 초과',sep=''),'')
	out.Tab_11_source1c[6,2] = ifelse(!is.na(GlucoseCut.goal[5]),paste(GlucoseCut.goal[5]*100,'%보다 작음 (',
		ifelse(floor(GlucoseCut.goal[5]*24)>0,paste(floor(GlucoseCut.goal[5]*24),'시간',sep=''),''),
		round(((GlucoseCut.goal[5]*24)%%1)*60),'분)',sep=''),'')
	out.Tab_11_source1c.theme = ttheme_minimal( base_family = 'NotoSansCJKkrR',
		core = list(bg_params=list(fill='#e3e3e3',col='#e3e3e3'),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=c(17,rep(17,5)),fontface=c('bold',rep('plain',5))))
	)

	out.Tab_11_source2 =  ifelse(GlucoseCut[2]==70 & GlucoseCut[3]==181, paste('범위 (',gsub(' - ','-',lab.add[3]),')에서 시간의 각 5% 증가율은 임상적으로 유익합니다.',sep=''),'')
	out.Tab_11_source2.theme = ttheme_minimal( base_family = 'NotoSansCJKkrR',
		core = list(bg_params=list(fill='#e3e3e3',col='#e3e3e3'),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=c(15),fontface=c('plain')))
	)

	out.Tab_11_source3 = data.frame(matrix(NA,nrow=4,ncol=2))
	out.Tab_11_source3[1,1] = ''
	out.Tab_11_source3[1,2] = ''
	out.Tab_11_source3[2,1] = ''
	out.Tab_11_source3[2,2] = ''
	out.Tab_11_source3[3,1] = '평균 혈당'
	out.Tab_11_source3[3,2] = paste(round(out.value4),' mg/dL',sep='')
	out.Tab_11_source3[4,1] = '혈당 관리 표시 (GMI)'
	out.Tab_11_source3[4,2] = paste(format(round(out.value5,1),nsmall=1),'%',sep='')
	if( Target=='HighRiskDM' ) {
		eA1c.target = 7.5 #미만
	} else if( Target=='T1DM' ) {
		eA1c.target = 7 # 미만
	} else if ( Target=='T2DM' ) {
		eA1c.target = 6.5 # 미만
	} else if ( Target %in% c('GDM','T1GDM','T2GDM')) {
		eA1c.target = 6 # 미만
	} else if ( Target %in% c('preDM','DIET') ) {
		eA1c.target = 5.601 # 5.6이하
	}
	out.value5.col = ifelse(out.value5>=eA1c.target,'#fe4600','#122747') # 혈당관리지표 또는 당화혈색소 값이 목표 벗어날 경우 강조처리
	out.Tab_11_source3.theme = ttheme_minimal( base_family = 'NotoSansCJKkrR',
		core = list(bg_params=list(fill=c('#e3e3e3',NA,NA,NA),col=c('#e3e3e3',NA,NA,NA)),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col=c(rep('#122747',3),out.value5.col),fontsize=c(1,1,20,20),fontface=rep('bold',4)))
	)

	out.Tab_11_source4 = '측정된 평균 당 수치로 A1c수치가 어느 정도 일지 예상할 수 있습니다.'
	out.Tab_11_source4.theme = ttheme_minimal( base_family = 'NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=c(17),fontface=c('plain')))
	)

	out.Tab_11_source5 = data.frame(matrix(NA,nrow=1,ncol=2))
	out.Tab_11_source5[1,1] = '혈당 변동성'
	out.Tab_11_source5[1,2] = paste(format(round(out.value6_1,1),nsmall=1),'%',sep='')
	out.value6_1.col = ifelse(out.value6_1>36,'#fe4600','#122747')
	out.Tab_11_source5.theme = ttheme_minimal( base_family = 'NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col=out.value6_1.col,fontsize=c(20),fontface=c('bold')))
	)

	out.Tab_11_source6 = data.frame(matrix(NA,nrow=2,ncol=1))
	out.Tab_11_source6[1,1] = paste('백분율 변동 계수(%CV)로 정의됨, 목표값≤',out.value6_2,'%',sep='') 
	out.Tab_11_source6[2,1] = '혈당 측정값이 평균 당 수치로부터 얼마나 떨어져 있는지를 나타냅니다.'
	out.Tab_11_source6.theme = ttheme_minimal( base_family = 'NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col=c('#122747','#4d5262'),fontsize=rep(17,2),fontface=rep('plain',2)))
	)


	### step2 =============================================================================##
	## (1단, 우측)                   -------------------## 
	## 범위내 시간 그래프 생성

	## 값 계산 ---
	GlucoseCutIn = vector('numeric',5)

	GlucoseCutIn[1] = ifelse(!is.na(GlucoseCut[1]),sum(data[which(data$log==1),]$glucose<GlucoseCut[1]),NA)
	GlucoseCutIn[2] = ifelse(!is.na(GlucoseCut[1]) & !is.na(GlucoseCut[2]),
		sum(data[which(data$log==1),]$glucose>=GlucoseCut[1] & data[which(data$log==1),]$glucose<GlucoseCut[2]),
		ifelse(is.na(GlucoseCut[1]) & !is.na(GlucoseCut[2]),
			sum(data[which(data$log==1),]$glucose<GlucoseCut[2]),NA))
	GlucoseCutIn[3] = ifelse(!is.na(GlucoseCut[2]) & !is.na(GlucoseCut[3]),
		sum(data[which(data$log==1),]$glucose>=GlucoseCut[2] & data[which(data$log==1),]$glucose<GlucoseCut[3]),NA)
	GlucoseCutIn[4] = ifelse(!is.na(GlucoseCut[3]) & !is.na(GlucoseCut[4]),
		sum(data[which(data$log==1),]$glucose>=GlucoseCut[3] & data[which(data$log==1),]$glucose<GlucoseCut[4]),
		ifelse(!is.na(GlucoseCut[3]) & is.na(GlucoseCut[4]),
			sum(data[which(data$log==1),]$glucose>=GlucoseCut[3]),NA))
	GlucoseCutIn[5] = ifelse(!is.na(GlucoseCut[4]),sum(data[which(data$log==1),]$glucose>=GlucoseCut[4]),NA)
	names(GlucoseCutIn) = c('TBR2','TBR1','TIR','TAR1','TAR2')

	break.tmp = GlucoseCut
	break.tmp[3:4] = GlucoseCut[3:4]-1

	## 그래프 소스 생성 ---

	out.Plot_12_source = data.frame(x=1,gr=factor(names(GlucoseCutIn),levels=names(GlucoseCutIn)),y=GlucoseCutIn/sum(GlucoseCutIn,na.rm=T))
	out.Plot_12_source$y = 100*out.Plot_12_source$y

	# 목표이탈시 폰트color 강조 #
	out.Plot_12_source$color.tmp = NA 
	if(!is.null(TAR_lev2.Goal)) {
		out.Plot_12_source$color.tmp[out.Plot_12_source$gr=='TAR2'] = ifelse(out.Plot_12_source$y[out.Plot_12_source$gr=='TAR2'] >= TAR_lev2.Goal && TAR_lev2.Goal!=0,'#fe4600','#122747')
	} 
	if(!is.null(TAR_lev1.Goal)) {
#		out.Plot_12_source$color.tmp[grepl('TAR',out.Plot_12_source$gr)] = ifelse(sum(out.Plot_12_source$y[grepl('TAR',out.Plot_12_source$gr)]) >= TAR_lev1.Goal,'#fe4600','#122747')
		out.Plot_12_source$color.tmp[out.Plot_12_source$gr=='TAR1'] = ifelse(sum(out.Plot_12_source$y[grepl('TAR',out.Plot_12_source$gr)],na.rm=T) >= TAR_lev1.Goal,'#fe4600','#122747')
	}
	if(!is.null(TBR_lev2.Goal)) {
		out.Plot_12_source$color.tmp[out.Plot_12_source$gr=='TBR2'] = ifelse(out.Plot_12_source$y[out.Plot_12_source$gr=='TBR2'] >= TBR_lev2.Goal && TBR_lev2.Goal!=0,'#fe4600','#122747')
	}
	if(!is.null(TBR_lev1.Goal)) {
#		out.Plot_12_source$color.tmp[grepl('TBR',out.Plot_12_source$gr)] = ifelse(sum(out.Plot_12_source$y[grepl('TBR',out.Plot_12_source$gr)]) >= TBR_lev1.Goal,'#fe4600','#122747')
		out.Plot_12_source$color.tmp[out.Plot_12_source$gr=='TBR1'] = ifelse(sum(out.Plot_12_source$y[grepl('TBR',out.Plot_12_source$gr)],na.rm=T) >= TBR_lev1.Goal,'#fe4600','#122747')
	}
	if(!is.null(TIR.Goal)) {
		out.Plot_12_source$color.tmp[out.Plot_12_source$gr=='TIR'] = ifelse(out.Plot_12_source$y[out.Plot_12_source$gr=='TIR'] < TIR.Goal,'#fe4600','#122747')
	}


	out.Plot_12_source$lab = ifelse(grepl('TAR',out.Plot_12_source$gr),'높음',ifelse(grepl('TBR',out.Plot_12_source$gr),'낮음',ifelse(grepl('TIR',out.Plot_12_source$gr),'목표 범위',NA)))
	out.Plot_12_source$lab = ifelse(grepl(2,out.Plot_12_source$gr),paste('매우',out.Plot_12_source$lab),out.Plot_12_source$lab)
#	out.Plot_12_source$labadd = paste(out.Plot_12_source$lab,'\n',lab.add,sep='') # temp 
	lab.add[5] = ifelse(is.na(lab.add[5]),NA,lab.add[5])
	out.Plot_12_source$labadd = paste('\n',lab.add,sep='')
	out.Plot_12_source$ytext = paste(round(out.Plot_12_source$y),'%',sep='')
	# 0일경우 조정
	out.Plot_12_source$y = ifelse(out.Plot_12_source$y<5,4,out.Plot_12_source$y)
	out.Plot_12_source$yaxis = cumsum(ifelse(is.na(out.Plot_12_source$y),0,out.Plot_12_source$y))

	guid.tmp = range(which(!is.na(GlucoseCutIn)))
	guid.tmp[2]= guid.tmp[2]-1
	guid.tmp = seq(from=out.Plot_12_source$yaxis[guid.tmp[1]],to=out.Plot_12_source$yaxis[guid.tmp[2]],length=diff(guid.tmp)+1)
	guid.tmp = c(min(guid.tmp)-15,guid.tmp)
	guid.tmp[1] = ifelse(guid.tmp[1]>0,0,guid.tmp[1])
	out.Plot_12_source = out.Plot_12_source[!is.na(out.Plot_12_source$y),]

	out.Plot_12 = ggplot(out.Plot_12_source, aes(x=x,y=y,fill=factor(gr,levels=rev(out.Plot_12_source$gr))))+
						scale_fill_manual(values=c('TBR2'='#b6202e','TBR1'='#d71920','TIR'='#40ac49','TAR1'='#fff100','TAR2'='#fcb813'))+
						scale_y_continuous(name='',breaks=out.Plot_12_source$yaxis[-dim(out.Plot_12_source)],labels=break.tmp[!is.na(break.tmp)])+
						geom_text(mapping=aes(x=1.3,y=(guid.tmp+11),label=lab),hjust=0,size=7,fontface='bold',color='#122747',family='NotoSansCJKkrR')+
						geom_text(mapping=aes(x=1.3,y=(guid.tmp+7.2),label=labadd),hjust=0,size=7,fontface='plain',color='#122747',family='NotoSansCJKkrR')+
						geom_text(mapping=aes(x=2,y=(guid.tmp+5),label=ytext),hjust=1,size=10,fontface='bold',color=out.Plot_12_source$color.tmp,family='NotoSansCJKkrR')+
						geom_hline(yintercept=guid.tmp,color='#767e89',lty='dashed',size=0.3)+
						geom_bar(stat='identity',width=0.4,position='stack')+

						theme(panel.background=element_rect(fill=NA), #fill='#ffffff' 
							legend.position='none',
							axis.ticks.x=element_blank(), axis.title.x = element_blank(), axis.text.x=element_blank(), 
							axis.ticks.y=element_blank(), axis.text.y = element_text(vjust=c(1,1,1,0), size=18, color='#4d5262', family='NotoSansCJKkrR'),
							plot.margin=margin(17,0,17,0,unit='points'))

	out.Plot_12_forWeb = ggplot(out.Plot_12_source, aes(x=x,y=y,fill=factor(gr,levels=rev(out.Plot_12_source$gr))))+
						scale_fill_manual(values=c('TBR2'='#b6202e','TBR1'='#d71920','TIR'='#40ac49','TAR1'='#fff100','TAR2'='#fcb813'))+
						scale_y_continuous(name='',breaks=out.Plot_12_source$yaxis[-dim(out.Plot_12_source)],labels=break.tmp[!is.na(break.tmp)])+
						geom_text(mapping=aes(x=1.3,y=(guid.tmp+11),label=lab),hjust=0,size=5.2,fontface='bold',color='#122747',family='NotoSansCJKkrR')+
						geom_text(mapping=aes(x=1.3,y=(guid.tmp+7.2),label=labadd),hjust=0,size=5.2,fontface='plain',color='#122747',family='NotoSansCJKkrR')+
						geom_text(mapping=aes(x=2,y=(guid.tmp+5),label=ytext),hjust=1,size=6.8,fontface='bold',color=out.Plot_12_source$color.tmp,family='NotoSansCJKkrR')+
						geom_hline(yintercept=guid.tmp,color='#767e89',lty='dashed',size=0.3)+
						geom_bar(stat='identity',width=0.4,position='stack')+

						theme(panel.background=element_rect(fill=NA), #fill='#ffffff' 
							legend.position='none',
							axis.ticks.x=element_blank(), axis.title.x = element_blank(), axis.text.x=element_blank(), 
							axis.ticks.y=element_blank(), axis.text.y = element_text(vjust=c(1,1,1,0), size=12, color='#4d5262', family='NotoSansCJKkrR'),
							plot.margin=margin(17,0,17,0,unit='points'))


	### step3 =============================================================================##
	## (2단, 전체(중앙))                   -------------------##
	## 24시간 연속 혈당 프로필 그래프 생성

	## 15분 간격 통계량 계산 ---

	data$timef = as.POSIXct(strptime(data$time, format = '%H:%M:%S'),tz='GMT')
	HMScut=strptime(format(seq.POSIXt(strptime('00:00:00',format='%H:%M:%S'),strptime('23:59:59',format='%H:%M:%S'),by='15 min'),format='%H:%M:%S'),format='%H:%M:%S',tz='GMT')
	data$timeCut1 = NA 
	# 분단위로 반올림
	for(i in 1:nrow(data)) data$timeCut1[i] = sum(round(data$timef[i],unit='min')>=HMScut)

	# timeCut1 단위당 glucose 의 0.05, 0.25, 0.5, 0.75, 0.95 quantile # 
	tmp = tapply(data[which(data$log==1),]$glucose,
		data[which(data$log==1),]$timeCut1,
		function(x){quantile(x,probs=c(0.05,0.25,0.5,0.75,0.95))})
	AGPbyTime = as.data.frame(matrix(unlist(tmp),nrow=length(tmp),ncol=5,byrow=T))
	## ver. 2021-02-26
	tmpRMIdx = setdiff(c(1:length(HMScut)),as.numeric(as.vector(names(tmp))))
	if(length(tmpRMIdx)==0) {
		AGPbyTime$time = as.POSIXct(HMScut,format='%H:%M:%S',tz='GMT') # 0-15분 : 0분 
	} else if (length(tmpRMIdx)>0){
		AGPbyTime$time = as.POSIXct(HMScut[-tmpRMIdx],format='%H:%M:%S',tz='GMT') # 0-15분 : 0분 
	
		AGPbyTime.add = as.data.frame(matrix(NA,nrow=length(tmpRMIdx),ncol=5,byrow=T))
		AGPbyTime.add$time = as.POSIXct(HMScut[tmpRMIdx],format='%H:%M:%S',tz='GMT')
		AGPbyTime = rbind(AGPbyTime,AGPbyTime.add)
	}
	#	AGPbyTime$time = as.POSIXct(HMScut,format='%H:%M:%S',tz='GMT')+15*60 # 0-15분 : 15분
	colnames(AGPbyTime) = c('Q05','Q25','Q50','Q75','Q95','time')

	AGPbyTime = AGPbyTime[order(AGPbyTime$time),]

	## smooth line ---
	out.Plot_21_source = AGPbyTime
	out.Plot_21_source$index = 1:nrow(out.Plot_21_source)

	loessModQ05 = loess(Q05~index,data=out.Plot_21_source,span=0.1)
	out.Plot_21_source$Q05.s = predict(loessModQ05,out.Plot_21_source$index)
	loessModQ25 = loess(Q25~index,data=out.Plot_21_source,span=0.1)
	out.Plot_21_source$Q25.s = predict(loessModQ25,out.Plot_21_source$index)
	loessModQ50 = loess(Q50~index,data=out.Plot_21_source,span=0.1)
	out.Plot_21_source$Q50.s = predict(loessModQ50,out.Plot_21_source$index)
	loessModQ75 = loess(Q75~index,data=out.Plot_21_source,span=0.1)
	out.Plot_21_source$Q75.s = predict(loessModQ75,out.Plot_21_source$index)
	loessModQ95 = loess(Q95~index,data=out.Plot_21_source,span=0.1)
	out.Plot_21_source$Q95.s = predict(loessModQ95,out.Plot_21_source$index)

## add line : 24:00:00  
	addnextline =  out.Plot_21_source[1,]
	addnextline$time = as.POSIXct(strptime(paste(as.Date(data$timef)[1]+1,'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'))
	addnextline$index = 97
	out.Plot_21_source = rbind(out.Plot_21_source,addnextline)

	## Target별 ymin, ymax
	ymin.Target = GlucoseCut[2] # <- 70
	ymax.Target = (GlucoseCut[3]-1)# <- 180

	## graph with smooth line ---
	out.Plot_21_source.ylim = ifelse(max(out.Plot_21_source$Q95.s,na.rm=T)>350,ifelse(max(out.Plot_21_source$Q95.s,na.rm=T)>400,ifelse(max(out.Plot_21_source$Q95.s,na.rm=T)>450,500,450),400),350)

	out.Plot_21 = ggplot(out.Plot_21_source)+
		geom_vline(xintercept=as.POSIXct(strptime(c('03:00','06:00','09:00','15:00','18:00','21:00'),format='%H:%M'),tz='GMT'),color='#bcbec0',lty=2,size=0.2)+
		geom_vline(xintercept=as.POSIXct(strptime('12:00',format='%H:%M'),tz='GMT'),color='#bcbec0',size=0.3)+

		# 05-95
		geom_line(aes(x=time,y=Q05.s),lty=2,col='#a2b5d4',size=0.5)+
		geom_line(aes(x=time,y=Q95.s),lty=2,col='#a2b5d4',size=0.5)+
		geom_ribbon(aes(x=time,ymin=Q05.s,ymax=Q95.s),fill='#dfe3ed',alpha=0.9)+
		# 25-75
		geom_ribbon(aes(x=time,ymin=Q25.s,ymax=Q75.s),fill='#a2b5d4')+
		# median
		geom_line(aes(x=time,y=Q50.s),col='#1f50ce',size=1)+
		# scales
		scale_y_continuous(name='',limits=c(0,out.Plot_21_source.ylim),breaks=setdiff(c(break.tmp,350,out.Plot_21_source.ylim),c(ymin.Target,ymax.Target)),labels=setdiff(c(break.tmp,350,out.Plot_21_source.ylim),c(ymin.Target,ymax.Target)))+
		scale_x_datetime(name='',date_labels='%H:%M',
			breaks=as.POSIXct(seq.POSIXt(min(out.Plot_21_source$time),max(out.Plot_21_source$time),by='3 hours')))+
		geom_hline(yintercept=c(ymin.Target,ymax.Target),color='#40ac49')+
		coord_cartesian(ylim=c(0,out.Plot_21_source.ylim),xlim=c(min(out.Plot_21_source$time),max(out.Plot_21_source$time)),expand=F,clip='off')+

		theme(panel.background=element_rect(fill=NA,color=NA), # fill='#ffffff'
			panel.border=element_rect(colour='#231f20',fill=NA,size=0.4),
			axis.text.y=element_text(color='#231f20',size=18,face='plain'),axis.ticks.y=element_line(size=0.3),
			axis.text.x=element_text(color=c('#231f20',rep('#808285',3),'#231f20',rep('#808285',3),'#231f20'),size=16,hjust=0.5,vjust=-3,face='plain'),axis.ticks.x=element_blank(),
			text=element_text(family='NotoSansCJKkrR'))
		
	## last value line ---

	last_value = out.Plot_21_source[max(setdiff(c(1:96),tmpRMIdx)),]
	last_date = strptime(paste(as.Date(data$timef)[1]+1,'01:30:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT')

	q25.ytmpA = ifelse(last_value$Q25.s>(last_value$Q05.s+10),last_value$Q25.s,(last_value$Q05.s+10))
	q50.ytmpA = ifelse(last_value$Q50.s>(q25.ytmpA+10),last_value$Q50.s,(q25.ytmpA+10))
	q75.ytmpA = ifelse(last_value$Q75.s>(q50.ytmpA+10),last_value$Q75.s,(q50.ytmpA+10))
	q95.ytmpA = ifelse(last_value$Q95.s>(q75.ytmpA+10),last_value$Q95.s,(q75.ytmpA+10))

	q25.ytmpW = ifelse(last_value$Q25.s>(last_value$Q05.s+20),last_value$Q25.s,(last_value$Q05.s+20))
	q50.ytmpW = ifelse(last_value$Q50.s>(q25.ytmpW+20),last_value$Q50.s,(q25.ytmpW+20))
	q75.ytmpW = ifelse(last_value$Q75.s>(q50.ytmpW+20),last_value$Q75.s,(q50.ytmpW+20))
	q95.ytmpW = ifelse(last_value$Q95.s>(q75.ytmpW+20),last_value$Q95.s,(q75.ytmpW+20))

	out.Plot_21 = out.Plot_21+theme(plot.margin=unit(c(28.35,42.52,0,14.17),'points')) 
	out.Plot_21_forApp = out.Plot_21+
		annotation_custom(grob=textGrob(expression('95%'),gp=gpar(fontsize=18,col='#bcbec0',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=last_date,xmax=last_date,ymin=q95.ytmpA,ymax=q95.ytmpA)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#bcbec0',lwd=0.8,lty=2)),xmin=last_date-90*60,xmax=last_date-75*60,ymin=last_value$Q95.s,ymax=q95.ytmpA)+
		annotation_custom(grob=textGrob(expression('75%'),gp=gpar(fontsize=18,col='#a2b5d4',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=last_date,xmax=last_date,ymin=q75.ytmpA,ymax=q75.ytmpA)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#a2b5d4',lwd=0.8)),xmin=last_date-90*60,xmax=last_date-75*60,ymin=last_value$Q75.s,ymax=q75.ytmpA)+
		annotation_custom(grob=textGrob(expression('50%'),gp=gpar(fontsize=18,col='#1f50ce',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=last_date,xmax=last_date,ymin=q50.ytmpA,ymax=q50.ytmpA)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#1f50ce',lwd=1)),xmin=last_date-90*60,xmax=last_date-75*60,ymin=last_value$Q50.s,ymax=q50.ytmpA)+
		annotation_custom(grob=textGrob(expression('25%'),gp=gpar(fontsize=18,col='#a2b5d4',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=last_date,xmax=last_date,ymin=q25.ytmpA,ymax=q25.ytmpA)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#a2b5d4',lwd=0.8)),xmin=last_date-90*60,xmax=last_date-75*60,ymin=last_value$Q25.s,ymax=q25.ytmpA)+
		annotation_custom(grob=textGrob(expression('5%'),gp=gpar(fontsize=18,col='#bcbec0',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=last_date,xmax=last_date,ymin=last_value$Q05.s,ymax=last_value$Q05.s)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#bcbec0',lwd=0.8,lty=2)),xmin=last_date-90*60,xmax=last_date-75*60,ymin=last_value$Q05.s,ymax=last_value$Q05.s)+
		
		annotation_custom(grob=textGrob(ymin.Target,gp=gpar(fontsize=20,col='#231f20',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-120*60),xmax=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-120*60),ymin=ymin.Target,ymax=ymin.Target)+ 
		annotation_custom(grob=textGrob(ymax.Target,gp=gpar(fontsize=20,col='#231f20',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-120*60),xmax=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-120*60),ymin=ymax.Target,ymax=ymax.Target)+ 
		annotation_custom(grob=linesGrob(gp=gpar(col='#40ac49',lwd=1)),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-50*60),xmax=as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT'),ymin=ymin.Target,ymax=ymin.Target)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#40ac49',lwd=1)),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-50*60),xmax=as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT'),ymin=ymax.Target,ymax=ymax.Target) 

	out.Plot_21_forWeb = out.Plot_21+
		annotation_custom(grob=textGrob(expression('95%'),gp=gpar(fontsize=12,col='#bcbec0',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=last_date,xmax=last_date,ymin=q95.ytmpW,ymax=q95.ytmpW)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#bcbec0',lwd=0.8,lty=2)),xmin=last_date-90*60,xmax=last_date-40*60,ymin=last_value$Q95.s,ymax=q95.ytmpW)+
		annotation_custom(grob=textGrob(expression('75%'),gp=gpar(fontsize=12,col='#a2b5d4',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=last_date,xmax=last_date,ymin=q75.ytmpW,ymax=q75.ytmpW)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#a2b5d4',lwd=0.8)),xmin=last_date-90*60,xmax=last_date-40*60,ymin=last_value$Q75.s,ymax=q75.ytmpW)+
		annotation_custom(grob=textGrob(expression('50%'),gp=gpar(fontsize=12,col='#1f50ce',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=last_date,xmax=last_date,ymin=q50.ytmpW,ymax=q50.ytmpW)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#1f50ce',lwd=1)),xmin=last_date-90*60,xmax=last_date-40*60,ymin=last_value$Q50.s,ymax=q50.ytmpW)+
		annotation_custom(grob=textGrob(expression('25%'),gp=gpar(fontsize=12,col='#a2b5d4',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=last_date,xmax=last_date,ymin=q25.ytmpW,ymax=q25.ytmpW)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#a2b5d4',lwd=0.8)),xmin=last_date-90*60,xmax=last_date-40*60,ymin=last_value$Q25.s,ymax=q25.ytmpW)+
		annotation_custom(grob=textGrob(expression('5%'),gp=gpar(fontsize=12,col='#bcbec0',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=last_date,xmax=last_date,ymin=last_value$Q05.s,ymax=last_value$Q05.s)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#bcbec0',lwd=0.8,lty=2)),xmin=last_date-90*60,xmax=last_date-40*60,ymin=last_value$Q05.s,ymax=last_value$Q05.s)+
		
		annotation_custom(grob=textGrob(ymin.Target,gp=gpar(fontsize=13,col='#231f20',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-120*60),xmax=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-120*60),ymin=ymin.Target,ymax=ymin.Target)+ 
		annotation_custom(grob=textGrob(ymax.Target,gp=gpar(fontsize=13,col='#231f20',fontfamily='NotoSansCJKkrR',fontface='bold')),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-120*60),xmax=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-120*60),ymin=ymax.Target,ymax=ymax.Target)+ 
		annotation_custom(grob=linesGrob(gp=gpar(col='#40ac49',lwd=1)),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-50*60),xmax=as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT'),ymin=ymin.Target,ymax=ymin.Target)+
		annotation_custom(grob=linesGrob(gp=gpar(col='#40ac49',lwd=1)),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-50*60),xmax=as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT'),ymin=ymax.Target,ymax=ymax.Target) 


	### step4 =============================================================================##
	## (3단, 전체(중앙))                   -------------------## 
	## 일일 혈당 프로필 그래프 생성

	## 7일 간격으로 그래프 생성 ---
	days.tmp = seq.Date(min(unique(data$date)),min(unique(data$date))+13,1)
	out.Plot_31_source = data[which(data$log==1 &!is.na(data$glucose)),]
	for ( j in 1:14 ) {
		if ( length(setdiff(c(1:96),out.Plot_31_source[out.Plot_31_source$date==days.tmp[j],]$timeCut1))>0 ) {
			addIdx.time = as.POSIXct(paste(days.tmp[j], format(HMScut[setdiff(c(1:96),out.Plot_31_source[out.Plot_31_source$date==days.tmp[j],]$timeCut1)],'%H:%M:%S')), format='%Y-%m-%d %H:%M:%S',tz='GMT')
			Plot_31_source.addtmp = out.Plot_31_source[1:length(addIdx.time),]
			Plot_31_source.addtmp[1:length(addIdx.time),] = NA ; Plot_31_source.addtmp$dateandtime = addIdx.time; Plot_31_source.addtmp$date = as.Date(Plot_31_source.addtmp$dateandtime); # Plot_31_source.addtmp$time = format(addIdx.time,format='%H:%M:%S')

			out.Plot_31_source = rbind(out.Plot_31_source,Plot_31_source.addtmp)
		}
	}


	### output =============================================================================##

	return(list(
		out.Tab_11_source1a=out.Tab_11_source1a,out.Tab_11_source1b=out.Tab_11_source1b,out.Tab_11_source1c=out.Tab_11_source1c,
		out.Tab_11_source1a.theme=out.Tab_11_source1a.theme,out.Tab_11_source1b.theme=out.Tab_11_source1b.theme,out.Tab_11_source1c.theme=out.Tab_11_source1c.theme,		
#		out.Tab_11_source1=out.Tab_11_source1,out.Tab_11_source1.theme=out.Tab_11_source1.theme,
		out.Tab_11_source2=out.Tab_11_source2,out.Tab_11_source2.theme=out.Tab_11_source2.theme,
		out.Tab_11_source3=out.Tab_11_source3,out.Tab_11_source3.theme=out.Tab_11_source3.theme,
		out.Tab_11_source4=out.Tab_11_source4,out.Tab_11_source4.theme=out.Tab_11_source4.theme,
		out.Tab_11_source5=out.Tab_11_source5,out.Tab_11_source5.theme=out.Tab_11_source5.theme,
		out.Tab_11_source6=out.Tab_11_source6,out.Tab_11_source6.theme=out.Tab_11_source6.theme,
		out.Plot_12_source = out.Plot_12_source,
		out.Plot_12=out.Plot_12, out.Plot_21_forApp=out.Plot_21_forApp, out.Plot_31_source=out.Plot_31_source, out.Plot_12_forWeb=out.Plot_12_forWeb,out.Plot_21_forWeb=out.Plot_21_forWeb))


}