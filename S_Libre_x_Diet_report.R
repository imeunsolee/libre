##########################################
## reportStyle_onW
## ---------------------------------------


### set style ============================================##

## pagetitle
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

## subtitle
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

## tabtitle
tabtitle.theme_forWeb = ttheme_minimal( 
    base_family='NotoSansCJKkrR',
	core=list(bg_params=list(fill=NA,col=NA),
	    fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=12,fontface='plain'))
)
tabtitlel.theme_forWeb = ttheme_minimal( 
    base_family='NotoSansCJKkrB',
	core=list(bg_params=list(fill=NA,col=NA),
	    fg_params=list(hjust=0,x=0.02,vjust=0.5,col='#122747',fontsize=12,fontface='plain'))
)
tabtitler.theme_forWeb = ttheme_minimal( 
    base_family='NotoSansCJKkrR',
	core=list(bg_params=list(fill=NA,col=NA),
	        fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=11,fontface='plain'))
)

tabtitle.theme_forApp = ttheme_minimal(
    base_family='NotoSansCJKkrR',
	core=list(bg_params=list(fill=NA,col=NA),
            fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=17,fontface='plain'))
)
tabtitlel.theme_forApp = ttheme_minimal( 
    base_family='NotoSansCJKkrB',
	core=list(bg_params=list(fill=NA,col=NA),
	        fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#122747',fontsize=23,fontface='plain'))
)
tabtitler.theme_forApp = ttheme_minimal( 
    base_family='NotoSansCJKkrR',
	core=list(bg_params=list(fill=NA,col=NA),
	        fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=17,fontface='plain'))
)

## remark
remark.theme_forWeb = ttheme_minimal(
    base_family='NotoSansCJKkrR',
    core=list(bg_params=list(fill=NA,col=NA),
            fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#767e89',fontsize=12,fontface='plain'))
)
remark.theme_forApp = ttheme_minimal(
    base_family='NotoSansCJKkrR',
    core=list(bg_params=list(fill=NA,col=NA),
        fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#767e89',fontsize=17,fontface='plain'))
)

## col
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

## timelogo
timelogo.theme_forWeb = ttheme_minimal( 
    base_family=c('NotoSansCJKkrB','NotoSansCJKkrR'),
    core=list(bg_params=list(fill=NA,col=NA),
	    fg_params=list(hjust=c(0,1,0,0),x=c(0,0.9,0.01,0),vjust=0.5,col=c('#122747'),fontsize=c(10,12,10,10),fontface=c('plain')))
)
timelogo.theme_forApp = ttheme_minimal( 
    base_family=c('NotoSansCJKkrB','NotoSansCJKkrR'),
    core=list(bg_params=list(fill=NA,col=NA),
	    fg_params=list(hjust=0,x=c(0,0,0.01,0),vjust=0.5,col=c('#122747'),fontsize=c(10,22,20,10),fontface=c('plain')))
) 

## avgPPG.A 
avgPPG.A.theme_forWeb = ttheme_minimal( 
	base_family=c('NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrR'), 
	core = list(bg_params=list(fill='#e3e3e3',col=NA),
			fg_params=list(hjust=c(-0.1,0,-0.1),x=c(0,0.1,0),vjust=c(0,0,0),y=c(0.35,0.33,0.35),col=c('#122747','#0096ff','#122747'),fontsize=c(13,13.5,10),fontface=c('plain','plain','plain')))
)
avgPPG.A.theme_forApp = ttheme_minimal( 
	base_family=c('NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrR'),  
	core = list(bg_params=list(fill='#e3e3e3',col=NA),
			fg_params=list(hjust=c(-0.1,0,-0.1),x=c(0,0.1,0),vjust=c(0,0,0),y=c(0.35,0.33,0.35),col=c('#122747','#0096ff','#122747'),fontsize=c(21,23,17),fontface=c('plain','plain','plain')))
)
avgPPG.A_null.theme_forApp = ttheme_minimal( 
	base_family='NotoSansCJKkrB',
	core = list(bg_params=list(fill=c('#e3e3e3'),col=NA),
			fg_params=list(hjust=0,x=0.02,vjust=0.5,col=c('#122747'),fontsize=c(24),fontface='plain'))
)


## dietplan table
dietPlanTableHead.theme_forApp = ttheme_minimal( 
	base_family=c('NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrR'),  
	core = list(bg_params=list(fill='#EAF0FF',col=NA),
			fg_params=list(hjust=0,x=c(0.02,0.02,0,0),vjust=0.5,col='#113694',fontsize=c(28,28,28,20)*0.75,fontface='plain'))
)
dietPlanTableHead.theme_forWeb = ttheme_minimal( 
	base_family=c('NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrR'),  
	core = list(bg_params=list(fill='#EAF0FF',col=NA),
			fg_params=list(hjust=0,x=c(0.02,0.02,0,0),vjust=0.5,col='#113694',fontsize=c(13,13,13,10),fontface='plain'))
)

	## theme 
	dietPlanTableBody.theme_forWeb = ttheme_minimal( 
		base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),#fill=c(NA,'#e3e3e3')
				fg_params=list(hjust=0,x=0.02,vjust=0.5,col='#122747',fontsize=13,fontface='plain'))
	)
	dietPlanTableBody.theme_forApp = ttheme_minimal( 
		base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),#fill=c(NA,'#e3e3e3')
				fg_params=list(hjust=0,x=0.02,vjust=0.5,col='#122747',fontsize=20,fontface='plain'))
	)
	dietPlanTableBody_null.theme_forApp = ttheme_minimal( 
		base_family='NotoSansCJKkrB',
		core = list(bg_params=list(fill=c(NA),col=NA),
					fg_params=list(hjust=0,x=0.02,vjust=0.5,col=c('#767e89'),fontsize=c(20),fontface='plain'))
	)
	dietPlanTableBody_null.Wf = tableGrob(data.frame(''),theme=ttheme_minimal(core=list(bg_params=list(fill=NA,col=NA))),cols=NULL,rows=NULL,widths=unit(280,'points'),heights=unit(1,'points'))
	out.Tab_spaceitem.Wf = tableGrob(data.frame(''),theme=ttheme_minimal(core=list(bg_params=list(fill=NA,col=NA))),cols=NULL,rows=NULL,widths=unit(18,'points'),heights=unit(1,'points'))


	## dietplan table's body format ##

	
    f_dietPlanTableBody = function( invalueLs, msg, all=T, hist=T ) {

		# all=T ; 모든테이블통합해서
		# hist=T ; 과거기록 표시

		if ( all==T ) {

		dietPlanTableBody.theme_forWeb = ttheme_minimal( 
			base_family='NotoSansCJKkrR',
			core = list(bg_params=list(fill=NA,col=NA),#fill=c(NA,'#e3e3e3')
					fg_params=list(hjust=0,x=0.02,vjust=0.5,col='#122747',fontsize=13,fontface='plain'))
		) # 30 #FFDA55(ffc908) 60 #FF922E 100 #FF4E4E

		invalue = data.frame(date=NA,menu=NA,z=NA,p=NA,rowNum=NA,sub=NA)

		for ( k in 1:length(invalueLs) ) {            
			if ( dim(invalueLs[[k]])[1]==1 && grepl('해당 없음',invalueLs[[k]][1,2]) ) {
				next
			} else {
				colnames(invalueLs[[k]]) = colnames(invalue)
				invalue = rbind(invalue,invalueLs[[k]])
			}
		}

		invalue = invalue[which(!is.na(invalue$z)),]
		invalue$menu = ifelse(invalue$menu=='','기록 없음',invalue$menu)
		invalue = try(invalue[order(invalue$z,decreasing=T),],silent=T)
		fontcol = ifelse(invalue$z>=100,'#FF4E4E',ifelse(invalue$z>=60,'#FF922E',ifelse(invalue$z>=30,'#ffc908','#122747')))
		dietPlanTableBody.theme_forWeb$core$fg_params$col = cbind(rep('#122747',dim(invalue)[1]),rep('#122747',dim(invalue)[1]),rep('#122747',dim(invalue)[1]),rep('#122747',dim(invalue)[1]),fontcol,rep('#122747',dim(invalue)[1]))
		if ( is.numeric(invalue$z) ) invalue$z = paste('▲',invalue$z,sep=' ')

		if ( hist==T ) {
			bgfill = ifelse(invalue$sub==1,NA,'#e3e3e3')
			dietPlanTableBody.theme_forWeb$core$bg_params$fill = bgfill
		}

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
			out.tab.Wf = tableGrob(invalue[,c('rowNum','date','menu','p','z')],theme=dietPlanTableBody.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(54,108,(416+27),48,(108-27)),'points'),heights=unit(tabHtFt.W(tab.nrowsW),'points')) ## add info: date, 
			for ( g in 1:nrow(out.tab.Wf) ) out.tab.Wf = gtable_add_grob(out.tab.Wf, grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc'),gp=gpar(col='#E9EDF3',lwd=2)),l=1,r=ncol(out.tab.Wf),t=g,b=g)

			if ( length(tab.nrows)==1 ) {
				out.tab.Af$grobs[which(out.tab.Af$layout$t==1 & out.tab.Af$layout$l==2 & out.tab.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
				out.tab.Wf$grobs[which(out.tab.Wf$layout$t==1 & out.tab.Wf$layout$l==2 & out.tab.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			}
		}
		}

		return(list(tab.Af=out.tab.Af,tab.Wf=out.tab.Wf,nrows.A=tab.nrows,nrows.W=tab.nrowsW))
	}


	tabHtFt.W = function(nline){
		return( 14*nline+5*(nline-1)+20 )
	}
	tabHtFt.A = function(nline){
		return( 21*nline+5*(nline-1)+20 )
	}
	tabHtFt2.A = function(nline){
		return( (34*nline+32)*0.75 )
	}
