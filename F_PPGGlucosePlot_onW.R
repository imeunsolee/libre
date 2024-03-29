##########################################
## PPGGlucosePlot
## ---------------------------------------
## input 
## . out_value :
## . data.glucose :
## . Target :
## . PPG.L :
## . PPG.U :
## ---------------------------------------

PPGGlucosePlot_onW = function( out_value, data.glucose, Target, PPG.L, PPG.U ) {

    errCode.sub = c()

    if ( Target %in% c('T1GDM','T2GDM','GDM') ) {
        FPG.L = 70; FPG.U = 95
    } else if ( Target=='DIET' ) {
        FPG.L = 70; FPG.U = 100
    } else {
        FPG.L = 70; FPG.U = 130
    }
        

        if ( dim(out_value)[1]!=0 ) {
            data.tmp = data.glucose[which(data.glucose$event %in% out_value$k),]
            mean.tmp = try(apply(data.tmp[which(data.tmp$x3==1 &  data.tmp$x2>=0),c('x2','glucose')],2,mean),silent=T)

        } else {
            mean.tmp = vector()
            class(mean.tmp) = 'try-error'

        }

        if ( class(mean.tmp)!='try-error' && dim(data.tmp)[1]!=0 && !is.na(mean.tmp['glucose']) ) {
            names(mean.tmp) = c('x.tmp','y.tmp')
            data.tmp$k = NA

            for ( i in 1:dim(data.tmp)[1] ) data.tmp$k[i] = grep(data.tmp$event[i],out_value$k)

            out.Plot = ggplot()+
                annotate('rect',xmin=-60,xmax=0,ymin=FPG.L,ymax=FPG.U,fill='#dcdddf')+ 
                annotate('rect',xmin=0,xmax=180,ymin=PPG.L,ymax=PPG.U,fill='#dcdddf')+ 
                scale_x_continuous(name='',limits=c(-60,180),breaks=c(-60,0,60,120,180),labels=c('-1시간','','+1시간','+2시간','+3시간'))+
                geom_vline(xintercept=c(0),color='#ffc926',size=0.3)+
                geom_vline(xintercept=c(60,120),color='#8a8b8a',lty=2,size=0.2)+
                coord_cartesian(ylim=c(0,350),xlim=c(-60,180),expand=F,clip='off')+
                theme(panel.background=element_rect(fill=NA,color=NA),panel.border=element_rect(fill=NA,color='#231f20',size=0.4),
                    axis.text.x=element_text(family='NotoSansCJKkrR',color='#767e89',size=18,hjust=c(0,0,1,1,1),margin=margin(t=-20,b=0)),axis.ticks.x=element_blank(),
                    axis.text.y=element_blank(),axis.ticks.y=element_blank(),
                    text=element_text(family='NotoSansCJKkrR'))

            x.tmp = sort(unique(data.tmp$x2))
            Eqn.tmp1 = try(loess(glucose ~ x2, data=data.tmp[grep(as.Date(out_value$k[which.max(out_value$z)]),as.Date(data.tmp$dateandtime)),],span=0.5,control=loess.control(surface='interpolate')),silent=T)
            y.tmp = try(predict(Eqn.tmp1,newdata=x.tmp),silent=T)
            if ( class(y.tmp)=='try-error' ) {
                out.Plot_forApp = out.Plot_forWeb = ggplot()+
                    scale_y_continuous(name='',limits=c(0,350))+
                    scale_x_continuous(name='',limits=c(-60,180),breaks=c(-50,0,60,120,170),labels=c('-1시간','','+1시간','+2시간','+3시간'))+
                    geom_vline(xintercept=c(0),color='#ffffff',size=0.3)+
                    geom_vline(xintercept=c(60,120),color='#ffffff',lty=2,size=0.2)+
                    coord_cartesian(ylim=c(0,350),xlim=c(-60,180),expand=F,clip='off')+
                    theme(panel.background=element_rect(fill=NA,color=NA),panel.border=element_rect(fill='#eff0f2',color='#231f20',size=0.4),
                        axis.text.x=element_text(color=NA,size=10,hjust=c(0,0,1,1,1),margin=margin(t=-20,b=0)),axis.ticks.x=element_blank(),
                        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
                        plot.margin=margin(20,40,30,10,unit='points'),
                        text=element_text(family='NotoSansCJKkrR'))
                
                ### output =============================================================================##
                out.Result = list(out.Plot_forApp=out.Plot_forApp, out.Plot_forWeb=out.Plot_forWeb)
                return(out.Result)
            }

            Eqn.tmp2 = try(loess(glucose ~ x2, data=data.tmp,span=0.75,control=loess.control(surface='interpolate')),silent=T)
            ymin.tmp = y.tmp - 6*predict(Eqn.tmp2,newdata=x.tmp,se=T)$se.fit
            ymax.tmp = y.tmp + 6*predict(Eqn.tmp2,newdata=x.tmp,se=T)$se.fit
            data.pred = data.frame(x2=x.tmp,ymin=ymin.tmp,ymax=ymax.tmp,y=y.tmp)

            out.Plot = out.Plot+
                geom_ribbon(data.pred,mapping=aes(x=x2,ymin=ymin,ymax=ymax),fill='#7d8791',alpha=0.5) + 
                geom_line(data.pred,mapping=aes(x=x2,y=ymin),lty=2,col='#7d8791',size=0.5)+ 
                geom_line(data.pred,mapping=aes(x=x2,y=ymax),lty=2,col='#7d8791',size=0.5)+ 
                geom_line(data.pred,mapping=aes(x=x2,y=y),lty=1,col='#404040',size=4)+
                
                scale_y_continuous(name='',limits=c(0,350),breaks=c(0,50,100,150,200,250,300,350),labels=c(0,50,'',150,'',250,'',350))


            out.Plot_forApp = out.Plot+
                theme(axis.text.y=element_text(family='NotoSansCJKkrR',color='#000000',size=13), axis.ticks.y=element_line(size=0.3), axis.ticks.length.y=unit(4.3,'points'),
                plot.margin=margin(20,40,30,10,unit='points'))+
                    
                annotation_custom(grob=textGrob('식사 전',gp=gpar(fontsize=18,color='#231f20',fontfamily='NotoSansCJKkrR',fontface='plain')),xmin=-30,xmax=-30,ymin=-30,ymax=-30)+
                annotation_custom(grob=textGrob('식사 후',gp=gpar(fontsize=18,color='#231f20',fontfamily='NotoSansCJKkrR',fontface='plain')),xmin=60,xmax=60,ymin=-30,ymax=-30)+

                annotation_custom(grob=textGrob(FPG.L,gp=gpar(fontsize=18,fontface='bold',col='#122747',fontfamily='NotoSansCJKkrB')),xmin=-80,xmax=-80,ymin=(FPG.L-15),ymax=(FPG.L-15))+ ## if mealtime==1
                annotation_custom(grob=textGrob(FPG.U,gp=gpar(fontsize=18,fontface='bold',col='#122747',fontfamily='NotoSansCJKkrB')),xmin=-80,xmax=-80,ymin=(FPG.U-15),ymax=(FPG.U-15))+ ## if mealtime==1
                annotation_custom(grob=linesGrob(gp=gpar(col='#000000',lwd=0.3)),xmin=-75,xmax=-60,ymin=FPG.L,ymax=FPG.L)+ ## if mealtime==1
                annotation_custom(grob=linesGrob(gp=gpar(col='#000000',lwd=0.3)),xmin=-75,xmax=-60,ymin=FPG.U,ymax=FPG.U)+ ## if mealtime==1

                annotation_custom(grob=textGrob(PPG.L,gp=gpar(fontsize=18,fontface='bold',col='#122747',fontfamily='NotoSansCJKkrB')),xmin=185,xmax=200,ymin=(PPG.L-15),ymax=(PPG.L-15))+ ## if mealtime==3
                annotation_custom(grob=textGrob(PPG.U,gp=gpar(fontsize=18,fontface='bold',col='#122747',fontfamily='NotoSansCJKkrB')),xmin=185,xmax=200,ymin=(PPG.U+20),ymax=(PPG.U+20))+ ## if mealtime==3
                annotation_custom(grob=linesGrob(gp=gpar(col='#000000',lwd=0.3)),xmin=180,xmax=195,ymin=PPG.L,ymax=PPG.L)+ ## if mealtime==1
                annotation_custom(grob=linesGrob(gp=gpar(col='#000000',lwd=0.3)),xmin=180,xmax=195,ymin=PPG.U,ymax=PPG.U) ## if mealtime==1

            out.Plot_forWeb = out.Plot+
                theme(axis.text.y=element_text(family='NotoSansCJKkrR',color='#000000',size=11), axis.ticks.y=element_line(size=0.3), axis.ticks.length.y=unit(4.3,'points'),
                plot.margin=margin(20,40,30,10,unit='points'))+

                annotation_custom(grob=textGrob('식사 전',gp=gpar(fontsize=10,color='#231f20',fontfamily='NotoSansCJKkrR',fontface='plain')),xmin=-30,xmax=-30,ymin=-20,ymax=-20)+
                annotation_custom(grob=textGrob('식사 후',gp=gpar(fontsize=10,color='#231f20',fontfamily='NotoSansCJKkrR',fontface='plain')),xmin=60,xmax=60,ymin=-20,ymax=-20)+

                annotation_custom(grob=textGrob(FPG.L,gp=gpar(fontsize=13,fontface='bold',col='#122747',fontfamily='NotoSansCJKkrB')),xmin=-100,xmax=-100,ymin=(FPG.L-15),ymax=(FPG.L-15))+ ## if mealtime==1
                annotation_custom(grob=textGrob(FPG.U,gp=gpar(fontsize=13,fontface='bold',col='#122747',fontfamily='NotoSansCJKkrB')),xmin=-100,xmax=-100,ymin=(FPG.U-15),ymax=(FPG.U-15))+ ## if mealtime==1
                annotation_custom(grob=linesGrob(gp=gpar(col='#000000',lwd=0.3)),xmin=-75,xmax=-60,ymin=FPG.L,ymax=FPG.L)+ ## if mealtime==1
                annotation_custom(grob=linesGrob(gp=gpar(col='#000000',lwd=0.3)),xmin=-75,xmax=-60,ymin=FPG.U,ymax=FPG.U)+ ## if mealtime==1

                annotation_custom(grob=textGrob(PPG.L,gp=gpar(fontsize=13,fontface='bold',col='#122747',fontfamily='NotoSansCJKkrB')),xmin=195,xmax=200,ymin=(PPG.L-15),ymax=(PPG.L-15))+ ## if mealtime==3
                annotation_custom(grob=textGrob(PPG.U,gp=gpar(fontsize=13,fontface='bold',col='#122747',fontfamily='NotoSansCJKkrB')),xmin=195,xmax=200,ymin=(PPG.U+20),ymax=(PPG.U+20))+ ## if mealtime==3
                annotation_custom(grob=linesGrob(gp=gpar(col='#000000',lwd=0.3)),xmin=180,xmax=205,ymin=PPG.L,ymax=PPG.L)+ ## if mealtime==1
                annotation_custom(grob=linesGrob(gp=gpar(col='#000000',lwd=0.3)),xmin=180,xmax=205,ymin=PPG.U,ymax=PPG.U)     

            out.Plot_forWeb$theme$axis.text.x$size = 10 

        } else {
            out.Plot_forApp = out.Plot_forWeb = ggplot()+
                scale_y_continuous(name='',limits=c(0,350))+
                scale_x_continuous(name='',limits=c(-60,180),breaks=c(-50,0,60,120,170),labels=c('-1시간','','+1시간','+2시간','+3시간'))+
                geom_vline(xintercept=c(0),color='#ffffff',size=0.3)+
                geom_vline(xintercept=c(60,120),color='#ffffff',lty=2,size=0.2)+
                coord_cartesian(ylim=c(0,350),xlim=c(-60,180),expand=F,clip='off')+
                theme(panel.background=element_rect(fill=NA,color=NA),panel.border=element_rect(fill='#eff0f2',color='#231f20',size=0.4),
                    axis.text.x=element_text(color=NA,size=10,hjust=c(0,0,1,1,1),margin=margin(t=-20,b=0)),axis.ticks.x=element_blank(),
                    axis.text.y=element_blank(),axis.ticks.y=element_blank(),
                    plot.margin=margin(20,40,30,10,unit='points'),
                    text=element_text(family='NotoSansCJKkrR'))
        }

    
    ### output =============================================================================##
    out.Result = list(out.Plot_forApp=out.Plot_forApp, out.Plot_forWeb=out.Plot_forWeb)
    return(out.Result)

}