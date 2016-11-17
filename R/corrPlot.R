corrPlot <-
function(x, y, data,...){
	xx=data[,x]
	yy=data[,y]
	plot(yy~xx,bty="n",mgp=c(2.3,1,0),xlab=x,ylab=y,...)
	abline(lm(yy~xx),col="red")
	cor=cor.test(xx,yy) 
	mtext(side=3,paste("r =",round(cor$estimate,2),", p =",signif(cor$p.value,2)))
}
