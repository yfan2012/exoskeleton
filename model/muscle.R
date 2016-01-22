##Static muscle tension - holding at different angles

plotdir="~/Dropbox/School/Hopkins/Senior/Honors/muscle.pdf"
require(ggplot2)
require(reshape2)

##Dorsiflexion angle
dorflex=seq(pi/6, pi*5/6, pi/72)

##Muscle data
gvivo=.55 #in vivo length gastrocnemius
grest=gvivo*.85 #resting length gastrocnemius
tvivo=.38
trest=tvivo*.85 #resting length tibialis anterior

##Nonlinear exponents
##Assume: thickness is linear to exoponent of nonlinear spring
##Assume: gastrocnemius is twice as thick as tibialis anterior
ga=seq(1, 6, .5)
ta=ga/2

##Angle to displacement
rgast=.07
rtibi=.05
tdist=sqrt((r1^2)+(tvivo^2)-2*r1*tvivo*cos(dorflex))-trest
tdist[tdist<0]=0
gdist=sqrt((r2^2)+(gvivo^2)-2*r2*gvivo*cos(180-dorflex))-grest
gdist[gdist<0]=0

##Displacement to force
##Assume: k1=k2=1.5
gten=matrix(nrow=length(gdist), ncol=length(ga))
tten=matrix(nrow=length(tdist), ncol=length(ta))

for (i in 1:length(ga)){
    gten[,i]=(gdist^ga[i])*1.5
    tten[,i]=(tdist^ta[i])*1.5
}

##Applied torque
gv=asin(sin(pi-dorflex)*gvivo/(gdist+grest))
tv=asin(sin(dorflex)*tvivo/(tdist+tvivo))
gtorq=gten*sin(pi-gv)
ttorq=tten*cos((pi/2)-tv)

##Needed torque
diff=as.data.frame(gtorq-ttorq)
names(diff)=as.character(ga)
diff$angle=dorflex
diff=melt(diff, id.vars="angle")

##Plot
pdf(plotdir, height=8.5, width=11)
print(ggplot(diff, aes(x=angle, y=value)) + geom_line(aes(colour=variable)) + theme_bw() + xlab("Dorsiflexion (rad)") + ylab("Torque (N)") + ggtitle("Static Torque (constant k)"))
dev.off()


