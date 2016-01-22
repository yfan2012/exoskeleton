##Model of Torque
##Refer to diagram for angles

require('ggplot2')
require('reshape2')

##Fixed - chosen rope angle in the lock position
psi=t(as.matrix(1/cos(seq(pi/6,pi/3,pi/36))))
label=c("30", "35", "40", "45", "50", "55", "60")

##Grant's foot parameters in n and m
w=11.428
l=.3
r=(1/2)*l

##Varying angles
alpha=seq(0, pi, pi/100)
theta=(pi/2)-alpha

##Torque by gravity
tau=as.matrix(cos(theta)*w*r)

##Calculate tension and plot
t=as.data.frame(tau%*%psi)/(r*4.44)
names(t)=label
t$Alpha=alpha

tension=melt(t, id.vars='Alpha')
names(tension)=c('Alpha', 'Angle', 'Tension')
pdf('~/Dropbox/School/Hopkins/Senior/Honors/tension.pdf', width=11, height=8.5)
print(ggplot(tension, aes(x=Alpha, y=Tension)) + geom_line(aes(colour=Angle)) + theme_bw() + ggtitle("Basic Free Body Model"))
dev.off()


