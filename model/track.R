##Track length calculations
plotpath="~/Dropbox/School/Hopkins/Senior/Honors/tracklength.pdf"
require(ggplot2)
require(reshape2)

##Fixed parameters
L=.22
fmax=.25

##Figure out max alpha
smax=sqrt((L^2)+(fmax^2))
amax=asin(L/smax)

##Get string lengths for each corresponding angle
angle=seq(amax, pi/2, .05)
s=sin(angle)/L

##Endpoints
Lr=(pi/2)+(pi/9)
rs=asin(L*sin(Lr)/s)
Ls=pi-(Lr+rs)
r1=sin(Ls)*s/(sin(Lr))
r2=sqrt((s^2)-(L^2))
length=r2-r1

relation=data.frame(angle=angle, length=length)

pdf(plotpath, height=8.5, width=11)
print(ggplot(relation, aes(x=angle, y=length)) + theme_bw() + geom_line() + ggtitle("Track Length") + xlab("RS Angle (rad)") + ylab("Length (m)"))
dev.off()
