
# Load EISMINT1 data 
load_eismint1_moving = function(fldr)
{
    xHt1 = read.table(file.path(fldr,"EISMINT1-moving_x-H_type1.txt"),header=TRUE)
    xHt2 = read.table(file.path(fldr,"EISMINT1-moving_x-H_type2.txt"),header=TRUE)
    xHe  = read.table(file.path(fldr,"EISMINT1-moving_x-H_exactmargin.txt"),header=TRUE)
    
    xuxy      = read.table(file.path(fldr,"EISMINT1-moving_x-uxy_mean.txt"),header=TRUE)
    xtprime   = read.table(file.path(fldr,"EISMINT1-moving_x-T_prime_base.txt"),header=TRUE)
    tprime    = read.table(file.path(fldr,"EISMINT1-moving_divide_T_prime.txt"),header=TRUE)
    uz        = read.table(file.path(fldr,"EISMINT1-moving_divide_uz.txt"),header=TRUE)

    eis1 = list(x=seq(0,750,by=10),s=seq(0,1,by=0.05))
    
    # Variables versus distance from divide x
    eis1$Ht1          = approx(x=xHt1$x,y=xHt1$H,       xout=eis1$x,rule=2)$y 
    eis1$Ht2          = approx(x=xHt2$x,y=xHt2$H,       xout=eis1$x,rule=2)$y 
    eis1$He           = approx(x=xHe$x, y=xHe$H,        xout=eis1$x,rule=2)$y 
    eis1$uxy_mean     = approx(x=xuxy$x,y=xuxy$uxy_mean,xout=eis1$x,rule=2)$y 
    eis1$T_prime_base = approx(x=xtprime$x,y=xtprime$T_prime_base,xout=eis1$x,rule=2)$y 

    # Variables at divide versus normalized ice thickness sigma 
    eis1$T_prime      = approx(x=tprime$sigma,y=tprime$T_prime,xout=eis1$s,rule=2)$y 
    eis1$uz           = approx(x=uz$sigma,    y=uz$uz,         xout=eis1$s,rule=2)$y 

    return(eis1)
}

load_eismint1_fixed = function(fldr)
{
    xHt1 = read.table(file.path(fldr,"EISMINT1-fixed_x-H_type1.txt"),header=TRUE)
    xHt2 = read.table(file.path(fldr,"EISMINT1-fixed_x-H_type2.txt"),header=TRUE)

    xuxy    = read.table(file.path(fldr,"EISMINT1-fixed_x-uxy_mean_3D.txt"),header=TRUE)
    # xtprime = read.table(file.path(fldr,"EISMINT1-fixed_x-T_prime_base.txt"),header=TRUE)

    eis1 = data.frame(x=seq(0,750,by=10))
    eis1$Ht1          = approx(x=xHt1$x,y=xHt1$H,       xout=eis1$x,rule=2)$y 
    eis1$Ht2          = approx(x=xHt2$x,y=xHt2$H,       xout=eis1$x,rule=2)$y 
    eis1$uxy_mean     = approx(x=xuxy$x,y=xuxy$uxy_mean,xout=eis1$x,rule=2)$y 
    # eis1$T_prime_base = approx(x=xtprime$x,y=xtprime$T_prime_base,xout=eis1$x,rule=2)$y 

    return(eis1)
}

# Check data
if (FALSE) {

    eis1m = load_eismint1_moving("./EISMINT1-moving/")
    eis1f = load_eismint1_fixed("./EISMINT1-fixed/")

    eis1 = eis1f 

    xlim = c(0,750)
    ylim = c(0,4000)

    plot(xlim,ylim,type="n",ann=FALSE,axes=FALSE)
    axis(1)
    axis(2)
    mtext(side=1,line=2.5,las=0,"Distance from divide [km]")
    mtext(side=2,line=2.5,las=0,"Ice thickness [m]")
    grid()

    lines(eis1$x,eis1$Ht1,col=1,lwd=2,lty=1)
    lines(eis1$x,eis1$Ht2,col=1,lwd=2,lty=2)
    # lines(eis1$x,eis1$He, col=1,lwd=2,lty=3)
    
    legend("bottomleft",inset=0.02,bty="n",col=c(1,1,1),lwd=c(2,2,2),lty=c(1,2,3),c("type I","type II","exact"))

    box() 

}