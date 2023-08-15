
#install and include necessary packages
if(!require(cluster)){
    install.packages("viridis")
    library(viridis)
}

#input data address.
mysurface = read.table(file='data/heatmapinput.txt', header=FALSE, sep="")

#matrix transformation.
mysurface = as.matrix(mysurface)
#mysurface = replace(mysurface, mysurface > 0.038, 0.038)

# find the min value position in the data file
# position[1] means the row and position[2] means the column
position = which(mysurface==mysurface[which.min(mysurface)],arr.ind=T)

#sort the matrix to find the max value and the min value
s = sort(mysurface)
s_min = s[1]
s_max = s[length(s)]

# calcuate the range between the max and value
distance = s_max-s_min

# this is for the keys which is in the right of the picture
step = distance/10

mysurface = apply(mysurface, 2, rev)
mysurface = apply(mysurface, 1, rev)
mysurface = apply(mysurface, 2, rev)

#Redefine the Heatmap function 
filled.contourR =
function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z = x$z
                y = x$y
                x = x$x
            }
            else {
                z = x
                x = seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y = x$y
        x = x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    mar.orig = (par.orig = par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w = (3 + mar.orig[2L]) * par("csi") * 2.54
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w*1.07)))
    par(las = las)
    mar = mar.orig
    mar[4L] = mar[2L]
    mar[2L] = 1
    par(mar = mar)
    par(mgp = c(3,2,0))
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")

#borders are removed
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col, border = NA)
	

    if (missing(key.axes)) {
        if (axes) 
            axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title)) 
        key.title
    mar = mar.orig
    mar[4L] = 1
    par(mar = mar+c(0,0,0,2.1))
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    .filled.contour(x, y, z, levels, col)
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
        }
    }
    else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...)
    else plot.title
    invisible()

#Black Rectangle Overlay
    rect(-0.829, 1.571, 0.829, 3.403, border = "black", lwd = 8)

# draw the red solid circle in the plot
# calculate the min value postion in the coordinate which is the point's position. the pch means the solid circle
# the cex means the size of the circle

    if (position[2]>16){

		 points((position[2]-16)/10, 1+(31-position[1])/10,col="red", pch=20,cex=5)
		
	}
    if (position[2]==16){
 		 points(0.0, 1+(31-position[1])/10,col="red", pch=20,cex=5)

	}
    if (position[2]<16){

             points(-(16-position[2])/10, 1+(31-position[1])/10,col="red", pch=20,cex=5)
	    }
}


#Set the Color limits
zmax = s_max
zmin = s_min
levelwidth = 0.0001
levels = (zmax-zmin)/levelwidth

#setup the text size for axis tick labels
textSize = 4.7


#save as png file
png(filename="data/heatmapinput_unmoved.png",width=1700, height=1200)

#to plot the heatmap in R run this, otherwise skip
#dev.new(width=9.1, height=5.92)

#fix the image blank space
par(mar=c(16.4,24,4.8,24))


color_rain = colorRampPalette(c("darkblue", "darkblue","blue", "dodgerblue2", 
     			  "lightskyblue", "lightskyblue", "cyan3", "green4", 
     			  "greenyellow", "greenyellow", "yellow", "gold", 
     			  "orange", "red", "darkred"))
#plot picture
filled.contourR(x=seq(from=-1.5,to=1.5,length=31),
    y=seq(from=1.0,to=4.0,length=31),
    z=mysurface,
    zlim=c(zmin,zmax),
    nlevels=levels,
    axes=TRUE,
    # color=colorRampPalette(c("darkblue", "darkblue","blue", "dodgerblue2", 
    # 			  "lightskyblue", "lightskyblue", "cyan3", "green4", 
    # 			  "greenyellow", "greenyellow", "yellow", "gold", 
    # 			  "orange", "red", "darkred")),
    color.palette=viridis,
    asp =1,
    key.title = title(main = ""),

    plot.axes = { axis(side=1, at=seq(-1.5, 1.5, by = 0.5),cex.axis=textSize, padj = 0.7, tck = -0.015, lwd = 3)
              axis(side=2, at=seq(1.0, 4.0, by = 0.5),cex.axis=textSize, tck = -0.015, lwd = 3)
         },

    key.axes = axis(4, c(s_min,s_min+step,s_min+2*step,s_min+3*step,s_min+4*step,s_min+5*step,s_min+6*step,s_min+7*step,s_min+8*step,s_min+9*step,s_max) ,cex.axis=textSize, tck = -0.2, lwd = 3)  ,

    cex.lab=textSize,

    xaxs = "itestsss",

plot.title= {
    windowsFonts(times=windowsFont("Times New Roman"))
#    title(xlab=italic('l'[x])~"   ", cex.lab=7, line=13.2, family="times")
#    mtext("          (feet)", 1, cex=5, line=11.6, family="times")
#    mtext(expression(italic('l'[z])), 2, cex=7, line=12, las=1, family="times")
#    mtext("\n\n(feet)", 2, cex=5, line=10, las=1, family="times")
	  title(xlab=italic('l')~"    (feet)",cex.lab=6, line=11.5, family="times")
	  mtext(expression(italic('x')~"         "), side = 1, cex=6.1, line=11.5, family="times")
	  title(ylab=italic('l')~"    (feet)",cex.lab=6, line=9, family="times")
	  mtext(expression(italic('z')~"         "), side = 2, cex=6.1, line=9, las = 0, family="times")
    }

)
#file saved
dev.off()









