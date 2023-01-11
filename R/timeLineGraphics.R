# https://cran.r-project.org/web/packages/magick/vignettes/intro.html

#library(magick) # image_read()
#library("berryFunctions") # rounded rectangles
#library(graphics)
#library(R2HTML)

#' timeLineGraphics

#' @import magick
#' @import berryFunctions
#' @import graphics
#' @import R2HTML
#' @import grDevices
#' @import pdftools

#' @description
#' This function is the driver that organizes the production of an html page
#' containing horizontal strips that symbolize events in a person's life.

#' @details
#' the parameters here work fine in general
#' but they were tuned to match the chart in https://mathigon.org/timeline
#' since my intention is to generate a consistent display of that chart and my chart above one another
#' a consideration is to have the horizontal strips sufficiently tall
#' so that the images of the in-laid paintings can be seen clearly
#' the default RBG values match the design in https://mathigon.org/timeline
#' construct graph whose
#' x axis is years
#' y axis is items to be plotted such as a person's years of  birth and death
#' 	within a broad colored horizontal line
#' 	overlain with pictures related to that person

#' @param l is a list
#' \itemize{
#'   \item character string $name the person's name
#'   \item integer $start starting year
#'   \item integer $end ending year
#'   \item character string $color background color for the horizontal line
#'   \item list of overlay pictures $pics, each entry contains
#'     \itemize{
#'       \item character string path name for a picture
#'       \item integer year for placement of left edge of picture
#'      }
#'    }

#' @param vfactor is height of a horizontal strip
#' @param abline is Boolean if TRUE add dates and corresponding vertical ablines
#' @param bkgRBG is ie c(13,65,130) specifying background color
#' @param ablineRBG specifying abline color
#' @param pdfWidth the width parameter to pdf()
#' @param pdfHeight the height parameter to pdf()
#' @param extendRight extends plot to the right to provide room to fit in names
#' @param yearsTicks number of years interval between x axis ticks
#' @param ymargin is fraction of maxy to leave as top and bottom margin
#' @param nameCEX control size of font in text() to display names associated with each horizontal strip
#' @param main main title for the graph
#' @param WidthHTML the width of the figure when imported into html page
#' @param imageDir where to save the image output files
#' @param leftOver if TRUE then pictures that do not fit in horizontal strip are placed to left of strip rather than omitted

#' @return {No return value, called for side effects}

#' @export
timeLineGraphics<-
function(l,vfactor=3,abline=TRUE,bkgRBG=c(13,65,130),ablineRBG=c(26,85,145),pdfWidth=20,pdfHeight=20,extendRight=50,yearsTicks=10,ymargin=.05,nameCEX=4,main="Timelines",WidthHTML=850,imageDir=tempdir(),leftOver=TRUE) {
		# the default RBG values match the design in https://mathigon.org/timeline
		# construct graph whose
		# x axis is years
		# y axis is items to be plotted such as a person's years of  birth and death
		# 	within a broad colored horizontal line
		# 	overlain with pictures related to that person
		# l is a list
		#	each element is a sub-list
		# 	the components of the sub-list are
		#		character string $name the person's name
		#		integer $start starting year
		#		integer $end ending year
		#		character string $color background color for the horizontal line
		#		list of overlay pictures $pics, each entry contains
		#			character string path name for a picture
		#			integer year for placement of left edge of picture
		# vfactor is height of a horizontal strip
		# abline is Boolean if TRUE add dates and corresponding vertical ablines
		# bkgRBG is ie c(13,65,130) specifying background color
		# ablineRBG specifying abline color
		# pdfWidth and pdfHeight=20 are the width and height parameters to pdf()
		# extendRight=50 extends plot to the right to provide room to fit in names
		# yearsTicks=10 number of years interval between x axis ticks
		# nameCEX=4 control size of font in text() to display names associated with each horizontal strip
		# main="Timelines" main title for the graph
		# WidthHTML=850 the width of the figure when imported into html page
		# imageDir="~" where to save the image output files
		#	note: use full path spelled out, html does not work if you use e.g. "~/"
		# leftOver=TRUE if TRUE then pictures that do not fit in horizontal strip are placed to left of strip rather than omitted
		
		# the default parameters here work fine in general
		# but they were tuned to match the chart in https://mathigon.org/timeline
		# since my intention is to generate a consistent display of that chart and my chart above one another
		# a consideration is to have the horizontal strips sufficiently tall
		# so that the images of the in-laid paintings can be seen clearly
		
		# can run using e.g. timeLineGraphics(example2())
		
		# determine the range of years
		roy<-roy(l)
		
		# vertical extent depends on number of entries in l
		y<-vfactor*length(l)
		
		# create an empty graph with correct horizontal extent
		#xlab<-"years"
		xlab<-""
		ylab<-""
		yaxt <-"n"
		for(i in 1:2) {
			# pdf allows wider margins that are needed here. cannot use jpeg or png
			#pdf(width=pdfWidth,height=pdfHeight,sprintf("~/Plot%d.pdf",i)) # height parameter controls the displayed thickness of the horizontal strips
			pdf(width=pdfWidth,height=pdfHeight,sprintf("%s/Plot%d.pdf",imageDir,i)) # height parameter controls the displayed thickness of the horizontal strips
			# use native mac Digital Color Meter App "Display in Generic RGB"
			# unfortunately the rendered color is a bit low in the green and blue channels
			# so we need to trial and error to find the slightly elevated values to feed rgb() to generate the desired coors
			#par(bg=rgb(13,49,106,maxColorValue=255)) # these are the measured values, but we need to input the elevated values in the next line
			oldpar<-par(no.readonly=TRUE)
			on.exit(par(oldpar))
			par(bg=rgb(bkgRBG[1],bkgRBG[2],bkgRBG[3],maxColorValue=255))
		
			plot(roy+c(0,extendRight),c(0,y),type="n",main=main,xlab=xlab,ylab=ylab,yaxt=yaxt,cex=3,cex.main=3,col.main="white",xaxt="n")
			# +c(0,extendRight) extends plot to the right to provide room to fit in names
			nmt<-nextMultiple10(roy[1])
			if(abline)
				abline(v=seq(nmt,roy[2]+yearsTicks,yearsTicks),lwd=5,col=rgb(ablineRBG[1],ablineRBG[2],ablineRBG[3],maxColorValue=255))
			if(i==1)
				dev.off() # creates background image in case we later need to pad left and right for overlay with other plot
			}
		#axis(1, at = seq(nmt, roy[2]+100, by = 10), las=2) # ticks at every multiple of 10 years
		axis(1, at = seq(nmt, roy[2]+yearsTicks, by = yearsTicks), las=2,col.axis="white",cex.axis=2) # ticks at every multiple of yearsTicks years
		
		# iteratively plot each horizontal strip
		stripPlot(l,y,vfactor,ymargin,nameCEX,leftOver)		
		dev.off()
		im<-magick::image_read_pdf(sprintf("%s/Plot%d.pdf",imageDir,1))
		magick::image_write(im,sprintf("%s/Plot%d.png",imageDir,1))
		im<-magick::image_read_pdf(sprintf("%s/Plot%d.pdf",imageDir,2))
		magick::image_write(im,sprintf("%s/Plot%d.png",imageDir,2))
		R2HTML::HTMLSetFile(sprintf("%s/Plot%d.html",imageDir,2))
		# need to use path.expand() because symbolic pathname seems to fail inside .html
		R2HTML::HTMLInsertGraph(path.expand(sprintf("%s/Plot%d.png",imageDir,2)),WidthHTML=WidthHTML,append=FALSE)
	}

#' nextMultiple10

#' @description
#' find next higher multiple of 10

#' @param x integer
#' @return integer next higher multiple of 10

#' @examples
#' nextMultiple10(1732)
#' nextMultiple10(1699)

#' @export
nextMultiple10<-
	function(x) {
		# find next higher multiple of 10
		for(i in 0:10)
			if(((x+i) %% 10) == 0)
		return(x+i)
	}

#' stripPlot

#' @description
#' iteratively plot each horizontal strip

#' @param l is a list
#' \itemize{
#'   \item character string $name the person's name
#'   \item integer $start starting year
#'   \item integer $end ending year
#'   \item character string $color background color for the horizontal line
#'   \item list of overlay pictures $pics, each entry contains
#'     \itemize{
#'       \item character string path name for a picture
#'       \item integer year for placement of left edge of picture
#'      }
#'    }
#' @param ht is total height of entire graph
#' @param vfactor is height of a horizontal strip
#' @param ymargin is fraction of maxy to leave as top and bottom margin
#' @param nameCEX control size of font in text() to display names associated with each horizontal strip
#' @param leftOver if TRUE then pictures that do not fit in horizontal strip are placed to left of strip rather than omitted

#' @return {No return value, called for side effects}

#' @export
stripPlot<-
	function(l,ht,vfactor,ymargin,nameCEX,leftOver) {
		# iteratively plot each horizontal strip
		# ht is total height of entire graph
		# vfactor is height of a horizontal strip

		for(i in 1:length(l)) { # loop through each horizontal strip
			y=ht-vfactor*i
			xmid<-(l[[i]]$start+l[[i]]$end)/2
			berryFunctions::roundedRect(l[[i]]$start,y,l[[i]]$end,y+vfactor, rounding=0.1,col=l[[i]]$color) # overlay main horizontal strip
			
			x<-l[[i]]$start
			xLeft<-x						
			for(j in 1:length(l[[i]]$pics)) { # loop through overlaying pictures for this person
				im<-magick::image_read(l[[i]]$pics[[j]])
				y2x<-aspectRatio(as.raster(im)) # maintain original aspect ratio	
				bfr<-backFitRaster(y2x,vfactor,ymargin)
				xmargin<-bfr*ymargin
				if(x+xmargin+bfr<=l[[i]]$end) {
					rasterImage(im,x+xmargin,y+vfactor*ymargin,x+xmargin+bfr,y+vfactor*(1-ymargin))
					x<-x+xmargin+bfr
					}
				else {
					if(leftOver) {
						rasterImage(im,xLeft-xmargin-bfr,y+vfactor*ymargin,xLeft-xmargin,y+vfactor*(1-ymargin))
						xLeft<-xLeft-xmargin-bfr
						}
					else {
						print(c("NOT ENOUGH ROOM TO ADD ",l[[i]]$pics[[j]]," to ",l[[i]]$name),quote=FALSE)
						}
					}
				}
				
			text(l[[i]]$end,y+vfactor*.5,l[[i]]$name,cex=nameCEX,pos=4,col="white") # add the name of this person to the right of x,y coords (pos = 4)
			}
	}


#' backFitRaster

#' @description
#' given a maximum vertical size in inches that is allowable, find x size of raster image

#' @details
#' graph units<-yinch(y in inches)

#' @param y2x is original aspect ratio of image
#' @param maxy is max vertical size in inches
#' @param ymargin is fraction of maxy to leave as top and bottom margin
#' @returns numerical x size of raster image

#' @examples
#' backFitRaster(1.333,10,.05)

#' @export	
backFitRaster<-
	function(y2x,maxy,ymargin) {
		# given a maximum vertical size in inches that is allowable, find x size of raster image
		# graph units<-yinch(y in inches)
		# y2x is original aspect ratio of image
		# maxy is max vertical size in inches
		# ymargin is fraction of maxy to leave as top and bottom margin
		
		return(xinch(1)/yinch(1)*maxy*(1-2*ymargin)/y2x)
	}

#' aspectRatio

#' @description
#' compute ratio needed for maintaining proper aspect ratio in graph overlay image

#' @param rim is rasterized version of im
#' @returns numerical ratio needed for maintaining proper aspect ratio in graph overlay image

#' @examples
#' aspectRatio(as.raster(magick::image_read(example2()[[1]]$pics[[1]])))

#' @export	
aspectRatio<-
	function(rim) {
		# compute ratio needed for maintaining proper aspect ratio in graph overlay image
		# rim is rasterized version of im
		drim<-dim(rim)
		height<-drim[1]
		width<-drim[2]
		
		return(as.numeric(height)/as.numeric(width))
	}

#' roy

#' @description
#' determine the range of years

#' @param l is a list
#' \itemize{
#'   \item character string $name the person's name
#'   \item integer $start starting year
#'   \item integer $end ending year
#'   \item character string $color background color for the horizontal line
#'   \item list of overlay pictures $pics, each entry contains
#'     \itemize{
#'       \item character string path name for a picture
#'       \item integer year for placement of left edge of picture
#'      }
#'    }
#' @returns integer vector containing the range of years

#' @examples
#' roy(example2())

#' @export	
roy<-
	function(l) {
		# determine the range of years
		mn<-10000
		mx<--10000
		for(i in 1:length(l)) {
			mn<-min(mn,l[[i]]$start)
			mx<-max(mx,l[[i]]$end)
		}
		return(c(mn,mx))
	}

#' example2

#' @description
#' generate structured list for mockup example

#' @returns structured list for mockup example

#' @export
example2<-
	function() {
		# address
		# school
		# career
		# medical
		# significant other

		x<-list()

		x[[1]]<-list()
		x[[1]]$name<-"500 First Street"
		x[[1]]$start<-1990
		x[[1]]$end<-2010
		x[[1]]$color<-"blue"
		x[[1]]$pics<-list()
		x[[1]]$pics[1]<-system.file("extdata/images/mockup/","house_amp029030.jpg",package="timeLineGraphics")

		x[[2]]<-list()
		x[[2]]$name<-"20 College Street"
		x[[2]]$start<-2010
		x[[2]]$end<-2018
		x[[2]]$color<-"blue"
		x[[2]]$pics<-list()
		x[[2]]$pics[1]<-system.file("extdata/images/mockup/","house_IMG_5629.jpg",package="timeLineGraphics")

		x[[3]]<-list()
		x[[3]]$name<-"Academy High School"
		x[[3]]$start<-2006
		x[[3]]$end<-2010
		x[[3]]$color<-"green"
		x[[3]]$pics<-list()
		x[[3]]$pics[1]<-system.file("extdata/images/mockup/","high_school.jpeg",package="timeLineGraphics")

		x[[4]]<-list()
		x[[4]]$name<-"University College"
		x[[4]]$start<-2010
		x[[4]]$end<-2014
		x[[4]]$color<-"green"
		x[[4]]$pics<-list()
		x[[4]]$pics[1]<-system.file("extdata/images/mockup/","college.jpeg",package="timeLineGraphics")

		x[[5]]<-list()
		x[[5]]$name<-"Computer Security Analyst"
		x[[5]]$start<-2014
		x[[5]]$end<-2018
		x[[5]]$color<-"gray"
		x[[5]]$pics<-list()
		x[[5]]$pics[1]<-system.file("extdata/images/mockup/","security_analyst.jpeg",package="timeLineGraphics")

		x[[6]]<-list()
		x[[6]]$name<-"Tonsillitis"
		x[[6]]$start<-2005
		x[[6]]$end<-2006
		x[[6]]$color<-"red"
		x[[6]]$pics<-list()
		x[[6]]$pics[1]<-system.file("extdata/images/mockup/","tonsillitis.jpeg",package="timeLineGraphics")

		x[[7]]<-list()
		x[[7]]$name<-"Infuenza"
		x[[7]]$start<-2015
		x[[7]]$end<-2016
		x[[7]]$color<-"red"
		x[[7]]$pics<-list()
		x[[7]]$pics[1]<-system.file("extdata/images/mockup/","flu.jpeg",package="timeLineGraphics")

		x[[8]]<-list()
		x[[8]]$name<-"Betty Mae"
		x[[8]]$start<-2011
		x[[8]]$end<-2018
		x[[8]]$color<-"pink"
		x[[8]]$pics<-list()
		x[[8]]$pics[1]<-system.file("extdata/images/mockup/","betty_mae.png",package="timeLineGraphics")
		
		return(x)
}

# timeLineGraphics(example2(),pdfHeight=10,yearsTick=5,nameCEX=3,main="Henry's Life")