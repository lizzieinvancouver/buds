## Started on 8 May 2018 ## 
## By Lizzie ##

## It makes the chill x force figures for just the 5 species of overlap with have with Isabelle #
## And it colors them by wood structure ##

## IMPORTANT: To run this code, you first run Pheno Budburst analysis.R #
# until Tidbit here does this figure for Isabelle ....
# run the code within the FALSE command then run the below ##

plotletsm <- function(x, y, xlab=NULL, ylab=NULL, data, groups = NULL, ...){
  if(is.null(xlab)) xlab = x; if(is.null(ylab)) ylab = y
  if(is.null(groups)) { col.pch = "black"; col.lines = "grey50" }
    else {
      colz = c("brown", "blue3")
      ccolz = rep(colz[1], length(groups))
      ccolz[groups == 2] = colz[2]
      col.pch = ccolz
      col.lines = alpha(ccolz, 0.4)
    }
  
  plot(
  data[grep(paste(x,"\\[",sep=""), rownames(data)),1],
  data[grep(paste(y,"\\[",sep=""), rownames(data)),1],
  pch = "+",
  ylab = ylab,
  xlab = xlab,
  col = col.pch,
  ...
  )

  abline(h=0, lty = 3, col = "grey60")
  abline(v=0, lty = 3, col = "grey60")
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"25%"],
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"75%"],
    length = 0, col = col.lines)
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"25%"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"75%"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"],
    length = 0, col = col.lines)
  
  # match with species names
  text( data[grep(paste(x,"\\[",sep=""), rownames(data)),1],
        data[grep(paste(y,"\\[",sep=""), rownames(data)),1],
        spphere,
        cex = 0.5, 
        pos = 3,
        col = col.pch)
}

# Set up the data
# This is not so pretty, but works for now
sumerb.sm <- sumerb[1,]
for (i in c(1:length(spprow))){
    bitbybit <- sumerb[grep(paste("\\[", spprow[i], "\\]", sep=""), rownames(sumerb)),]
    sumerb.sm <- rbind(sumerb.sm, bitbybit)
    }
sumerb.sm <- sumerb.sm[-1,]

sumerl.sm <- sumerl[1,]
for (i in c(1:length(spprow))){
    bitbybit <- sumerl[grep(paste("\\[", spprow[i], "\\]", sep=""), rownames(sumerl)),]
    sumerl.sm <- rbind(sumerl.sm, bitbybit)
    }
sumerl.sm <- sumerl.sm[-1,]

porestrue <- c(5,5,4,4,3) # this is the data we have in Species traits.xlsx, according to http://insidewood.lib.ncsu.edu/menu/type/modern?2 5 is diffuse, 4 is semi-ring and 3 is ring
pores <- c(1,1,2,2,2)

# And the figure ....
pdf(file.path(figpath, "FigChill2_4panel_IsabelleSpp.pdf"), width = 7, height = 7)

par(mar=rep(1,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = TRUE),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n",bty="n",xaxt="n",yaxt="n",ylab="",xlab="")}

plotblank() 
text(5,5, "Budburst \n Change (days) due to 5° warming", font = 2, srt = 90) # \n\n add two line breaks

plotletsm("b_chill1", "b_warm",
         #  ylab = "Advance due to  chilling", 
         # xlab = "Advance due to 4 hr longer photoperiod", 
         ylim = c(-27, 0.5),
         xlim = c(-28, -4),
         #  xaxt="n",
         group = pores,
         data = sumerb.sm)

legend("topleft", bty = "n", inset = 0.035, legend = "A.", text.font=2)

legend("bottomright",
       pch = "+",
       col = colz,
       legend = c("Diffuse","Ring and semi-ring"),
       inset = 0.02, 
       bg = 'white')

plotletsm("b_chill2", "b_warm", 
        # ylab = "Advance due to 5° warming", 
        #  xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-27, 0.5),
        xlim = c(-28, -4),
        yaxt="n",
        # xaxt="n",
        group = pores,
        data = sumerb.sm)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "B.", text.font=2)

plotblank()
text(5,5, "Leafout \n Change (days) due to 5° warming", font = 2, srt = 90)

plotletsm("b_chill1", "b_warm", 
        #    ylab = "Advance due to 5° warming", 
        #     xlab = "Advance due to 4 hr longer photoperiod", 
        ylim = c(-27, 0.5),
        xlim = c(-28, -4),
        group = pores,
        data = sumerl.sm)
legend("topleft", bty = "n", inset = 0.035, legend = "C.", text.font=2)

plotletsm("b_chill2", "b_warm", 
        #   ylab = "Advance due to 5° warming", 
        #   xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-27, 0.5),
        xlim = c(-28, -4),
        yaxt="n",
        group = pores,
        data = sumerl.sm)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "D.", text.font=2)

plotblank()

plotblank()
text(5.5, 5, "Change (days) due to 30d 4.0° chilling", font = 2, pos = 3)

plotblank()
text(5.5, 5, "Change (days) due to 30d 1.5° chilling", font = 2, pos = 3)

dev.off();

