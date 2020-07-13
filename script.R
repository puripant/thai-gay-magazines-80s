# install.packages(c("pdftools", "png"))
library(pdftools)
library(png)
library(tools)

path <- "mithuna_junior"
path_short <- "mithuna"
issue_num <- 96

# scale <- 40
gap <- 1
page_height <- 15

## generate each page and combine into an issue
# files <- list.files(path=path, pattern="*.pdf", full.names=FALSE, recursive=TRUE)
# lapply(files, function(file) {
for (count in 1:issue_num) {
  tryCatch({
    folder_path <- paste(path, "/issue", count, "/", sep="")
    filename_sans_ext <- paste(path_short, count, sep="")

    setwd(folder_path)

    pdf_path <- paste(filename_sans_ext, ".pdf", sep="")
    pages_stat <- pdf_pagesize(pdf_path)
    # pdf_convert(pdf_path)

    setwd("../..")

    cumwidth <- rbind(0, cumsum(pages_stat["width"]*page_height/pages_stat["height"])) #rbind(0, cumsum(pages_stat["width"]/scale))
    totalwidth <- 2000 #sum(pages_stat["width"]/scale) + nrow(pages_stat)*gap
    totalheight <- page_height #max(pages_stat["height"]/scale)

    png(paste(folder_path, filename_sans_ext, ".png", sep=""), totalwidth, totalheight)
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i')
    plot(0, 0, type='n', xlim=c(0, totalwidth), ylim=c(0, totalheight), asp=1, bty="n", axes=FALSE)
    for (i in 1:nrow(pages_stat)) {
      fname <- paste(folder_path, filename_sans_ext, "_", i, ".png", sep="")
      img <- readPNG(fname)

      # imgwidth <- pages_stat[i,'width']/scale
      # imgheight <- pages_stat[i,'height']/scale
      imgwidth <- dim(img)[2]*page_height/dim(img)[1] #dim(img)[2]/scale
      imgheight <- page_height #dim(img)[1]/scale

      x <- cumwidth[i, "width"] + i*gap
      y <- 0

      rasterImage(img, xleft=x, ybottom=y+imgheight, xright=x+imgwidth, ytop=y)
    }
    dev.off()
  }, error=function(e){ cat("ERROR :", conditionMessage(e), "\n") })
}

## combine issues for a big pictures
totalwidth <- 2200
totalheight <- 100 + issue_num*(page_height + gap)

png(paste(path, "/", path_short, ".png", sep=""), totalwidth, totalheight)
par(mar=c(0,0,0,0), xaxs='i', yaxs='i')
plot(0, 0, type='n', xlim=c(0, totalwidth), ylim=c(0, totalheight), asp=1, bty="n", axes=FALSE)

for (count in 1:issue_num) {
  tryCatch({
    folder_path <- paste(path, "/issue", count, "/", sep="")
    filename_sans_ext <- paste(path_short, count, sep="")
    
    fname <- paste(folder_path, filename_sans_ext, ".png", sep="")
    img <- readPNG(fname, info=TRUE)
    
    imgwidth <- dim(img)[2]
    imgheight <- dim(img)[1]
    
    x <- 50
    y <- 40 + count*(imgheight + gap)
    
    rasterImage(img, xleft=x, ybottom=y+imgheight, xright=x+imgwidth, ytop=y)
  }, error=function(e){ cat("ERROR :", conditionMessage(e), "\n") })
}

dev.off()