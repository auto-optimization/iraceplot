iheatmap <- function(iraceResults, fileName = NULL){






  #If the value in fileName is added the pdf file is created
  if(!is.null(fileName)){
    pdf(paste0(fileName,".pdf"),width = 6.79,height = 2.32)
    plot(p)
    dev.off()
  #If you do not add the value of fileName, the plot is displayed
  }else{
    p
  }
}
