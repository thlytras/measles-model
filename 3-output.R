# Run this script third, after running runMe.R and sens.R

library(coda)

load("res.RData")

cairo_pdf("Figure2.pdf", width=10, height=6, pointsize=14)
(function(){
  par(mar=c(6,4.5,3,2), family="Fira Sans")  
  plot(density(v("Re[1]")), col="orchid", lwd=4, bty="l", main=NA,
    xlab=expression(paste("Effective reproduction number", (R[e]))),
    xlim=c(0,1.7), ylim=c(0,5.3), ylab="Posterior probability density")
  points(density(v("Re[2]")), col="deepskyblue", lwd=4, type="l")
  points(density(v("Re[3]")), col="seagreen", lwd=4, type="l")
  abline(v=1, lty="dotted", lwd=2, col="gray")
  legend("topright", colnames(data1)[1:3], 
    col=c("orchid", "deepskyblue", "seagreen"), lwd=4, bty="n", seg.len=5)
  text(x=a["Re[1]",3], y=3.6, sprintf("%.2f\n(%.2f–%.2f)", a["Re[1]",3], a["Re[1]",1], a["Re[1]",5]), 
    col="orchid", family=2, cex=0.9)
  text(x=a["Re[2]",3], y=3.1, sprintf("%.2f\n(%.2f–%.2f)", a["Re[2]",3], a["Re[2]",1], a["Re[2]",5]), 
    col="deepskyblue", family=2, cex=0.9)
  text(x=a["Re[3]",3], y=4.6, sprintf("%.2f\n(%.2f–%.2f)", a["Re[3]",3], a["Re[3]",1], a["Re[3]",5]), 
    col="seagreen", family=2, cex=0.9)  
})()
dev.off()



cairo_pdf("Figure3.pdf", width=10, height=6, pointsize=14)
(function(){
  par(mar=c(5,5.5,3,2), family="Fira Sans")
  ciCol <- rgb(t(col2rgb("skyblue")), alpha=70, max=255)
  ciCol2 <- rgb(t(col2rgb("skyblue")), alpha=120, max=255)
  fep <- t(sapply(2:52, function(i) round(quantile(v(varnames(res)[15+i]) + v(varnames(res)[15+52+i]) + v(varnames(res)[15+2*52+i]), c(0.5,0.025,0.975)))))
  hci <- rbind(cbind(rowSums(data1[,-4]), NA, NA), fep)
  bp <- barplot(hci[,1], names.arg="", las=1, ylim=c(0,max(pretty(hci))),
    col=c(rep("skyblue4", nrow(data1)), rep(ciCol2, nrow(hci)-nrow(data1))),
    border=NA, main=NA)
  axis(1, at=c(mean(bp[13:14,]), mean(bp[26:27,]), mean(bp[39:40,]), mean(bp[52:53,])),
    label=c("01/2018", "26/2018", "01/2019", "26/2019"))
  abline(v=mean(bp[17:18,]), lty="dotted")
  mtext("Week of symptom onset (1 bar = 2 weeks)", side=1, line=2.5)
  mtext("Number of reported / projected cases", side=2, line=3.5)
  polygon(x=c(bp, rev(bp)), y=c(hci[,2], rev(hci[,3])), col=ciCol, border=NA)
})()
dev.off()



load("sens1.RData")
cairo_pdf("Figure4.pdf", width=10, height=10, pointsize=18)
(function(){
  par(oma=c(3,0,0,0), mar=c(3,5,3,3), family="Fira Sans", mfrow=c(3,1))  
  cols <- c("darkorchid","deepskyblue","seagreen")
  trCol <- lapply(cols, function(x) rgb(t(col2rgb(x)), alpha=40, max=255))
  for (i in 1:3) {
    plot(1:(dim(sensRes)[3]), type="n", bty="n", xaxt="n", xlab=NA, ylab="Re", ylim=c(0, max(pretty(sensRes[i+3,,]))))
    points(sensRes[i+3,2,], type="l", lwd=3, col=cols[i])
    polygon(x=c(1:(dim(sensRes)[3]), (dim(sensRes)[3]):1), y=c(sensRes[i+3,1,], rev(sensRes[i+3,3,])), col=trCol[[i]], border=NA)
    axis(1, 1:(dim(sensRes)[3]), labels=NA)
    axis(1, 1:(dim(sensRes)[3]), labels=gsub("/", "\n", rownames(data1)[6:17]), line=1, lwd=0)
    legend("topright", c("Greek Roma", "Greek non-Roma", "Foreign")[i], col=cols[i], bty="n", seg.len=4, lwd=3, inset=0.05)
    abline(h=1, lty="dotted")
    mtext("Week number", cex=0.7, side=1, line=3.5)
  }
})()
dev.off()


