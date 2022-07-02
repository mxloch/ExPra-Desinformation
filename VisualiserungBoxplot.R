plotOverlappingHist(surveyR01_1$FSumme,surveyR01_2$FSumme)
axis(2, at=seq(0, 14, 2))
legend("topleft",inset = 0.05, c("gerine Kontrolle (N=31)", "hohe Kontrolle (N=41)", "Überschneidung der Gruppen"), 
       col=c("skyblue","steelblue","navyblue"),
       lwd=10, box.lty=0.9, cex=0.6)

plotOverlappingHist(surveyR01_1$groupTime17759,surveyR01_2$groupTime17759)
legend("topright", c("gerine Kontrolle (N=31)", "hohe Kontrolle (N=41)", "Überschneidung der Gruppen"), 
       col=c("skyblue","steelblue","navyblue"),
       lwd=10, box.lty=0.9, cex=0.8)
axis(2, at=seq(0, 25, 5))
