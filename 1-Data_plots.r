source("5-Functions.r")


#read in the data and prepare the data format for plotting
dat.raw=data.read.raw()
gg.dat=data.clean(obj=dat.raw)

#save all the invidual ratings to "individual_ratings_all.csv" for rater variability analysis. 
save.individual.scores(dat=gg.dat$g.uni.dat, filename="individual_ratings_all.csv")
################################################################################

#Figure 3 of the paper
total.err.bar.plot(dat=gg.dat$ga.dat)

total.hist.plot(dat=gg.dat$ga.dat)

##############
#Means scores for groups
group.mean.summary(dat=gg.dat)

##########################################

#Figure 4 of the paper

two.group.bar.err.plot(dat=gg.dat)


########
#Figures 5 and 6 of the paper
######################################################
#write means to ind_mean.csv
ind.crt.mean.std.cmpt(dat=gg.dat, type="mean")

#write sd to ind_sd.csv
ind.crt.mean.std.cmpt(dat=gg.dat, type="sd")

#go to 2-Radar_plot.py to make radar plots for Figures 5 and 6. 
 
#########################################################
#Figure 7 of the paper

ind.hist.plot(dat=gg.dat$g1a.dat, yvar="C1", bw=1/5, my.xlab="Rating Score (Rescaled to 5)", my.title=gg.dat$gg.crt.names[1], ss=5)

ind.hist.plot(dat=gg.dat$g2a.dat, yvar="C2", bw=1/2, my.xlab="Rating Score (Rescaled to 5)", my.title=gg.dat$gg.crt.names[2], ss=2)
 
 
ind.hist.plot(dat=gg.dat$g3a.dat, yvar="C3", bw=1/3, my.xlab="Rating Score (Rescaled to 5)", my.title=gg.dat$gg.crt.names[3], ss=3)
  
  
  
############################################################
#Figure 8 of the paper

ind.hist.plot(dat=gg.dat$g1a.dat, yvar="C1_1", bw=0.25, my.xlab="Rating Score (Out of 5)", my.title=paste0("Q1: ", gg.dat$crt.names[1]))
  
ind.hist.plot(dat=gg.dat$g1a.dat, yvar="C1_2", bw=0.25, my.xlab="Rating Score (Out of 5)", my.title=paste0("Q2: ", gg.dat$crt.names[2]))
 
ind.hist.plot(dat=gg.dat$g1a.dat, yvar="C1_3", bw=0.25, my.xlab="Rating Score (Out of 5)", my.title=paste0("Q3: ", gg.dat$crt.names[3]))

ind.hist.plot(dat=gg.dat$g1a.dat, yvar="C1_4", bw=0.25, my.xlab="Rating Score (Out of 5)", my.title=paste0("Q4: ", gg.dat$crt.names[4]))
 
ind.hist.plot(dat=gg.dat$g1a.dat, yvar="C1_5", bw=0.25, my.xlab="Rating Score (Out of 5)", my.title=paste0("Q5: ", gg.dat$crt.names[5]))

ind.hist.plot(dat=gg.dat$g2a.dat, yvar="C2_1", bw=0.25, my.xlab="Rating Score (Out of 5)", my.title=paste0("Q6: ", gg.dat$crt.names[6]))

ind.hist.plot(dat=gg.dat$g2a.dat, yvar="C2_2", bw=0.25, my.xlab="Rating Score (Out of 5)", my.title=paste0("Q7: ", gg.dat$crt.names[7]))

ind.hist.plot(dat=gg.dat$g3a.dat, yvar="C3_1", bw=0.25, my.xlab="Rating Score (Out of 5)", my.title=paste0("Q8: ", gg.dat$crt.names[8]))
  
ind.hist.plot(dat=gg.dat$g3a.dat, yvar="C3_2", bw=0.25, my.xlab="Rating Score (Out of 5)", my.title=paste0("Q9: ", gg.dat$crt.names[9]))
 
ind.hist.plot(dat=gg.dat$g3a.dat, yvar="C3_3", bw=0.25, my.xlab="Rating Score (Out of 5)", my.title=paste0("Q10: ", gg.dat$crt.names[10]))

################################################################################








 

 