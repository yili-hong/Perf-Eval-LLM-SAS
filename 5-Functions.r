library(ggplot2)
library(fmsb)
library(RColorBrewer)
library(scales)
library(grid)
#library(lme4)
#library(lmerTest)


###################################################################
data.read.raw=function()
{

  g1.dat=read.csv(file="group1_rating.csv", header=T)
  g2.dat=read.csv(file="group2_rating.csv", header=T)
  g3.dat=read.csv(file="group3_rating.csv", header=T)
  
  
  res=list(g1.dat=g1.dat, g2.dat=g2.dat, g3.dat=g3.dat)
   
  return(res)
  
}

######################################################################
data.clean=function(obj)
{
   g1.dat=obj$g1.dat
   g2.dat=obj$g2.dat
   g3.dat=obj$g3.dat
   
   #################################
   g1a.dat=tmp.group.mean.fun(dat=g1.dat, col.ids=c("C1_1", "C1_2", "C1_3", "C1_4", "C1_5"), fun="mean")
   g1sd.dat=tmp.group.mean.fun(dat=g1.dat, col.ids=c("C1_1", "C1_2", "C1_3", "C1_4", "C1_5"), fun="sd")   

   g2a.dat=tmp.group.mean.fun(dat=g2.dat, col.ids=c("C2_1", "C2_2"), fun="mean")
   g2sd.dat=tmp.group.mean.fun(dat=g2.dat, col.ids=c("C2_1", "C2_2"), fun="sd")    
      
   g3a.dat=tmp.group.mean.fun(dat=g3.dat, col.ids=c("C3_1", "C3_2", "C3_3"), fun="mean")
   g3sd.dat=tmp.group.mean.fun(dat=g3.dat, col.ids=c("C3_1", "C3_2", "C3_3"), fun="sd")    

   #################################
   g1a.dat=data.frame(g1a.dat, C1=rowSums(g1a.dat[, c("C1_1", "C1_2", "C1_3", "C1_4", "C1_5")]))
   g2a.dat=data.frame(g2a.dat, C2=rowSums(g2a.dat[, c("C2_1", "C2_2")]))
   g3a.dat=data.frame(g3a.dat, C3=rowSums(g3a.dat[, c("C3_1", "C3_2", "C3_3")]))
   
   #browser()
   
   g1a.dat=g1a.dat[order(g1a.dat[, "ID"], g1a.dat[, "Model"]),]
   g2a.dat=g2a.dat[order(g2a.dat[, "ID"], g2a.dat[, "Model"]),]
   g3a.dat=g3a.dat[order(g3a.dat[, "ID"], g3a.dat[, "Model"]),]
   
   ga.dat=data.frame(g1a.dat[,1:2], Score=g1a.dat[,"C1"]+g2a.dat[,"C2"]+g3a.dat[,"C3"])
   
   
   ############################
   g.uni.dat=tmp.to.univariate(dat1=g1.dat, dat2=g2.dat, dat3=g3.dat)
   g1.uni.dat=tmp.uni.subset(dat=g.uni.dat, gg.name="C1", col.ids=c("C1_1", "C1_2", "C1_3", "C1_4", "C1_5"))
   g2.uni.dat=tmp.uni.subset(dat=g.uni.dat, gg.name="C2", col.ids=c("C2_1", "C2_2"))   
   g3.uni.dat=tmp.uni.subset(dat=g.uni.dat, gg.name="C3", col.ids=c("C3_1", "C3_2", "C3_3"))   
   
   #############################
   
   
   crt.names=c("Data/model Structure", "Data/variable Correctness", "Model and Output", "Readability and Structure", "Code Conciseness", "Variable/dataset Error", "Other Error/warning", "Output Correctness", "Relevance/conciseness", "Output Redundancy")
   
   gg.crt.names=c("Code Quality", "Code Executability", "Output Quality")
   
   #llm.names=c("GPT35", "GPT4", "Llama")
   llm.names=c("GPT-3.5", "GPT-4.0", "Llama-3.1 70B")
   
    
   
   res=list(g1.dat=g1.dat, g2.dat=g2.dat, g3.dat=g3.dat, g1a.dat=g1a.dat, g2a.dat=g2a.dat, g3a.dat=g3a.dat, g1sd.dat=g1sd.dat, g2sd.dat=g2sd.dat, g3sd.dat=g3sd.dat, ga.dat=ga.dat, g.uni.dat=g.uni.dat, g1.uni.dat=g1.uni.dat, g2.uni.dat=g2.uni.dat, g3.uni.dat=g3.uni.dat, crt.names=crt.names, gg.crt.names=gg.crt.names, llm.names=llm.names)

   return(res)
   
}
######################################################################
tmp.group.mean.fun=function(dat, col.ids, fun)
{
   tmp1<- aggregate(x=dat[, col.ids[1]], by = list(ID=dat[,"ID"], Model=dat[,"Model"]),FUN = mean)
   
   res=matrix(0, nrow=nrow(tmp1), ncol=length(col.ids))
   
   for(i in 1:(length(col.ids)))
   {
   
     if(fun=="mean")
     {
       xtmp<- aggregate(x=dat[, col.ids[i]], by = list(ID=dat[,"ID"], Model=dat[,"Model"]),FUN = mean)
     }

     if(fun=="sd")
     {
       xtmp<- aggregate(x=dat[, col.ids[i]], by = list(ID=dat[,"ID"], Model=dat[,"Model"]),FUN = sd)
     }
     
     res[,i]=xtmp[,3]  
        
   }
   
   res=cbind(tmp1[,1:2], res)
   colnames(res)=c(c("ID", "Model"), col.ids)
   res=as.data.frame(res)
   res=res[order(res[,"ID"], res[,"Model"]),]
   return(res)
   
}
######################################################################
tmp.to.univariate=function(dat1, dat2, dat3)
{
  col.ids=c("C1_1", "C1_2", "C1_3", "C1_4", "C1_5")
  
  res=NULL
  
  for(i in 1:(length(col.ids)))
  {
     xdat=data.frame(dat1[,1:3], Criterion=col.ids[i], Score=dat1[, col.ids[i]])
     res=rbind(res, xdat)
  
  }
  
  
  col.ids=c("C2_1", "C2_2")
  
  for(i in 1:(length(col.ids)))
  {
     xdat=data.frame(dat2[,1:3], Criterion=col.ids[i], Score=dat2[, col.ids[i]])
     res=rbind(res, xdat)
  
  }
    
  col.ids=c("C3_1", "C3_2", "C3_3")

  for(i in 1:(length(col.ids)))
  {
     xdat=data.frame(dat3[,1:3], Criterion=col.ids[i], Score=dat3[, col.ids[i]])
     res=rbind(res, xdat)
  
  }  
    
  rownames(res)=NULL
  res=as.data.frame(res)
  return(res)
  
}
######################################################################
tmp.uni.subset=function(dat, gg.name, col.ids)
{
  tdat=dat[dat[,"Criterion"] %in% col.ids, ]

  res<- aggregate(x=tdat[, "Score"], by = list(ID=tdat[,"ID"], Model=tdat[,"Model"], Rater=tdat[,"Rater"]),FUN = sum)
  
  colnames(res)=c("ID", "Model", "Rater", gg.name)
  
  res=res[order(res[,"ID"], res[, "Model"], res[, "Rater"]), ]
  
  rownames(res)=NULL
  
  return(res) 
  
}

######################################################################
total.err.bar.plot=function(dat)
{
  cat("Total scores. \n")
  print(mean(dat[,"Score"])/10)
  print(sd(dat[,"Score"])/10)

  means=tapply(dat[,"Score"], dat[, "Model"], "mean")
  sds=tapply(dat[,"Score"], dat[, "Model"], "sd")
  llm=names(means)
  
  llm[llm=="GPT35"]="GPT-3.5"
  llm[llm=="GPT4"]="GPT-4.0"
  llm[llm=="Llama"]="Llama-3.1 70B"    
  
  
  means=means/10
  sds=sds/10
  
  pdat=data.frame(means=round(means, 3), sds=round(sds, 3), llm=llm)
  

  
  ggplot(pdat, aes(x = llm, y=means, fill = factor(llm))) + 
  geom_col(alpha=0.5) +
  geom_errorbar(aes(ymin = means - sds, ymax = means + sds), width =.2) + 
  geom_label(aes(label = paste(means, "\ub1", sds)), nudge_y = 5/10, size = 5,
             label.size = 0, label.r = unit(0, "pt"), fill = "#ebebeb") +
  geom_point(size = 3)+theme(axis.text=element_text(size=16), axis.title=element_text(size=16), legend.text=element_text(size=16), legend.title=element_text(size=16), legend.position = "none")+labs(x = "LLM", y = "Total Rating Score (Rescaled to 5)")
  
}
#####################################################################
two.group.bar.err.plot=function(dat)
{
  g1a.dat=dat$g1a.dat
  g2a.dat=dat$g2a.dat
  g3a.dat=dat$g3a.dat

  C1=tapply(g1a.dat[,"C1"], g1a.dat[, "Model"], "mean")
  C2=tapply(g2a.dat[,"C2"], g2a.dat[, "Model"], "mean")
  C3=tapply(g3a.dat[,"C3"], g3a.dat[, "Model"], "mean")

  s1=tapply(g1a.dat[,"C1"], g1a.dat[, "Model"], "sd")
  s2=tapply(g2a.dat[,"C2"], g2a.dat[, "Model"], "sd")
  s3=tapply(g3a.dat[,"C3"], g3a.dat[, "Model"], "sd")

  C1=round(C1/25*5, 2)
  C2=round(C2/10*5, 2)
  C3=round(C3/15*5, 2)  

  s1=round(s1/25*5, 2)
  s2=round(s2/10*5, 2)
  s3=round(s3/15*5, 2)  




  gg.crt.names=dat$gg.crt.names
  
  print(gg.crt.names)
  
  data=rbind(data.frame(gg="C1", model=names(C1), means=C1, sds=s1), data.frame(gg="C2", model=names(C2), means=C2, sds=s2), data.frame(gg="C3", model=names(C3), means=C3, sds=s3))
  
  rownames(data)=NULL
  



  data[data[,"model"]=="GPT35", "model"]="GPT-3.5"
  data[data[,"model"]=="GPT4", "model"]="GPT-4.0"
  data[data[,"model"]=="Llama", "model"]="Llama-3.1 70B"
  


g1 <- ggplot(data = data, aes(x = interaction(model, gg), y = means, fill = factor(model))) +
  geom_col(alpha=0.5) +
  coord_cartesian(ylim = c(0, 26.25/25*5))+
  geom_errorbar(aes(ymin = means - sds, ymax = means + sds), width =.15) + 
  geom_label(aes(label = paste(means, "\ub1", sds)), nudge_y = 2.5/25*5, size = 4,
             label.size = 0, label.r = unit(0, "pt"), fill = "#ebebeb") +
  geom_point(size = 2)+
  annotate("text", x = 1:9, y = - 3/25*5,
           label = rep(c("GPT-3.5", "GPT-4.0","Llama-3.1 70B"), 3), size=5) +
  annotate("text", c(2, 5, 8), y = -4.5/25*5, label = c("Code Quality (Rescaled to 5)", "Code Executability (Rescaled to 5)", "Output Quality (Rescaled to 5)"), size=5) + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), legend.text=element_text(size=16), legend.title=element_text(size=16), legend.position = "none")+theme(plot.margin = unit(c(1, 1, 4, 1), "lines"), axis.title.x = element_blank(), axis.text.x = element_blank())+labs(y = "Rating Score")


g2 <- ggplot_gtable(ggplot_build(g1))
g2$layout$clip[g2$layout$name == "panel"] <- "off"
grid.draw(g2) 
}

#####################################################################
total.hist.plot=function(dat)
{
  dat[,"Score"]=dat[,"Score"]/10
  
  dat[dat[,"Model"]=="GPT35", "Model"]="GPT-3.5"
  dat[dat[,"Model"]=="GPT4", "Model"]="GPT-4.0"
  dat[dat[,"Model"]=="Llama", "Model"]="Llama-3.1 70B" 

  
  p<-ggplot(dat, aes(x=Score, fill=Model, color=Model)) + geom_histogram(position="identity", alpha=0.5, binwidth=2/10)+facet_grid(Model ~ .)+theme(axis.text=element_text(size=16), axis.title=element_text(size=16), legend.text=element_text(size=16), legend.title=element_text(size=16), legend.position = "none", strip.text.y = element_text(size = 16))+labs(x = "Total Rating Score (Rescaled to 5)", y = "Counts")
  
  p
  
}  
######################################################################


################################################################################
ind.hist.plot=function(dat, yvar="C1", bw=1, my.xlab="Rating Score (Out of 30)", my.title="Code Quality", ss=1)
{
  
  dat1=data.frame(Model=dat[, c("Model")], Score=dat[,yvar]/ss)
  
  
  dat1[dat1[,"Model"]=="GPT35", "Model"]="GPT-3.5"
  dat1[dat1[,"Model"]=="GPT4", "Model"]="GPT-4.0"
  dat1[dat1[,"Model"]=="Llama", "Model"]="Llama-3.1 70B"
  
  p<-ggplot(dat1, aes(x=Score, fill=Model, color=Model)) + geom_histogram(position="identity", alpha=0.5, binwidth=bw)+facet_grid(Model ~ .)+theme(axis.text=element_text(size=16), axis.title=element_text(size=16), legend.text=element_text(size=16), legend.title=element_text(size=16), legend.position = "none", strip.text.y = element_text(size = 16))+labs(x = my.xlab, y = "Counts", title=my.title)+theme(plot.title = element_text(hjust = 0.5), title =element_text(size=16))
  
  p
  
}
#####################################################################
ind.crt.mean.std.cmpt=function(dat, type="mean")
{
  g1a.dat=dat$g1a.dat
  g2a.dat=dat$g2a.dat
  g3a.dat=dat$g3a.dat

  res=matrix(0, nrow=3, ncol=10)

  if(type=="mean")
  {
   for(i in 1:5)
   {
    print(colnames(g1a.dat)[i+2])
    res[,i]=tapply(g1a.dat[,i+2], g1a.dat[, "Model"], "mean")
    
   }

   for(i in 6:7)
   {
    res[,i]=tapply(g2a.dat[,i-3], g2a.dat[, "Model"], "mean")
    print(colnames(g2a.dat)[i-3])
   }

   for(i in 8:10)
   {
    tmp=tapply(g3a.dat[,i-5], g3a.dat[, "Model"], "mean")
    res[,i]=tmp
    print(colnames(g3a.dat)[i-5])
   }

   xx=paste0("Q", 1:10, ": ")
   colnames(res) <- paste0(xx, dat$crt.names)
   rownames(res) <- names(tmp)
   
   res <- as.data.frame(res)
   res=cbind(Model=rownames(res), res)

   res[,"Model"]=c("GPT-3.5", "GPT-4.0", "Llama-3.1 70B")
   print(res)
   
   
   write.csv(res, file="ind_mean.csv", row.names=F)
  }

  if(type=="sd")
  {
   for(i in 1:5)
   {
    print(colnames(g1a.dat)[i+2])
    res[,i]=tapply(g1a.dat[,i+2], g1a.dat[, "Model"], "sd")
    
   }

   for(i in 6:7)
   {
    res[,i]=tapply(g2a.dat[,i-3], g2a.dat[, "Model"], "sd")
    print(colnames(g2a.dat)[i-3])
   }

   for(i in 8:10)
   {
    tmp=tapply(g3a.dat[,i-5], g3a.dat[, "Model"], "sd")
    res[,i]=tmp
    print(colnames(g3a.dat)[i-5])
   }

   xx=paste0("Q", 1:10, ": ")
   colnames(res) <- paste0(xx, dat$crt.names)
   rownames(res) <- names(tmp)
   
   res <- as.data.frame(res)
   res=cbind(Model=rownames(res), res)

   res[,"Model"]=c("GPT-3.5", "GPT-4.0", "Llama-3.1 70B")
   print(res)

   write.csv(res, file="ind_sd.csv", row.names=F)
  }   
   


}



################################################################################
group.mean.summary=function(dat)
{
  g1a.dat=dat$g1a.dat
  g2a.dat=dat$g2a.dat
  g3a.dat=dat$g3a.dat

  cat("Total score out of 25, 10, 15 for each group.\n")
  print(c(mean(g1a.dat[,"C1"]), mean(g1a.dat[,"C1"])/25*100, sd(g1a.dat[,"C1"])))
  print(c(mean(g2a.dat[,"C2"]), mean(g2a.dat[,"C2"])/10*100, sd(g2a.dat[,"C2"])))
  print(c(mean(g3a.dat[,"C3"]), mean(g3a.dat[,"C3"])/15*100, sd(g3a.dat[,"C3"])))

  cat("Rescaled to 5.\n")
  print(c(mean(g1a.dat[,"C1"]/5), mean(g1a.dat[,"C1"])/25*100, sd(g1a.dat[,"C1"]/5)))
  print(c(mean(g2a.dat[,"C2"]/2), mean(g2a.dat[,"C2"])/10*100, sd(g2a.dat[,"C2"]/2)))
  print(c(mean(g3a.dat[,"C3"]/3), mean(g3a.dat[,"C3"])/15*100, sd(g3a.dat[,"C3"]/3)))


}
################################################################################

save.individual.scores=function(dat, filename)
{
   #browser()  
   write.csv(dat, file=filename, row.names=F)

}


###################################################################

bootstrap_LMM_task_rater <- function(model, df, B = 500, seed = 12345) 
{
  set.seed(seed)
  re_task  <- ranef(model)$ID[[1]]
  re_rater <- ranef(model)$Rater[[1]]
  task_eff  <- as.numeric(re_task)
  rater_eff <- as.numeric(re_rater)
  var_task  <- as.numeric(VarCorr(model)$ID[1])
  var_rater <- as.numeric(VarCorr(model)$Rater[1])
  var_eps   <- sigma(model)^2
  task_eff_adj  <- sqrt(var_task  / mean(task_eff^2))  * task_eff
  rater_eff_adj <- sqrt(var_rater / mean(rater_eff^2)) * rater_eff
  eps     <- resid(model)
  eps_adj <- sqrt(var_eps / mean(eps^2)) * eps
  df2 <- df |> arrange(ID, Rater)
  beta <- fixef(model)
  p    <- length(beta)
  C1 <- rep(0, p)
  C2 <- rep(0, p)
  C3 <- rep(0, p)
  idx_GPT4  <- which(names(beta) == "ModelGPT4")
  idx_Llama <- which(names(beta) == "ModelLlama")
  C1[idx_GPT4]  <- 1     
  C2[idx_Llama] <- 1    
  C3[idx_Llama] <- 1     
  C3[idx_GPT4]  <- -1    
  out1 <- out2 <- out3 <- outRater <- outTaskUID <- outResid <-numeric(B)
  
  conv.status<-numeric(B)
  
  for (b in 1:B) 
  {
    cat("Bootstrap ", b, "done.\n")
    task_star  <- sample(task_eff_adj,  length(task_eff_adj), replace = TRUE)
    rater_star <- sample(rater_eff_adj, length(rater_eff_adj), replace = TRUE)
    eps_star   <- sample(eps_adj, nrow(df2), replace = TRUE)  
    df2$u_task  <- task_star[df2$ID]
    df2$u_rater <- rater_star[df2$Rater]
    X <- model.matrix(~ Model + Criterion, df2)
    df2$y_star <- X %*% beta + df2$u_task + df2$u_rater + eps_star
    fit_b <- lmer(
      y_star ~ Model + Criterion + (1 | ID) + (1 | Rater),
      data = df2,
      REML = TRUE
    )
    
    conv.status[b]=is.null(fit_b@optinfo$conv$lme4$messages)
    
    params <- fixef(fit_b)
    out1[b] <- sum(C1 * params)  # GPT4 vs GPT35
    out2[b] <- sum(C2 * params)  # Llama vs GPT35
    out3[b] <- sum(C3 * params)  # Llama vs GPT4
    outRater[b] <- VarCorr(fit_b)$Rater[1]
    outTaskUID[b] <- VarCorr(fit_b)$ID[1]
    vc <- VarCorr(fit_b)
    resid_var <- attr(vc, "sc")^2
    outResid[b] <- resid_var
  }
  
  ci <- function(x)
  { 
     quantile(x, c(0.025, 0.975))
  }
  
  bootstrap_pvalue <- function(samples) 
  {
    2 * min(mean(samples >= 0), mean(samples <= 0))
  }
  
  idx=(conv.status==1)
  print(sum(idx))
  
  p_raw <- c(
    bootstrap_pvalue(out1[idx]),
    bootstrap_pvalue(out2[idx]),
    bootstrap_pvalue(out3[idx])
  )
  
  names(p_raw) <- c("GPT4_vs_GPT35", "Llama_vs_GPT35", "Llama_vs_GPT4")
  

  
  res=list(
    ci = list(
      GPT4_vs_GPT35  = ci(out1[idx]),
      Llama_vs_GPT35 = ci(out2[idx]),
      Llama_vs_GPT4  = ci(out3[idx])
    ),
    mean_est = list(
      GPT4_vs_GPT35_avg = mean(out1[idx]),
      Llama_vs_GPT35_avg = mean(out2[idx]),
      Llama_vs_GPT4_avg = mean(out3[idx])
    ),
    p_raw = p_raw,
    p_BH = p.adjust(p_raw, method = "BH")
  )
  
  return(res)
  
}
############################################################################
