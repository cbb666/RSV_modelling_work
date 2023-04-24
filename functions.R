genHospRate <- function(eachAgeDefIncome,choice=NULL){  # used for meta-analyses for hospitalisation rates
if(length(unique(eachAgeDefIncome$Study))<2){
  return(NULL)
}else{
  message(eachAgeDefIncome$Case_definition[1],eachAgeDefIncome$Agegr[1],eachAgeDefIncome$income[1])
 eachAgeDefIncome <- left_join(eachAgeDefIncome,
                       eachAgeDefIncome %>% group_by(Study) %>% dplyr::summarise(num=n()))
eachAgeDefIncome <- eachAgeDefIncome[order(eachAgeDefIncome$num),]
ref.df <- data.frame(Study=unique(eachAgeDefIncome$Study),order=1:length(unique(eachAgeDefIncome$Study)))
eachAgeDefIncome <- left_join(eachAgeDefIncome,ref.df)
fit_eachAgeDefIncome_meta <- rma.mv(yi, V=sei^2, random = ~ 1 |Study/Study_period, 
                             data = eachAgeDefIncome, 
                             digits=3)
rows <- (1:fit_eachAgeDefIncome_meta$k) + cumsum(c(0,diff(eachAgeDefIncome$order)))
if(choice=="0-11"){
  pdf(file = paste("Figures/meta/",eachAgeDefIncome$Case_definition[1],eachAgeDefIncome$Agegr[1],
                   eachAgeDefIncome$income[1],".pdf", sep = ""),
         width = 12, height = 3+fit_eachAgeDefIncome_meta$k *0.25)
forest(fit_eachAgeDefIncome_meta, 
       rows=rows, 
       slab = eachAgeDefIncome$Study,
       ilab = eachAgeDefIncome$Study_period,
       ilab.xpos=2,
       xlim = c(-6,20),
       # alim=c(-4,10),
       ylim=c(-1,max(rows)+3),
       at = log(c(125,500,2000,8000,32000)), 
       # log(max(all_cause_mortality_meta$`upper CI mean`));log(max(all_cause_mortality_meta$`lower CI mean revised`)) 
       atransf = exp,
       refline =coef(fit_eachAgeDefIncome_meta),
       header=FALSE,
       showweights=TRUE,
       mlab="RE Model",
       cex=1.3,
       xlab = "Rate (log scale)",
       cex.axis=0.8,
       # addpred=TRUE,
       digits = 2)
par(font=2) 
text(-6,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1, "Study", pos = 4)
text(20,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1, "Rate per 100,000 (95% CI)", pos = 2)
text(12.5,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1,"Weight")
text(2,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1,"Study period")
par(font=1) 
# ggsave(forestPlot,
#        filename = paste("Figures/meta/",eachAgeDefIncome$Case_definition[1],eachAgeDefIncome$Agegr[1],".pdf", sep = ""),
#         width = 12, height = 3+fit_eachAgeDefIncome_meta$k *0.25)
  }else{
    pdf(file = paste("Figures/meta/",eachAgeDefIncome$Case_definition[1],eachAgeDefIncome$Agegr[1],
                     eachAgeDefIncome$income[1],".pdf", sep = ""),
        width = 12, height = 3+fit_eachAgeDefIncome_meta$k *0.25)
     forest(fit_eachAgeDefIncome_meta, 
                         rows=rows, 
                         slab = eachAgeDefIncome$Study,
                         ilab = eachAgeDefIncome$Study_period,
                         ilab.xpos=0,# revised
                         xlim = c(-8,18), # revised
                         # alim=c(-4,10),
                         ylim=c(-1,max(rows)+3),
                         at = log(c(25,125,500,2000)), # revised
                         # log(max(all_cause_mortality_meta$`upper CI mean`));log(max(all_cause_mortality_meta$`lower CI mean revised`)) 
                         atransf = exp,
                         refline =coef(fit_eachAgeDefIncome_meta),
                         header=FALSE,
                         showweights=TRUE,
                         mlab="RE Model",
                         cex=1.3,
                         xlab = "Rate (log scale)",
                         cex.axis=0.8,
                         # addpred=TRUE,
                         digits = 2)
    par(font=2) 
    text(-8,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1, "Study", pos = 4)
    text(18,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1, "Rate per 100,000 (95% CI)", pos = 2)
    text(11,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1,"Weight")
    text(0,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1,"Study period")
    par(font=1) 
  } 
 }
}


genMorRate <- function(eachAgeDefIncome,choice=NULL){   # used for meta-analyses for mortality rates
  if(length(unique(eachAgeDefIncome$Study))<2){ 
    return(NULL)
  }else{
    message(eachAgeDefIncome$Case_definition[1],eachAgeDefIncome$Agegr[1],eachAgeDefIncome$income[1])
    eachAgeDefIncome <- left_join(eachAgeDefIncome,
                                  eachAgeDefIncome %>% group_by(Study) %>% dplyr::summarise(num=n()))
    eachAgeDefIncome <- eachAgeDefIncome[order(eachAgeDefIncome$num),]
    ref.df <- data.frame(Study=unique(eachAgeDefIncome$Study),order=1:length(unique(eachAgeDefIncome$Study)))
    eachAgeDefIncome <- left_join(eachAgeDefIncome,ref.df)
    fit_eachAgeDefIncome_meta <- rma.mv(yi, V=sei^2, random = ~ 1 |Study/Study_period, 
                                        data = eachAgeDefIncome, 
                                        digits=3)
    rows <- (1:fit_eachAgeDefIncome_meta$k) + cumsum(c(0,diff(eachAgeDefIncome$order)))
    if(choice %in% c("65-125","75-125")){
      pdf(file = paste("Figures/meta/",eachAgeDefIncome$Case_definition[1],eachAgeDefIncome$Agegr[1],
                       eachAgeDefIncome$income[1],".pdf", sep = ""),
          width = 12, height = 3+fit_eachAgeDefIncome_meta$k *0.25)
      forest(fit_eachAgeDefIncome_meta, 
             rows=rows, 
             slab = eachAgeDefIncome$Study,
             ilab = eachAgeDefIncome$Study_period,
             ilab.xpos=-2,
             xlim = c(-10,16),
             # alim=c(-4,10),
             ylim=c(-1,max(rows)+3),
             at = log(c(5^1,5^2,5^3,5^4)) , 
             # log(max(all_cause_mortality_meta$`upper CI mean`));log(max(all_cause_mortality_meta$`lower CI mean revised`)) 
             atransf = exp,
             refline =coef(fit_eachAgeDefIncome_meta),
             header=FALSE,
             showweights=TRUE,
             mlab="RE Model",
             cex=1.3,
             xlab = "Rate (log scale)",
             cex.axis=0.8,
             # addpred=TRUE,
             digits = 2)
      par(font=2) 
      text(-10,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1, "Study", pos = 4)
      text(16,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1, "Rate per 100,000 (95% CI)", pos = 2)
      text(9.5,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1,"Weight")
      text(-2,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1,"Study period")
      par(font=1) 
      # ggsave(forestPlot,
      #        filename = paste("Figures/meta/",eachAgeDefIncome$Case_definition[1],eachAgeDefIncome$Agegr[1],".pdf", sep = ""),
      #         width = 12, height = 3+fit_eachAgeDefIncome_meta$k *0.25)
    }else{
      pdf(file = paste("Figures/meta/",eachAgeDefIncome$Case_definition[1],eachAgeDefIncome$Agegr[1],
                       eachAgeDefIncome$income[1],".pdf", sep = ""),
          width = 12, height = 3+fit_eachAgeDefIncome_meta$k *0.25)
      forest(fit_eachAgeDefIncome_meta, 
             rows=rows, 
             slab = eachAgeDefIncome$Study,
             ilab = eachAgeDefIncome$Study_period,
             ilab.xpos=-6,# revised
             xlim = c(-14,12),# revised
             # alim=c(-4,10),
             ylim=c(-1,max(rows)+3),
             at = log(c(0.1,0.5,2.5,12.5,62.5)), # revised
             # log(max(all_cause_mortality_meta$`upper CI mean`));log(max(all_cause_mortality_meta$`lower CI mean revised`)) 
             atransf = exp,
             refline =coef(fit_eachAgeDefIncome_meta),
             header=FALSE,
             showweights=TRUE,
             mlab="RE Model",
             cex=1.3,
             xlab = "Rate (log scale)",
             cex.axis=0.8,
             # addpred=TRUE,
             digits = 2)
      par(font=2) 
      text(-14,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1, "Study", pos = 4)
      text(12,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1, "Rate per 100,000 (95% CI)", pos = 2)
      text(6,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1,"Weight")
      text(-6,fit_eachAgeDefIncome_meta$k+length(unique(eachAgeDefIncome$Study))+1,"Study period")
      par(font=1) 
    } 
  }
}
