## Prosocial Emotion Appraisal Study

####### TABLE OF CONTENTS #######
#
# 1. Variable List
# 2. Import Data/Load Packages
# 3. Load/Write Functions
# 4. Prepare Data for Analysis
#
##################################


##### 1. Variable List #####

# SubjectID: Subject identification number
# experiment: Experimenter, 1 = Yuching, 2 = Miriam, 3 = Eric

# pitygroup, compgroup, empgroup, tengroup, symgroup: codes the grouping of emotions; emotions with the same group are considered the same thing by the subject
# pityexclude, compexclude, tendexclude, empexclude, sympexclude: if the code is 1, then subjects didn't write about that emotion because they said it was the same as another emotion


## Appraisals
# 7 point scale with verbal anchors; 1 = not at all, 2 = slightly, 3= somewhat, 4 = moderately, 5 = very much, 6 = mostly, 7 = extremely
# only the pity appraisals are listed, the appraisals for the other emotions are the same and just change the p to the first letter of the emotion

## pleasantness
# ppleas: How pleasant did you feel in the situation? 
# punpleas: How unpleasant did you feel in the situation?

## goal conduciveness/goal relevance
# pwanted: How much did you feel like the situation was something that you wanted to happen? 
# pnotwanted: How much did you feel like the situation was something that you did not want to happen?

## responsibility for situation
# pmecause: How much did you feel like you caused the situation?
# pmeresp: How much did you feel responsible for what happened?
# psitcont: How much did you feel like the situation was due to circumstances that were beyond anyone's control?
# pnoblame: How much did you feel like the events happened because somebody was to blame?
# ptargcause: Think about the person or people who you felt pity for. How much did you feel like they caused what happened?
# ptargresp: Think about the person or people who you felt pity for. How much did you feel like they were responsible for what happened?
# p3pcause: Aside from you and the person or people who you felt pity for, how much did you feel like someone else caused the situation?
# p3presp: Aside from you and the person or people who you felt pity for, how much did you feel like someone else was responsible for what happened?

## stability
# ptemp: When you were feeling pity, how much did you feel like the situation was only temporary and would change?
# plastlong: When you were feeling pity, did you feel like the situation would last for a long time?

## coping potential
# pinfluence: When you were feeling pity, how much dif you feel that you had power to influence the situation?
# ppowercare: Think about the person or people who you felt pity for. When you were feeling pity, how much did you feel that you had power to take care of them?

## self-relevance
# pclose: Think about the person or people who you felt pity for. When you were feeling pity, how close to them did you feel?
# psim: Think about the person or people who you felt pity for. Did you think they were like you?

## superiority
# pbetter: Think about the person or people you felt pity for. When you were feeling pity, how much did you feel like you were better than them?
# plookdown: Think about the person or people you felt pity for. When you were feeling pity, how uch did you feel like you were looking down on them?

## vulnerability - disposition
# pdvuln: Think about the person or people who you felt pity for. How much did you feel like they were generally vulnerable?
# pdtakecare: Think about the person or people who you felt pity for. How much did you feel like they were the kind of people who could take care of themselves in general?

## vulnerability - situation
# psvuln: Think about the situation the person or people who you felt pity for were in. How much did you feel like the situation would make most people vulnerable?
# pstakecare: Think about the situation the person or people who you felt pity for were in. How much did you feel like most people would be able to take care of themselves in the same situation?

## deservingness
# pdeserve: Think about the person or people who you felt pity for. How much did you feel like they deserved your pity?
# pothers: Think about the person or people who you felt pity for. How much did you feel like most people should feel pity for them as well?

## responsibility for fixing problem
# pmedo: Think about what caused you to feel pity. How much did you feel like you should do something about the situation?
# pmecare: Think about the person or people who you felt pity for. How much did you feel like you should take care of them?
# ptargdo: Think about what caused you to feel pity. How much did you feel like they should do something about the situation?
# ptargcare: Think about the person or people who you felt pity for. How much did you feel like they should take care of themselves?
# p3pdo: Aside from you and the person or people who you felt pity for, how much did you feel like someone else should do something about the situation?
# p3pcare: Think about the person or people you felt pity for. Aside from you and them, how much did you feel like someone else should take care of them?




##### 2. Import Data/Load Packages #####

pe <- read.csv('pedata.csv', header=TRUE)
pe <- pe[pe$total_prob!=1,]

library(psych)
library(ggplot2)


##### 3. Load/Write Functions #####

source('../R functions/multiplot.R')

## Write function to complete skipped emotions with a synonymous completed emotion
fill.emot <- function(focal, ...) {
  
  # put the non-focal emotions in a list
  others <- list(...)
  
  # check if there was a problem with the particular emotion or if subjects saw this as a unique emotion
  if(focal[1]==1 | focal[2]==0) {
    focal <- focal
  } else {  # otherwise fill it in with the first emotion that was in the same group
    j <- 1
    
    # check to see if the group for the focal emotion is the same as the group of emotion j
    # also make sure that emotion j was not excluded
    # also make sure that there was no problem with emotion j
    # if any condition is not met, then move on to the j + 1 group
    while(focal[3]!=unlist(others[[j]][3]) | others[[j]][2]==1 | others[[j]][1]==1) {
      j <- j+1
      if(j>length(others)){break} # if the counter j exceeds the number of emotions, then the subject did not group the focal emotion with anything else, so quit
    }
    
    # if the while loop was able to find another emotion with the same group, then fill in the focal emotion with data from emotion j, otherwise leave it alone
    if(j <= length(others)) { 
      focal <- unlist(others[[j]])
    } else {focal <- focal}
  }
  
  return(focal)
}


##### 4. Prepare Data for Analysis #####

### separate data by emotion
pity <- with(pe, data.frame(pity_prob, exclude_pity, pity_group, ppleas, pwanted, pnotwanted, pmecause, ptemp, plastlong, pinfluence, pmeresp, psitcont, pclose, punpleas, psim, pbetter, pnoblame, plookdown, pdvuln, ptargcause, ptargresp, p3pcause, p3presp, psvuln, pstakecare, pdeserve, pdtakecare, pmedo, pmecare, ptargdo, ptargcare, p3pdo, p3pcare, ppowercare, pothers))

tender <- with(pe, data.frame(tend_prob, exclude_tenderness, tend_group, tpleas, twanted, tnotwanted, tmecause, ttemp, tlastlong, tinfluence, tmeresp, tsitcont, tclose, tunpleas, tsim, tbetter, tnoblame, tlookdown, tdvuln, ttargcause, ttargresp, t3pcause, t3presp, tsvuln, tstakecare, tdeserve, tdtakecare, tmedo, tmecare, ttargdo, ttargcare, t3pdo, t3pcare, tpowercare, tothers))  

empathy <- with(pe, data.frame(emp_prob, exclude_empathy, emp_group, epleas, ewanted, enotwanted, emecause, etemp, elastlong, einfluence, emeresp, esitcont, eclose, eunpleas, esim, ebetter, enoblame, elookdown, edvuln, etargcause, etargresp, e3pcause, e3presp, esvuln, estakecare, edeserve, edtakecare, emedo, emecare, etargdo, etargcare, e3pdo, e3pcare, epowercare, eothers))

symp <- with(pe, data.frame(symp_prob, exclude_sympathy, symp_group, spleas, swanted, snotwanted, smecause, stemp, slastlong, sinfluence, smeresp, ssitcont, sclose, sunpleas, ssim, sbetter, snoblame, slookdown, sdvuln, stargcause, stargresp, s3pcause, s3presp, ssvuln, sstakecare, sdeserve, sdtakecare, smedo, smecare, stargdo, stargcare, s3pdo, s3pcare, spowercare, sothers))

comp <- with(pe, data.frame(comp_prob, exclude_compassion, comp_group, cpleas, cwanted, cnotwanted, cmecause, ctemp, clastlong, cinfluence, cmeresp, csitcont, cclose, cunpleas, csim, cbetter, cnoblame, clookdown, cdvuln, ctargcause, ctargresp, c3pcause, c3presp, csvuln, cstakecare, cdeserve, cdtakecare, cmedo, cmecare, ctargdo, ctargcare, c3pdo, c3pcare, cpowercare, cothers))


## Fill in excluded emotions with synonymous emotion data
symp <- data.frame(t(sapply(1:dim(symp)[1], simplify=TRUE, function(i) fill.emot(symp[i,], comp[i,], pity[i,], tender[i,], empathy[i,]))))
comp <- data.frame(t(sapply(1:dim(comp)[1], function(i) fill.emot(comp[i,], symp[i,], pity[i,], tender[i,], empathy[i,]))))
tender.names <- names(tender) # for some reason, the function replaces the names of the variables in the tenderness data with the names of the variables in the compassion data, so this will be used to reassign the names
tender <- data.frame(t(sapply(1:dim(tender)[1], function(i) fill.emot(tender[i,], symp[i,], pity[i,], comp[i,], empathy[i,]))))
names(tender) <- tender.names
empathy <- data.frame(t(sapply(1:dim(empathy)[1], function(i) fill.emot(empathy[i,], symp[i,], pity[i,], comp[i,], tender[i,]))))
pity <- data.frame(t(sapply(1:dim(pity)[1], function(i) fill.emot(pity[i,], symp[i,], empathy[i,], comp[i,], tender[i,]))))

## Unlist everything in the data frame
symp <- data.frame(t(apply(symp, 1, unlist)))
comp <- data.frame(t(apply(comp, 1, unlist)))
empathy <- data.frame(t(apply(empathy, 1, unlist)))
tender <- data.frame(t(apply(tender, 1, unlist)))
pity <- data.frame(t(apply(pity, 1, unlist)))


## If there was a problem with the particular emotion, set all values to NA
pity[pity$pity_prob==1, 4:length(pity)] <- NA
tender[tender$tend_prob==1, 4:length(tender)] <- NA
empathy[empathy$emp_prob==1, 4:length(empathy)] <- NA
symp[symp$symp_prob==1, 4:length(symp)] <- NA
comp[comp$comp_prob==1, 4:length(comp)] <- NA


## Recombine the data
pe <- cbind(pe[,1:4], symp, pity, comp, empathy, tender)



##### 5. Grouping Frequencies #####
group_matrix <- matrix(rep(NA, 25), nrow=5)
colnames(group_matrix) <- c('pity','compassion','sympathy','empathy','tenderness')
rownames(group_matrix) <- c('pity','compassion','sympathy','empathy','tenderness')
diag(group_matrix) <- rep(196, 5)


group_matrix[2,1] <- group_matrix[1,2] <- table(pe$pity_group==pe$comp_group)[2]
group_matrix[3,1] <- group_matrix[1,3] <- table(pe$pity_group==pe$symp_group)[2]
group_matrix[4,1] <- group_matrix[1,4] <- table(pe$pity_group==pe$emp_group)[2]
group_matrix[5,1] <- group_matrix[1,5] <- table(pe$pity_group==pe$tend_group)[2]

group_matrix[3,2] <- group_matrix[2,3] <- table(pe$comp_group==pe$symp_group)[2]
group_matrix[4,2] <- group_matrix[2,4] <- table(pe$comp_group==pe$emp_group)[2]
group_matrix[5,2] <- group_matrix[2,5] <- table(pe$comp_group==pe$tend_group)[2]

group_matrix[4,3] <- group_matrix[3,4] <- table(pe$symp_group==pe$emp_group)[2]
group_matrix[5,3] <- group_matrix[3,5] <- table(pe$symp_group==pe$tend_group)[2]

group_matrix[5,4] <- group_matrix[4,5] <- table(pe$emp_group==pe$tend_group)[2]

group_matrix
group_perc <- round(group_matrix/196, digits=2)
group_perc
save(group_matrix, group_perc, file='groupings.RData')


  ##### 6. Data Reduction #####

## prep appraisal items for PCA

## Parallel analysis to decide on factor extraction


fa.parallel(pity[,4:35], fm='ml', n.iter=500) # 7 for both
fa.parallel(symp[,4:35], fm='ml', n.iter=500) # 7 for both
fa.parallel(comp[,4:35], fm='ml', n.iter=500) # 8 for FA, 6 for PCA
fa.parallel(empathy[,4:35], fm='ml', n.iter=500) # 6 for both 
fa.parallel(tender[,4:35], fm='ml', n.iter=500) # 7 for FA, 6 for PCA

principal(pity[,4:35], fm='ml', rotate='promax', nfactors=7)
principal(symp[,4:35], fm='ml', rotate='promax', nfactors=7)
principal(comp[,4:35], fm='ml', rotate='promax', nfactors=7)
principal(empathy[,4:35], fm='ml', rotate='promax', nfactors=7)
principal(tender[,4:35], fm='ml', rotate='promax', nfactors=7)



##### 7. Exploratory Data Plots #####

pelong <- data.frame(emotion=rep(c('compassion','empathy','sympathy','pity','tenderness'), each=194), 
                     pleas=c(pe$cpleas,pe$epleas,pe$spleas, pe$ppleas, pe$tpleas), 
                     unpleas=c(pe$cunpleas,pe$eunpleas,pe$sunpleas, pe$punpleas, pe$tunpleas), 
                     wanted=c(pe$cwanted,pe$ewanted,pe$swanted, pe$pwanted, pe$twanted), 
                     notwanted=c(pe$cnotwanted,pe$enotwanted,pe$snotwanted, pe$pnotwanted, pe$tnotwanted), 
                     sitcont=c(pe$csitcont, pe$esitcont, pe$ssitcont, pe$psitcont, pe$tsitcont), 
                     noblame=c(pe$cnoblame, pe$enoblame, pe$snoblame, pe$pnoblame, pe$tnoblame), 
                     targcause=c(pe$ctargcause, pe$etargcause, pe$stargcause, pe$ptargcause, pe$ttargcause), 
                     targresp=c(pe$ctargresp, pe$etargresp, pe$stargresp, pe$ptargresp, pe$ttargcause), 
                     mecause=c(pe$cmecause, pe$emecause, pe$smecause, pe$pmecause, pe$tmecause), 
                     selfresp=c(pe$cmeresp, pe$emeresp, pe$smeresp, pe$pmeresp, pe$tmeresp), 
                     cause3p=c(pe$c3pcause, pe$e3pcause, pe$s3pcause, pe$p3pcause, pe$t3pcause), 
                     resp3p=c(pe$c3presp, pe$e3presp, pe$s3presp, pe$p3presp, pe$t3presp), 
                     influence=c(pe$cinfluence, pe$einfluence, pe$sinfluence, pe$pinfluence, pe$tinfluence), 
                     power=c(pe$cpowercare, pe$epowercare, pe$spowercare, pe$ppowercare, pe$tpowercare), 
                     temp=c(pe$ctemp, pe$etemp, pe$stemp, pe$ptemp, pe$ttemp), 
                     lastlong=c(pe$clastlong, pe$elastlong, pe$slastlong, pe$plastlong, pe$tlastlong),
                     dvuln=c(pe$cdvuln, pe$edvuln, pe$sdvuln, pe$pdvuln, pe$tdvuln),
                     dtakecare=c(pe$cdtakecare, pe$edtakecare, pe$sdtakecare, pe$pdtakecare, pe$tdtakecare),
                     svuln=c(pe$csvuln, pe$esvuln, pe$ssvuln, pe$psvuln, pe$tsvuln),
                     stakecare=c(pe$cstakecare, pe$estakecare, pe$sstakecare, pe$pstakecare, pe$tstakecare),
                     sim=c(pe$csim, pe$esim, pe$ssim, pe$psim, pe$tsim),
                     close=c(pe$cclose, pe$eclose, pe$sclose, pe$pclose, pe$tclose),
                     mecare=c(pe$cmecare, pe$emecare, pe$smecare, pe$pmecare, pe$tmecare),
                     medo=c(pe$cmedo, pe$emedo, pe$smedo, pe$pmedo, pe$tmedo),
                     targcare=c(pe$ctargcare, pe$etargcare, pe$stargcare, pe$ptargcare, pe$ttargcare),
                     targdo=c(pe$ctargdo, pe$etargdo, pe$stargdo, pe$ptargdo, pe$ttargdo),
                     care3p=c(pe$c3pcare, pe$e3pcare, pe$s3pcare, pe$p3pcare, pe$t3pcare),
                     do3p=c(pe$c3pdo, pe$e3pdo, pe$s3pdo, pe$p3pdo, pe$t3pdo),
                     better=c(pe$cbetter, pe$ebetter, pe$sbetter, pe$pbetter, pe$tbetter),
                     lookdown=c(pe$clookdown, pe$elookdown, pe$slookdown, pe$plookdown, pe$tlookdown),
                     deserve=c(pe$cdeserve, pe$edeserve, pe$sdeserve, pe$pdeserve, pe$tdeserve),
                     others=c(pe$cothers, pe$eothers, pe$sothers, pe$pothers, pe$tothers))

pelong$ipleas <- rowMeans(cbind(pelong$pleas, (pelong$unpleas-8)*(-1)))
pelong$cong <- rowMeans(cbind(pelong$wanted, (pelong$unwanted-8)*(-1)))
p.comp <- ggplot(pelong[pelong$emotion=='compassion',], aes(y=jitter(ipleas), x=jitter(cong), color=emotion)) + geom_point() + stat_density2d()
p.symp <- ggplot(pelong[pelong$emotion=='sympathy',], aes(y=jitter(ipleas), x=jitter(cong), color=emotion)) + geom_point() + stat_density2d()
p.emp <- ggplot(pelong[pelong$emotion=='empathy',], aes(y=jitter(ipleas), x=jitter(cong), color=emotion)) + geom_point() + stat_density2d()
p.tend <- ggplot(pelong[pelong$emotion=='tenderness',], aes(y=jitter(ipleas), x=jitter(cong), color=emotion)) + geom_point() + stat_density2d()
p.pity <- ggplot(pelong[pelong$emotion=='pity',], aes(y=jitter(ipleas), x=jitter(cong), color=emotion)) + geom_point() + stat_density2d()
p.layout <- matrix(c(1,2,3,4,5,0), nrow=3)
multiplot(p.comp, p.symp, p.emp, p.tend, p.pity, layout=p.layout)

pelong$sitcause <- rowMeans(cbind(pelong$sitcont, (pelong$noblame-8)*(-1)))
pelong$targblame <- rowMeans(cbind(pelong$targcause, pelong$targresp))
p.comp <- ggplot(pelong[pelong$emotion=='compassion',], aes(y=jitter(sitcause), x=jitter(targblame), color=emotion)) + geom_point() + stat_density2d()
p.symp <- ggplot(pelong[pelong$emotion=='sympathy',], aes(y=jitter(sitcause), x=jitter(targblame), color=emotion)) + geom_point() + stat_density2d()
p.emp <- ggplot(pelong[pelong$emotion=='empathy',], aes(y=jitter(sitcause), x=jitter(targblame), color=emotion)) + geom_point() + stat_density2d()
p.tend <- ggplot(pelong[pelong$emotion=='tenderness',], aes(y=jitter(sitcause), x=jitter(targblame), color=emotion)) + geom_point() + stat_density2d()
p.pity <- ggplot(pelong[pelong$emotion=='pity',], aes(y=jitter(sitcause), x=jitter(targblame), color=emotion)) + geom_point() + stat_density2d()
p.layout <- matrix(c(1,2,3,4,5,0), nrow=3)
multiplot(p.comp, p.symp, p.emp, p.tend, p.pity, layout=p.layout)

pelong$sitcause <- rowMeans(cbind(pelong$sitcont, (pelong$noblame-8)*(-1)))
pelong$targblame <- rowMeans(cbind(pelong$targcause, pelong$targresp))
p.comp <- ggplot(pelong[pelong$emotion=='compassion',], aes(y=jitter(sitcause), x=jitter(targblame), color=emotion)) + geom_point() + stat_density2d()
p.symp <- ggplot(pelong[pelong$emotion=='sympathy',], aes(y=jitter(sitcause), x=jitter(targblame), color=emotion)) + geom_point() + stat_density2d()
p.emp <- ggplot(pelong[pelong$emotion=='empathy',], aes(y=jitter(sitcause), x=jitter(targblame), color=emotion)) + geom_point() + stat_density2d()
p.tend <- ggplot(pelong[pelong$emotion=='tenderness',], aes(y=jitter(sitcause), x=jitter(targblame), color=emotion)) + geom_point() + stat_density2d()
p.pity <- ggplot(pelong[pelong$emotion=='pity',], aes(y=jitter(sitcause), x=jitter(targblame), color=emotion)) + geom_point() + stat_density2d()
p.layout <- matrix(c(1,2,3,4,5,0), nrow=3)
multiplot(p.comp, p.symp, p.emp, p.tend, p.pity, layout=p.layout)

pelong$agency3p <- rowMeans(cbind(pelong$resp3p, pelong$cause3p))
pelong$selfagency <- rowMeans(cbind(pelong$mecause, pelong$meresp))
p.comp <- ggplot(pelong[pelong$emotion=='compassion',], aes(y=jitter(agency3p), x=jitter(selfagency), color=emotion)) + geom_point() + stat_density2d()
p.symp <- ggplot(pelong[pelong$emotion=='sympathy',], aes(y=jitter(agency3p), x=jitter(selfagency), color=emotion)) + geom_point() + stat_density2d()
p.emp <- ggplot(pelong[pelong$emotion=='empathy',], aes(y=jitter(agency3p), x=jitter(selfagency), color=emotion)) + geom_point() + stat_density2d()
p.tend <- ggplot(pelong[pelong$emotion=='tenderness',], aes(y=jitter(agency3p), x=jitter(selfagency), color=emotion)) + geom_point() + stat_density2d()
p.pity <- ggplot(pelong[pelong$emotion=='pity',], aes(y=jitter(agency3p), x=jitter(selfagency), color=emotion)) + geom_point() + stat_density2d()
p.layout <- matrix(c(1,2,3,4,5,0), nrow=3)
multiplot(p.comp, p.symp, p.emp, p.tend, p.pity, layout=p.layout)

pelong$stability <- rowMeans(cbind(pelong$lastlong, (pelong$temp-8)*(-1)))
pelong$power <- rowMeans(cbind(pelong$powercare, pelong$influence))
p.comp <- ggplot(pelong[pelong$emotion=='compassion',], aes(y=jitter(stability), x=jitter(power), color=emotion)) + geom_point() + stat_density2d()
p.symp <- ggplot(pelong[pelong$emotion=='sympathy',], aes(y=jitter(stability), x=jitter(power), color=emotion)) + geom_point() + stat_density2d()
p.emp <- ggplot(pelong[pelong$emotion=='empathy',], aes(y=jitter(stability), x=jitter(power), color=emotion)) + geom_point() + stat_density2d()
p.tend <- ggplot(pelong[pelong$emotion=='tenderness',], aes(y=jitter(stability), x=jitter(power), color=emotion)) + geom_point() + stat_density2d()
p.pity <- ggplot(pelong[pelong$emotion=='pity',], aes(y=jitter(stability), x=jitter(power), color=emotion)) + geom_point() + stat_density2d()
p.layout <- matrix(c(1,2,3,4,5,0), nrow=3)
multiplot(p.comp, p.symp, p.emp, p.tend, p.pity, layout=p.layout)

pelong$sitvuln <- rowMeans(cbind(pelong$svuln, (pelong$stakecare-8)*(-1)))
pelong$dispvuln <- rowMeans(cbind(pelong$dvuln, (pelong$dtakecare-8)*(-1)))
p.comp <- ggplot(pelong[pelong$emotion=='compassion',], aes(y=jitter(sitvuln), x=jitter(dispvuln), color=emotion)) + geom_point() + stat_density2d()
p.symp <- ggplot(pelong[pelong$emotion=='sympathy',], aes(y=jitter(sitvuln), x=jitter(dispvuln), color=emotion)) + geom_point() + stat_density2d()
p.emp <- ggplot(pelong[pelong$emotion=='empathy',], aes(y=jitter(sitvuln), x=jitter(dispvuln), color=emotion)) + geom_point() + stat_density2d()
p.tend <- ggplot(pelong[pelong$emotion=='tenderness',], aes(y=jitter(sitvuln), x=jitter(dispvuln), color=emotion)) + geom_point() + stat_density2d()
p.pity <- ggplot(pelong[pelong$emotion=='pity',], aes(y=jitter(sitvuln), x=jitter(dispvuln), color=emotion)) + geom_point() + stat_density2d()
p.layout <- matrix(c(1,2,3,4,5,0), nrow=3)
multiplot(p.comp, p.symp, p.emp, p.tend, p.pity, layout=p.layout)

pelong$selfcare <- rowMeans(cbind(pelong$mecare, pelong$medo))
pelong$relevance <- rowMeans(cbind(pelong$sim, pelong$close))
p.comp <- ggplot(pelong[pelong$emotion=='compassion',], aes(y=jitter(selfcare), x=jitter(relevance), color=emotion)) + geom_point() + stat_density2d()
p.symp <- ggplot(pelong[pelong$emotion=='sympathy',], aes(y=jitter(selfcare), x=jitter(relevance), color=emotion)) + geom_point() + stat_density2d()
p.emp <- ggplot(pelong[pelong$emotion=='empathy',], aes(y=jitter(selfcare), x=jitter(relevance), color=emotion)) + geom_point() + stat_density2d()
p.tend <- ggplot(pelong[pelong$emotion=='tenderness',], aes(y=jitter(selfcare), x=jitter(relevance), color=emotion)) + geom_point() + stat_density2d()
p.pity <- ggplot(pelong[pelong$emotion=='pity',], aes(y=jitter(selfcare), x=jitter(relevance), color=emotion)) + geom_point() + stat_density2d()
p.layout <- matrix(c(1,2,3,4,5,0), nrow=3)
multiplot(p.comp, p.symp, p.emp, p.tend, p.pity, layout=p.layout)

pelong$targfix <- rowMeans(cbind(pelong$targcare, pelong$targdo))
pelong$fix3p <- rowMeans(cbind(pelong$care3p, pelong$do3p))
p.comp <- ggplot(pelong[pelong$emotion=='compassion',], aes(y=jitter(targfix), x=jitter(fix3p), color=emotion)) + geom_point() + stat_density2d()
p.symp <- ggplot(pelong[pelong$emotion=='sympathy',], aes(y=jitter(targfix), x=jitter(fix3p), color=emotion)) + geom_point() + stat_density2d()
p.emp <- ggplot(pelong[pelong$emotion=='empathy',], aes(y=jitter(targfix), x=jitter(fix3p), color=emotion)) + geom_point() + stat_density2d()
p.tend <- ggplot(pelong[pelong$emotion=='tenderness',], aes(y=jitter(targfix), x=jitter(fix3p), color=emotion)) + geom_point() + stat_density2d()
p.pity <- ggplot(pelong[pelong$emotion=='pity',], aes(y=jitter(targfix), x=jitter(fix3p), color=emotion)) + geom_point() + stat_density2d()
p.layout <- matrix(c(1,2,3,4,5,0), nrow=3)
multiplot(p.comp, p.symp, p.emp, p.tend, p.pity, layout=p.layout)

pelong$deservingness <- rowMeans(cbind(pelong$others, pelong$deserve))
pelong$superiority <- rowMeans(cbind(pelong$lookdown, pelong$better))
p.comp <- ggplot(pelong[pelong$emotion=='compassion',], aes(y=jitter(deservingness), x=jitter(superiority), color=emotion)) + geom_point() + stat_density2d()
p.symp <- ggplot(pelong[pelong$emotion=='sympathy',], aes(y=jitter(deservingness), x=jitter(superiority), color=emotion)) + geom_point() + stat_density2d()
p.emp <- ggplot(pelong[pelong$emotion=='empathy',], aes(y=jitter(deservingness), x=jitter(superiority), color=emotion)) + geom_point() + stat_density2d()
p.tend <- ggplot(pelong[pelong$emotion=='tenderness',], aes(y=jitter(deservingness), x=jitter(superiority), color=emotion)) + geom_point() + stat_density2d()
p.pity <- ggplot(pelong[pelong$emotion=='pity',], aes(y=jitter(deservingness), x=jitter(superiority), color=emotion)) + geom_point() + stat_density2d()
p.layout <- matrix(c(1,2,3,4,5,0), nrow=3)
multiplot(p.comp, p.symp, p.emp, p.tend, p.pity, layout=p.layout)



##### MDS #####
group_distances <- abs(group_matrix-196)
mds.fit <- cmdscale(group_distances, dim= eig=TRUE)
plot(mds.fit$eig)

x <- mds.fit$points[, 1]
y <- mds.fit$points[, 2]
plot(x, y, pch=19, xlim=c(-100,100), ylim=c(-100,100))
emonames <- c('pity', 'compassion', 'sympathy', 'empathy', 'tenderness')
text(x, y, pos=4, labels=emonames)
x <- 0-x
y <- 0-y
plot(x, y, pch=19, xlim=c(-100,100), ylim=c(-100,100))
emonames <- c('pity', 'compassion', 'sympathy', 'empathy', 'tenderness')
text(x, y, pos=4, labels=emonames)

install.packages('igraph')
library(igraph)
g <- graph.full(nrow(group_distances))
V(g)$label <- emonames
layout <- layout.mds(g, dist=as.matrix(group_distances))
plot(g, layout=layout, vertex.size=3)

install.packages('SensoMineR')
library(SensoMineR)
data(napping)
nappeplot(napping.don)
resindscal<- indscal(napping.don, napping.words)
dev.new()
prefpls(cbind(resindscal$points, napping.words))
dev.new()
pmfa(napping.don, napping.words, mean.conf = resindscal$points)

install.packages(c("vegan", "ecodist", "labdsv", "ape", "ade4", "smacof"))
library(vegan)
library(ecodist)
library(labdsv)
library(ape)
library(ade4)
library(smacof)

#cmdscale
group_distances <- abs(group_matrix-196)
mds.fit <- cmdscale(group_distances, k=2)
plot(mds.fit[,1], mds.fit[,2], xlim=c(-100,100), ylim=c(-100,100), type='n')
text(mds.fit[,1], mds.fit[,2], colnames(group_distances))

mds.fit1 <- smacofSym(group_distances, ndim=1)
mds.fit2 <- smacofSym(group_distances, ndim=2)
mds.fit3 <- smacofSym(group_distances, ndim=3)
mds.fit4 <- smacofSym(group_distances, ndim=4)
stress <- c(mds.fit1$stress, mds.fit2$stress, mds.fit3$stress, mds.fit4$stress)
plot(stress)
lines(stress, type='l') # bend is at the second dimension, so probably a two-dimensional solution


