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

library(rjags)
library(R2jags)



##### 3. Load/Write Functions #####

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
symp <- data.frame(t(sapply(1:dim(symp)[1], simplify='array', function(i) fill.emot(symp[i,], comp[i,], pity[i,], tender[i,], empathy[i,]))))
comp <- data.frame(t(sapply(1:dim(comp)[1], function(i) fill.emot(comp[i,], symp[i,], pity[i,], tender[i,], empathy[i,]))))
tender <- data.frame(t(sapply(1:dim(tender)[1], function(i) fill.emot(tender[i,], symp[i,], pity[i,], comp[i,], empathy[i,]))))
emp <- data.frame(t(sapply(1:dim(empathy)[1], function(i) fill.emot(empathy[i,], symp[i,], pity[i,], comp[i,], tender[i,]))))
pity <- data.frame(t(sapply(1:dim(pity)[1], function(i) fill.emot(pity[i,], symp[i,], empathy[i,], comp[i,], tender[i,]))))



## If there was a problem with the particular emotion, set all values to NA
pity[pity$pity_prob==1, 4:length(pity)] <- NA
tender[tender$tend_prob==1, 4:length(tender)] <- NA
empathy[empathy$emp_prob==1, 4:length(empathy)] <- NA
symp[symp$symp_prob==1, 4:length(symp)] <- NA
comp[comp$comp_prob==1, 4:length(comp)] <- NA



