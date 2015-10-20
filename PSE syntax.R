## Prosocial Emotion Appraisal Study

####### TABLE OF CONTENTS #######
#
# 1. Variable List
# 2. Import Data/Load Packages
# 3. Load/Write Functions
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
  
  # check if subjects saw this as a unique emotion
  if(focal[2]==0 | is.na(focal[2])==TRUE | sum(is.na(unlist(lapply(others, '[', 2))))) {
    focal <- focal
  }
  
  # otherwise fill it in with the first emotion that was in the same group
  else {
    j=1
    
    # check to see if the group for the focal emotion is the same as the group of emotion i
    # also make sure that emotion i was not excluded
    # if either condition is not met, then move on to the i + 1 group
    while(focal[3]!=others[[j]][3] | others[[j]][2]==1) {
      j <- j+1
    }
    # if both conditions are met, then fill in the focal emotion with the data from emotion i
    focal <- unlist(others[[j]])
  }
  
  return(focal)
}