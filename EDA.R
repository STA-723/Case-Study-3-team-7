library(tidyverse)
load("~/Duke/Statistics/STA723/Case-Study-3-team-7/data_1993.rdata")
load("~/Duke/Statistics/STA723/Case-Study-3-team-7/data_1997.rdata")
load("~/Duke/Statistics/STA723/Case-Study-3-team-7/data_1999.rdata")
load("~/Duke/Statistics/STA723/Case-Study-3-team-7/data_2001.rdata")

npr = dat99 %>% select((1:ncol(dat99))[grepl("DRPROB",colnames(dat99)) | grepl("DRIVE",colnames(dat99))])
npr = npr %>% mutate(probprop = rowSums(.,na.rm = T) / rowSums(!is.na(.)))
npr = npr %>% mutate(nadrop = sum(is.na(.)) >= 4)

# my99 = cbind(dat99, npr %>% select(probprop, nadrop))
# my99 %>% filter(nadrop == FALSE)

dat99 =dat99 %>% mutate(firstgen =  1*(G18!=4|G19!=4))

frat = dat99 %>% select(MEMGREEK, NUMPROB) %>% group_by(MEMGREEK) %>% 
  summarise(mean(NUMPROB, na.rm = T))

dat99 = dat99 %>% mutate(HSDrinking = factor(G10, labels = c("None","Infrequent", "Infrequent","Frequent","Frequent","Frequent","Frequent")))
dat99 = dat99 %>% mutate(SCHOOLYEAR = factor(A3, labels = c("FRESH","SOPH", "JUN", "SEN", "FIFTH", "GRAD")))

pred = dat99 %>% select(firstgen, MEMGREEK, HSDrinking, SEX, SCHOOLYEAR)

apply(pred, 2, function(x) sum(is.na(x)))


## HSDrinking vs College

ggplot(data = na.omit(dat99 %>% select(NUMPROB, HSDrinking,firstgen)), mapping = aes(x = factor(HSDrinking), y = NUMPROB, fill = factor(firstgen))) +
  geom_boxplot() +
  ggtitle("Problem Drinking vs HS Drinking and First Gen Student") +
  ylab("Proportion of Problems") +
  xlab("High School Drinking") +
  labs(fill = "First Gen")


## GREEK First GEN

ggplot(data = na.omit(dat99 %>% select(NUMPROB, MEMGREEK,firstgen)), mapping = aes(x = factor(firstgen), y = NUMPROB, fill = factor(MEMGREEK))) +
  geom_boxplot() +
  ggtitle("Problem Drinking vs Sex and First Gen Student") +
  ylab("Proportion of Problems") +
  xlab("First Generation") +
  labs(fill = "Greek Life")

ggplot(data = na.omit(dat99 %>% select(NUMPROB, MEMGREEK,firstgen)), mapping = aes(x = factor(MEMGREEK), y = NUMPROB, fill = factor(firstgen))) +
  geom_bar(position="dodge",stat="summary",fun.y="mean") +
  ggtitle("Problem Drinking vs Greek Life and First Gen Student")

## SEX FIRST GEN

ggplot(data = na.omit(dat99 %>% select(NUMPROB, SEX,firstgen)), mapping = aes(x = factor(SEX), y = NUMPROB, fill = factor(firstgen))) +
  geom_boxplot() +
  ggtitle("Problem Drinking vs Greek Life and First Gen Student") +
  ylab("Proportion of Problems") +
  xlab("Sex") +
  labs(fill = "First Gen")

ggplot(data = na.omit(dat99 %>% select(NUMPROB, SEX,firstgen)), mapping = aes(x = factor(SEX), y = NUMPROB, fill = factor(firstgen))) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  ggtitle("Problem Drinking vs Greek Life and First Gen Student") +
  ylab("Proportion of Problems") +
  xlab("Sex") +
  labs(fill = "First Gen")

## Grade / Firstgen

ggplot(data = na.omit(dat99 %>% select(NUMPROB, A3,firstgen)), 
       mapping = aes(x = factor(A3, labels = c("Fresh", "Soph","Jun","Sen","Fifth","Grad")), 
                     y = NUMPROB, 
                     fill = factor(firstgen))) +
  geom_boxplot() +
  ggtitle("Problem Drinking vs Grade and First Gen Student") +
  ylab("Proportion of Problems") +
  xlab("Grade") +
  labs(fill = "First Gen")

ggplot(data = na.omit(dat99 %>% select(NUMPROB, A3,firstgen)), 
       mapping = aes(x = factor(A3, labels = c("Fresh", "Soph","Jun","Sen","Fifth","Grad")), 
                     y = NUMPROB, 
                     fill = factor(firstgen))) +
  geom_bar(position="dodge",stat="summary",fun.y="mean") +
  ggtitle("Problem Drinking vs Grade and First Gen Student") +
  ylab("Proportion of Problems") +
  xlab("Grade") +
  labs(fill = "First Gen")

ggplot(data = na.omit(dat99 %>% select(NUMPROB, RACE,firstgen)), mapping = aes(x = factor(RACE), y = NUMPROB, fill = factor(firstgen))) +
  geom_boxplot() +
  ggtitle("Problem Drinking vs Race and First Gen")


ggplot(data = na.omit(dat99 %>% select(F4, MEMGREEK,firstgen)), mapping = aes(x = factor(firstgen), y = F4, fill = factor(MEMGREEK))) +
  geom_boxplot() +
  labs(title = "Grades vs Greek Life and First Gen Student",
       x = "First Generation",
       y = "Normalized Grades",
       fill = "Greek Life")

ggplot(data = na.omit(dat99 %>% select(F4, MEMGREEK,firstgen)), mapping = aes(x = factor(firstgen), y = F4, fill = factor(MEMGREEK))) +
  geom_bar(position = "dodge",stat = "summary", fun.y = "mean") +
  ggtitle("Grades vs Greek Life and First Gen Student") +
  xlab("First Gen") +
  ylab("Normalized Grades")


ggplot(data = na.omit(dat99 %>% select(NUMPROB, CLASS,firstgen)), mapping = aes(x = factor(CLASS), y = NUMPROB, fill = factor(firstgen))) +
  geom_bar(position="dodge",stat="summary", fun.y = "mean") +
  ggtitle("Problem Drinking vs Grade/Class and First Gen Student")


nrow(dat01) + nrow(dat93) + nrow(dat97) + nrow(dat99)

dat99 %>% select(1:ncol(dat99)[grepl()])


head(npr)

apply(npr, 2, function(x) mean(is.na(x))) %>% pivot_longer(names_to = "Problem", values_to = "mean")

na.resp =apply(npr,1, function(x) sum(is.na(x))) %>% as.data.frame()
colnames(na.resp) = c("NA.Count")

ggplot(na.resp, aes(x = NA.Count)) + geom_histogram() +
  labs(title = "Response Missingness Count")
