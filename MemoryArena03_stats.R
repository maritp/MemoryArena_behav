# memoryarena_behav

install.packages("BayesFactor") 
install.packages("lsr")
install.packages("Rmisc")

library(BayesFactor)
library(lsr) #etaSquared
library(Rmisc) # CI & summarySE


options(scipen = 999)
rm(list = ls())

home <- 1

if (home == 0) {
dat_dir <- "Z:\\staresib-01\\MemoryArena_behav_data\\results1\\prepR\\memdat.txt"
dat_dir50 <- "Z:\\staresib-01\\MemoryArena_behav_data\\results50\\prepR\\memdat.txt"
} else {
  dat_dir <- "/Volumes/MEMTOSH/MemoryArena_behav_data/results1/prepR/memdat.txt"
  dat_dir50 <- "/Volumes/MEMTOSH/MemoryArena_behav_data/results50/prepR/memdat.txt"
}

# read data
dat_ <- read.table(dat_dir,
                   col.names = c("perf", "seq", "pldist", "cond", "del", "int", "time")) 
dat50 <- read.table(dat_dir50, 
                       col.names = c("perf", "seq", "pldist", "cond", "del", "int", "time"))

dat <- rbind(dat50, dat_)
dat["subj"] <- c(c(c(1:(nrow(dat50)/2), c(1:(nrow(dat50)/2)))), c(c(1:(nrow(dat_)/2), c(1:(nrow(dat_)/2))))+60) # add subject vector

# define variables as factors
dat$cond <- factor(dat$cond,c("sleep", "sleep int", "wake", "wake int"), levels = sort(unique(dat$cond)))
dat$int <- factor(dat$int, c("noInt", "Int"), levels = sort(unique(dat$int))) # factor retrieval difficulty
dat$del <- factor(dat$del, c("sleep", "wake"), levels = sort(unique(dat$del)))  # factor delay
dat$time <- factor(dat$time, c("pre", "post"), levels = sort(unique(dat$time)))

dat$mstr <- c(replicate(nrow(dat50),0), replicate(nrow(dat_),1))
dat$mstr <- factor(dat$mstr, c("50", "70"), levels = sort(unique(dat$mstr))) # factor memory strength

# calculate relative change from pre to post retrieval 
datd <- reshape(dat, idvar = "subj", v.names = c("perf", "seq", "pldist"), timevar = "time",
                   direction = "wide")

datd$seq <- datd$seq.post/(datd$seq.pre/100)
datd$pldist <- datd$pldist.post/(datd$pldist.pre/100)

# ------------------------ retrieval 1 performance ----------------------

# identify sensitive measurement 
modret01_seq <- aov(seq.pre ~ del*int*mstr, data = datd) 
summary(modret01_seq)
etaSquared(modret01_seq)
1/lmBF(seq.pre ~ del, data = datd) 
1/lmBF(seq.pre ~ int, data = datd) 

modret01_pldist <- aov(pldist.pre ~ del*int*mstr, data = datd) 
summary(modret01_pldist)
etaSquared(modret01_pldist)
1/lmBF(pldist.pre ~ del, data = datd) 
1/lmBF(pldist.pre ~ int, data = datd) 


#---- 3 factor model. Sequence performance ----
mod01_seq <- aov(seq ~ del*int*mstr, data = datd) # parametric ANOVA
summary(mod01_seq)
etaSquared(mod01_seq)


# ------------------------ no interference. Sequence performance ----------------------

datd_noint <- subset(datd, int == "noInt")
summarySE(datd_noint, measurevar = "seq", groupvars = c("del", "mstr"))

mod02_seq <- aov(seq ~ del*mstr, data = datd_noint)
summary(mod02_seq)
etaSquared(mod02_seq)

# -- post hoc comparisons
var.test(x = datd_noint$seq[datd_noint$mstr == "70" & datd_noint$del == "sleep"],
         y = datd_noint$seq[datd_noint$mstr == "70" & datd_noint$del == "wake"])
t.test(x = datd_noint$seq[datd_noint$mstr == "70" & datd_noint$del == "sleep"],
       y = datd_noint$seq[datd_noint$mstr == "70" & datd_noint$del == "wake"], paired = FALSE)
cohensD(formula = seq[mstr == "70"] ~ del[mstr == "70"], data = datd_noint)
1/ttestBF(x = datd_noint$seq[datd_noint$mstr == "70" & datd_noint$del == "sleep"],
                            y = datd_noint$seq[datd_noint$mstr == "70" & datd_noint$del == "wake"])

t.test(x = datd_noint$seq[datd_noint$mstr == "50" & datd_noint$del == "sleep"],
       y = datd_noint$seq[datd_noint$mstr == "50" & datd_noint$del == "wake"], paired = FALSE)
cohensD(formula = seq[mstr == "50"] ~ del[mstr == "50"], data = datd_noint)

# ------------------------ interference. Sequence performance ----------------------

datd_int <- subset(datd, int == "Int")
summarySE(datd_int, measurevar = "seq", groupvars = c("del", "mstr"))

mod03_seq <- aov(seq ~ del*mstr, data = datd_int)
summary(mod03_seq)
etaSquared(mod03_seq)

1/lmBF(seq ~ del:mstr, data = datd_int)

# -- post hoc comparisons
t.test(x = datd_int$seq[datd_int$mstr == "50" & datd_int$del == "sleep"],
       y = datd_int$seq[datd_int$mstr == "50" & datd_int$del == "wake"], paired = FALSE)
cohensD(formula = seq[mstr == "50"] ~ del[mstr == "50"], data = datd_int)

t.test(x = datd_int$seq[datd_int$mstr == "70" & datd_int$del == "sleep"],
       y = datd_int$seq[datd_int$mstr == "70" & datd_int$del == "wake"], paired = FALSE)
cohensD(formula = seq[mstr == "70"] ~ del[mstr == "70"], data = datd_int)


# ------------------------ weaker & stronger memories. Sequence performance ----------------------
datd50 <- subset(datd, mstr == "50")
datd70 <- subset(datd, mstr == "70")

mod04_seq <- aov(seq ~ del*int, data = datd50)
summary(mod04_seq)
etaSquared(mod04_seq)
1/lmBF(seq ~ del:int, data = datd50)

mod05_seq <- aov(seq ~ del*int, data = datd70)
summary(mod05_seq)
etaSquared(mod05_seq)


# ------------------------ Supplement. placement distance ----------------------


#---- 3 factor model. placement distance ----
mod01_pldist <- aov(pldist ~ del*int*mstr, data = datd) # parametric ANOVA
summary(mod01_pldist)
etaSquared(mod01_pldist)
1/lmBF(pldist ~ del:int:mstr, data = datd)


# ------------------------ no interference. placement distance ----------------------

summarySE(datd_noint, measurevar = "pldist", groupvars = c("del", "mstr"))

mod02_pldist <- aov(pldist ~ del*mstr, data = datd_noint)
summary(mod02_pldist)
etaSquared(mod02_pldist)

1/lmBF(pldist ~ del, data = datd_noint)
1/lmBF(pldist~del:mstr, dat = datd_noint)


# ------------------------ interference. placement distance ----------------------

summarySE(datd_int, measurevar = "pldist", groupvars = c("del", "mstr"))

mod03_pldist <- aov(pldist ~ del*mstr, data = datd_int)
summary(mod03_pldist)
etaSquared(mod03_pldist)

1/lmBF(pldist ~ del:mstr, data = datd_int)

# -- post hoc comparisons
t.test(x = datd_int$pldist[datd_int$mstr == "70" & datd_int$del == "sleep"],
       y = datd_int$pldist[datd_int$mstr == "70" & datd_int$del == "wake"], paired = FALSE)
cohensD(formula = pldist[mstr == "70"] ~ del[mstr == "70"], data = datd_int)

t.test(x = datd_int$pldist[datd_int$mstr == "50" & datd_int$del == "sleep"],
       y = datd_int$pldist[datd_int$mstr == "50" & datd_int$del == "wake"], paired = FALSE)
cohensD(formula = pldist[mstr == "50"] ~ del[mstr == "50"], data = datd_int)
1/ttestBF(x = datd_int$pldist[datd_int$mstr == "50" & datd_int$del == "sleep"],
          y = datd_int$pldist[datd_int$mstr == "50" & datd_int$del == "wake"], paired = FALSE)




# ------ same analysis but excluding outliers 

del_l <- c("sleep", "wake")
mstr_l <- c("70", "50")

idx_out <- rep(NA, 4) # getting outliers for 4 conditions (sleep strong, sleep weak, wake strong, wake weak)
sd_mstr <- 2.5
subj_excl <- c()

for (idel in 1:2) {
  for (imstr in 1:2) {
    dat_tmp <- datd_int$pldist[datd_int$del == del_l[idel] & datd_int$mstr == mstr_l[imstr]] 
    subj_tmp <- datd_int$subj[datd_int$del == del_l[idel] & datd_int$mstr == mstr_l[imstr]] 
    idx_out <- which(dat_tmp < (mean(dat_tmp) - sd_mstr*sd(dat_tmp)) | dat_tmp > (mean(dat_tmp) + sd_mstr*sd(dat_tmp)))
    subj_excl <- c(subj_excl, subj_tmp[idx_out])
  }}

#-- excl outliers
idx_ <- c()
for (i in 1:length(subj_excl)) {
idx_ <- c(idx_, which(datd_int$subj == subj_excl[i]))
}

datd_int_excl <- datd_int[-idx_,]

mod032_pldist <- aov(pldist ~ del*mstr, data = datd_int_excl)
summary(mod032_pldist)
etaSquared(mod032_pldist)

t.test(x = datd_int_excl$pldist[datd_int_excl$mstr == "70" & datd_int_excl$del == "sleep"],
       y = datd_int_excl$pldist[datd_int_excl$mstr == "70" & datd_int_excl$del == "wake"], paired = FALSE)


# ------------------------ weaker & stronger memories. placement distance ----------------------

mod04_pldist <- aov(pldist ~ del*int, data = datd70)
summary(mod04_pldist)
etaSquared(mod04_pldist)

mod05_pldist <- aov(pldist ~ del*int, data = datd50)
summary(mod05_pldist)
etaSquared(mod05_pldist)
1/lmBF(pldist ~ del:int, data = datd50)

