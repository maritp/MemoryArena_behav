# memoryarena_behav. statistical analyses

install.packages("BayesFactor") 
install.packages("lsr")
install.packages("Rmisc")

library(BayesFactor)
library(lsr) #etaSquared
library(Rmisc) # CI & summarySE


options(scipen = 999)
rm(list = ls())

dat_dir70 <- "/Volumes/MEMTOSH/MemoryArena_behav_data/results70/prepR/memdat.txt"
dat_dir50 <- "/Volumes/MEMTOSH/MemoryArena_behav_data/results50/prepR/memdat.txt"

# read in data
dat70 <- read.table(dat_dir70,
                   col.names = c("perf", "seq", "pldist","dur_enc", "dur_train", "ntrain","cond", "del", "int", "time")) 
dat50 <- read.table(dat_dir50, 
                       col.names = c("perf", "seq", "pldist", "dur_enc", "dur_train", "ntrain", "cond", "del", "int", "time"))

dat <- rbind(dat50, dat70)
dat["subj"] <- c(c(c(1:(nrow(dat50)/2), c(1:(nrow(dat50)/2)))), c(c(1:(nrow(dat70)/2), c(1:(nrow(dat70)/2))))+60) # add subject vector

# define variables as factors
dat$cond <- factor(dat$cond,c("sleep", "sleep int", "wake", "wake int"), levels = sort(unique(dat$cond)))
dat$int <- factor(dat$int, c("noInt", "Int"), levels = sort(unique(dat$int))) # factor retrieval difficulty
dat$del <- factor(dat$del, c("sleep", "wake"), levels = sort(unique(dat$del)))  # factor delay
dat$time <- factor(dat$time, c("pre", "post"), levels = sort(unique(dat$time)))

dat$mstr <- c(replicate(nrow(dat50),0), replicate(nrow(dat70),1))
dat$mstr <- factor(dat$mstr, c("50", "70"), levels = sort(unique(dat$mstr))) # factor memory strength

# calculate relative change from pre to post retrieval 
datd <- reshape(dat, idvar = "subj", v.names = c("perf", "seq", "pldist"), timevar = "time",
                   direction = "wide")

datd$seq <- datd$seq.post/(datd$seq.pre/100)
datd$pldist <- datd$pldist.post/(datd$pldist.pre/100)
datd$perf <- datd$perf.post/(datd$perf.pre/100)

datd$durTotal <- datd$dur_enc + datd$dur_train # training duration 

# ---- training duration & training rounds -----
summarySE(datd, measurevar = "durTotal", groupvars = c("mstr","int","del"))
mod_traindur <- aov(durTotal ~ del*int*mstr, data = datd) 
summary(mod_traindur)
etaSquared(mod_traindur)
1/lmBF(durTotal ~ del, data = datd) 
1/lmBF(durTotal ~ int, data = datd)
summarySE(datd, measurevar = "durTotal", groupvars = "mstr")

summarySE(datd, measurevar = "ntrain", groupvars = c("mstr","int","del"))
mod_ntrain <- aov(ntrain ~ del*int*mstr, data = datd) 
summary(mod_ntrain)
etaSquared(mod_ntrain)
1/lmBF(ntrain ~ del, data = datd) 
1/lmBF(ntrain ~ int, data = datd) 
summarySE(datd, measurevar = "ntrain", groupvars = "mstr")


# ---- retrieval 1 performance -----

# -- identify sensitive measurement 
mod_seq_pre <- aov(seq.pre ~ del*int*mstr, data = datd) 
summary(mod_seq_pre)
etaSquared(mod_seq_pre)
1/lmBF(seq.pre ~ del, data = datd) 
1/lmBF(seq.pre ~ int, data = datd) 

mod_pldist_pre <- aov(pldist.pre ~ del*int*mstr, data = datd) 
summary(mod_pldist_pre)
etaSquared(mod_pldist_pre)
1/lmBF(pldist.pre ~ del, data = datd) 
1/lmBF(pldist.pre ~ int, data = datd) 

# ---- descriptive data. sequnence performance ----
summarySE(datd, measurevar = "pldist.pre", groupvars = c("int", "mstr", "del"))
summarySE(datd, measurevar = "pldist.post", groupvars = c("int", "mstr", "del"))

#---- 3 factor model. sequence performance ----
mod01_seq <- aov(seq ~ del*int*mstr, data = datd)
summary(mod01_seq)
etaSquared(mod01_seq)

# ----------------- no interference. Sequence performance ----------------------
datd_noint <- subset(datd, int == "noInt")
summarySE(datd_noint, measurevar = "seq", groupvars = c("del", "mstr"))
mod02_seq <- aov(seq ~ del*mstr, data = datd_noint)
summary(mod02_seq)
etaSquared(mod02_seq)

# -- post-hoc t-tests
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

# -- pre vs. post
t.test(x = datd$seq.pre[datd$int == "noInt" & datd$mstr == "50" & datd$del == "wake"],
       y = datd$seq.post[datd$int == "noInt" & datd$mstr == "50" & datd$del == "wake"], paired = TRUE)

# -- test for normal distribution
shapiro.test(datd$seq.pre[datd$mstr == "70" & datd$del == "sleep" & datd$int == "noInt"])
shapiro.test(datd$seq.pre[datd$mstr == "70" & datd$del == "wake" & datd$int == "noInt"])

# ----------------- interference. Sequence performance ----------------------
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

# -- pre vs. post
t.test(x = datd$seq.pre[datd$int == "Int" & datd$mstr == "50" & datd$del == "wake"],
       y = datd$seq.post[datd$int == "Int" & datd$mstr == "50" & datd$del == "wake"], paired = TRUE)

# -- test for normal distribution
shapiro.test(datd$seq.post[datd$mstr == "70" & datd$del == "sleep" & datd$int == "Int"])
shapiro.test(datd$seq.post[datd$mstr == "70" & datd$del == "wake" & datd$int == "Int"])

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

# ------------------------ placement distance & overall performance as dvs ----------------------

# ---- descriptive data. placement distance ----
summarySE(datd, measurevar = "pldist.pre", groupvars = c("int", "mstr", "del"))
summarySE(datd, measurevar = "pldist.post", groupvars = c("int", "mstr", "del"))

#---- 3 factor model. placement distance ----
mod01_pldist <- aov(pldist ~ del*int*mstr, data = datd)
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

# -- pre vs. post
t.test(x = datd$pldist.pre[datd$int == "noInt" & datd$mstr == "70" & datd$del == "sleep"],
       y = datd$pldist.post[datd$int == "noInt" & datd$mstr == "70" & datd$del == "sleep"], paired = TRUE)

# -- test for normal distribution
shapiro.test(datd$pldist.pre[datd$mstr == "70" & datd$del == "sleep" & datd$int == "noInt"])
shapiro.test(datd$pldist.pre[datd$mstr == "70" & datd$del == "wake" & datd$int == "noInt"])

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

# -- pre vs. post
t.test(x = datd$pldist.pre[datd$int == "Int" & datd$mstr == "70" & datd$del == "sleep"],
       y = datd$pldist.post[datd$int == "Int" & datd$mstr == "70" & datd$del == "sleep"], paired = TRUE)

# -- test for normal distribution
shapiro.test(datd$pldist.pre[datd$mstr == "70" & datd$del == "sleep" & datd$int == "Int"])
shapiro.test(datd$pldist.pre[datd$mstr == "70" & datd$del == "wake" & datd$int == "Int"])

# ------------------------ weaker & stronger memories. placement distance ----------------------
mod04_pldist <- aov(pldist ~ del*int, data = datd70)
summary(mod04_pldist)
etaSquared(mod04_pldist)

mod05_pldist <- aov(pldist ~ del*int, data = datd50)
summary(mod05_pldist)
etaSquared(mod05_pldist)
1/lmBF(pldist ~ del:int, data = datd50)

#----descriptive data. overall performance ----------
summarySE(datd, measurevar = "perf.pre", groupvars = c("int", "mstr", "del"))
summarySE(datd, measurevar = "perf.post", groupvars = c("int", "mstr", "del"))

#---- 3 factor model. overall performance ----
mod01_perf <- aov(perf ~ del*int*mstr, data = datd)
summary(mod01_perf)
etaSquared(mod01_perf)

# ------------------------ no interference. overall performance ----------------------
summarySE(datd_noint, measurevar = "perf", groupvars = c("del", "mstr"))

mod02_perf <- aov(perf ~ del*mstr, data = datd_noint)
summary(mod02_perf)
etaSquared(mod02_perf)

# -- post hoc comparisons
t.test(x = datd_noint$perf[datd_noint$mstr == "70" & datd_noint$del == "sleep"],
       y = datd_noint$perf[datd_noint$mstr == "70" & datd_noint$del == "wake"], paired = FALSE)
cohensD(formula = perf[mstr == "70"] ~ del[mstr == "70"], data = datd_noint)
1/ttestBF(x = datd_noint$perf[datd_noint$mstr == "70" & datd_noint$del == "sleep"],
          y = datd_noint$perf[datd_noint$mstr == "70" & datd_noint$del == "wake"])
t.test(x = datd_noint$perf[datd_noint$mstr == "50" & datd_noint$del == "sleep"],
       y = datd_noint$perf[datd_noint$mstr == "50" & datd_noint$del == "wake"], paired = FALSE)
cohensD(formula = perf[mstr == "50"] ~ del[mstr == "50"], data = datd_noint)

# -- pre vs. post
t.test(x = datd$perf.pre[datd$int == "noInt" & datd$mstr == "70" & datd$del == "sleep"],
       y = datd$perf.post[datd$int == "noInt" & datd$mstr == "70" & datd$del == "sleep"], paired = TRUE)

# -- test for normal distribution
shapiro.test(datd$perf.pre[datd$mstr == "50" & datd$del == "sleep" & datd$int == "noInt"])
shapiro.test(datd$perf.pre[datd$mstr == "50" & datd$del == "wake" & datd$int == "noInt"])


# ------------------------ interference. overall performance ----------------------
summarySE(datd_int, measurevar = "perf", groupvars = c("del", "mstr"))

mod03_perf <- aov(perf ~ del*mstr, data = datd_int)
summary(mod03_perf)
etaSquared(mod03_perf)

1/lmBF(perf ~ del:mstr, data = datd_int)

# -- post hoc comparisons
t.test(x = datd_int$perf[datd_int$mstr == "50" & datd_int$del == "sleep"],
       y = datd_int$perf[datd_int$mstr == "50" & datd_int$del == "wake"], paired = FALSE)
cohensD(formula = perf[mstr == "50"] ~ del[mstr == "50"], data = datd_int)
t.test(x = datd_int$perf[datd_int$mstr == "70" & datd_int$del == "sleep"],
       y = datd_int$perf[datd_int$mstr == "70" & datd_int$del == "wake"], paired = FALSE)
cohensD(formula = perf[mstr == "70"] ~ del[mstr == "70"], data = datd_int)

# -- pre vs. post
t.test(x = datd$perf.pre[datd$int == "Int" & datd$mstr == "70" & datd$del == "sleep"],
       y = datd$perf.post[datd$int == "Int" & datd$mstr == "70" & datd$del == "sleep"], paired = TRUE)

# -- test for normal distribution
shapiro.test(datd$perf.pre[datd$mstr == "50" & datd$del == "sleep" & datd$int == "Int"])
shapiro.test(datd$perf.pre[datd$mstr == "50" & datd$del == "wake" & datd$int == "Int"])


# ---- weaker & stronger memories. overall performance ------
mod04_perf <- aov(perf ~ del*int, data = datd50)
summary(mod04_perf)
etaSquared(mod04_perf)
1/lmBF(perf ~ del:int, data = datd50)

mod05_perf <- aov(perf ~ del*int, data = datd70)
summary(mod05_perf)
etaSquared(mod05_perf)

