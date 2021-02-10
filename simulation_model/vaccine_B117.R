# Structure of this code:
# - Restructuring BBC pandemic and POLYMOD data to DK situation
# - SEIR model
# - Simulation and plotting

# Restructuring BBC pandemic and POLYMOD data to DK situation ####
## Grouping and converting contact rates
# setwd(<source file location>)
library(data.table)
popUK <- fread("Pop_UK_2017.txt", skip=2)
popDK <- fread("Pop_DK_2017.txt", skip=2)
popDK

library(openxlsx)
CTypes <- getSheetNames("BBC_matrices_reciprocal_conv.xlsx")[1:8]
tmp <- read.xlsx("BBC_matrices_reciprocal_filled.xlsx", rowNames = TRUE, sheet = CTypes[1])

listCT <- list()
for (type in CTypes){
  listCT[[type]] <- read.xlsx("BBC_matrices_reciprocal_filled.xlsx", rowNames = TRUE, sheet = type)
}

rn <- rownames(tmp)
lower.breaks <- as.numeric(sapply(strsplit(rn,"[-+]"), function(x)x[[1]]))
popDK[,Age:=as.numeric(substr(Age,1,3))]
popUK[,Age:=as.numeric(substr(Age,1,3))]

popDK[,AG1:=lower.breaks[rowSums(outer(Age, lower.breaks,">="))]]
popUK[,AG1:=lower.breaks[rowSums(outer(Age, lower.breaks,">="))]]

popCont <- popDK[,.(Total=sum(Total)), keyby=.(AG1)][,Popu:=sum(Total)]
popCont[,AG1p:=Total/Popu]
popContUK <- popUK[,.(Total=sum(Total)), keyby=.(AG1)][,Popu:=sum(Total)]
popContUK[,AG1p:=Total/Popu]


propDK <- popCont[,AG1p]
propUK <- popContUK[,AG1p]

plot(lower.breaks ,propDK, ylab="DK", main = "Proportion of population per age group")

######################### ##
merge.contacts <- function(groups, lower.breaks, prop, m, make.symmetric=TRUE){ 
  # Groups: list with vectors for each desired group
  # lower.breaks: Vector with lower bound of each interval. Last is open to Inf age
  # prop: Proportion of population in each age group
  # m: number of contacts per individual per day (From in columns and to in rows)
  # make.symmertric: Should T be made symmetric before grouping?
  
  # First scaling to original scale
  cont <- as.matrix(m)*rep(prop, each=length(prop))
  if (make.symmetric)  
    cont <- (cont + t(cont))/2
  
  ## New group labels
  rn <- sapply(groups,function(x)lower.breaks[x[1]])  
  dimNames <- paste0(rn, "-",c(rn[-1]-1,""))
  
  ng <- length(groups)
  gc <- matrix(NA, ng, ng)
  gp <- numeric(ng)
  for (i in 1:ng){
    gp[i] <- sum(prop[groups[[i]]])
    for (j in 1:ng){
      gc[i,j] <- sum(cont[groups[[i]], groups[[j]]])
    }
  }
  mgc <- gc/rep(gp, each=length(gp))
  dimnames(mgc) <- list(dimNames, dimNames)
  names(gp) <- dimNames
  return(list(m=mgc, prop=gp)) # Contacts and proportion in each group
}


# Filling with polymod data for 0-4 & 5-10 year old
polymod_all <- matrix(c(1.92, 0.65,0.95,6.64), nrow = 2, byrow = TRUE) *0.25   ## Multiplied with 0.25 as PolyMod in 10-14 and 15-19 are at same level as 5-9
polymod_phys <- matrix(c(1.49, 0.59, 0.74, 3.82), nrow = 2, byrow = TRUE) *0.25
polymod_con <-  polymod_all-polymod_phys

## First conversational: Norming such that  5-9 year old have same number as 10-19 year old
#ch
(ch <- merge.contacts(groups = list(1,2,3:4,5:6),lower.breaks = c(0,5,10,13,15,18), prop = propUK[1:6], m = listCT[[1]][1:6,1:6], make.symmetric = TRUE))
listCT[[1]][1:2,1:2] <- polymod_con/polymod_con[2,2]*mean(diag(ch$m)[3:4])
#cs
(cs <- merge.contacts(groups = list(1,2,3:4,5:6),lower.breaks = c(0,5,10,13,15,18), prop = propUK[1:6], m = listCT[[3]][1:6,1:6], make.symmetric = TRUE))
listCT[[3]][1:2,1:2] <- polymod_con/polymod_con[2,2]*mean(diag(cs$m)[3:4])

## Same for physical contacts
#ph
(ph <- merge.contacts(groups = list(1,2,3:4,5:6),lower.breaks = c(0,5,10,13,15,18), prop = propUK[1:6], m = listCT[[5]][1:6,1:6], make.symmetric = TRUE))
listCT[[5]][1:2,1:2] <- polymod_phys/polymod_phys[2,2]*mean(diag(ph$m)[3:4])
#ps
(ps <- merge.contacts(groups = list(1,2,3:4,5:6),lower.breaks = c(0,5,10,13,15,18), prop = propUK[1:6], m = listCT[[7]][1:6,1:6], make.symmetric = TRUE))
listCT[[7]][1:2,1:2] <- polymod_phys/polymod_phys[2,2]*mean(diag(ps$m)[3:4])

# ## Hvor stor en reduktion skal der til for at få R0 under 1.
# ## Her er Social distancing ikke taget med.
# 
# # Først med 0.2 som vækt på samtaler
# vaekt <- rep(c(0.2, 1), each=4)
# m <- listCT[[1]]*vaekt[1]
# for (i in 2:7){
#   m <- m + listCT[[i]]*vaekt[i]
# }
# eigen(m)$values[1]
# 
# max.eigen <- function(work, other, PvsC=0.2){
#   vaekt <- as.numeric(outer(c(1, work , 0.0, other),c(PvsC,1)))
#   m2 <- listCT[[1]]*vaekt[1]
#   for (i in 2:7){
#     m2 <- m2 + listCT[[i]]*vaekt[i]
#   }
#   eigen(m2)$values[1]
#   
# }


########################### ##
##  Grouping in 5yr groups
groups <- list(1,2,3:4,5:6 ,7:8,9, 10,11, 12,13, 14,15, 16,17, 18,19) # Angiv grupper baseret på lower breaks

listTypes <- list()
for (i in names(listCT)){
  listTypes[[i]] <- merge.contacts(groups = groups, lower.breaks = lower.breaks, prop = propDK, m = listCT[[i]] )
}
ageMin <- c((0:15)*5)


## Counts for all oand merged into four ####
#cont <- as.matrix(m)*rep(prop, each=length(prop))
nAgeGr <- length(listTypes[[1]]$prop)
listCounts<- list()
for (i in names(listTypes)){
  listCounts[[i]] <- listTypes[[i]]$m*rep(listTypes[[i]]$prop, each=nAgeGr)
}


names(listCounts)
weightConversational <- 0.2


######################### ####
merge.counts2contacts <- function(groups, lower.breaks, prop, cont){ 
  # Groups: list with vectors for each desired group
  # lower.breaks: Vector with lower bound of each interval. Last is open to Inf age
  # prop: Proportion of population in each age group
  # cont: number of contacts (may be scaled) between age groups and assumed symmetric (From in columns and to in rows)

  ## New group names
  rn <- sapply(groups,function(x)lower.breaks[x[1]])  
  dimNames <- paste0(rn, "-",c(rn[-1]-1,""))
  
  ng <- length(groups)
  gc <- matrix(NA, ng, ng)
  gp <- numeric(ng)
  for (i in 1:ng){
    gp[i] <- sum(prop[groups[[i]]])
    for (j in 1:ng){
      gc[i,j] <- sum(cont[groups[[i]], groups[[j]]])
    }
  }
  mgc <- gc/rep(gp, each=length(gp))
  dimnames(mgc) <- list(dimNames, dimNames)
  names(gp) <- dimNames
  return(list(m=mgc, prop=gp)) # Contacts and proportion in each group
}



## weights
listSce <- list(normal=list(0.2,0.2,0.2,0.2,1,1,1,1))

for (i in names(listSce)){
  names(listSce[[i]]) <- names(listCounts)
}


ageMin10 <- (0:7)*10
groups5to10 <- list(1:2, 3:4, 5:6, 7:8, 9:10, 11:12, 13:14, 15:16)
EffCont <- matrix(0, nrow = length(groups5to10), ncol=length(groups5to10))
red <- "normal"
for (i in names(listCounts)){
  tmp <- merge.counts2contacts(groups = groups5to10, lower.breaks = ageMin, prop = propDK, cont = listCounts[[i]]*listSce[[red]][[i]] )
  
    weight <- ifelse(grepl("physical",i),1,0.2)
  EffCont <- EffCont + weight * tmp$m
}
beta0 <-  EffCont / tmp$prop * 0.15
prop10 <- tmp$prop

eigen(beta0)$va


############################# #
## SE2I2R ODE model        ####
library(deSolve)


# Dual strain model
SS2E2I2RD <- function(t,x,p){
  ## Unpack state by hand 
  x <- as.numeric(x)
  n   <- length(x) / 14
  S   <- x[1:n]
  E1A  <- x[n + (1:n)]
  E2A  <- x[2*n + (1:n)]
  I1A <- x[3*n + (1:n)] # infected at home 
  I2A <- x[4*n + (1:n)]
  E1B  <- x[5*n + (1:n)]
  E2B  <- x[6*n + (1:n)]
  I1B <- x[7*n + (1:n)] # infected at home 
  I2B <- x[8*n + (1:n)]
  H  <- x[9*n + (1:n)] # at hospital 
  R   <- x[10*n + (1:n)]
  D   <- x[11*n + (1:n)]
  CH  <- x[12*n + (1:n)] # cumulative hospital
  V <- x[13*n + (1:n)] # Vaccinated
  
  #print(p)
  with(p, {
    ## New infections
    dNewInfA <- numeric(n)
    dNewInfB <- numeric(n)
    for(i in 1:n){
      dNewInfA[i] <- S[i] * beta[i,] %*% (I1A+I2A) * RR  
      dNewInfB[i] <- S[i] * betaScaleAB * beta[i,] %*% (I1B+I2B) * RR  
    }
    
    ## Vaccinating from oldest
    vacCap <- vacCapVec[sum(t>=vacCapTime)]
    vacRate <- numeric(n)
    wantsVac <- vacMaxProp - V
    if (any(wantsVac > 1e-6)){
      vacAgeGr <- max(which(wantsVac > 1e-6)) ## ID for agegr to vaccinate      
    } else {
      vacAgeGr <- 0
    }

    if (vacAgeGr>1){
      vacRemainAgr <- wantsVac[vacAgeGr]
      if (vacRemainAgr<1e-4){
        vacRate[vacAgeGr+(-1:0)] <- vacCap * c(1-(vacRemainAgr/1e4)^0.1 , (vacRemainAgr/1e4)^0.1)
      } else {
        vacRate[vacAgeGr] <- vacCap
      }
    }
    if (vacAgeGr == 1){
      vacRate[1] <- vacCap
    }

    ## Right hand side of differential equations
    dX <- c( dS = - dNewInfA - dNewInfB - vacRate * vacEff,  # dS
             dE1A = dNewInfA - 2*gammaEI * E1A,              # dE1
             dE2A = 2*gammaEI*E1A - 2*gammaEI*E2A,           # dE2
             dI1A = 2*gammaEI * E2A       - 2*recI * I1A,    # dI1R
             dI2A = 2*recI*I1A               - 2*recI * I2A, # dI2R

             dE1B = dNewInfB - 2*gammaEI * E1B,              # dE1
             dE2B = 2*gammaEI*E1B - 2*gammaEI*E2B,           # dE2
             dI1B = 2*gammaEI * E2B  - 2*recI * I1B,         # dI1R
             dI2B = 2*recI*I1B       - 2*recI * I2B,         # dI2R
             
             # Hospital
             dH = 2*recI * (I2A+I2B) * (1-pIR) - recH* H,    # dH
             
             dR = 2*recI*(I2A+I2B)*pIR + recH*H*pHR + vacRate * vacEff,         # dR 
             dD = recH*H*(1-pHR),                            # dD 
             dCH = 2*recI * (I2A + I2B) * (1-pIR),           # dHC
             dV = vacRate                    # dV # Andel vaccineret
    )
    return(list(dX))
  }
  )}

## Extending beta0 to split out 80+ with same parm as 70+
betae <- matrix(NA,ncol=9, nrow=9)
betae[-9,-9] <- beta0
betae[9,] <- betae[8,]
betae[,9] <- betae[,8]
betae[8,9] <- betae[9,8] <- betae[9,7]
ageGrNames <- paste0((0:8)*10, rep(c("-","+"),c(8,1)), c((0:7)*10+9,""))
dimnames(betae) <- list(ageGrNames, ageGrNames)
tmp <- 1/sqrt(diag(betae))
betaNorm <- diag(tmp) %*% betae %*% diag(tmp)
dimnames(betaNorm) <- dimnames(betae)

## Notes:
## https://github.com/mariefly/HOPE/raw/master/HOPE_report_2021-01-02.pdf
## SumContacts1m in November 2020
#c(20,  30,  40,  50,  60, 70) # Lower bound agegr
#c(7.5, 5.5, 5.5, 5.5, 4.5, 3)
# Expanded to nine 10 yr age groups
hopeAgeGr <- c(0,  10, 20,  30,  40,  50,  60, 70, 80) # Lower bound agegr
sumCont1m <- c(6, 7.5, 7.5, 5.5, 5.5, 5.5, 4.5, 3, 3)
scaling <- sqrt(sumCont1m / sum(sumCont1m))
betae2 <- diag(scaling) %*% betaNorm %*% diag(scaling)
dimnames(betae2) <- dimnames(betae)


## Loading risk of hosp and death
library(openxlsx)
risks <- read.xlsx("ArcGISWeb_cases_hosp_death_2021-01-05.xlsx", sheet = 2)

n <- ncol(betae)
props <- risks$Folketal / sum(risks$Folketal)
names(props) <- rownames(betae)
Npop <- 1e6
NpopAg <- props*Npop


p1 <- list(gammaEI= 1/5, # 1/Latent period
           recI= 1/5,  # 1/days before recovery or hospital
           recH= 1/6,  # 1/days in hospital before discharge
           pIR= 1 - risks$pHosp, # Probability of recovery without hospitalization (1- P(hospitalization))
           pHR= 1 - risks$pDeathOfHosp, # Probability of recovery (vs death) in hospital (ignoring ICU)

           beta=betae2,
           betaScaleAB = 1.6,
           RR=4,
           vacCapVec = c(5000/Npop, 5000/Npop),
           vacCapTime = c(0, 60),
           vacEff = 0.95,
           vacMaxProp = props * 0.9
)



tmp <-  risks$Cases/NpopAg 
initDistAgeGr <- tmp/sum(tmp)

initAgr <- 1000* initDistAgeGr
initPropB <- 0.0#25

initN0 <- c(S= rep(0, n),# NpopAg,
            E1A = initAgr*(1-initPropB),
            E2A = initAgr*(1-initPropB), 
            I1A = initAgr*(1-initPropB),
            I2A = initAgr*(1-initPropB),
            E1B = initAgr*initPropB,
            E2B = initAgr*initPropB, 
            I1B = initAgr*initPropB,
            I2B = initAgr*initPropB,
            H= initAgr*risks$pHosp ,# / p1$recH,   # From "SSI overvågningsrapport 16042020":  c(0, 8, 38, 59, 144, 228, 211, 600),
            R= rep(0, n),
            D= rep(0, n),
            CH= rep(0, n),
            V = rep(0,n))
initN0[1:n] <- NpopAg - rowSums(matrix(initN0, n))  # S0 are those that are not infected initially
init0Pre <- initN0/Npop


plotStates <- function(solv, plotThese = c("E1", "E2", "I1", "I2","H","D","V")){
  
  par(mfrow=c(2, ceiling(length(plotThese)/2)), mar=c(3.3, 3.3, 2, 1), mgp=c(2, 0.7,0))
  for (i in plotThese){
    matplot(solv[,1], Npop*solv[,grep(paste0("^",i), colnames(solv))], col = hcl.colors(9), type="l", 
            lty=rep(1:2, each=n), lwd=2, main = i , ylab="Individuals per million", xlab="Time [days]")
  }
  plot(1,1, type="n", xaxt="n", yaxt="n", xlab="",ylab="", frame.plot = FALSE)
  legend("top", legend=names(props), lwd=2, col = hcl.colors(9))
  
}


times <- 1:180


#################################### #
## An example
p1$RR <- 3.9*1.0 # The factor 3.9 gives Rt=1
p1$vacCap <- rep(0,2)
solPre <- ode(y = init0Pre, times = c(0,30), func = SS2E2I2RD, parms = p1)
solPre[,grep("^D", colnames(solPre))] <- 0
times <- 1:180
p1$vacCapVec <- c(500/Npop, 5000/Npop)

# Vac & No B.1.1.7
init0 <- solPre[2,-1]
sol1VacR1 <- ode(y = init0, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)
plotStates(solv=sol1VacR1)


## Nu med 5% B.1.1.7 
init0 <- solPre[2,-1]
idA <- grep("^[EI][12]A",names(init0))
idB <- grep("^[EI][12]B",names(init0))
initPropB <- 0.05
init0[idB] <-  initPropB * init0[idA]
init0[idA] <-  (1-initPropB) * init0[idA]

sol2VacR1 <- ode(y = init0, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)
plotStates(solv=sol2VacR1)


my.extract <- function(sol, pop){
  data.frame(t = sol[,1],
        I = rowSums(sol[,grep("^I[12]", colnames(sol))]) * pop,
        H = rowSums(sol[,grep("^H", colnames(sol))]) * pop,
        D = rowSums(sol[,grep("^D", colnames(sol))]) * pop
        )
  
}

######################## #
## 2021-02-03          ###
## Figure for paper   ####

par(mfrow=c(2,2), mar=c(3.3, 3.3,2,1), mgp=c(2, 0.7,0))

# Baseline
p1$betaScaleAB <-  1.6
p1$RR <- 3.9*1.0 # Rt=1.0
p1$vacCap <- rep(0,2)
solPre <- ode(y = init0Pre, times = c(0,30), func = SS2E2I2RD, parms = p1)
solPre[,grep("^D", colnames(solPre))] <- 0
times <- 1:180
p1$vacCapVec <- c(500/Npop, 5000/Npop)


## Init states without B.1.1.7 
init0 <- solPre[2,-1]
## Init states with 5% B.1.1.7 
init0B117 <- solPre[2,-1]
idA <- grep("^[EI][12]A",names(init0B117))
idB <- grep("^[EI][12]B",names(init0B117))
initPropB <- 0.05
init0B117[idB] <-  initPropB * init0B117[idA]
init0B117[idA] <-  (1-initPropB) * init0B117[idA]

#DK popRef <- popDK[,sum(Total)] # 5.8 mio
popRef <- Npop # 1 mio
1200/popDK[,sum(Total)]*Npop ## Rescaled hosp capacity
plotToPng <- !TRUE
plotToPdf <- TRUE
if(plotToPng){
  png(filename = "Alternative_scenarios.png", width=2000, height=1500, pointsize = 40)
  par(mfrow=c(2,2), mar=c(3.3, 3.3,2,1), mgp=c(1.9, 0.7,0))
} else { if (plotToPdf){
  pdf(file = "Alternative_scenarios.pdf", width = 8, height = 2, pointsize = 10)
  par(mfrow=c(1,4), mar=c(3., 3.,3,1), mgp=c(1.8, 0.7,0))
  
}}
useCol <- c("#f44601","#055413", "#3a3e85")#
useLty <- c("solid", "solid","41")
useYlim <- c(0, 1200)
useLwd <- ifelse(plotToPdf, 3, 5)
addAll <- function(){
  axis(2, at=(0:6)*200, labels = c(0,NA,2,NA,4,NA,6)*200)
  abline(h=210, lty=3, lwd=2)
}


## Baseline
# RR 1
p1$betaScaleAB <-  1.6
p1$vacCapVec <- c(500/Npop, 5000/Npop)
p1$RR <- 3.9*1.0 # Rt = 1.0
sol2VacR1 <- ode(y = init0B117, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)
sol1VacR1 <- ode(y = init0, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)
p1$RR <- 0.9* 3.9 # Rt = 0.9
# Vac & No B.1.1.7
sol2VacR09 <- ode(y = init0B117, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)

# Summary
Vac1R1 <- my.extract(sol1VacR1, pop = popRef)
Vac2R09 <- my.extract(sol2VacR09, pop = popRef)
Vac2R1 <- my.extract(sol2VacR1, pop = popRef)

matplot(Vac1R1$t , cbind(Vac2R1$H, Vac2R09$H, Vac1R1$H) , type = "l",lwd=useLwd, 
        main="A: Baseline", xlab="Days", ylab="Hospitalized",
        col=useCol, lty=useLty, ylim=useYlim, yaxt="n")
addAll()


## Less effective restrictions
# RR 1.1
p1$betaScaleAB <-  1.6
p1$vacCapVec <- c(500/Npop, 5000/Npop)
p1$RR <- 3.9*1.1 # Rt = 1.1
sol2VacR1 <- ode(y = init0B117, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)
sol1VacR1 <- ode(y = init0, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)
p1$RR <- 0.9* 3.9*1.1 # Rt = 1.1*0.9
# Vac & No B.1.1.7
sol2VacR09 <- ode(y = init0B117, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)

# Summary
Vac1R1 <- my.extract(sol1VacR1, pop = popRef)
Vac2R09 <- my.extract(sol2VacR09, pop = popRef)
Vac2R1 <- my.extract(sol2VacR1, pop = popRef)

matplot(Vac1R1$t , cbind(Vac2R1$H, Vac2R09$H, Vac1R1$H) , type = "l",lwd=useLwd, 
        main="B: 10% less effective restrictions", xlab="Days", ylab="Hospitalized",
        col=useCol, lty=useLty, ylim=useYlim, yaxt="n")
addAll()




## B.1.1.7 more infectious
# RR 1
p1$betaScaleAB <-  1.7
p1$vacCapVec <- c(500/Npop, 5000/Npop)
p1$RR <- 3.9*1.0 # Rt = 1.0
sol2VacR1 <- ode(y = init0B117, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)
sol1VacR1 <- ode(y = init0, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)
p1$RR <- 0.9* 3.9 # Rt = 0.9
# Vac & No B.1.1.7
sol2VacR09 <- ode(y = init0B117, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)

# Summary
Vac1R1 <- my.extract(sol1VacR1, pop = popRef)
Vac2R09 <- my.extract(sol2VacR09, pop = popRef)
Vac2R1 <- my.extract(sol2VacR1, pop = popRef)

matplot(Vac1R1$t , cbind(Vac2R1$H, Vac2R09$H, Vac1R1$H) , type = "l",lwd=useLwd, 
        main="C: 10 % pts higher transmission\n for B.1.1.7", xlab="Days", ylab="Hospitalized",
        col=useCol, lty=useLty, ylim=useYlim, yaxt="n")
addAll()




## More vaccines from the start
# RR 1
p1$betaScaleAB <-  1.6
p1$vacCapVec <- c(2500/Npop, 5000/Npop)
p1$RR <- 3.9*1.0 # Rt = 1.0
sol2VacR1 <- ode(y = init0B117, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)
sol1VacR1 <- ode(y = init0, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)
p1$RR <- 0.9* 3.9 # Rt = 0.9
# Vac & No B.1.1.7
sol2VacR09 <- ode(y = init0B117, times = times, func = SS2E2I2RD, parms = p1, maxsteps =2e4)

# Summary
Vac1R1 <- my.extract(sol1VacR1, pop = popRef)
Vac2R09 <- my.extract(sol2VacR09, pop = popRef)
Vac2R1 <- my.extract(sol2VacR1, pop = popRef)

matplot(Vac1R1$t , cbind(Vac2R1$H, Vac2R09$H, Vac1R1$H) , type = "l",lwd=useLwd, 
        main="D: Additional vaccination\n first 60 days", xlab="Days", ylab="Hospitalized",
        col=useCol, lty=useLty, ylim=useYlim, yaxt="n")
addAll()

if(plotToPng | plotToPdf){
  dev.off()
}

