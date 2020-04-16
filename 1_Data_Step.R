rm(list = ls())

library(tidyverse)
library(caret)
library(splines2)
setwd("C:/Users/User1/Desktop/WorkFlow/ST540Midterm/Simulation/")

## A function that deal with data
shoe.deal <- function(org){
  
  ## Deal with name
  tmp <- org$match_name
  name <- sort(tmp[!duplicated(tmp)])
  match_name <- rep(NA, length(tmp))
  
  for (ii in 1:length(tmp)){
    match_name[ii] <- which(name == tmp[ii])
  }
  
  ## Deal with age and gender
  tmp <- org$name_age
  age <- gender <- rep(NA, length(tmp))
  
  left <- str_locate(tmp, "\\(")[,1]
  right <- str_locate(tmp, "\\)")[,1]
  len <- right - left
  for (ii in 1:length(len)){
    # age init
    if (len[ii] > 3) {
      age[ii] <- as.numeric(substr(as.character(tmp[ii]),left[ii]+2, right[ii]-1))
    }
    
    # gender
    gen <- substr(as.character(tmp[ii]),left[ii]+1,left[ii]+1)
    if (gen == "F") {
      gender[ii] <- 1
    } else {
      gender[ii] <- 0
    }
  }
  
  # fill in age
  tmp <- org$year
  for (ii in 1:length(match_name)){
    indi <- which(match_name == ii)
    if ( (length(which(is.na( age[indi]))) > 0) && (length(which(is.na( age[indi]))) < length(indi)) ){
      diff <- tmp[indi] - age[indi]
      id <- is.na(diff)
      diffwona <- diff[!id]
      diffna <- which(id)
      for (jj in 1:length(diffna)){
        age[indi[diffna[jj]]] <- tmp[indi[diffna[jj]]] - mean(diffwona);
      }
    }
    
  }
  
  # # Gather first attend-age and deal with missing age
  # tmp <- org$year
  # age.first <- c(0)
  # age.fix <- rep(-1,length(age))
  # age.indi <- rep(0,length(age))
  # for (ii in 1:length(name)){
  #   indi <- which(match_name == ii)
  #   if (length(which(!is.na(age[indi])))){
  #     age.first <- c(age.first, min(age[indi]))
  #   } else {
  #     age.fix[indi] <- tmp[indi] - min(tmp[indi])
  #     age.indi[indi] <- ii
  #   }
  # }
  # age.first <- age.first[-1]
  
  
  ## Deal with course
  tmp <- paste(as.character(org$year), org$marathon, sep = "-")
  course <- sort(tmp[!duplicated(tmp)])
  cours <- rep(NA, length(tmp))
  for (ii in 1:length(tmp)){
    cours[ii] <- which(course == tmp[ii])
  }
  
  ## Deal with vaporfly
  vp <- as.double(org$vaporfly)
  
  ## Delete redundant data
  ord <- order(match_name)
  res <- data.frame(runner = match_name[ord], gender = gender[ord], age = age[ord], course = cours[ord], vp = vp[ord], time = org$time_minutes[ord])
  # age.miss <- list(age.first, age.fix[ord], age.indi[ord])
  
  # return(list(res,name,course,age.miss))
  return(list(res,name,course))
}

## Gathering data
Men <- read.csv("C:/Users/User1/Desktop/WorkFlow/ST540Midterm/Simulation/0_men_sampled_shoe.csv", header = TRUE)
Women <- read.csv("C:/Users/User1/Desktop/WorkFlow/ST540Midterm/Simulation/0_women_sampled_shoe.csv", header = TRUE)

## Whole Data Set
shoe.data <- shoe.deal(bind_rows(Men,Women))
#shoe.data <- shoe.miss <- shoe.deal(bind_rows(Men,Women))

# ## ages
# fixd <- shoe.miss[[4]]
# fixd[[2]] <- fixd[[2]][which(!is.na(shoe.miss[[1]][,5]))]
# saveRDS(fixd, file = "1_vp_fixdata.RDS")

#shoe.miss[[1]] <- shoe.miss[[1]][!is.na(shoe.miss[[1]][,5]),]
# summary(shoe.miss[[1]])
# length(unique(shoe.miss[[1]][,1]))


## Complete Data Set
shoe.data[[1]] <- shoe.data[[1]][!is.na(shoe.data[[1]][,3]),]
shoe.data[[1]] <- shoe.data[[1]][!is.na(shoe.data[[1]][,5]),]
write.csv(shoe.data[[1]], "C:/Users/User1/Desktop/WorkFlow/ST540Midterm/Simulation/1_vp_miss_f.csv")

# change runner's indi
run.check <- unique(shoe.data[[1]]$runner)
run.incl <- rep(FALSE, length(shoe.data[[1]][,1]))
run.new <- rep(0,length(shoe.data[[1]][,1]))
flag <- 0
for (ii in 1:length(shoe.data[[2]])){
  indi <- which(shoe.data[[1]]$runner == ii)
  if (length(indi) > 0){
    flag <- flag + 1
    run.new[indi] <- flag
    run.incl[ii] <- TRUE
  }
}
shoe.data[[2]] <- shoe.data[[2]][run.incl]
shoe.data[[1]]$runner <- run.new

# change course's indi
cour.check <- unique(shoe.data[[1]]$course)
cour.incl <- rep(FALSE, length(shoe.data[[1]][,1]))
cour.new <- rep(0,length(shoe.data[[1]][,1]))
flag <- 0
for (ii in 1:length(shoe.data[[3]])){
  indi <- which(shoe.data[[1]]$course == ii)
  if (length(indi) > 0){
    flag <- flag + 1
    cour.new[indi] <- flag
    cour.incl[ii] <- TRUE
  }
}
shoe.data[[3]] <- shoe.data[[3]][cour.incl]
shoe.data[[1]]$course <- cour.new

# summary(shoe.data[[1]])
# length(unique(shoe.data[[1]][,1]))

## Save
write.csv(shoe.data[[1]], "C:/Users/User1/Desktop/WorkFlow/ST540Midterm/Simulation/1_vp_comp_f.csv")
#write.csv(shoe.miss[[1]], "C:/Users/User1/Desktop/WorkFlow/ST540Midterm/Simulation/1_vp_miss_f.csv")
write.csv(shoe.data[[2]], "C:/Users/User1/Desktop/WorkFlow/ST540Midterm/Simulation/1_vp_name.csv")
write.csv(shoe.data[[3]], "C:/Users/User1/Desktop/WorkFlow/ST540Midterm/Simulation/1_vp_cors.csv")

## Save Using data
dat <- read.csv(file = "C:/Users/User1/Desktop/WorkFlow/ST540Midterm/Simulation/1_vp_comp_f.csv", header = TRUE)
indi <- c(1:1502)
y <- dat[indi,7]
f1 <- dat[indi,2]
f2 <- dat[indi,5]
f3 <- dat[indi,3]+1
age <- dat[indi,c(4)]
age.s <- as.data.frame(bSpline(age, df = NULL, knots = NULL, degree = 3, intercept = FALSE))
names(age.s) <- c("age.s1","age.s2","age.s3")
x <- cbind(dat[indi, c(6)], age.s)
Shoe.data <- list(y=y, x=x, f1=f1, f2=f2, f3=f3, n=length(y), nf1=max(f1), nf2=max(f2), nf3=2, p=4)
set.seed(1)
indi <- createDataPartition(c(1:length(y)), times = 1, p = 0.9, list = FALSE)
saveRDS(Shoe.data, file = "1_data.RDS")
saveRDS(indi, file = "1_indi.RDS")

## Save using function
FoldsCV.test <- function(samples, model, X, Y){
  ## Pre-allocate
  res <- rep(0, 5)
  
  ## Calc MSE
  beta <- summary(samples)$statistics[,1]
  res[1] <- mean((Y - X %*% beta)^2)
  
  ## Calc DIC
  tmp <- dic.samples(model, n.iter=50000, progress.bar="none")
  res[2] <- sum(tmp$deviance) + sum(tmp$penalty)
  
  ## Test Effect Sample Size
  ef <- effectiveSize(samples)
  n <- length(ef)
  res[3] <- length(which(ef < 1000)) / n
  
  ## Test Convergence 1
  gel <- gelman.diag(samples)
  res[4] <- length(which(gel[[1]][,2] > 1.1)) / n
  
  ## Test Convergence 2
  gew <- geweke.diag(samples[[1]])
  res[5] <- length(which(abs(gew[[1]]) > 2)) / n
  
  return(res)
  
}
saveRDS(FoldsCV.test, file = "1_func.RDS")


## Save Test DATA
factor1 <- matrix(0, ncol = max(f1), nrow = length(f1))
for (ii in 1:length(f1)){
  factor1[ii,f1[ii]] <- 1
}

factor2 <- matrix(0, ncol = max(f2), nrow = length(f2))
for (ii in 1:length(f2)){
  factor2[ii,f2[ii]] <- 1
}

factor3 <- matrix(0, ncol = max(f3), nrow = length(f3))
for (ii in 1:length(f3)){
  factor3[ii,f3[ii]] <- 1
}


X1 <- cbind(factor1, factor2, factor3, x)
saveRDS(X1, "1_dat1.RDS")
saveRDS(y, "1_daty.RDS")

