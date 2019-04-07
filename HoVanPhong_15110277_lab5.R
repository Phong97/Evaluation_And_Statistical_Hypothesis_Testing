bm25 <- read.table("bm25.eval", header = TRUE, sep = " ")
map.bm25 <- bm25[5]
r_prec.bm25 <- bm25[6]
b_pref.bm25 <- bm25[7]
recip_rank.bm25 <- bm25[8]

classic <- read.table("classic.eval", header = TRUE, sep = " ")
map.classic <- classic[5]
r_prec.classic <- classic[6]
b_pref.classic <- classic[7]
recip_rank.classic <- classic[8]


dfr <- read.table("dfr.eval", header = TRUE, sep = " ")
map.dfr <- dfr[5]
r_prec.dfr <- dfr[6]
b_pref.dfr <- dfr[7]
recip_rank.dfr <- dfr[8]


ib <- read.table("ib.eval", header = TRUE, sep = " ")
map.ib <- ib[5]
r_prec.ib <- ib[6]
b_pref.ib <- ib[7]
recip_rank.ib <- ib[8]


lmdrichlet <- read.table("lmdrichlet.eval", header = TRUE, sep = " ")
map.lmdrichlet <- lmdrichlet[5]
r_prec.lmdrichlet <- lmdrichlet[6]
b_pref.lmdrichlet <- lmdrichlet[7]
recip_rank.lmdrichlet <- lmdrichlet[8]


lmjelinekmercer <- read.table("lmjelinekmercer.eval", header = TRUE, sep = " ")
map.lmjelinekmercer <- lmjelinekmercer[5]
r_prec.lmjelinekmercer <- lmjelinekmercer[6]
b_pref.lmjelinekmercer <- lmjelinekmercer[7]
recip_rank.lmjelinekmercer <- lmjelinekmercer[8]

t_test <- function(x,y) {
  x <- unlist(x, use.names=FALSE)
  y <- unlist(y, use.names=FALSE)
  return(t.test(x, y, mu=0, alternative="two.sided", paired=TRUE, conf.level=0.95)[[5]][[1]])
}

rank_test <- function(x,y) {
  x <- unlist(x, use.names=FALSE)
  y <- unlist(y, use.names=FALSE)
  return(wilcox.test(x, y, mu=0, alternative="two.sided", paired=TRUE, conf.level=0.95)[[3]])
}



sign_test <- function(x,y) {
  x <- unlist(x, use.names=FALSE)
  y <- unlist(y, use.names=FALSE)
  return(binom.test(sum(x>y), 114, p=0.5, alternative="two.sided", conf.level=0.95)[[5]][[1]])
}

rand_test <- function(x, y, B=10000) {
  x <- unlist(x, use.names=FALSE)
  y <- unlist(y, use.names=FALSE)
  dist <- replicate(B, mean((2*rbinom(length(x), 1, 1/2)-1)*(x-y)))
  return (mean(mean(x-y) > dist))
}

bootstrap_test <- function(x, y, B=10000) {
  x <- unlist(x, use.names=FALSE)
  y <- unlist(y, use.names=FALSE)
  dist <- replicate(B, mean(sample(x-y, length(x-y), replace=T)))
  return (mean(mean(x-y) > 0))
}

data.map <- c(map.bm25, map.classic, map.dfr, map.ib, map.lmdrichlet, map.lmjelinekmercer)
data.r_prec <- c(r_prec.bm25, r_prec.classic, r_prec.dfr, r_prec.ib, r_prec.lmdrichlet, r_prec.lmjelinekmercer)
data.b_pref <- c(b_pref.bm25, b_pref.classic, b_pref.dfr, b_pref.ib, b_pref.lmdrichlet, b_pref.lmjelinekmercer)
data.recip_rank <- c(recip_rank.bm25, recip_rank.classic, recip_rank.dfr, recip_rank.ib, recip_rank.lmdrichlet, recip_rank.lmjelinekmercer)

handle <- function(handling) {
  p05 <- 0
  p01 <- 0
  p001 <- 0
  for(i in 1:(length(data.map)-1)){
    for(j in (i+1):length(data.map)){
      result <- handling(data.map[i], data.map[j])
      if (result < 0.05) {
        p05 <- p05 + 1
      } else if (result < 0.01) {
        p01 <- p01 + 1
      } else if (result < 0.001) {
        p001 <- p001 + 1
      }
    }
  }
  
  map.result <- c(p05, p01, p001)
  
  p05 <- 0
  p01 <- 0
  p001 <- 0
  for(i in 1:(length(data.map)-1)){
    for(j in (i+1):length(data.map)){
      result <- handling(data.r_prec[i], data.r_prec[j])
      if (result < 0.05) {
        p05 <- p05 + 1
      } else if (result < 0.01) {
        p01 <- p01 + 1
      } else if (result < 0.001) {
        p001 <- p001 + 1
      }
    }
  }
  
  r_prec.result <- c(p05, p01, p001)
  
  p05 <- 0
  p01 <- 0
  p001 <- 0
  for(i in 1:(length(data.map)-1)){
    for(j in (i+1):length(data.map)){
      result <- handling(data.b_pref[i], data.b_pref[j])
      if (result < 0.05) {
        p05 <- p05 + 1
      } else if (result < 0.01) {
        p01 <- p01 + 1
      } else if (result < 0.001) {
        p001 <- p001 + 1
      }
    }
  }
  
  b_pref.result <- c(p05, p01, p001)
  
  p05 <- 0
  p01 <- 0
  p001 <- 0
  for(i in 1:(length(data.map)-1)){
    for(j in (i+1):length(data.map)){
      result <- handling(data.recip_rank[i], data.recip_rank[j])
      if (result < 0.05) {
        p05 <- p05 + 1
      } else if (result < 0.01) {
        p01 <- p01 + 1
      } else if (result < 0.001) {
        p001 <- p001 + 1
      }
    }
  }
  
  recip_rank.result <- c(p05, p01, p001)
  
  
  return ( rbind(measure = labels,
                 map = map.result,
                 r_prec = r_prec.result,
                 b_pref = b_pref.result,
                 recip_rank = recip_rank.result
  ))
}

labels <- c("p<0.05", "p<0.01", "p<0.001")
print("Student's t-test")
handle(t_test)

print("Wilcoxon signed rank test")
handle(rank_test)

print("Sign test")
handle(sign_test)

print("Randomization test")
handle(rand_test)

print("Bootstrap test")
handle(bootstrap_test)
