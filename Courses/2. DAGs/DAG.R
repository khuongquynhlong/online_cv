library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(ggforce)
#------------------------
# some useful function
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
}

#-----------------------

R = matrix(cbind(1,0,0,0,0,0,0,
                 0,1,0,0.5,0,0,0,
                 0,0,1,0.5,0,0,0,
                 0,0.5,0.5,1,0.5,0,0,
                 0,0,0,0.5,1,0.5,0.5,
                 0,0,0,0,0.5,1,0,
                 0,0,0,0,0.5,0,1), nrow = 7)
U = t(chol(R))
nvars = dim(U)[1]
numobs = 100000
set.seed(12)
random.normal = matrix(rnorm(nvars*numobs,10,1), nrow=nvars, ncol=numobs)
X = U %*% random.normal
newX = t(X)
raw = as.data.frame(newX)
orig.raw = as.data.frame(t(random.normal))
names(raw) = c("C1","C2", "C3", "E", "O", "C4", "C5")
cor(raw)

cor1 = cor(raw, method="pearson")%>%round(.,3)
corsig = cor.mtest(raw) %>% round(.,3)

cor1 %>%
    corrplot(.,
             type="lower",
             method="square",
             diag = T,
             sig.level = 0.05,
             tl.srt=45,
             p.mat = corsig,
             insig = "n",
             tl.cex=1,
             col=rev(brewer.pal(10,"Spectral")))

hc1 = raw %>% 
    ggplot(aes(x = C1)) +
    geom_histogram(color = "white", fill = "#fc4e2a") +
    theme_bw()+
    labs(title = "C1")
hc2 = raw %>% 
    ggplot(aes(x = C2)) +
    geom_histogram(color = "white", fill = "#41ae76") +
    theme_bw()+
    labs(title = "C2")
hc3 = raw %>% 
    ggplot(aes(x = C3)) +
    geom_histogram(color = "white", fill = "#74c476") +
    theme_bw()+
    labs(title = "C3")
hc4 = raw %>% 
    ggplot(aes(x = C4)) +
    geom_histogram(color = "white", fill = "#2171b5") +
    theme_bw()+
    labs(title = "C4")
hc5 = raw %>% 
    ggplot(aes(x = C5)) +
    geom_histogram(color = "white", fill = "#084594") +
    theme_bw()+
    labs(title = "C5")
hE = raw %>% 
    ggplot(aes(x = E)) +
    geom_histogram(color = "white", fill = "#005824") +
    theme_bw()+
    labs(title = "Exposure")
ho = raw %>% 
    ggplot(aes(x = O)) +
    geom_histogram(color = "white", fill = "blue") +
    theme_bw()+
    labs(title = "Outcome")

gridExtra::grid.arrange(hc1, hc2, hc3, hc4, hc5, hE, ho, ncol = 4)

m = lm(O ~ E + C4, data = raw)
summary(m)

#--------------------------

make.dag.bias = function(nmax = 100000, n.step = 1000, seed = 123){
    # tao function simulate so lieu C1 - C5
    make.dag.data <- function(n = 1000, seed = 123){
        R = matrix(cbind(1,0,0,0,0,0,0,
                         0,1,0,0.5,0,0,0,
                         0,0,1,0.5,0,0,0,
                         0,0.5,0.5,1,0.5,0,0,
                         0,0,0,0.5,1,0.5,0.5,
                         0,0,0,0,0.5,1,0,
                         0,0,0,0,0.5,0,1), nrow = 7)
        U = t(chol(R))
        nvars = dim(U)[1]
        numobs = n
        set.seed(seed)
        random.normal = matrix(rnorm(nvars*numobs,10,1), nrow=nvars, ncol=numobs)
        X = U %*% random.normal
        newX = t(X)
        raw = as.data.frame(newX)
        names(raw) = c("C1","C2", "C3", "E", "O", "C4", "C5")
        return(raw)
    }
    # Tinh bias
    sample_size = c(seq(100,1000, by = 100),seq(2000,nmax, by = n.step))
    c1 = rep(NA, length(sample_size))
    c2 = rep(NA, length(sample_size))
    c3 = rep(NA, length(sample_size))
    c4 = rep(NA, length(sample_size))
    c5 = rep(NA, length(sample_size))
    for (i in 1:length(sample_size)) {
        df = make.dag.data(n = sample_size[i], seed = seed)
        m0 = lm(O~E, data = df)
        m1 = lm(O~E + C1, data = df)
        m2 = lm(O~E + C2, data = df)
        m3 = lm(O~E + C3, data = df)
        m4 = lm(O~E + C4, data = df)
        m5 = lm(O~E + C5, data = df)
        
        c1[i] = summary(m1)$coefficients[2] - summary(m0)$coefficients[2]
        c2[i] = summary(m2)$coefficients[2] - summary(m0)$coefficients[2]
        c3[i] = summary(m3)$coefficients[2] - summary(m0)$coefficients[2] 
        c4[i] = summary(m4)$coefficients[2] - summary(m0)$coefficients[2]
        c5[i] = summary(m5)$coefficients[2] - summary(m0)$coefficients[2]
    }
    bias.data = tibble(sample_size, c1,c2,c3,c4,c5)
    return(bias.data)
}
    
#-----------------------------------------------------------
make.dag.variance = function(nmax = 100000, n.step = 1000, seed = 123){
    # tao function simulate so lieu C1 - C5
    make.dag.data <- function(n = 1000, seed = 123){
        R = matrix(cbind(1,0,0,0,0,0,0,
                         0,1,0,0.5,0,0,0,
                         0,0,1,0.5,0,0,0,
                         0,0.5,0.5,1,0.5,0,0,
                         0,0,0,0.5,1,0.5,0.5,
                         0,0,0,0,0.5,1,0,
                         0,0,0,0,0.5,0,1), nrow = 7)
        U = t(chol(R))
        nvars = dim(U)[1]
        numobs = n
        set.seed(seed)
        random.normal = matrix(rnorm(nvars*numobs,10,1), nrow=nvars, ncol=numobs)
        X = U %*% random.normal
        newX = t(X)
        raw = as.data.frame(newX)
        names(raw) = c("C1","C2", "C3", "E", "O", "C4", "C5")
        return(raw)
    }
    # Tinh bias
    sample_size = c(seq(100,1000, by = 100),seq(2000,nmax, by = n.step))
    c1 = rep(NA, length(sample_size))
    c2 = rep(NA, length(sample_size))
    c3 = rep(NA, length(sample_size))
    c4 = rep(NA, length(sample_size))
    c5 = rep(NA, length(sample_size))
    for (i in 1:length(sample_size)) {
        df = make.dag.data(n = sample_size[i], seed = seed)
        m0 = lm(O~E, data = df)
        m1 = lm(O~E + C1, data = df)
        m2 = lm(O~E + C2, data = df)
        m3 = lm(O~E + C3, data = df)
        m4 = lm(O~E + C4, data = df)
        m5 = lm(O~E + C5, data = df)
        
        c1[i] = vcov(m1)[2,2]
        c2[i] = vcov(m2)[2,2]
        c3[i] = vcov(m3)[2,2]
        c4[i] = vcov(m4)[2,2]
        c5[i] = vcov(m5)[2,2]
    }
    variance.data = tibble(sample_size, c1,c2,c3,c4,c5)
    return(variance.data)
}

#---------------------------------------------------------

df1 = make.dag.bias(seed = 12345)
df2 = make.dag.variance(seed = 12345)


df1 %>% gather(c1,c4,c5, key = "var", value = "value") %>%
    ggplot(aes(x = sample_size, y = value, col = var)) +
    geom_line(aes(linetype = var), size = 1) + 
    theme_bw() +
    labs(title = "Unnecessary adjustment",
         x = "Sample size",
         y = "Relative Bias")

df1 %>% gather(c1,c4,c5, key = "var", value = "value") %>%
    ggplot(aes(x = sample_size, y = value, col = var)) +
    geom_line(aes(linetype = var), size = 1) + 
    theme_bw() + 
    facet_zoom(x = sample_size <= 2000)+
    labs(title = "Unnecessary adjustment",
         x = "Sample size",
         y = "Relative Bias")

#--------------------------------------------------------
df2 %>% gather(c1:c5, key = "var", value = "value") %>%
    ggplot(aes(x = sample_size, y = value, col = var)) +
    geom_line(aes(linetype = var), size = 1) + 
    theme_bw() +
    labs(title = "Unnecessary adjustment",
         x = "Sample size",
         y = "Slope Variance")

df2 %>% gather(c1:c5, key = "var", value = "value") %>%
    ggplot(aes(x = sample_size, y = value, col = var)) +
    geom_line(aes(linetype = var), size = 1) + 
    theme_bw() + 
    facet_zoom(x = sample_size <= 2000)+
    labs(title = "Unnecessary adjustment",
         x = "Sample size",
         y = "Slope Variance")
