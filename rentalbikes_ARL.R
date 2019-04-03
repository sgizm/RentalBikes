---
  title: "R Notebook"
output: html_notebook
---
  
  # Introduction
  
  # Load libraries
  
  ```{r,eval=TRUE,include=FALSE}
library(ggplot2)
library(GGally)
library(vcd)
library(data.table)
#library(gmodels)
library(Hmisc)
library(corrplot)
library(magrittr)
library(reshape2)
library(scales)
library(readr)
library(car)
library(rgl)
library(nFactors)
library(cluster)
library(pvclust)
library(plyr)
library(PerformanceAnalytics)
library(gplots)
library(factoextra)
library(class)
library(graphics)
library(NbClust)
#library(ComplexHeatmap)
library(arules)
library(arulesViz)
```

# Load data
```{r}
clus_data <- read.csv("clus_data.csv")
clus_data <- data.frame(clus_data)
str(clus_data)
cols <- c(7:20, 26:35, 37:46) # Selecting the numerical columns
clus_data_selected <- clus_data[, cols]
numericColumns <- sapply(clus_data, is.numeric)
str(clus_data_selected)
# and a scaled version of that with NA's removed
clus_data_scaled <- scale(na.omit(clus_data_selected))
```

# Recoding
```{r}
# Numerical columns starting with "INV".
qData <- clus_data[, grepl("^INV",colnames(clus_data)) & numericColumns ]
clus_data <- cbind(clus_data, apply(qData, 2, function(x) {x[x == 1 |x == 2] <- "disagree"; x[x == 3] <- "indecisive" ; x[x == 4 |x == 5] <- "agree"; x}))
# Numerical columns starting with "USER".
qData2 <- suppressWarnings(clus_data[, grepl("^USER",colnames(clus_data)) & numericColumns ])
clus_data <- cbind(clus_data, apply(qData2, 2, function(x) {x[x == 1 |x == 2] <- "disagree"; x[x == 3] <- "indecisive" ; x[x == 4 |x == 5] <- "agree"; x}))
# Numerical columns starting with "INF.".
qData3 <- suppressWarnings(clus_data[, grepl("^INF",colnames(clus_data)) & numericColumns ])
clus_data <- cbind(clus_data, apply(qData3, 2, function(x) {x[x == 1 |x == 2] <- "disagree"; x[x == 3] <- "indecisive" ; x[x == 4 |x == 5] <- "agree"; x}))
# Numerical columns starting with "X3.3.".
qData4 <- suppressWarnings(clus_data[, grepl("^X3.3",colnames(clus_data)) & numericColumns ])
clus_data <- cbind(clus_data, apply(qData4, 2, function(x) {x[x == 1 |x == 2] <- "disagree"; x[x == 3] <- "indecisive" ; x[x == 4 |x == 5] <- "agree"; x}))
# Numerical columns starting with "X3.3.".
qData5 <- suppressWarnings(clus_data[, grepl("^M",colnames(clus_data)) & numericColumns ])
clus_data <- cbind(clus_data, apply(qData5, 2, function(x) {x[x == 1 |x == 2] <- "not often"; x[x == 3] <- "indecisive" ; x[x == 4 |x == 5] <- "often"; x}))
# Worktime":
WORKTIM <- suppressWarnings(car::recode(clus_data$worktime,
                                        "c(2,3,4,5,6,7,8,9,10,11)='1 year'; c(12, 24, 36) ='1-3 year'; c(48, 60)='4-5 year'; c(72, 84, 96, 108)='6-9 year'; c(120, 132)='11-12 year'"))
WORKTIM  <- data.frame(WORKTIM)
clus_data <- cbind(clus_data, WORKTIM)
```


# all non numericals
```{r}
#clus_data <- read.csv("factors.csv")
clus_data <- clus_data[, -1]
is.fact <- sapply(clus_data, is.factor)
factors.df <- clus_data[, is.fact]
str(factors.df) # selecting only the factors 

# forgot what the below is doing 
rmcols <- rev(seq(1,ncol(clus_data))[sapply(clus_data, is.numeric)])
for (i in rmcols) clus_data[[i]] <- NULL
colnames(clus_data)
class(clus_data)
str(clus_data)

cols <- c(4, 7:9, 10, 12:15, 20, 21, 23, 25, 35, 43, 44, 36, 37) 
trial <- factors.df[, cols]
trial_non <- na.omit(trial)
str(trial_non)
```

```{r}
# First a sort function
setMethod("sort", signature(x = "associations"),
          function (x, decreasing = TRUE, na.last = NA, by = "support", ...) {
            q <- quality(x)
            q <- q[, pmatch(by, colnames(q)), drop = FALSE]
            if(is.null(q)) stop("Unknown interest measure to sort by.")
            if(length(x) == 0) return(x)
            x[do.call(order, c(q, list(na.last = na.last, decreasing = decreasing)))]
          }) 

rules = apriori(trial, parameter=list(support=0.05, confidence=0.5, minlen=3, maxlen=36, maxtime=150), appearance = list( rhs=c("condexp=Yes, actively"), default = "lhs"))

#, appearance = list( rhs=c("X3.3B.1=agree"), default = "lhs")
#, appearance = list(lhs=c("INVE.1=agree"))
sorted_mult <- sort(rules, decreasing = TRUE, by=c("confidence", "count"))
inspect(sorted_mult[1:100])

rules
topRules <- rules[1:10]
inspect(rules[1:15])
top.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(top.confidence[1:40])
top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(top.lift[1:40])
plot(sorted_mult[1:7], method = "graph")
plot(topRules, method = "grouped")

```
# Mosaic confirmations
```{r}
subs_pal <- colorspace::diverge_hcl(7)

mosaic( ROLE ~ COMPANY , data = clus_data, highlighting_fill = subs_pal[1:4], rot_labels=c(45,45,45,45)  )
```


