require(dplyr)
require(zoo)
require(ggplot2)
require(lubridate)
#require(mvtnorm)

timeformats<-c("%d/%m/%Y %H:%M:%S", "%d/%m/%Y")
tagtypes<-c('AI', 'AC', 'EI', 'FC', 'FI', 'FX', 'GM', 'HC', 'II', 'KM', 'LC', 'LI', 'PC', 'PI', 'PDI', 'TC', 'TI')
tagpattern <- paste0('^(\\d\\d(', paste(tagtypes, collapse ='|'), '))')
#tagpattern1 <- '.(SP|OP)$'
tagpattern1 <- '.(SP)$'

tagnames<-read.delim('Properties', encoding='Spanish_Spain.1252', stringsAsFactors = FALSE, header = TRUE)

#
#  Read Mean data
#

meandata<-read.delim('Mean', encoding='Spanish_Spain.1252', stringsAsFactors = FALSE, header = TRUE)
names(meandata)[1]<-'t'

meandata <- meandata %>%
            select(t, one_of(paste0('X',tagnames$Tag[grep(tagpattern, tagnames$Tag)]))) %>%
            select(-one_of(paste0('X',tagnames$Tag[grep(tagpattern1, tagnames$Tag)]))) %>%
            mutate_each(funs(as.numeric(sub(',', '.', .))),-t) %>% 
            mutate(t=parse_date_time(t, timeformats)) %>%
            select(t, which(apply(., 2, function(x) sd(x, na.rm=TRUE))!=0)) %>%
            select(t, which(colSums(is.na(.))/nrow(.)<0.25))

meandata<-meandata[,!duplicated(t(meandata))]

#
#  Read SD data
#

sddata<-read.delim('SD', encoding='Spanish_Spain.1252', stringsAsFactors = FALSE, header = TRUE)
names(sddata)[1]<-'t'

sddata <- sddata %>%
      select(one_of(as.character(names(meandata)))) %>%
      mutate_each(funs(as.numeric(sub(',', '.', .))),-t) %>% 
      mutate(t=parse_date_time(t, timeformats))

names(sddata)<-sub('^X','S', names(sddata))

#
# Merge into data and normalize
#

#data<- left_join(meandata, sddata)
data <- meandata

n<-NCOL(data)
data.mean <- data %>% select(-t) %>% summarize_each(funs(mean(., na.rm = TRUE)))
data.sd <- data %>% select(-t) %>% summarize_each(funs(sd(., na.rm = TRUE)))
data.range <- data %>% select(-t) %>% summarize_each(funs(max(., na.rm = TRUE)-min(., na.rm = TRUE)))
data.mean<-data.mean[rep(1, nrow(data)), ]
data.sd<-data.sd[rep(1, nrow(data)), ]
data.range<-data.range[rep(1, nrow(data)), ]
dataN <- cbind(t=data$t, (data[,2:n] - data.mean) / data.sd)

#
#  Evaluate the covariance matrix and the mean row
#

SigmaM <- cov(dataN[,-1], use = "pairwise.complete.obs")
#SigmaM[SigmaM<0.01]<-0
SigmaMInv <- solve(SigmaM)

e<-eigen(SigmaM, only.values = TRUE, symmetric = TRUE)
c<--(nrow(SigmaM)/2)*log(2*pi)-0.5*sum(log(abs(e$values)))
p<-NULL
for(i in 1:nrow(dataN)){
      x<-as.matrix(dataN[i, -1])
      x[is.na(x)]<-0
      p<-c(p, c - 0.5 * (x %*% SigmaMInv %*% t(x)))      
}

zp<-zoo(p, order.by = dataN$t)
autoplot(zp)

p.df<-data.frame(t=dataN$t, p=p)
q2 <- p.df$p < log(0.05)

pmax<-data.frame(t=as.POSIXct(character()), C1=character(), C2=character(),
                 C3=character(),C4=character(),C5=character(), 
                 stringsAsFactors=FALSE)

irs<-which(q2)
for(i in 1:nrow(dataN[q2,])){
      ir<-irs[i]
      pmax[i, 1]<-dataN[ir,1]
      pmax[i,-1]<-names(dataN)[(order(abs(dataN[ir,-1]), decreasing = TRUE)+1)[1:5]]
}

ir=5
df<-dataN %>% select(t, one_of(as.character(pmax[ir,-1])))
zz<-zoo(x=df[,-1], order.by=df$t)
autoplot(window(zz, start=pmax$t[ir]-2*86400, end=pmax$t[ir]+2*86400)) + facet_free() + geom_vline(xintercept=as.numeric(pmax$t[ir]))
tagnames %>% filter(Tag %in% substring(as.character(pmax[ir,-1]),2))

pruG<-gather(dataN, key, zval, -t)
g <- ggplot(pruG, aes(x=t, y=key, fill=abs(zval))) 
g <- g + geom_tile() + scale_fill_continuous(low="white", high="red", limits=c(0,10))
g <- g + geom_vline(xintercept=as.numeric(pmax$t), color = 'blue')
g <- g + scale_y_discrete(breaks=NULL)
g
