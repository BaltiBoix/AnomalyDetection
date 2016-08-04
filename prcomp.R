require(dplyr)
require(zoo)
require(ggplot2)
require(lubridate)
require(mvtnorm)

timeformats<-c("%d/%m/%Y %H:%M:%S", "%d/%m/%Y")
tagtypes<-c('AI', 'AC', 'EI', 'FC', 'FI', 'FX', 'GM', 'HC', 'II', 'KM', 'LC', 'LI', 'PC', 'PI', 'PDI', 'TC', 'TI')
tagpattern <- paste0('^(\\d\\d(', paste(tagtypes, collapse ='|'), '))')
tagpattern1 <- '.(SP|OP)$'
#tagpattern1 <- '.(SP)$'

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
      select(t, which(colSums(is.na(.))/nrow(.)<0.01))

meandata <- meandata[,!duplicated(t(meandata))]

meandata <- meandata[complete.cases(meandata),]

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
#  Merge Mean and SD data
#

#dataAll<- left_join(meandata, sddata)
dataAll<- meandata

data.pca <- prcomp(dataAll[,-1], scale. = TRUE)

data <- as.data.frame(data.pca$x[,1:250])
data <- cbind(t=dataAll$t, data)

SigmaM <- cov(data[,-1])

d <- dmvnorm(data[,-1], sigma = SigmaM, log=TRUE)
d.z <- zoo(order.by=data$t, x=d)
autoplot(d.z)

d.df <- data.frame(t=data$t, d=d)
q2 <- d.df$d < quantile(d, 0.01)

dmax<-data.frame(t=as.POSIXct(character()), C1=character(), C2=character(),
                 C3=character(),C4=character(),C5=character(), 
                 stringsAsFactors=FALSE)

irs<-which(q2)
for(i in 1:nrow(dataAll[q2,])){
      ir<-irs[i]
      dmax[i, 1]<-dataAll[ir,1]
      dmax[i,-1]<-names(dataAll)[(order(abs((dataAll[ir,-1]-data.pca$center)/data.pca$scale), decreasing = TRUE)+1)[1:5]]
}

ir=12
df<-dataAll %>% select(t, one_of(as.character(dmax[ir,-1])))
zz<-zoo(x=df[,-1], order.by=df$t)
autoplot(window(zz, start=dmax$t[ir]-2*86400, end=dmax$t[ir]+2*86400)) + facet_free() + geom_vline(xintercept=as.numeric(dmax$t[ir]))
left_join(data.frame(Tag=substring(as.character(dmax[ir,-1]),2), stringsAsFactors = FALSE), tagnames)
