library(Distance)

daytime <- read.csv("DaytimeDistances.txt", header=TRUE, sep="\t")
daytime <- subset(daytime, select=-c(utm.e, utm.n))
daytime <- daytime[, c("Region.Label", "Area", "multiplier",
                       "Sample.Label", "Effort", "distance")]

conversion <- convert_units("meter", NULL, "square kilometer")
trunc.list <- list(left=2, right=15)
mybreaks <- c(seq(2,8,1), 10, 12, 15)

d.hr0 <- ds(daytime, transect = "point", key="hr", adjustment = NULL,
                  cutpoints = mybreaks, truncation = trunc.list,
                  convert.units=conversion)
d.hr1 <- ds(daytime, transect = "point", key="hr", adjustment = "poly",
            cutpoints = mybreaks, truncation = trunc.list, order=1,
            convert.units=conversion)

d.hn0 <- ds(daytime, transect = "point", key="hn", adjustment = NULL,
            cutpoints = mybreaks, truncation = trunc.list,
            convert.units=conversion)
d.hn1 <- ds(daytime, transect = "point", key="hn", adjustment = "cos",
            cutpoints = mybreaks, truncation = trunc.list, order=2,
            convert.units=conversion)
d.hn2 <- ds(daytime, transect = "point", key="hn", adjustment = "cos",
            cutpoints = mybreaks, truncation = trunc.list, order=3,
            convert.units=conversion)

d.un1 <- ds(daytime, transect = "point", key="unif", adjustment = "cos",
            cutpoints = mybreaks, truncation = trunc.list, order=1,
            convert.units=conversion)
d.un2 <- ds(daytime, transect = "point", key="unif", adjustment = "cos",
            cutpoints = mybreaks, truncation = trunc.list, order=2,
            convert.units=conversion)
d.un3 <- ds(daytime, transect = "point", key="unif", adjustment = "cos",
            cutpoints = mybreaks, truncation = trunc.list, order=3,
            convert.units=conversion)


hnQAIC <- QAIC(d.hn0, d.hn1, d.hn2)
uniQAIC <- QAIC(d.un1, d.un2, d.un3)
hrQAIC <- QAIC(d.hr0)#, d.hr1)

out <- data.frame(UNname=c("un1","un2","un3"), UNIF=uniQAIC[,2], 
                  HNname=c("hn0","hn1","hn2"), HN=hnQAIC[,2])#, HR=c(hrQAIC, NULL, NULL))
knitr::kable(out)

chats <- chi2_select(d.hn1, d.un3, d.hr0)
modnames <- unlist(lapply(list(d.hn1, d.un3, d.hr0), function(x) x$ddf$name.message))
results <- data.frame(modnames, chats)
results.sort <- results[order(results$criteria),]
knitr::kable(results.sort, digits=2, row.names = FALSE,
             caption="Model selection second phase for duiker daytime")
