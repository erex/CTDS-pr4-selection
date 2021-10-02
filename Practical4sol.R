## ----include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(eval=TRUE, echo=TRUE, message=FALSE, warnings=FALSE)
solution <- TRUE


## ----reload----------------------------------------------------------------------------------------------------------------------------
library(Distance)
data("DuikerCameraTraps")
load("duikermodels.RData")


## ----pass1, eval=TRUE------------------------------------------------------------------------------------------------------------------
hnQAIC <- QAIC(hn0, hn1, hn2)
uniQAIC <- QAIC(uni1, uni2)
hrQAIC <- QAIC(hr0, hr1, hr2)


## ----pass1a, echo=FALSE, eval=TRUE-----------------------------------------------------------------------------------------------------
knitr::kable(hnQAIC, caption="QAIC values for half normal key models.")


## ----pass1b, echo=FALSE, eval=TRUE-----------------------------------------------------------------------------------------------------
knitr::kable(uniQAIC, caption="QAIC values for uniform key models.")


## ----pass1c, echo=FALSE, eval=TRUE-----------------------------------------------------------------------------------------------------
knitr::kable(hrQAIC, caption="QAIC values for hazard rate key models.")


## ----pass2, eval=TRUE------------------------------------------------------------------------------------------------------------------
chats <- chi2_select(hn1, uni1, hr0)
modnames <- unlist(lapply(list(hn1, uni1, hr0), function(x) x$ddf$name.message))
results <- data.frame(modnames, chats)
results.sort <- results[order(results$criteria),]
knitr::kable(results.sort, digits=2, row.names = FALSE,
             caption="Compare with Table S5 of Howe et al. (2018)")


## --------------------------------------------------------------------------------------------------------------------------------------
p_a <- hr0$ddf$fitted[1]
w <- 15
rho <- sqrt(p_a * w^2)


## ---- theplot, eval=TRUE---------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(hr0, main="Peak activity", xlab="Distance (m)",
     showpoints=FALSE, lwd=3, xlim=c(0, 15))
plot(hr0, main="Peak activity", xlab="Distance (m)", pdf=TRUE,
     showpoints=FALSE, lwd=3, xlim=c(0, 15))


## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,1))


## ---- sampfrac, eval=TRUE--------------------------------------------------------------------------------------------------------------
viewangle <- 42 # degrees
samfrac <- viewangle / 360
conversion <- convert_units("meter", NULL, "square kilometer")
peak.hr.dens <- dht2(hr0, flatfile=DuikerCameraTraps, strat_formula = ~1,
                     sample_fraction = samfrac, er_est = "P2", convert_units = conversion)
print(peak.hr.dens, report="density")

