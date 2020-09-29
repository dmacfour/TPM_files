require(lme4)
TPMsubtransformedMOD <-TPMsubtransformed

TPMsubtransformedMOD$nleaver <- ifelse(is.na(TPMsubtransformedMOD$nleaver), 0, TPMsubtransformedMOD$nleaver)

mixedmod <- lm(nleaver ~ SUBJECT_AREA * CATEGORY * region, data = filter(TPMsubtransformedMOD, !region %in% c("xxother", "null"), YEAR %in% c(2012:2016)))
mixedmod <- glm(nleaver ~ YEAR + SUBJECT_AREA * CATEGORY + region, family = poisson(), data = filter(TPMsubtransformedMOD, !region %in% c("xxother", "null"), YEAR %in% c(2012:2016)))

summary(mixedmod)

newdat <- filter(TPMsubtransformedMOD, !region %in% c("xxother", "null"), YEAR == 2017)

newdat$preds <- predict(mixedmod,newdata = newdat) 

sqrt(mean((newdat$resid <- newdat$nleaver - newdat$preds)^2))
