# Compilation of methods for weekly and daily nowcasting 

## Methods addressed so far

### Weekly
- Nowcaster using INLA and using mgcv
- baselinenowcast package
- (processing now) surveillance package: unif, lawless, bayes.trunc & bayes.trunc.ddcp
- (others) epinowcast package, NobBS package 

### Daily
- baselinenowcast package
- epinowcast package
- surveillance package
- NowcastPNN + new development (undergoing development)
- others to explore (uncertain if they will work, or depend on parameters specific to disease): RIVM P-spline, Lison et al. renewal model

## What is missing so far?
- Finish weekly methods code
- Develop comparison framework and metrics (using the 95% interval as in nowcaster, then samples?)
- Finish daily methods comparison
- Apply comparison framework and metrics
