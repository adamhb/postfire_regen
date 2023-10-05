source('analysis1/03_results_A1.Rmd')

## Plots of RRI end vs. post-fire precipitation 
plot(modData$pptYr1_3_annual_mean,modData$RRI_end)
plot(df4$pptYr1_3_annual_mean, df4$RRI_end)
