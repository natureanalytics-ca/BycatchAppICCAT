# Generate an aggregated version of logdatExample
logdatExampleAggregated<- logdatExample |>
  group_by(Year,season,EW) |>
  summarise(sets=sum(sets),
            trips=sum(trips)) |>
  ungroup()
logdatExampleAggregated

write.csv(logdatExampleAggregated, file = paste0(getwd(), "/www/example_datasets/logbookdata_example_aggregate.csv"),  row.names = FALSE)

# Change logNum to "trips"
setupObj<-bycatchSetup(
  obsdat = obsdatExample,
  logdat = logdatExampleAggregated,
  yearVar = "Year",
  obsEffort = "sampled.sets",
  logEffort = "sets",
  obsCatch = "Catch",
  catchUnit = "number",
  catchType = "dead discard",
  logNum = "trips",
  sampleUnit = "trips",
  factorVariables = c("Year","season","EW"),
  numericVariables = NA,
  EstimateBycatch = TRUE,
  baseDir = getwd(),
  runName = "Example",
  runDescription = "Example with simulated data not aggregated",
  common = "Simulated species",
  sp = "Genus species",
  reportType = "html"
)
# No change in bycatchDesign
bycatchDesign(
  setupObj = setupObj,
  designScenario="aggregated",
  designMethods = "Ratio",
  designVars = "Year",
  groupVar = "Year",
  designPooling = FALSE,
  poolTypes = NULL,
  pooledVar = NULL,
  adjacentNum = NULL,
  minStrataUnit = 1,
  reportType = "html"
)
