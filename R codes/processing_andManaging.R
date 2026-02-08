library(data.table)
library(ggplot2)
library(tidyr)
library(lubridate)

data <- fread("QVI_data.csv")

data[, YEARMONTH := year(DATE) * 100 + month(DATE)]


measureOverTime <- data[, .(
  totSales = sum(TOT_SALES),
  nCustomers = uniqueN(LYLTY_CARD_NBR),
  nTxnPerCust = .N / uniqueN(LYLTY_CARD_NBR),
  nChipsPerTxn = sum(PROD_QTY) / .N,
  avgPricePerUnit = sum(TOT_SALES) / sum(PROD_QTY)
), by = .(STORE_NBR, YEARMONTH)][order(STORE_NBR, YEARMONTH)]


storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% storesWithFullObs, ]

calculateCorrelation <- function(inputTable, metricCol, trialStore) {
  stores <- unique(inputTable$STORE_NBR)
  out <- data.table()

  for (s in stores) {
    corr <- cor(
      inputTable[STORE_NBR == trialStore, eval(metricCol)],
      inputTable[STORE_NBR == s, eval(metricCol)]
    )
    out <- rbind(out, data.table(Store1 = trialStore, Store2 = s, corr_measure = corr))
  }
  out
}

calculateMagnitudeDistance <- function(inputTable, metricCol, trialStore) {
  stores <- unique(inputTable$STORE_NBR)
  out <- data.table()

  for (s in stores) {
    temp <- data.table(
      Store1 = trialStore,
      Store2 = s,
      YEARMONTH = inputTable[STORE_NBR == trialStore, YEARMONTH],
      measure = abs(
        inputTable[STORE_NBR == trialStore, eval(metricCol)] -
        inputTable[STORE_NBR == s, eval(metricCol)]
      )
    )
    out <- rbind(out, temp)
  }

  minMax <- out[, .(minD = min(measure), maxD = max(measure)), by = YEARMONTH]
  out <- merge(out, minMax, by = "YEARMONTH")
  out[, mag := 1 - (measure - minD) / (maxD - minD)]

  out[, .(mag_measure = mean(mag)), by = .(Store1, Store2)]
}

trial_store <- 77

corr_sales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_cust  <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

mag_sales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
mag_cust  <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

score_sales <- merge(corr_sales, mag_sales, by = c("Store1", "Store2"))
score_sales[, scoreSales := 0.5 * corr_measure + 0.5 * mag_measure]

score_cust <- merge(corr_cust, mag_cust, by = c("Store1", "Store2"))
score_cust[, scoreCust := 0.5 * corr_measure + 0.5 * mag_measure]

final_score <- merge(score_sales[, .(Store2, scoreSales)],
                     score_cust[, .(Store2, scoreCust)],
                     by = "Store2")

final_score[, finalScore := 0.5 * scoreSales + 0.5 * scoreCust]

control_store <- final_score[Store2 != trial_store][order(-finalScore)][1, Store2]
control_store

scalingFactor <- preTrialMeasures[STORE_NBR == trial_store, sum(totSales)] /
                 preTrialMeasures[STORE_NBR == control_store, sum(totSales)]

controlScaled <- measureOverTime[STORE_NBR == control_store,
                                 .(YEARMONTH, controlSales = totSales * scalingFactor)]

trialSales <- measureOverTime[STORE_NBR == trial_store,
                              .(YEARMONTH, trialSales = totSales)]

pctDiff <- merge(controlScaled, trialSales, by = "YEARMONTH")
pctDiff[, pctDiff := abs(trialSales - controlSales) / controlSales]

stdDev <- sd(pctDiff[YEARMONTH < 201902, pctDiff])
stdDev

scalingFactorCust <- preTrialMeasures[
  STORE_NBR == trial_store, sum(nCustomers)
] / preTrialMeasures[
  STORE_NBR == control_store, sum(nCustomers)
]


controlCustScaled <- measureOverTime[
  STORE_NBR == control_store,
  .(YEARMONTH, controlCustomers = nCustomers * scalingFactorCust)
]

trialCust <- measureOverTime[
  STORE_NBR == trial_store,
  .(YEARMONTH, trialCustomers = nCustomers)
]

pctDiffCust <- merge(controlCustScaled, trialCust, by = "YEARMONTH")
pctDiffCust[, pctDiff := abs(trialCustomers - controlCustomers) / controlCustomers]


stdDevCust <- sd(pctDiffCust[YEARMONTH < 201902, pctDiff])
stdDevCust

pctDiffCust[YEARMONTH >= 201902]

trial_store <- 86

corr_sales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_cust  <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

mag_sales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
mag_cust  <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

score_sales <- merge(corr_sales, mag_sales, by = c("Store1", "Store2"))
score_sales[, scoreSales := 0.5 * corr_measure + 0.5 * mag_measure]

score_cust <- merge(corr_cust, mag_cust, by = c("Store1", "Store2"))
score_cust[, scoreCust := 0.5 * corr_measure + 0.5 * mag_measure]

final_score <- merge(
  score_sales[, .(Store2, scoreSales)],
  score_cust[,  .(Store2, scoreCust)],
  by = "Store2"
)

final_score[, finalScore := 0.5 * scoreSales + 0.5 * scoreCust]

control_store <- final_score[Store2 != trial_store][order(-finalScore)][1, Store2]
control_store
scalingFactor <- preTrialMeasures[STORE_NBR == trial_store, sum(totSales)] /
                 preTrialMeasures[STORE_NBR == control_store, sum(totSales)]


controlScaled <- measureOverTime[
  STORE_NBR == control_store,
  .(YEARMONTH, controlSales = totSales * scalingFactor)
]

trialSales <- measureOverTime[
  STORE_NBR == trial_store,
  .(YEARMONTH, trialSales = totSales)
]

pctDiff <- merge(controlScaled, trialSales, by = "YEARMONTH")
pctDiff[, pctDiff := abs(trialSales - controlSales) / controlSales]

stdDev <- sd(pctDiff[YEARMONTH < 201902, pctDiff])
stdDev

pctDiff[YEARMONTH >= 201902]

scalingFactorCust <- preTrialMeasures[STORE_NBR == trial_store, sum(nCustomers)] /
                     preTrialMeasures[STORE_NBR == control_store, sum(nCustomers)]

controlCustScaled <- measureOverTime[
  STORE_NBR == control_store,
  .(YEARMONTH, controlCustomers = nCustomers * scalingFactorCust)
]

trialCust <- measureOverTime[
  STORE_NBR == trial_store,
  .(YEARMONTH, trialCustomers = nCustomers)
]

pctDiffCust <- merge(controlCustScaled, trialCust, by = "YEARMONTH")
pctDiffCust[, pctDiff := abs(trialCustomers - controlCustomers) / controlCustomers]

stdDevCust <- sd(pctDiffCust[YEARMONTH < 201902, pctDiff])
stdDevCust
pctDiffCust[YEARMONTH >= 201902]


trial_store <- 88

corr_sales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_cust  <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

mag_sales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
mag_cust  <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

score_sales <- merge(corr_sales, mag_sales, by = c("Store1", "Store2"))
score_sales[, scoreSales := 0.5 * corr_measure + 0.5 * mag_measure]

score_cust <- merge(corr_cust, mag_cust, by = c("Store1", "Store2"))
score_cust[, scoreCust := 0.5 * corr_measure + 0.5 * mag_measure]

final_score <- merge(
  score_sales[, .(Store2, scoreSales)],
  score_cust[,  .(Store2, scoreCust)],
  by = "Store2"
)

final_score[, finalScore := 0.5 * scoreSales + 0.5 * scoreCust]

control_store <- final_score[Store2 != trial_store][order(-finalScore)][1, Store2]
control_store
scalingFactor <- preTrialMeasures[
  STORE_NBR == trial_store, sum(totSales)
] / preTrialMeasures[
  STORE_NBR == control_store, sum(totSales)
]
controlScaled <- measureOverTime[
  STORE_NBR == control_store,
  .(YEARMONTH, controlSales = totSales * scalingFactor)
]

trialSales <- measureOverTime[
  STORE_NBR == trial_store,
  .(YEARMONTH, trialSales = totSales)
]

pctDiff <- merge(controlScaled, trialSales, by = "YEARMONTH")
pctDiff[, pctDiff := abs(trialSales - controlSales) / controlSales]
stdDev <- sd(pctDiff[YEARMONTH < 201902, pctDiff])
stdDev
pctDiff[YEARMONTH >= 201902]
scalingFactorCust <- preTrialMeasures[
  STORE_NBR == trial_store, sum(nCustomers)
] / preTrialMeasures[
  STORE_NBR == control_store, sum(nCustomers)
]
controlCustScaled <- measureOverTime[
  STORE_NBR == control_store,
  .(YEARMONTH, controlCustomers = nCustomers * scalingFactorCust)
]

trialCust <- measureOverTime[
  STORE_NBR == trial_store,
  .(YEARMONTH, trialCustomers = nCustomers)
]

pctDiffCust <- merge(controlCustScaled, trialCust, by = "YEARMONTH")
pctDiffCust[, pctDiff := abs(trialCustomers - controlCustomers) / controlCustomers]
stdDevCust <- sd(pctDiffCust[YEARMONTH < 201902, pctDiff])
stdDevCust
pctDiffCust[YEARMONTH >= 201902]
