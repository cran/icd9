## ----setup, echo = FALSE, cache = FALSE----------------------------------
suppressWarnings({
  suppressMessages({
    #library(knitr, warn.conflicts = FALSE) # for opts_chunk only
    library(icd9)
    library(magrittr)
    })
  })

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)

patientData <- data.frame(
  visitId = c(1000, 1000, 1000, 1000, 1001, 1001, 1002),
  icd9 = c("40201", "2258", "7208", "25001", "34400", "4011", "4011"),
  poa = c("Y", NA, "N", "Y", "X", "Y", "E"),
  stringsAsFactors = FALSE
  )

## ----showdatlong,echo=FALSE----------------------------------------------
patientData

## ----showdatwide,echo=FALSE----------------------------------------------
oldsaf <- options("stringsAsFactors" = FALSE)
pd2 <- patientData %>% icd9LongToWide
options(oldsaf)
pd2[is.na(pd2)] <- ""
pd2

## ----getcomorbidities----------------------------------------------------
icd9ComorbidAhrq(patientData)[, 1:8]

## ----getcomorbidities1a--------------------------------------------------
icd9ComorbidQuanDeyo(patientData)[, 1:8]

## ----getcomorbidities2---------------------------------------------------
patientData %>% icd9FilterPoaYes %>% icd9ComorbidAhrq %>% extract(1:8)

## ----"conversionSimple"--------------------------------------------------
icd9DecimalToShort(c("1", "10.20", "100", "123.45"))
icd9ShortToDecimal(c("1", "22", "2244", "1005"))

# similar operations with magrittr, also showing invalid codes
codes <- c("87.65", "9999", "Aesop", -100, "", NA)
icd9DecimalToShort(codes)

## ----validation----------------------------------------------------------
icd9IsValidDecimal("V10.2")
icd9IsValidShort(c("099.17", "-1"))
icd9IsValidDecimal(c("099.17", "-1.1"))
icd9IsValidShort(c("1", "001", "100", "123456", "003.21"))

## ----invalidint, eval = FALSE--------------------------------------------
#  #icd9IsValidShort(100) # gives an error

## ----ranges--------------------------------------------------------------
# get all possible codes
"003" %i9sa% "0033" %>% head(9) # show first 9 of 111 values
# just get the ones which correspond to diagnoses (keeping the 3-digit chapters)
"003" %i9s% "0033"

"10099" %i9sa% "10101"
"V10" %i9da% "V10.02"
"E987" %i9da% "E988.1"

# can't range between different types:
# "V10" %i9s% "E800" # throws an error

## ----rangeanomaly--------------------------------------------------------
icd9ExpandRangeShort("V100", "V1002", onlyReal = TRUE) # default, equivalent to %i9s%
icd9ExpandRangeShort("V100", "V1002", onlyReal = FALSE) # V10.0 is not a leaf node, equivalent to %i9sa%

## ----"childrenReal"------------------------------------------------------
icd9Children("391")
# mid-level code
icd9Children("0032")
# leaf node has no children
icd9Children("00321")

## ----"childrenAll"-------------------------------------------------------
# first ten possible ICD-9 child codes from 391
icd9Children("391", onlyReal = FALSE)[1:10]

## ----explainSimple-------------------------------------------------------
icd9Explain("1.0") # 'decimal' format code inferred
icd9Explain("0019") # 'short' format code inferred

## ----explainComplex------------------------------------------------------
# we can be explicit about short vs decimal
icd9Explain("434.00", isShort = FALSE) 
icd9Explain(c("43410","43491"), isShort = TRUE)
#explain top level code with children
"391" %>% icd9Explain # single three-digit code
"391" %>% icd9Children # let's see the child codes
"391" %>% icd9Children %>% icd9Explain # children condensed to parent code
"391" %>% icd9Children %>% icd9Explain(doCondense = FALSE) # prevent condense

## ----explainArb----------------------------------------------------------
icd9Explain(list(somecodes = c("001", "391.0"), 
                 morecodes = c("001.1", "001.9")))

## ----cholera-------------------------------------------------------------
icd9Explain(list(cholera = "001", rheumatic_heart = "390"))

## ----noexplain, eval = FALSE---------------------------------------------
#  s <- icd9ExplainDecimal("001.5") # gives warning

## ----ExampleQDDementia---------------------------------------------------
length(quanDeyoComorbid[["Dementia"]]) # 133 possible ICD-9 codes
# icd9Explain summarizes these to just two groups:
quanDeyoComorbid[["Dementia"]] %>% icd9Explain(warn = FALSE)
# contrast with:
quanDeyoComorbid[["Dementia"]] %>% icd9Explain(doCondense = FALSE, warn = FALSE)

## ----ShowRangeOperator---------------------------------------------------
length("390" %i9da% "392.1")
"390" %i9da% "392.1" %>% icd9Explain(warn = FALSE)

## ----ShowPoaChoices, echo=FALSE------------------------------------------
icd9PoaChoices

## ----simplepoa-----------------------------------------------------------
patientData %>% icd9FilterPoaYes

## ----notnopoa------------------------------------------------------------
patientData %>% icd9FilterPoaNotNo

## ----ahrq----------------------------------------------------------------
#ahrqComorbid <- icd9:::parseAhrqSas() # user doesn't need to do this
names(ahrqComorbid)

## ----elix----------------------------------------------------------------
names(elixComorbid)

## ----quanElix------------------------------------------------------------
names(quanDeyoComorbid)
names(quanElixComorbid)

## ----chainpoatocomorbid--------------------------------------------------
patientData %>%
  icd9FilterPoaNotNo %>%
  icd9ComorbidAhrq %>%
  extract(1:9)

## ----elixvsquanelix------------------------------------------------------
difference <- icd9DiffComorbid(elixComorbid, quanElixComorbid, 
                 names = c("CHF", "PHTN", "HTN", "Valvular"))
# reuslts also returned as data
str(difference)

## ----quanonlyphtn--------------------------------------------------------
difference$PHTN$only.y %>% icd9GetReal %>% icd9Explain

## ----cardiacgrep---------------------------------------------------------
icd9Hierarchy[
  grepl(pattern = "(heart)|(cardiac)",
        x = c(icd9Hierarchy$descLong, icd9Hierarchy$descShort),
        ignore.case = TRUE),
  "icd9"] %>% unique -> cardiac

## ----cardiacChainExplainExample------------------------------------------
cardiac %>% icd9Explain(warn = FALSE) %>% head(10)

## ----speed, cache = TRUE-------------------------------------------------
# codes selected from AHRQ mapping
many_patients <- icd9:::randomPatients(100000) 

system.time(
  icd9ComorbidAhrq(many_patients)
  )[["elapsed"]] # result in seconds

## ----"arbitraryMapping"--------------------------------------------------
names(icd9Chapters)[c(1:5, 14)]
myMap <- icd9:::icd9ChaptersToMap(icd9Chapters[c(2, 5, 14)])
icd9Comorbid(patientData, myMap) # no +ve 

## ----realmapping---------------------------------------------------------
ahrqStrict <- lapply(ahrqComorbid, icd9GetReal)
str(ahrqComorbid[1:5]) # first five of the original:
str(ahrqStrict[1:5]) # and first five of the result:

