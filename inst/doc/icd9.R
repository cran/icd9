## ----setup, eval=TRUE, echo=FALSE, cache=FALSE------------------------------------------
suppressWarnings({
  suppressMessages({
    library(knitr, warn.conflicts = FALSE)
    library(icd9, warn.conflicts = FALSE)
    library(magrittr, warn.conflicts = FALSE)
  })
})

## font size options from the highlight package: c("normalsize", "tiny", "scriptsize", "footnotesize", "small", "large", "Large", "LARGE", "huge", "Huge")
opts_chunk$set(size = "scriptsize")
opts_chunk$set(cache = TRUE) # unless otherwise stated
options(width = 90)

patientData <- data.frame(
  visitId = c(1000, 1000, 1000, 1000, 1001, 1001, 1002),
  icd9 = c("402010", "27801", "7208", "25001", "34400", "4011", "4011"),
  poa = factor(c("Y", "Y", "N", "Y", "N", "Y", "N"))
)

## ----begin------------------------------------------------------------------------------
patientData

## ----getcomorbidities-------------------------------------------------------------------
icd9ComorbidAhrq(patientData)[, 1:10]

## ----getcomorbidities1a-----------------------------------------------------------------
icd9ComorbidQuanDeyo(patientData)[, 1:10]

## ----getcomorbidities2------------------------------------------------------------------
patientData %>%
  icd9FilterPoaYes() %>%
  icd9ComorbidAhrq() %>%
  extract(1:5)

## ----"comorbidUsingDefaultOpts", eval=FALSE---------------------------------------------
#  icd9Comorbid(icd9df = icdFilterPoaYes(patientData),
#               icd9Mapping = ahrqComorbid,
#               visitId = "visitId",
#               icd9Field = "icd9",
#               isShort = icd9:::icd9GuessIsShort(icd9df[[icd9Field]]),
#               shortMapping = TRUE)[1:5]

## ----"conversionSimple"-----------------------------------------------------------------
icd9DecimalToShort(c("1", "10.20", "100", "123.45"))
icd9ShortToDecimal(icd9DecimalToShort(c("1", "10.20", "100", "123.45")))
icd9DecimalToShort(c("1", "22", "22.44", "1005"))
icd9ShortToDecimal(c("1", "22", "2244", "1005"))

# similar with magrittr, also showing invalid codes
codes <- c("9", "87.65", "100.5", "9999", "Aesop", NA)
codes %>% icd9DecimalToShort
codes %>% icd9DecimalToShort %>% icd9ShortToDecimal

## ----validation-------------------------------------------------------------------------
icd9IsValidDecimal("V10.2")
icd9IsValidShort(c("099.17", "-1"))
icd9IsValidDecimal(c("099.17", "-1"))
icd9IsValidShort(c("1", "001", "100", "123456", "003.21"))

## ----invalidint, eval=FALSE-------------------------------------------------------------
#  icd9IsValidShort(100) # warns

## ----ranges-----------------------------------------------------------------------------
"10099" %i9sa% "10101"
"V10" %i9da% "V10.02"
"E987" %i9da% "E988.1"

# get all possible codes
"003" %i9sa% "0033" %>% head(9) # returns 111 values
# just get the ones which correspond to billable codes (but keep the 3-digit chapters)
"003" %i9s% "0033"

## ----rangeanomaly-----------------------------------------------------------------------
icd9ExpandRangeShort("V100", "V1002", onlyReal = TRUE) # default
icd9ExpandRangeShort("V100", "V1002", onlyReal = FALSE) # V10.0 is not billable

## ----"childrenReal"---------------------------------------------------------------------
icd9Children("391", onlyReal = TRUE)

## ----"childrenAll"----------------------------------------------------------------------
icd9ChildrenShort("391", onlyReal = FALSE)[1:10]

## ----explainSimple----------------------------------------------------------------------
icd9ExplainDecimal("1.0")
icd9ExplainShort("0019")

## ----explainComplex---------------------------------------------------------------------
icd9Explain("1.0", isShort = FALSE)
icd9Explain(c("0010","4131"), isShort = TRUE)
# combine with some conversions
icd9ExplainDecimal(icd9ShortToDecimal("0019"))
"4139" %>% icd9ShortToDecimal() %>% icd9ExplainDecimal()
"413.1" %>% icd9DecimalToShort() %>% icd9ExplainShort()

#explain top level code with children
"391" %>% icd9ChildrenShort(onlyReal = TRUE)
"391" %>% icd9ExplainShort()
# default is to condense down to three-digit "major" level
"391" %>% icd9ChildrenShort() %>% icd9ExplainShort()
"391" %>% icd9ChildrenShort() %>% icd9ExplainShort(doCondense = FALSE)

## ----explainArb-------------------------------------------------------------------------
icd9ExplainDecimal(list(cholera = c("001", "001.0", "001.1", "001.9")))
# same using decimal codes without a list
icd9ExplainDecimal(c("001", "001.0", "001.1", "001.9"))

## ----echo=FALSE,eval=FALSE--------------------------------------------------------------
#  # 001/cholera doesn't itself have an explanation: TODO walk down children to get next level explanations.
#  icd9ExplainDecimal(list(cholera = "001", rheumatic_heart = "390"))

## ----noexplain, eval=FALSE--------------------------------------------------------------
#  icd9ExplainDecimal("001.5") # gives warning

## ----cardiacgrep------------------------------------------------------------------------
icd9Hierarchy[
  grepl(
    pattern = "(heart)|(cardiac)",
    x = c(icd9Hierarchy[["descLong"]],
          icd9Hierarchy[["descShort"]]),
    ignore.case = TRUE),
  "icd9"] %>% unique -> cardiac

## ----cardiacChainExplainExample---------------------------------------------------------
cardiac %>% icd9ExplainShort %>% head(10)

## ----ExampleQDDementia------------------------------------------------------------------
quanDeyoComorbid[["Dementia"]] %>% icd9ExplainShort
icd9GetInvalidShort(quanDeyoComorbid[["Dementia"]])

## ----ShowRangeOperator------------------------------------------------------------------
length("390" %i9da% "392.1")

"390" %i9da% "392.1" %>%
  icd9DecimalToShort() %>%
  icd9ExplainShort()

"390" %i9da% "392.1" %>% icd9ExplainDecimal()

## ----ShowPoaChoices---------------------------------------------------------------------
icd9PoaChoices

## ----MagrittrSetupData, cache=FALSE-----------------------------------------------------
myData <- data.frame(
  visitId = c("v1", "v2", "v3", "v4"),
  icd9 = c("39891", "39790", "41791", "4401"),
  poa = c("Y", "N", NA, "Y"),
  stringsAsFactors = FALSE)

## ----simplepoa--------------------------------------------------------------------------
myData %>% icd9FilterPoaYes()

## ----notnopoa---------------------------------------------------------------------------
myData %>% icd9FilterPoaNotNo()

## ----chainpoatocomorbid-----------------------------------------------------------------
myData %>%
  icd9FilterPoaNotNo() %>%
  icd9ComorbidAhrq(isShort = TRUE) %>%
  extract(1:9)

## ----customnamesinchain-----------------------------------------------------------------
myData %>% icd9FilterPoaYes(poaField = "poa") %>%
  icd9ComorbidAhrq(visitId = "visitId", isShort = TRUE) %>%
  extract(1:9)

## ----specifiedComorbidityMapChain-------------------------------------------------------
myData %>%
  icd9FilterPoaYes() %>%
  icd9Comorbid(icd9Mapping = quanElixComorbid,
               icd9Field = "icd9", visitId = "visitId",
               isShort = TRUE, isShortMapping = TRUE
  )  %>%
  extract(1:9)

## ----"arbitraryMapping"-----------------------------------------------------------------
icd9Chapters[1:5]

## ----"mapFromChapters", eval=TRUE-------------------------------------------------------
myMap <- icd9:::icd9ChaptersToMap(icd9Chapters[c(1,2:4)])

system.time(
  patientChapters <- icd9Comorbid(
    icd9df = patientData,
    isShort = TRUE,
    icd9Mapping = myMap)
)
# much faster the second time because the internal lookup is memoised:
system.time(
  patientChapters <- icd9Comorbid(
    icd9df = patientData,
    isShort = TRUE,
    icd9Mapping = myMap)
)

patientChapters


## ----elixvsquanelix---------------------------------------------------------------------
in_both <- intersect(elixComorbid$Pulmonary, quanElixComorbid$Pulmonary) # ~1800
only_in_elix <- setdiff(elixComorbid$Pulmonary, quanElixComorbid$Pulmonary) # none
only_in_quanElix <- setdiff(quanElixComorbid$Pulmonary, elixComorbid$Pulmonary) #about 50

in_both %>% icd9GetReal %>% icd9Explain
only_in_quanElix %>% icd9GetReal %>% icd9Explain

## ----realmapping------------------------------------------------------------------------
ahrqStrict <- lapply(ahrqComorbid, icd9GetReal)

# first five of the original:
str(ahrqComorbid[1:5])
# and we see the first five of the updated list is much shorter:
str(ahrqStrict[1:5])

## ----diffcomorbid-----------------------------------------------------------------------
# compare the first five comorbidities on these mappings
icd9DiffComorbid(ahrqComorbid[1:5], elixComorbid[1:5])

## ----ahrq, eval=FALSE-------------------------------------------------------------------
#  ahrqComorbid <- parseAhrqSas()

## ----ahrqcontents-----------------------------------------------------------------------
head(summary(ahrqComorbid))

## ----eval=FALSE-------------------------------------------------------------------------
#  parseAhrqSas()

## ----"exampleAhrqComorbid"--------------------------------------------------------------
ahrqComorbid[c("Obesity", "Depression")]

## ----"condenseMapping"------------------------------------------------------------------
lapply(ahrqComorbid[c("Obesity", "Depression")], icd9CondenseToMajorShort, onlyReal = TRUE)
ahrqComorbid[c("Obesity", "Depression")] %>% icd9ExplainShort(doCondense = FALSE)

## ----elix-------------------------------------------------------------------------------
names(elixComorbid)

## ----quanElix---------------------------------------------------------------------------
names(quanDeyoComorbid)
names(quanElixComorbid)

