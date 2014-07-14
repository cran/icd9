## ----setup, eval=TRUE, echo=FALSE, cache=FALSE------------------------------------------
suppressMessages(library("knitr", quietly = TRUE, warn.conflicts = FALSE))
suppressMessages(library(icd9, quietly = TRUE, warn.conflicts = FALSE))
suppressMessages(library("magrittr", quietly = TRUE, warn.conflicts = FALSE))

## font size options from the highlight package: c("normalsize", "tiny", "scriptsize", "footnotesize", "small", "large", "Large", "LARGE", "huge", "Huge")
opts_chunk$set(size="scriptsize")
options(width = 90)

patientData <- data.frame(
  visitId = c(1000, 1000, 1000, 1000, 1001, 1001, 1002),
  icd9 = c("402010", "27801", "7208", "25001", "34400", "4011", "4011"),
  poa = factor(c("Y", "Y", "N", "Y", "N", "Y", "N"))
)

## ----begin, cache=TRUE------------------------------------------------------------------
patientData

## ----getcomorbidities, cache=TRUE-------------------------------------------------------
icd9ComorbiditiesAhrq(icd9df = patientData)[, 1:10]

## ----getcomorbidities1a, cache=TRUE-----------------------------------------------------
icd9ComorbiditiesQuanDeyo(patientData)[, 1:10]

## ----getcomorbidities2, cache=TRUE------------------------------------------------------
patientData %>% icd9FilterPoaYes() %>%
  icd9ComorbiditiesAhrq(isShort = TRUE) %>% extract(1:5)

## ----"comorbidUsingDefaultOpts",eval=FALSE, cache=TRUE----------------------------------
#  icd9Comorbidities(icd9df = icdFilterPoaYes(patientData),
#                    visitId = "visitId",
#                    icd9Field = "icd9",
#                    icd9Mapping = ahrqComorbid,
#                    validateMapping = FALSE,
#                    shortMapping = TRUE)[1:5]

## ----"conversionSimple", cache=TRUE-----------------------------------------------------
icd9DecimalToShort(c("10.20", "100", "123.45"))
icd9ShortToDecimal(icd9DecimalToShort(c("10.20", "100", "123.45")))

## ----"conversionNeedZero", cache=TRUE---------------------------------------------------
icd9DecimalToShort(c("1", "22", "22.44", "100"))
icd9ShortToDecimal(icd9DecimalToShort(c("1", "1.2", "123.45")))
icd9ShortToDecimal(icd9DecimalToShort(c("1", "2.2", "100")))

## ----validation, cache=TRUE-------------------------------------------------------------
icd9ValidDecimal("V10.2")
icd9ValidShort(c("099.17", "-1"))
icd9ValidShort(c("1", "001", "100", "10023"))

## ----invalidint, eval=FALSE-------------------------------------------------------------
#  icd9ValidShort(100) # throws an error

## ----ranges, cache=TRUE-----------------------------------------------------------------
"10099" %i9s% "10101"
"V10" %i9d% "V10.02"
# "E987" %i9d% "E988.9"

## ----rangeanomaly, cache=TRUE-----------------------------------------------------------
icd9ExpandRangeShort("V100", "V1002")

## ----"childrenReal", cache=TRUE---------------------------------------------------------
icd9ChildrenShort("391", onlyReal = TRUE)

## ----"childrenAll", cache=TRUE----------------------------------------------------------
icd9ChildrenShort("391", onlyReal = FALSE)[1:10]

## ----explainSimple, cache=TRUE----------------------------------------------------------
icd9ExplainDecimal("1.0")
icd9ExplainShort("0019")

## ----explainComplex, cache=TRUE---------------------------------------------------------
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

## ----explainArb, cache=TRUE-------------------------------------------------------------
icd9ExplainDecimal(list(cholera = c("001", "001.0", "001.1", "001.9")))
# same using decimal codes without a list
icd9ExplainDecimal(c("001", "001.0", "001.1", "001.9"))

## ----echo=FALSE,eval=FALSE--------------------------------------------------------------
#  # 001/cholera doesn't itself have an explanation: TODO walk down children to get next level explanations.
#  icd9ExplainDecimal(list(cholera = "001", rheumatic_heart = "390"))

## ----noexplain, cache=TRUE--------------------------------------------------------------
icd9ExplainDecimal("001.5")

## ----chaining1, cache=TRUE--------------------------------------------------------------
c("001.1", "391") %>% icd9DecimalToShort %>% icd9ExplainShort

## ----cardiacgrep, cache=TRUE------------------------------------------------------------
cardiac <- unique(c(
  icd9Hierarchy[
    grepl(
      pattern="(heart)|(cardiac)",
      x = icd9Hierarchy[["descLong"]],
      ignore.case = TRUE
    ),
    "icd9"],
  icd9Hierarchy[
    grepl(
      pattern="(heart)|(cardiac)",
      x = icd9Hierarchy[["descShort"]],
      ignore.case = TRUE
    ),
    "icd9"]
))

## ----cardiacChainExplainExample, cache=TRUE---------------------------------------------
cardiac %>% icd9ExplainShort %>% extract(2) %>% head(10)

## ----ExampleQDDementia, cache=TRUE------------------------------------------------------
quanDeyoComorbid[["Dementia"]] %>%
  icd9ExplainShort() %>%
  extract(c("ICD-9","Description"))

## ----ShowRangeOperator, cache=TRUE------------------------------------------------------
length("390" %i9d% "392.1")

"390" %i9d% "392.1" %>%
  icd9DecimalToShort() %>%
  icd9ExplainShort()

"390" %i9d% "392.1" %>% icd9ExplainDecimal()

## ----ShowPoaChoices, cache=TRUE---------------------------------------------------------
icd9PoaChoices

## ----MagrittrSetupData, cache=FALSE-----------------------------------------------------
myData <- data.frame(
  visitId = c("v1", "v2", "v3", "v4"),
  icd9 = c("39891", "39790", "41791", "4401"),
  poa = c("Y", "N", NA, "Y"),
  stringsAsFactors = FALSE)

## ----simplepoa, cache=TRUE--------------------------------------------------------------
myData %>% icd9FilterPoaYes()

## ----notnopoa, cache=TRUE---------------------------------------------------------------
myData %>% icd9FilterPoaNotNo()

## ----chainpoatocomorbid, cache=TRUE-----------------------------------------------------
myData %>%
  icd9FilterPoaNotNo() %>%
  icd9ComorbiditiesAhrq(isShort = TRUE) %>%
  extract(1:9)

## ----customnamesinchain, cache=TRUE-----------------------------------------------------
myData %>% icd9FilterPoaYes(poaField="poa") %>%
  icd9ComorbiditiesAhrq(visitId = "visitId") %>%
  extract(1:9)

## ----specifiedComorbidityMapChain, cache=TRUE-------------------------------------------
myData %>%
  icd9FilterPoaYes() %>%
  icd9Comorbidities(
    icd9Field = "icd9", visitId = "visitId",
    icd9Mapping = quanElixhauserComorbid,
    validateMapping = TRUE, isShortMapping = TRUE
  )  %>%
  extract(1:9)

## ----"arbitraryMapping"-----------------------------------------------------------------
icd9Chapters[1:5]

## ----"mapFromChapters", eval=TRUE, cache=TRUE-------------------------------------------
myMap <- icd9:::icd9ChaptersToMap(icd9Chapters[c(1,2:4)])

system.time(
  patientChapters <- icd9Comorbidities(
    icd9df = patientData, icd9Mapping = myMap)
)
# much faster the second time because the internal lookup is memoised:
system.time(
  patientChapters <- icd9Comorbidities(
    icd9df = patientData, icd9Mapping = myMap)
)

patientChapters


## ----ahrq, eval=FALSE-------------------------------------------------------------------
#  ahrqComorbid <- parseAhrqSas()

## ----ahrqcontents, cache=TRUE-----------------------------------------------------------
head(summary(ahrqComorbid))

## ----eval=FALSE, cache=TRUE-------------------------------------------------------------
#  parseAhrqSas()

## ----"exampleAhrqComorbid", cache=TRUE--------------------------------------------------
ahrqComorbid[c("Obesity", "Depression")]

## ----"condenseMapping", cache=TRUE------------------------------------------------------
lapply(ahrqComorbid[c("Obesity", "Depression")], icd9CondenseToMajor, onlyReal = T)
ahrqComorbid[c("Obesity", "Depression")] %>% icd9ExplainShort(doCondense = FALSE)

## ----elix, cache=TRUE-------------------------------------------------------------------
names(elixhauserComorbid)

## ----quanElix, cache=TRUE---------------------------------------------------------------
names(quanDeyoComorbid)
names(quanElixhauserComorbid)

