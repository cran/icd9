## ----setup, eval=TRUE, echo=FALSE----------------------------------------

set.seed = 1441

#oldOpt <- options(width = 20) # prevent stupid hbox overflows.
#on.exit(options(oldOpt))

suppressMessages(library(icd9, quietly = TRUE, warn.conflicts = FALSE))
suppressMessages(library("magrittr", quietly = TRUE, warn.conflicts = FALSE))

patientData <- data.frame(
  visitId = c(1000, 1000, 1000, 1001, 1001, 1002),
  icd9 = c("27801", "7208", "25001", "34400", "4011", "4011"),
  poa = factor(c("Y", "N", "Y", "N", "Y", "N"))
)

## ----begin, eval=TRUE----------------------------------------------------
patientData

## ----getcomorbidities, eval=TRUE-----------------------------------------
icd9Comorbidities(icd9df = patientData)[, 1:5]

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  icd9Comorbidities(icd9df = patientData,
#                    visitId = "visitId",
#                    icd9Field = "icd9",
#                    icd9Mapping = ahrqComorbid,
#                    validateMapping = FALSE,
#                    shortMapping = TRUE)

## ----conversion,eval=T---------------------------------------------------
icd9DecimalToShort(c("10.20", "100", "123.45"))
icd9ShortToDecimal(icd9DecimalToShort(c("10.20", "100", "123.45")))

# only a subset of short codes can suffer dropping of leading zeroes.
icd9DecimalToShort(c("1", "22", "22.44", "100"))

icd9ShortToDecimal(icd9DecimalToShort(c("1", "1.2", "123.45")))

icd9ShortToDecimal(icd9DecimalToShort(c("1", "2.2", "100")))

## ----validation,eval=T---------------------------------------------------
icd9ValidDecimal("V10.2")
icd9ValidShort(c("099.17", "-1"))
icd9ValidShort(c("1", "001", "100", "10023"))

## ----invalidint,eval=F---------------------------------------------------
#  icd9ValidShort(100) # throws an error

## ----ranges,eval=T-------------------------------------------------------
"10099" %i9s% "10101"
"V10" %i9d% "V10.02"
# "E987" %i9d% "E988.9"

## ----rangeanomaly--------------------------------------------------------
icd9ExpandRangeShort("V100", "V1002")

## ----explain,eval=T------------------------------------------------------
icd9ExplainDecimal("1.0")
icd9Explain("1.0", isShort = FALSE)
icd9ExplainDecimal("001.1")
icd9ExplainDecimal(icd9ShortToDecimal("0019"))
icd9ExplainShort("0019")
# named list(s) of codes
icd9ExplainDecimal(list(cholera = c("001", "001.0", "001.1", "001.9")))
# same using decimal codes without a list
icd9ExplainDecimal(c("001", "001.0", "001.1", "001.9"))

# 001/cholera doesn't itself have an explanation: TODO walk down children to get next level explanations.
icd9ExplainDecimal(list(cholera = "001", rheumatic_heart = "390"))



## ----noexplain,eval=T----------------------------------------------------
icd9ExplainDecimal("001.5")

## ----chaining1,eval=T----------------------------------------------------
c("001.1", "391") %>% icd9DecimalToShort %>% icd9ExplainShort

## ------------------------------------------------------------------------
cardiac <- unique(c(
  icd9CmDesc[
    grepl(
      pattern="(heart)|(cardiac)",
      x = icd9CmDesc[["descLong"]],
      ignore.case = TRUE
    ),
    "icd9"],
  icd9CmDesc[
    grepl(
      pattern="(heart)|(cardiac)",
      x = icd9CmDesc[["descShort"]],
      ignore.case = TRUE
    ),
    "icd9"]
))

## ----eval=TRUE,echo=TRUE-------------------------------------------------
cardiac %>% icd9ExplainShort %>% extract(2) %>% head(10)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
quanDeyoComorbid[["Dementia"]] %>%
  icd9ExplainShort() %>%
  extract(c("ICD-9","Description"))

# use a range with more than two hundred ICD-9 codes

length("390" %i9d% "392.1")
"390" %i9d% "392.1" %>% icd9DecimalToShort() %>% icd9ExplainShort()
"390" %i9d% "392.1" %>% icd9ExplainDecimal()


## ----ahrq,eval=F---------------------------------------------------------
#  ahrqComorbid <- parseAhrqSas()

## ----ahrqcontents--------------------------------------------------------
head(summary(ahrqComorbid))

## ----echo=FALSE--------------------------------------
options(width = 55)

## ----echo=TRUE,eval=FALSE----------------------------
#  parseAhrqSas()

## ----example_ahrqComorbid----------------------------
ahrqComorbid[c("OBESE", "DEPRESS")]
lapply(ahrqComorbid[c("OBESE", "DEPRESS")], icd9ChildrenShort)

## ----elix--------------------------------------------
names(elixhauserComorbid)

## ----quanElix----------------------------------------
names(quanDeyoComorbid)
names(quanElixhauserComorbid)

