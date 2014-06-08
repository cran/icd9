## ----setup, eval=TRUE, echo=FALSE----------------------------------------

#oldOpt <- options(width = 20) # prevent stupid hbox overflows.
#on.exit(options(oldOpt))

suppressMessages(library(icd9, quietly = TRUE, warn.conflicts = FALSE))

patientData <- data.frame(
  visitId = c(1000,1000,1000,1001,1001,1002),
  icd9 = c("27801", "7208", "25001", "34400", "4011", "4011"),
  poa = factor(c("Y","N","Y","N","Y","N"))
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

## ----validation,eval=T---------------------------------------------------
icd9ValidDecimal("V10.2")
icd9ValidShort(c("099.17", "-1"))

## ----invalidint,eval=F---------------------------------------------------
#  icd9ValidShort(100) # throws an error

## ----explain,eval=T------------------------------------------------------
icd9Explain("001")
icd9Explain("001.1")
icd9Explain(list(cholera = c("001", "001.0", "001.1", "001.9")))

## ----noexplain,eval=T----------------------------------------------------
icd9Explain("001.5")

## ----ahrq,eval=F---------------------------------------------------------
#  ahrqComorbid <- parseAhrqSas(save = F)

## ----ahrqcontents--------------------------------------------------------
head(summary(ahrqComorbid))

## ----echo=FALSE--------------------------------------
options(width = 55)

## ----echo=TRUE,eval=FALSE----------------------------
#  parseAhrqSas()

## ----example_ahrqComorbid----------------------------
ahrqComorbid[c("OBESE", "DEPRESS")]

