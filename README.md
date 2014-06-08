
icd9
====

ICD-9 code manipulation, validation and comorbidity generation in R
-------------------------------------------------------------------

ICD-9 codes are not numbers, and great care is needed when matching individual codes and ranges of codes. It is easy to make mistakes, hence the need for this package. ICD-9 codes can be presented in 'short' 5 character format, or 'decimal' format, with a decimal place seperating the code into two groups. There are also codes beginning with V and E which have different validation rules. Zeros after a decimal place are meaningful, so numeric ICD-9 codes cannot be used in most cases. In addition, most clinical databases contain invalid codes, and even decimal and non-decimal format codes in different places. Given these problems, this package offers the following main features:

 * validation of ICD-9 codes (decimal or non-decimal "short" form), including V and E prefixes
 * conversion of ICD-9 codes between decimal and short forms
 * allocation of cases (e.g. patients or patient visits) to standard groups of co-morbidities according to ICD-9 coding and arbitrary mappings of ICD-9 codes to co-morbidities. AHRQ mapping is provided.
 * sorting of ICD-9 codes
 * generation of child codes from lists of higher-level ICD-9 codes, allowing testing of whether a specific ICD-9 code falls under a more general code category.

The initial release of this package contains ICD-9 to co-morbidity mappings from [AHRQ](http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp). Since the data is provided in SAS source code format, this package contains functions to parse this SAS source code and generate R data structures. This processing is limited to what is needed for this purpose, although may be generalizable and useful in other contexts.

More detail can be found in the accompanying vignette, the thorough documentation of package functions, and by examination of the source code including the comprehensive unit tests.

Suggestions for improvement and contributions of code are very welcome. Contributions will be credited. I ask that, if you use this package for published work, you cite this work citation(package='icd9')


Further work
------------
There are a number of outstanding opportunities and problems:
* ICD-10 is imminent. There exists an official ICD-9-CM to ICD-10-CM mapping from the CDC (in pdf format...)
* ICD-9 is not the same as ICD-9-CM (clinical modification). The original ICD coding system was intended for disease surveillance. I haven't identified yet whether ICD-9 is a simple subset of ICD-9-CM. It is somewhat irrelevant since ICD-9 codes are really only used for the last decade in the USA in the form of ICD-9-CM, and these have had annual updates until a couple of years ago (frozen in anticipation of ICD-10-CM).
* since ICD-9-CM codes have changed over time, a thorough implementation would account for the date of the code (or encoding?). In parallel, AHRQ have released annual updates of their allocation of ICD-9-CM codes to co-morbidities.
* performance is currently reasonable. There is a benchmark script which picks out some common use cases, and bottlenecks. It can allocate a million rows to a dozen comorbidities in less than ten seconds, on a modest workstation. Almost everything is vectorized, but some of the string processing could probably be done more quickly in C++. The test suite is fairly robust, so refactoring for performance shouldn't be too troublesome.
* test coverage is only 70% at function resolution (see my fork of [testthat](https://github.com/jackwasey/testthat) for how to do this.)
* additional testing against more real world ICD-9 data would be very helpful, and thereby generation of new tests.
* validation of codes is partially implemented. Some functions have optional validation. There should be trickle-down of validation, so that, if a set of codes is already validated by a function, then other functions called by the validating function need not do so. Processing Exxx.x codes adds a considerable burden, being even more different from xxx.xx codes than Vxx.xx codes in structure. Most cases can handle E codes: those very few which can't will throw errors, instead of silently returning incorrect data. These only relate to processing ranges of E codes, which is currently not needed.
 * the codes would best be presented in an object oriented model. This would be a significant refactoring but would have some advantages, most notable dispatch on long/short codes, self validation (which could be done once per instantiation, if S5 objects are used), and overall would be a more pleasing, although more complicated way of presenting the interface to a user. S3 classes might be preferable, making the base icd9 class 'short' form, then print.icd9 could interpret the code into a human-readable description, for example. Low-level compare method would then allow use of sort(). Not sure this has a lot of use cases, beyond interpreting the comorbidity definitions, which already works fine. Using an OO model would, however, place more burden on the package user. Perhaps the two approaches could co-exist and share a lot of code.
* the AHRQ comorbidities include various hypertension, renal failure and CHF subgroups. These can be processed further to update other fields, and generate a 'complications of hypertension' field. AHRQ provides code for this which still needs implementation in this package. In the meantime, the raw subgroups are provided as comorbidity fields.
* allow user to specify ranges, but include parent codes even if not all child codes are to be included in the range requested. E.g. "V10.09"" to "V10.11" would not normally include "V10.1"

