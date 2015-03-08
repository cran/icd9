// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/icd9.h"
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// icd9ComorbidShortCpp
SEXP icd9ComorbidShortCpp(const SEXP& icd9df, const List& icd9Mapping, const std::string visitId, const std::string icd9Field, const int threads, const int chunkSize, const int ompChunkSize, bool aggregate);
static SEXP icd9_icd9ComorbidShortCpp_try(SEXP icd9dfSEXP, SEXP icd9MappingSEXP, SEXP visitIdSEXP, SEXP icd9FieldSEXP, SEXP threadsSEXP, SEXP chunkSizeSEXP, SEXP ompChunkSizeSEXP, SEXP aggregateSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const SEXP& >::type icd9df(icd9dfSEXP);
    Rcpp::traits::input_parameter< const List& >::type icd9Mapping(icd9MappingSEXP);
    Rcpp::traits::input_parameter< const std::string >::type visitId(visitIdSEXP);
    Rcpp::traits::input_parameter< const std::string >::type icd9Field(icd9FieldSEXP);
    Rcpp::traits::input_parameter< const int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< const int >::type chunkSize(chunkSizeSEXP);
    Rcpp::traits::input_parameter< const int >::type ompChunkSize(ompChunkSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type aggregate(aggregateSEXP);
    __result = Rcpp::wrap(icd9ComorbidShortCpp(icd9df, icd9Mapping, visitId, icd9Field, threads, chunkSize, ompChunkSize, aggregate));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9ComorbidShortCpp(SEXP icd9dfSEXP, SEXP icd9MappingSEXP, SEXP visitIdSEXP, SEXP icd9FieldSEXP, SEXP threadsSEXP, SEXP chunkSizeSEXP, SEXP ompChunkSizeSEXP, SEXP aggregateSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9ComorbidShortCpp_try(icd9dfSEXP, icd9MappingSEXP, visitIdSEXP, icd9FieldSEXP, threadsSEXP, chunkSizeSEXP, ompChunkSizeSEXP, aggregateSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9MajMinToCode
CharacterVector icd9MajMinToCode(const CharacterVector major, const CharacterVector minor, bool isShort);
static SEXP icd9_icd9MajMinToCode_try(SEXP majorSEXP, SEXP minorSEXP, SEXP isShortSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const CharacterVector >::type major(majorSEXP);
    Rcpp::traits::input_parameter< const CharacterVector >::type minor(minorSEXP);
    Rcpp::traits::input_parameter< bool >::type isShort(isShortSEXP);
    __result = Rcpp::wrap(icd9MajMinToCode(major, minor, isShort));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9MajMinToCode(SEXP majorSEXP, SEXP minorSEXP, SEXP isShortSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9MajMinToCode_try(majorSEXP, minorSEXP, isShortSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9MajMinToShort
CharacterVector icd9MajMinToShort(const CharacterVector major, const CharacterVector minor);
static SEXP icd9_icd9MajMinToShort_try(SEXP majorSEXP, SEXP minorSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const CharacterVector >::type major(majorSEXP);
    Rcpp::traits::input_parameter< const CharacterVector >::type minor(minorSEXP);
    __result = Rcpp::wrap(icd9MajMinToShort(major, minor));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9MajMinToShort(SEXP majorSEXP, SEXP minorSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9MajMinToShort_try(majorSEXP, minorSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9MajMinToDecimal
CharacterVector icd9MajMinToDecimal(const CharacterVector major, const CharacterVector minor);
static SEXP icd9_icd9MajMinToDecimal_try(SEXP majorSEXP, SEXP minorSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const CharacterVector >::type major(majorSEXP);
    Rcpp::traits::input_parameter< const CharacterVector >::type minor(minorSEXP);
    __result = Rcpp::wrap(icd9MajMinToDecimal(major, minor));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9MajMinToDecimal(SEXP majorSEXP, SEXP minorSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9MajMinToDecimal_try(majorSEXP, minorSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9PartsToShort
CharacterVector icd9PartsToShort(const List parts);
static SEXP icd9_icd9PartsToShort_try(SEXP partsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const List >::type parts(partsSEXP);
    __result = Rcpp::wrap(icd9PartsToShort(parts));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9PartsToShort(SEXP partsSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9PartsToShort_try(partsSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9PartsToDecimal
CharacterVector icd9PartsToDecimal(const List parts);
static SEXP icd9_icd9PartsToDecimal_try(SEXP partsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const List >::type parts(partsSEXP);
    __result = Rcpp::wrap(icd9PartsToDecimal(parts));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9PartsToDecimal(SEXP partsSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9PartsToDecimal_try(partsSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9MajMinToParts
List icd9MajMinToParts(const CharacterVector major, const CharacterVector minor);
static SEXP icd9_icd9MajMinToParts_try(SEXP majorSEXP, SEXP minorSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const CharacterVector >::type major(majorSEXP);
    Rcpp::traits::input_parameter< const CharacterVector >::type minor(minorSEXP);
    __result = Rcpp::wrap(icd9MajMinToParts(major, minor));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9MajMinToParts(SEXP majorSEXP, SEXP minorSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9MajMinToParts_try(majorSEXP, minorSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9MajMinToParts_list
List icd9MajMinToParts_list(const CharacterVector major, const CharacterVector minor);
static SEXP icd9_icd9MajMinToParts_list_try(SEXP majorSEXP, SEXP minorSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const CharacterVector >::type major(majorSEXP);
    Rcpp::traits::input_parameter< const CharacterVector >::type minor(minorSEXP);
    __result = Rcpp::wrap(icd9MajMinToParts_list(major, minor));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9MajMinToParts_list(SEXP majorSEXP, SEXP minorSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9MajMinToParts_list_try(majorSEXP, minorSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9ShortToParts
List icd9ShortToParts(const CharacterVector icd9Short, const String minorEmpty);
static SEXP icd9_icd9ShortToParts_try(SEXP icd9ShortSEXP, SEXP minorEmptySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const CharacterVector >::type icd9Short(icd9ShortSEXP);
    Rcpp::traits::input_parameter< const String >::type minorEmpty(minorEmptySEXP);
    __result = Rcpp::wrap(icd9ShortToParts(icd9Short, minorEmpty));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9ShortToParts(SEXP icd9ShortSEXP, SEXP minorEmptySEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9ShortToParts_try(icd9ShortSEXP, minorEmptySEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9DecimalToParts
List icd9DecimalToParts(const CharacterVector icd9Decimal, const String minorEmpty);
static SEXP icd9_icd9DecimalToParts_try(SEXP icd9DecimalSEXP, SEXP minorEmptySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const CharacterVector >::type icd9Decimal(icd9DecimalSEXP);
    Rcpp::traits::input_parameter< const String >::type minorEmpty(minorEmptySEXP);
    __result = Rcpp::wrap(icd9DecimalToParts(icd9Decimal, minorEmpty));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9DecimalToParts(SEXP icd9DecimalSEXP, SEXP minorEmptySEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9DecimalToParts_try(icd9DecimalSEXP, minorEmptySEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9ShortToDecimal
CharacterVector icd9ShortToDecimal(const CharacterVector icd9Short);
static SEXP icd9_icd9ShortToDecimal_try(SEXP icd9ShortSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const CharacterVector >::type icd9Short(icd9ShortSEXP);
    __result = Rcpp::wrap(icd9ShortToDecimal(icd9Short));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9ShortToDecimal(SEXP icd9ShortSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9ShortToDecimal_try(icd9ShortSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9DecimalToShort
CharacterVector icd9DecimalToShort(const CharacterVector icd9Decimal);
static SEXP icd9_icd9DecimalToShort_try(SEXP icd9DecimalSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const CharacterVector >::type icd9Decimal(icd9DecimalSEXP);
    __result = Rcpp::wrap(icd9DecimalToShort(icd9Decimal));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9DecimalToShort(SEXP icd9DecimalSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9DecimalToShort_try(icd9DecimalSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9GetMajor
CharacterVector icd9GetMajor(const CharacterVector icd9, const bool isShort);
static SEXP icd9_icd9GetMajor_try(SEXP icd9SEXP, SEXP isShortSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const CharacterVector >::type icd9(icd9SEXP);
    Rcpp::traits::input_parameter< const bool >::type isShort(isShortSEXP);
    __result = Rcpp::wrap(icd9GetMajor(icd9, isShort));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9GetMajor(SEXP icd9SEXP, SEXP isShortSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9GetMajor_try(icd9SEXP, isShortSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9IsV
std::vector<bool> icd9IsV(const std::vector<std::string>& icd9);
static SEXP icd9_icd9IsV_try(SEXP icd9SEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const std::vector<std::string>& >::type icd9(icd9SEXP);
    __result = Rcpp::wrap(icd9IsV(icd9));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9IsV(SEXP icd9SEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9IsV_try(icd9SEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9IsE
std::vector<bool> icd9IsE(const std::vector<std::string>& icd9);
static SEXP icd9_icd9IsE_try(SEXP icd9SEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const std::vector<std::string>& >::type icd9(icd9SEXP);
    __result = Rcpp::wrap(icd9IsE(icd9));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9IsE(SEXP icd9SEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9IsE_try(icd9SEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9IsN
std::vector<bool> icd9IsN(const std::vector<std::string>& icd9);
static SEXP icd9_icd9IsN_try(SEXP icd9SEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const std::vector<std::string>& >::type icd9(icd9SEXP);
    __result = Rcpp::wrap(icd9IsN(icd9));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9IsN(SEXP icd9SEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9IsN_try(icd9SEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9LongToWideCpp
CharacterVector icd9LongToWideCpp(const SEXP& icd9df, const std::string visitId, const std::string icd9Field, bool aggregate);
static SEXP icd9_icd9LongToWideCpp_try(SEXP icd9dfSEXP, SEXP visitIdSEXP, SEXP icd9FieldSEXP, SEXP aggregateSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const SEXP& >::type icd9df(icd9dfSEXP);
    Rcpp::traits::input_parameter< const std::string >::type visitId(visitIdSEXP);
    Rcpp::traits::input_parameter< const std::string >::type icd9Field(icd9FieldSEXP);
    Rcpp::traits::input_parameter< bool >::type aggregate(aggregateSEXP);
    __result = Rcpp::wrap(icd9LongToWideCpp(icd9df, visitId, icd9Field, aggregate));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9LongToWideCpp(SEXP icd9dfSEXP, SEXP visitIdSEXP, SEXP icd9FieldSEXP, SEXP aggregateSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9LongToWideCpp_try(icd9dfSEXP, visitIdSEXP, icd9FieldSEXP, aggregateSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9AddLeadingZeroesMajorSingle
String icd9AddLeadingZeroesMajorSingle(String major);
static SEXP icd9_icd9AddLeadingZeroesMajorSingle_try(SEXP majorSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< String >::type major(majorSEXP);
    __result = Rcpp::wrap(icd9AddLeadingZeroesMajorSingle(major));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9AddLeadingZeroesMajorSingle(SEXP majorSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9AddLeadingZeroesMajorSingle_try(majorSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9AddLeadingZeroesMajor
CharacterVector icd9AddLeadingZeroesMajor(CharacterVector major);
static SEXP icd9_icd9AddLeadingZeroesMajor_try(SEXP majorSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< CharacterVector >::type major(majorSEXP);
    __result = Rcpp::wrap(icd9AddLeadingZeroesMajor(major));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9AddLeadingZeroesMajor(SEXP majorSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9AddLeadingZeroesMajor_try(majorSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9AddLeadingZeroesShort
CharacterVector icd9AddLeadingZeroesShort(CharacterVector icd9Short);
static SEXP icd9_icd9AddLeadingZeroesShort_try(SEXP icd9ShortSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< CharacterVector >::type icd9Short(icd9ShortSEXP);
    __result = Rcpp::wrap(icd9AddLeadingZeroesShort(icd9Short));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9AddLeadingZeroesShort(SEXP icd9ShortSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9AddLeadingZeroesShort_try(icd9ShortSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9AddLeadingZeroesDecimal
CharacterVector icd9AddLeadingZeroesDecimal(CharacterVector icd9Decimal);
static SEXP icd9_icd9AddLeadingZeroesDecimal_try(SEXP icd9DecimalSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< CharacterVector >::type icd9Decimal(icd9DecimalSEXP);
    __result = Rcpp::wrap(icd9AddLeadingZeroesDecimal(icd9Decimal));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9AddLeadingZeroesDecimal(SEXP icd9DecimalSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9AddLeadingZeroesDecimal_try(icd9DecimalSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9AddLeadingZeroes
CharacterVector icd9AddLeadingZeroes(CharacterVector icd9, bool isShort);
static SEXP icd9_icd9AddLeadingZeroes_try(SEXP icd9SEXP, SEXP isShortSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< CharacterVector >::type icd9(icd9SEXP);
    Rcpp::traits::input_parameter< bool >::type isShort(isShortSEXP);
    __result = Rcpp::wrap(icd9AddLeadingZeroes(icd9, isShort));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9AddLeadingZeroes(SEXP icd9SEXP, SEXP isShortSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9AddLeadingZeroes_try(icd9SEXP, isShortSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9ExpandMinor
CharacterVector icd9ExpandMinor(std::string minor, bool isE);
static SEXP icd9_icd9ExpandMinor_try(SEXP minorSEXP, SEXP isESEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< std::string >::type minor(minorSEXP);
    Rcpp::traits::input_parameter< bool >::type isE(isESEXP);
    __result = Rcpp::wrap(icd9ExpandMinor(minor, isE));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9ExpandMinor(SEXP minorSEXP, SEXP isESEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9ExpandMinor_try(minorSEXP, isESEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9ChildrenShortCpp
CharacterVector icd9ChildrenShortCpp(CharacterVector icd9Short, bool onlyReal);
static SEXP icd9_icd9ChildrenShortCpp_try(SEXP icd9ShortSEXP, SEXP onlyRealSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< CharacterVector >::type icd9Short(icd9ShortSEXP);
    Rcpp::traits::input_parameter< bool >::type onlyReal(onlyRealSEXP);
    __result = Rcpp::wrap(icd9ChildrenShortCpp(icd9Short, onlyReal));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9ChildrenShortCpp(SEXP icd9ShortSEXP, SEXP onlyRealSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9ChildrenShortCpp_try(icd9ShortSEXP, onlyRealSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9ChildrenDecimalCpp
CharacterVector icd9ChildrenDecimalCpp(CharacterVector icd9Decimal, bool onlyReal);
static SEXP icd9_icd9ChildrenDecimalCpp_try(SEXP icd9DecimalSEXP, SEXP onlyRealSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< CharacterVector >::type icd9Decimal(icd9DecimalSEXP);
    Rcpp::traits::input_parameter< bool >::type onlyReal(onlyRealSEXP);
    __result = Rcpp::wrap(icd9ChildrenDecimalCpp(icd9Decimal, onlyReal));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9ChildrenDecimalCpp(SEXP icd9DecimalSEXP, SEXP onlyRealSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9ChildrenDecimalCpp_try(icd9DecimalSEXP, onlyRealSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9ChildrenCpp
CharacterVector icd9ChildrenCpp(CharacterVector icd9, bool isShort, bool onlyReal);
static SEXP icd9_icd9ChildrenCpp_try(SEXP icd9SEXP, SEXP isShortSEXP, SEXP onlyRealSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< CharacterVector >::type icd9(icd9SEXP);
    Rcpp::traits::input_parameter< bool >::type isShort(isShortSEXP);
    Rcpp::traits::input_parameter< bool >::type onlyReal(onlyRealSEXP);
    __result = Rcpp::wrap(icd9ChildrenCpp(icd9, isShort, onlyReal));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9ChildrenCpp(SEXP icd9SEXP, SEXP isShortSEXP, SEXP onlyRealSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9ChildrenCpp_try(icd9SEXP, isShortSEXP, onlyRealSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// icd9InReferenceCode
LogicalVector icd9InReferenceCode(CharacterVector icd9, CharacterVector icd9Reference, bool isShort, bool isShortReference);
static SEXP icd9_icd9InReferenceCode_try(SEXP icd9SEXP, SEXP icd9ReferenceSEXP, SEXP isShortSEXP, SEXP isShortReferenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< CharacterVector >::type icd9(icd9SEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type icd9Reference(icd9ReferenceSEXP);
    Rcpp::traits::input_parameter< bool >::type isShort(isShortSEXP);
    Rcpp::traits::input_parameter< bool >::type isShortReference(isShortReferenceSEXP);
    __result = Rcpp::wrap(icd9InReferenceCode(icd9, icd9Reference, isShort, isShortReference));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_icd9InReferenceCode(SEXP icd9SEXP, SEXP icd9ReferenceSEXP, SEXP isShortSEXP, SEXP isShortReferenceSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_icd9InReferenceCode_try(icd9SEXP, icd9ReferenceSEXP, isShortSEXP, isShortReferenceSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// strimCpp
std::string strimCpp(std::string& s);
static SEXP icd9_strimCpp_try(SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< std::string& >::type s(sSEXP);
    __result = Rcpp::wrap(strimCpp(s));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_strimCpp(SEXP sSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_strimCpp_try(sSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// trimCpp
std::vector<std::string> trimCpp(std::vector<std::string>& sv);
static SEXP icd9_trimCpp_try(SEXP svSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< std::vector<std::string>& >::type sv(svSEXP);
    __result = Rcpp::wrap(trimCpp(sv));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_trimCpp(SEXP svSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_trimCpp_try(svSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// assertFactorOrCharacter
bool assertFactorOrCharacter(SEXP x);
static SEXP icd9_assertFactorOrCharacter_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    __result = Rcpp::wrap(assertFactorOrCharacter(x));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_assertFactorOrCharacter(SEXP xSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_assertFactorOrCharacter_try(xSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// getOmpCores
int getOmpCores();
static SEXP icd9_getOmpCores_try() {
BEGIN_RCPP
    Rcpp::RObject __result;
    __result = Rcpp::wrap(getOmpCores());
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP icd9_getOmpCores() {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(icd9_getOmpCores_try());
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}

// validate (ensure exported C++ functions exist before calling them)
static int icd9_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("SEXP(*icd9ComorbidShortCpp)(const SEXP&,const List&,const std::string,const std::string,const int,const int,const int,bool)");
        signatures.insert("CharacterVector(*icd9MajMinToCode)(const CharacterVector,const CharacterVector,bool)");
        signatures.insert("CharacterVector(*icd9MajMinToShort)(const CharacterVector,const CharacterVector)");
        signatures.insert("CharacterVector(*icd9MajMinToDecimal)(const CharacterVector,const CharacterVector)");
        signatures.insert("CharacterVector(*icd9PartsToShort)(const List)");
        signatures.insert("CharacterVector(*icd9PartsToDecimal)(const List)");
        signatures.insert("List(*icd9MajMinToParts)(const CharacterVector,const CharacterVector)");
        signatures.insert("List(*icd9MajMinToParts_list)(const CharacterVector,const CharacterVector)");
        signatures.insert("List(*icd9ShortToParts)(const CharacterVector,const String)");
        signatures.insert("List(*icd9DecimalToParts)(const CharacterVector,const String)");
        signatures.insert("CharacterVector(*icd9ShortToDecimal)(const CharacterVector)");
        signatures.insert("CharacterVector(*icd9DecimalToShort)(const CharacterVector)");
        signatures.insert("CharacterVector(*icd9GetMajor)(const CharacterVector,const bool)");
        signatures.insert("std::vector<bool>(*icd9IsV)(const std::vector<std::string>&)");
        signatures.insert("std::vector<bool>(*icd9IsE)(const std::vector<std::string>&)");
        signatures.insert("std::vector<bool>(*icd9IsN)(const std::vector<std::string>&)");
        signatures.insert("CharacterVector(*icd9LongToWideCpp)(const SEXP&,const std::string,const std::string,bool)");
        signatures.insert("String(*icd9AddLeadingZeroesMajorSingle)(String)");
        signatures.insert("CharacterVector(*icd9AddLeadingZeroesMajor)(CharacterVector)");
        signatures.insert("CharacterVector(*icd9AddLeadingZeroesShort)(CharacterVector)");
        signatures.insert("CharacterVector(*icd9AddLeadingZeroesDecimal)(CharacterVector)");
        signatures.insert("CharacterVector(*icd9AddLeadingZeroes)(CharacterVector,bool)");
        signatures.insert("CharacterVector(*icd9ExpandMinor)(std::string,bool)");
        signatures.insert("CharacterVector(*icd9ChildrenShortCpp)(CharacterVector,bool)");
        signatures.insert("CharacterVector(*icd9ChildrenDecimalCpp)(CharacterVector,bool)");
        signatures.insert("CharacterVector(*icd9ChildrenCpp)(CharacterVector,bool,bool)");
        signatures.insert("LogicalVector(*icd9InReferenceCode)(CharacterVector,CharacterVector,bool,bool)");
        signatures.insert("std::string(*strimCpp)(std::string&)");
        signatures.insert("std::vector<std::string>(*trimCpp)(std::vector<std::string>&)");
        signatures.insert("bool(*assertFactorOrCharacter)(SEXP)");
        signatures.insert("int(*getOmpCores)()");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP icd9_RcppExport_registerCCallable() { 
    R_RegisterCCallable("icd9", "icd9_icd9ComorbidShortCpp", (DL_FUNC)icd9_icd9ComorbidShortCpp_try);
    R_RegisterCCallable("icd9", "icd9_icd9MajMinToCode", (DL_FUNC)icd9_icd9MajMinToCode_try);
    R_RegisterCCallable("icd9", "icd9_icd9MajMinToShort", (DL_FUNC)icd9_icd9MajMinToShort_try);
    R_RegisterCCallable("icd9", "icd9_icd9MajMinToDecimal", (DL_FUNC)icd9_icd9MajMinToDecimal_try);
    R_RegisterCCallable("icd9", "icd9_icd9PartsToShort", (DL_FUNC)icd9_icd9PartsToShort_try);
    R_RegisterCCallable("icd9", "icd9_icd9PartsToDecimal", (DL_FUNC)icd9_icd9PartsToDecimal_try);
    R_RegisterCCallable("icd9", "icd9_icd9MajMinToParts", (DL_FUNC)icd9_icd9MajMinToParts_try);
    R_RegisterCCallable("icd9", "icd9_icd9MajMinToParts_list", (DL_FUNC)icd9_icd9MajMinToParts_list_try);
    R_RegisterCCallable("icd9", "icd9_icd9ShortToParts", (DL_FUNC)icd9_icd9ShortToParts_try);
    R_RegisterCCallable("icd9", "icd9_icd9DecimalToParts", (DL_FUNC)icd9_icd9DecimalToParts_try);
    R_RegisterCCallable("icd9", "icd9_icd9ShortToDecimal", (DL_FUNC)icd9_icd9ShortToDecimal_try);
    R_RegisterCCallable("icd9", "icd9_icd9DecimalToShort", (DL_FUNC)icd9_icd9DecimalToShort_try);
    R_RegisterCCallable("icd9", "icd9_icd9GetMajor", (DL_FUNC)icd9_icd9GetMajor_try);
    R_RegisterCCallable("icd9", "icd9_icd9IsV", (DL_FUNC)icd9_icd9IsV_try);
    R_RegisterCCallable("icd9", "icd9_icd9IsE", (DL_FUNC)icd9_icd9IsE_try);
    R_RegisterCCallable("icd9", "icd9_icd9IsN", (DL_FUNC)icd9_icd9IsN_try);
    R_RegisterCCallable("icd9", "icd9_icd9LongToWideCpp", (DL_FUNC)icd9_icd9LongToWideCpp_try);
    R_RegisterCCallable("icd9", "icd9_icd9AddLeadingZeroesMajorSingle", (DL_FUNC)icd9_icd9AddLeadingZeroesMajorSingle_try);
    R_RegisterCCallable("icd9", "icd9_icd9AddLeadingZeroesMajor", (DL_FUNC)icd9_icd9AddLeadingZeroesMajor_try);
    R_RegisterCCallable("icd9", "icd9_icd9AddLeadingZeroesShort", (DL_FUNC)icd9_icd9AddLeadingZeroesShort_try);
    R_RegisterCCallable("icd9", "icd9_icd9AddLeadingZeroesDecimal", (DL_FUNC)icd9_icd9AddLeadingZeroesDecimal_try);
    R_RegisterCCallable("icd9", "icd9_icd9AddLeadingZeroes", (DL_FUNC)icd9_icd9AddLeadingZeroes_try);
    R_RegisterCCallable("icd9", "icd9_icd9ExpandMinor", (DL_FUNC)icd9_icd9ExpandMinor_try);
    R_RegisterCCallable("icd9", "icd9_icd9ChildrenShortCpp", (DL_FUNC)icd9_icd9ChildrenShortCpp_try);
    R_RegisterCCallable("icd9", "icd9_icd9ChildrenDecimalCpp", (DL_FUNC)icd9_icd9ChildrenDecimalCpp_try);
    R_RegisterCCallable("icd9", "icd9_icd9ChildrenCpp", (DL_FUNC)icd9_icd9ChildrenCpp_try);
    R_RegisterCCallable("icd9", "icd9_icd9InReferenceCode", (DL_FUNC)icd9_icd9InReferenceCode_try);
    R_RegisterCCallable("icd9", "icd9_strimCpp", (DL_FUNC)icd9_strimCpp_try);
    R_RegisterCCallable("icd9", "icd9_trimCpp", (DL_FUNC)icd9_trimCpp_try);
    R_RegisterCCallable("icd9", "icd9_assertFactorOrCharacter", (DL_FUNC)icd9_assertFactorOrCharacter_try);
    R_RegisterCCallable("icd9", "icd9_getOmpCores", (DL_FUNC)icd9_getOmpCores_try);
    R_RegisterCCallable("icd9", "icd9_RcppExport_validate", (DL_FUNC)icd9_RcppExport_validate);
    return R_NilValue;
}
