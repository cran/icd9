// [[Rcpp::interfaces(r, cpp)]]

// # EXCLUDE COVERAGE START

#include <Rcpp.h>
#include <icd9.h>
using namespace Rcpp;

// slow versions of functions for benchmarking

// [[Rcpp::export]]
std::vector<bool> icd9IsV_cpp_slower(std::vector< std::string > sv) {
  int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    std::string svt = icd9::strim_cpp(sv[i]);
    if (svt.size() == 0) continue;
    out[i] = svt.at(0) == 'V' || svt.at(0) == 'v';
  }
  return out;
}

// [[Rcpp::export]]
List icd9ShortToParts_cpp_slow(CharacterVector icd9Short, String minorEmpty = "") {

  CharacterVector major;
  CharacterVector minor;
  // can only reserve with std::vector, but can pre-size a Rcpp::Vector
  //major.reserve(icd9Short.size())
  //minor.reserve(icd9Short.size())

  for (CharacterVector::iterator i(icd9Short.begin());
  i < icd9Short.end();
  ++i) {

    std::string s = as<std::string>(*i); // do i need to convert?
    s = icd9::strim_cpp(s); // minimal speed difference

    if (s.find_first_of("Ee") == std::string::npos) { // not an E code
    switch (s.size()) {
      case 1:
      case 2:
      case 3:
      major.push_back(s.substr(0, s.size()));
      minor.push_back(minorEmpty);
      continue;
      case 4:
      case 5:
      major.push_back(s.substr(0, 3));
      minor.push_back(s.substr(3, s.size()-3));
      continue;
      default:
      major.push_back(NA_STRING); minor.push_back(NA_STRING); continue;
    }
    } else { // E code
    if (s.size()<4) {
      major.push_back(NA_STRING); minor.push_back(NA_STRING); continue;
    }
    major.push_back(s.substr(0, 4));
    if (s.size()<5) {
      minor.push_back(minorEmpty);
    } else {
      minor.push_back(s.substr(4, 1));
    }
    } // E code
  } // for
  return List::create();
  // TODO: return icd9MajMinToParts(major, minor);
}

// [[Rcpp::export]]
List icd9MajMinToParts_slower(CharacterVector major, CharacterVector minor) {
  List returned_frame = List::create(
    _["major"] = major,
    _["minor"] = minor);

    StringVector sample_row = returned_frame(0);
    StringVector row_names(sample_row.length());
    for (int i = 0; i < sample_row.length(); ++i) {
      char name[9]; // this is the buffer, so make a good number of chats. Could log10+1 the number of rows. Or use std::string
      sprintf(&(name[0]), "%d", i);
      row_names(i) = name;
    }

    returned_frame.attr("row.names") = row_names;
    returned_frame.attr("class") = "data.frame";

    return returned_frame;
}

bool icd9IsA_cpp_slow(std::string s, const char* c) {
  return s.find_first_of(c) != std::string::npos;
}
std::vector<bool> icd9Is_cpp_slow(std::vector< std::string > sv, const char* c) {
  int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    out[i] = icd9IsA_cpp_slow(sv[i], c);
  }
  return out;
}

// [[Rcpp::export]]
std::vector<bool> icd9IsV_cpp_slow(std::vector< std::string > sv) { return icd9Is_cpp_slow(sv, "Vv");}

// [[Rcpp::export]]
std::vector<bool> icd9IsE_cpp_slow(std::vector< std::string > sv) { return icd9Is_cpp_slow(sv, "Ee");}

// [[Rcpp::export]]
std::vector<bool> icd9IsVE_cpp_slow(std::vector< std::string > sv) { return icd9Is_cpp_slow(sv, "VvEe"); }

// [[Rcpp::export]]
List icd9ShortToParts_cpp_test(CharacterVector icd9Short, std::string minorEmpty = "") {

  CharacterVector major(icd9Short.size());
  CharacterVector minor(icd9Short.size());

  for (int i = 0; i < icd9Short.size(); ++i) {
    std::string s = as<std::string>(icd9Short[i]);
    s = icd9::strim_cpp(s); // minimal speed difference

    if (s.at(0) != char('E') && s.at(0) != char('e')) { // not an E code // char match makes little difference to speed
    switch (s.size()) {
      case 1:
      case 2:
      case 3: major[i] = s.substr(0, s.size()); minor[minorEmpty]; continue;
      case 4:
      case 5: major[i] = s.substr(0, 3); minor[i] = s.substr(3, s.size()-3); continue;
      default: major[i] = NA_STRING; minor[i] = NA_STRING; continue;
    }
    } else { // E code
    if (s.size()<4) {
      major[i] = NA_STRING; minor[i] = NA_STRING; continue;
    }
    major[i] = s.substr(0, 4);
    if (s.size()<5) {
      minor[i] = minorEmpty;
    } else {
      minor[i] = s.substr(4, 1);
    }
    } // E code
  } // for

  return icd9::icd9MajMinToParts(major, minor);
}
// # EXCLUDE COVERAGE END
