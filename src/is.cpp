// Copyright (C) 2014 - 2015  Jack O. Wasey
//
// This file is part of icd9.
//
// icd9 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// icd9 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with icd9. If not, see <http://www.gnu.org/licenses/>.

// [[Rcpp::interfaces(r, cpp)]]
#include <string>
#include <vector>

bool icd9IsASingle(const char* s, const char* x) {
	while (*s == ' ')
		++s;
	while (*x) {
		if (*s == *x)
			return true;
		++x;
	}
	return false;
}

bool icd9IsASingleV(const char* s) {
	while (*s == ' ')
		++s;
	return *s == 'V' || *s == 'v';
}

bool icd9IsASingleE(const char* s) {
	while (*s == ' ')
		++s;
	return *s == 'E' || *s == 'e';
}

bool icd9IsASingleVE(const char* s) {
	while (*s == ' ')
		++s;
	return *s == 'V' || *s == 'E' || *s == 'v' || *s == 'e';
}

//' @title test whether elements of vector begin with V, E (or any other
//'   character)
//' @description Current returns a std::vector<bool> which is not thread safe,
//'   or particularly fast, although it is memory efficient in the standard
//'   borked implementation. As of icd9 version 1.2, this is not called by
//'   threaded code, but this could change, so beware! ASCII spaces are trimmed
//'   from the start of the string before testing, but no other whitesapce
//' @param sv std::vector<std::string>&
//' @param x const char* of choices of first character to match
//' @keywords internal
// [[Rcpp::export]]
std::vector<bool> icd9IsA(const std::vector<std::string>& sv, const char* x,
		bool invert = false) {
	int len = sv.size();
	std::vector<bool> out(len);
	for (int i = 0; i < len; ++i) {
		out[i] = invert != (icd9IsASingle(sv[i].c_str(), x));
	}
	return out;
}
