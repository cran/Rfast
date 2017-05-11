//Author: Manos Papadakis

#ifndef NAMESPACE_EXPORT
#define NAMESPACE_EXPORT

#include <vector>
#include <string>
#include <fstream>
#include <dirent.h>
#include <fstream>

using namespace std;
using namespace Rcpp;

vector<string> split_words(string);
void writeFile(vector<string>,string);
vector<string> readFile(string,int&);
bool find_export(string,string);
vector<string> readDirectory(const string);

#endif
