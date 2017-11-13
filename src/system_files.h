//Author: Manos Papadakis

#ifndef SYSTEM_FILES
#define SYSTEM_FILES

#include <vector>
#include <string>
#include <fstream>
#include <dirent.h>

using namespace std;
using namespace Rcpp;

vector<string> split_words(string);
void writeFile(vector<string>,string);
vector<string> readFile(string,int&);
bool find_export(string,string);
vector<string> readDirectory(const string,const int);
bool is_alias(const char *s,int);
bool next_alias(ifstream &,string &);
vector<string> read_aliases(ifstream &);
vector<string> find_which(vector<string>,vector<string>);
vector<string> find_duplis(vector<string>);
bool is_example(const char *,int);
bool get_example(ifstream&,string&);
vector<string> read_directory(string);
string read_example(ifstream &file,int& long_lines);
bool binary_help(vector<string>::iterator,vector<string>::iterator,string&,vector<string>::iterator&);
void dont_read_man(vector<string>&,vector<string>&);

#endif
