//Author: Manos Papadakis

#ifndef SYSTEM_FILES
#define SYSTEM_FILES

#include <vector>
#include <string>
#include <fstream>
#include <dirent.h>
#include <Rcpp.h>

using Rcpp::List;
using std::vector;
using std::string;
using std::ifstream;


void print_error();

template<class T,class... Args>
void print_error(T values,Args... args){
	Rcpp::Rcout<<values<<"\n";
	print_error(args...);
}


//#define PRINT_ERRORS
#ifdef PRINT_ERRORS
#define DEBUG print_error
#else
#define DEBUG(...);
#endif


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
string read_example(ifstream &,int&);
bool binary_help(vector<string>::iterator,vector<string>::iterator,string&,vector<string>::iterator&);
void dont_read_man(vector<string>&,vector<string>&);
vector<string> read_usage(ifstream &);
string read_function_from_r_file(ifstream &);
void remove_spaces(string&);
List read_examples(string,vector<string>);

template<class T>
bool find_string(string& s,T f){
  return s.find(f)!=string::npos;
}

#endif
