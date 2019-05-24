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
vector<string> read_usage(ifstream &);
string read_function_from_r_file(ifstream &);
void remove_spaces(string&);
List read_examples(string);
bool check_read_file(ifstream&,char);
void dont_read_man(vector<string>&,vector<string>&);
void reset_file(ifstream& file);

bool is_dont_read(string& s,char attr);
bool is_export(string& s);
string read_current_signature_function_from_r_file(string& line,string keyword_function,ifstream &file,const int position_of_function_key);
void read_functions_from_r_file(const string filename,vector<string> &exported_functions_names,vector<string> &not_exported_functions_names,List& signatures,bool& found_dont_read);
List read_functions_and_signatures(string path);

template<class T>
bool find_string(string& s,T f){
  return s.find(f)!=string::npos;
}

#endif
