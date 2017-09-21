// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <fstream>
#include <dirent.h>
#include "system_files.h"

using namespace std;
using namespace Rcpp;

//[[Rcpp::export]]
List read_examples(string path_man,vector<string> no_read){
  ifstream file;
  vector<string> examples,all_rd_files=read_directory(path_man),files_long_lines;
  string tmp;
  int longlines=0;
  dont_read_man(all_rd_files,no_read);
  for(unsigned int i=0;i<all_rd_files.size();++i){
    file.open(path_man+all_rd_files[i]);
    if(!file.is_open()){
      stop("Can't open file \"%s\".",all_rd_files[i]);
    }
    longlines=0;
    tmp=read_example(file,longlines);
    if(longlines){
    	files_long_lines.push_back(all_rd_files[i]);
    }
    file.close();
    examples.push_back(tmp);
  }
  List l;
  l["examples"]=examples;
  l["files"]=all_rd_files;
  l["long_lines"]=files_long_lines;
  return l;
}

RcppExport SEXP Rfast_read_examples(SEXP path_manSEXP,SEXP no_readSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< string >::type path_man(path_manSEXP);
    traits::input_parameter< vector<string> >::type no_read(no_readSEXP);
    __result = wrap(read_examples(path_man,no_read));
    return __result;
END_RCPP
}
