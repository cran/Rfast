//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include <string>
#include <algorithm>
#include "system_files.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
vector<string> check_namespace(const string dir_to_export,const string dir_to_file){
  int which_string_has_export=0,len_which_not_exp=1;
  vector<string> allfiles=readDirectory(dir_to_file,2),which_undefined_function,all_exported_files;
  if(allfiles.empty()){
    stop("Warning: empty folder.\n");
  }
  vector<string> data_export=readFile(dir_to_export,which_string_has_export);
  if(which_string_has_export==-1){
    stop("Error. can't find \"export\" function in NAMESPACE file.\n");
  }
  string exported_files=data_export[which_string_has_export];
  exported_files.erase(exported_files.end()-1);
  exported_files.erase(exported_files.begin(),exported_files.begin()+7);
  all_exported_files=split_words(exported_files);
  sort(allfiles.begin(),allfiles.end());
  for(unsigned int i=0;i<all_exported_files.size();++i){
    if(binary_search(allfiles.begin(),allfiles.end(),all_exported_files[i])==false){
      which_undefined_function.resize(len_which_not_exp);
      which_undefined_function[len_which_not_exp-1]=all_exported_files[i];
      len_which_not_exp++;
    }
  }
  return which_undefined_function;
}

RcppExport SEXP Rfast_check_namespace(SEXP dir_to_exportSEXP,SEXP dir_to_fileSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const string >::type dir_to_export(dir_to_exportSEXP);
    traits::input_parameter< const string >::type dir_to_file(dir_to_fileSEXP);
    __result = wrap(check_namespace(dir_to_export,dir_to_file));
    return __result;
END_RCPP
}
