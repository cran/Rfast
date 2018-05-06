//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "system_files.h"

using namespace Rcpp;


using std::vector;
using std::string;
using std::binary_search;


//[[Rcpp::export]]
vector<string> add_to_namespace(const string dir_to_export,const string dir_to_file,const bool sorting,vector<string> no_read){
  int which_string_has_export=0,len_which_not_exp=1;
  vector<string> newfiles=readDirectory(dir_to_file,2),already_exported_files,which_not_exported;
  if(newfiles.empty()){
  	stop("Warning: empty folder.\n");
  }
  vector<string> data_export=readFile(dir_to_export,which_string_has_export);
  if(which_string_has_export==-1){
  	stop("Error. can't find \"export\" function in NAMESPACE file with path \"%s\".\n",dir_to_export);
  }
  string exported_files=data_export[which_string_has_export];
  exported_files.erase(exported_files.end()-1);
  exported_files.erase(exported_files.begin(),exported_files.begin()+7);
  if(exported_files.size()==0 || sorting){
    if(sorting){
      exported_files.clear();
    }
  	dont_read_man(newfiles,no_read);
    for(unsigned int i=0;i<newfiles.size();++i){
        exported_files+=newfiles[i]+',';
    }
    which_not_exported=newfiles;
    exported_files[exported_files.size()-1]=')';
  }else{
    already_exported_files=split_words(exported_files);
    dont_read_man(already_exported_files,no_read);
    sort(already_exported_files.begin(),already_exported_files.end());
    for(unsigned int i=0;i<newfiles.size();++i){
      if(binary_search(already_exported_files.begin(),already_exported_files.end(),newfiles[i])==false){
        exported_files+=','+newfiles[i];
        which_not_exported.resize(len_which_not_exp);
        which_not_exported[len_which_not_exp-1]=newfiles[i];
        len_which_not_exp++;
      }
    }
    exported_files+=')';
  }
  data_export[which_string_has_export]="export("+exported_files;
  writeFile(data_export,dir_to_export);
  return which_not_exported;
}

RcppExport SEXP Rfast_add_to_namespace(SEXP dir_to_exportSEXP,SEXP dir_to_fileSEXP,SEXP sortingSEXP,SEXP no_readSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const string >::type dir_to_export(dir_to_exportSEXP);
    traits::input_parameter< const string >::type dir_to_file(dir_to_fileSEXP);
    traits::input_parameter< const bool >::type sorting(sortingSEXP);
    traits::input_parameter< vector<string>  >::type no_read(no_readSEXP);
    __result = add_to_namespace(dir_to_export,dir_to_file,sorting,no_read);
    return __result;
END_RCPP
}
