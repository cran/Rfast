
//Author: Manos Papadakis
#include <RcppArmadillo.h>
#include <dirent.h>
#include "system_files.h"

using namespace Rcpp;

using std::vector;
using std::string;


vector<string> read_directory(string path){
  DIR *dir=NULL;
  struct dirent *ent;
  vector<string> files;
  string textf;
  if((dir = opendir(path.c_str())) != NULL) {
    readdir(dir);
    readdir(dir);
    while((ent = readdir(dir)) != NULL) {
      textf=ent->d_name;
      files.push_back(textf);
    }
    closedir(dir);
  }else{
    stop("Error: Could not open directory with path \""+path+"\"");
  }
  return files;
}


RcppExport SEXP Rfast_read_directory(SEXP pathSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const string >::type path(pathSEXP);
    __result = wrap(read_directory(path));
    return __result;
END_RCPP
}


/////////////////////////////////////////////////////////////////////////


using std::ifstream;
using std::vector;
using std::string;

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

//////////////////////////////////////////////////////////////////////////
