//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include <string>
#include <algorithm>
#include "system_files.h"

using namespace Rcpp;
using namespace std;

using std::binary_search;
using std::ifstream;
using std::getline;
using std::vector;
using std::string;
using std::sort;
using std::remove;


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
      ++len_which_not_exp;
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
    __result = check_namespace(dir_to_export,dir_to_file);
    return __result;
END_RCPP
}

///////////////////////////////////////////////////////////////////////////////////////////////////

List check_true_false(string path_to_man){
  List ex = read_examples(path_to_man),L;
  CharacterVector names=ex["files"];
  vector<string> exams=as<vector<string> >(ex["examples"]);
  string s;
  CharacterVector trues,falses;
  for(unsigned int i=0;i<exams.size();++i){
    s=exams[i];
    remove(s.begin(),s.end(),' ');
    if(find_string(s,"=T)") || find_string(s,"=T,")){
      trues.push_back(names[i]);
    }else if(find_string(s,"=F)") || find_string(s,"=F,")){
      falses.push_back(names[i]);
    }
  } 
  L["TRUE"] = sort_unique(trues);
  L["FALSE"] = sort_unique(falses);
  L["dont read"]=ex["dont read"];
  return L;
}

RcppExport SEXP Rfast_check_true_false(SEXP path_manSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< string >::type path_man(path_manSEXP);
    __result = check_true_false(path_man);
    return __result;
END_RCPP
}


/////////////////////////////////////////////////////////////////////////////////////////////////
using std::remove;

List check_aliases(const string path_man,const string path_rf){
  ifstream file;
  vector<string> aliases,all_r_files=readDirectory(path_rf,2),all_rd_files=readDirectory(path_man,3),tmp,dontread_rd;
  for(unsigned int i=0;i<all_rd_files.size();++i){
    file.open(path_man+all_rd_files[i]+".Rd");
    if(!file.is_open()){
      Rcout<<"Can't open file "<<all_rd_files[i]<<".\n";
      continue;
    }
    if(check_read_file(file,'%')){
      tmp=read_aliases(file);
      for(unsigned int j=0;j<tmp.size();++j){
        aliases.push_back(tmp[j]);
      }
    }else{
      DEBUG("Find attribute dont read file with name: "+all_rd_files[i]);
      dontread_rd.push_back(all_rd_files[i]);
    }
    file.close();
  }
  sort(aliases.begin(),aliases.end());
  sort(all_r_files.begin(),all_r_files.end());
  List ls;
  ls["Missing Man files"]=find_which(all_r_files,aliases);
  ls["Missing R files"]=find_which(aliases,all_r_files);
  ls["Duplicate alias"]=find_duplis(aliases);
  ls["dont read"]=List::create(_["Rd"]=dontread_rd);
  return ls;
}

RcppExport SEXP Rfast_check_aliases(SEXP dir_to_manSEXP,SEXP dir_to_fileSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const string >::type dir_to_man(dir_to_manSEXP);
    traits::input_parameter< const string >::type dir_to_file(dir_to_fileSEXP);
    __result = check_aliases(dir_to_man,dir_to_file);
    return __result;
END_RCPP
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct File : public ifstream {
  string name;
  void fopen(string path,string name){
    this->name=name;
    this->open(path+name);
  }
  void fopen2(string full_path,string name){
      this->name=name;
      this->open(full_path);
  }
  void fclose(){
    this->close();
    name="";
  }
};
using std::remove;

//[[Rcpp::export]]
List check_usage(string path_man,string path_rf){
  DEBUG("START");
  File file_rd,file_r;
  vector<string> all_rd_files=read_directory(path_man),dontread_rd,dontread_r;
  vector<string> missing_functions,aliases,functions,missmatch_functions,name_of_functions_in_usage;
  string r_file;
  for(unsigned int i=0;i<all_rd_files.size();++i){
    file_rd.fopen(path_man,all_rd_files[i]);
    if(!file_rd.is_open()){
      Rcout<<"Can't open file "<<all_rd_files[i]<<".\n";
      continue;
    }
    if(!check_read_file(file_rd,'%')){
        dontread_rd.push_back(file_rd.name);
      DEBUG("Find attribute dont read file with name: "+file_rd.name);
    }else{
        aliases=read_aliases(file_rd);
        functions=read_usage(file_rd);
    
        string curr_func,func_from_r_file;
        for(auto& al : aliases){
          for(auto& tmp : functions){
            if(tmp.compare(0,al.size(),al)==0 && tmp[al.size()]=='('){ // otan to onoma einai idio akrivos kai ameso meta ksekinaei "("
              curr_func=tmp;
              break;
            }
          }
          if(curr_func.empty()){
            missing_functions.push_back(al+" not in "+file_rd.name);
          }else{
            r_file=path_rf+al+".R";
            if(access(r_file.c_str(),F_OK)!=-1){
                file_r.fopen2(r_file,al); // open file aliase.R
                if(!file_r.is_open()){
                    Rcout<<"Can't open file "<<r_file<<".\n";
                    continue;
                }else{
                    if(al == ""){//if(file_r.name.empty()){ //check if aliase is empty
                    	Rcout<<"Error: can't find function name of file "<<r_file<<".\n";
                        continue;
                    }
                }
                if(check_read_file(file_r,'#')){
                  func_from_r_file=read_function_from_r_file(file_r);
                  DEBUG("current: "+curr_func+" , fromRfile: "+func_from_r_file);
                  if(curr_func!=func_from_r_file){
                      DEBUG(curr_func+" : "+func_from_r_file);
                      missmatch_functions.push_back(file_r.name+" != "+file_rd.name);
                  }
                }else{
                    dontread_r.push_back(file_r.name);
                    DEBUG("Find attribute dont read in file: "+file_r.name);
                }
              file_r.close();
            }
          }
        }
    }
    file_rd.fclose();
  }
  List L;
  L["missing functions"]=missing_functions;
  L["missmatch_functions"]=missmatch_functions;
  L["dont read"]=List::create(_["R"]=dontread_r,_["Rd"]=dontread_rd);
  DEBUG("END");
  return L;
}

RcppExport SEXP Rfast_check_usage(SEXP path_manSEXP,SEXP path_rfSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< string >::type path_man(path_manSEXP);
    traits::input_parameter< string >::type path_rf(path_rfSEXP);
    __result = check_usage(path_man,path_rf);
    return __result;
END_RCPP
}
