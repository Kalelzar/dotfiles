#include "flag.hpp"
#include <vector>
#include <list>

int main(int argc, char** argv){

  std::cout<<std::boolalpha;
  std::cerr<<std::boolalpha;

  Flag<std::list> f;

  auto i = f.flag<int>("iter", 1, "Iterations to execute.");
  auto d = f.flag<double>("iter2", 1, "Iterations to execute.");
  auto b = f.flag<bool>("dry", false, "Only mock execution.");
  auto s = f.flag<char*>("file", "/dev/null", "File to execute on.");
  auto v = f.flag("dry2", "Only mock execution.");
  auto il = f.flag<std::list<int>>("list", {}, "List of Ints");

  require(i);
  require(v);

  auto read = f.parse(argc-1, argv+1);
  std::cout<<"Didn't parse: ";
  for(auto i = read+1; i < argc; i++){
    std::cout<<argv[i]<<" ";
  }
  std::cout<<std::endl;
  f.usage();

  std::cout<<"END"<<std::endl;

  return 0;
}
