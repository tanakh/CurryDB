#include <map>
#include <string>
#include <iostream>
#include <sstream>
using namespace std;

int main()
{
  map<string, string> tbl;
  for (string word; cin >> word; ) {
    istringstream iss(tbl[word]);
    int cnt = 0;
    iss >> cnt;
    ostringstream oss; oss << (cnt + 1);
    tbl[word] = oss.str();
  }
  return 0;
}
