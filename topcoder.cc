// Main Skeleton

#include <iostream>
#include <string>
#include <vector>

using std::cout;
using std::cin;
using std::endl;

class FooClass {
public:
  int foo(int a, int b) {
    return a+b;
  }
};

int main() {
  FooClass f;
  int r = f. foo(1,2);

  cout << r << endl;

  return 0;
}
