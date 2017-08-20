#include <cmath>
#include <vector>
#include <iostream>
using namespace std;
typedef vector<int> vi;

static vector<bool> sieve(int n) {
  int i, j;
  vector<bool> primes(n);
  for (i = 0; i < n; i++)
    primes[i] = true;
  primes[0] = primes[1] = false;
  for (i = 2; i <= sqrt(n); i++)
    if (primes[i])
      for (j = i; j*i < n; j++)
        primes[i*j] = false;
  return primes;
}

int main() {
  vector<bool> primes = sieve(10000);

  // for (auto i:primes)
  for (vector<bool>::iterator i=primes.begin(); i != primes.end(); ++i)
    if (*i)
      cout << i - primes.begin() << " ";
  cout << endl;
}
