#include <iostream>
#include <vector>
#include <algorithm>
//#include <bits/stdc++.h>

using namespace std;
typedef vector<int> vi;

static int minstep(int n) {
  int d = n%5, r = d%2;
  return (n/5) + d/2 + r;
}
static int solve(int n, vi v) {
  int step=0, delta=0;

  if (n == 1)
    return 0;
  
  for (int i = 1; i < n; i++) {
    int d = v[i]+delta-v[i-1];
    //    cout << v[i] << ':' << d << ':' << minstep(d) << '\n';
    step += minstep(d);
    delta += d;
  }
  return step;
}
int main() {
  int t;
  cin >> t;
  for (int i = 0; i < t; i++) {
    int n;
    cin >> n;
    vi v(n);
    for (int j = 0; j < n; j++)
      cin >> v[j];
    sort(v.begin(), v.end());
    cout << solve(n, v) << '\n';
  }
  return 0;
}
