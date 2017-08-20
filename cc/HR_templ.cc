/*
#ifdef ssu1
#define _GLIBCXX_DEBUG
#endif
#undef NDEBUG

#include <algorithm>
#include <functional>
#include <numeric>
#include <iostream>
#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <cstring>
#include <cassert>
#include <vector>
#include <list>
#include <map>
#include <set>
#include <deque>
#include <queue>
#include <bitset>
#include <sstream>

using namespace std;

#define fore(i, l, r) for(int i = (l); i < (r); ++i)
#define forn(i, n) fore(i, 0, n)
#define fori(i, l, r) fore(i, l, (r) + 1)
#define sz(v) int((v).size())
#define all(v) (v).begin(), (v).end()
#define pb push_back
#define mp make_pair
#define X first
#define Y second

#if ( _WIN32 || __WIN32__ )
    #define LLD "%I64d"
#else
    #define LLD "%lld"
#endif

typedef long long li;
typedef long double ld;
typedef pair<int, int> pt;

template<typename T> T abs(T a) { return a < 0 ? -a : a; }
template<typename T> T sqr(T a) { return a*a; }

const int INF = (int)1e9;
const ld EPS = 1e-9;
const ld PI = 3.1415926535897932384626433832795;

int readInt(int l, int r){
  int x;
  if(scanf("%d", &x) != 1){
    fprintf(stderr, "Expected int in range [%d, %d], but haven't found!", l, r);
    throw;
  }
  if(!(l <= x && x <= r)){
    fprintf(stderr, "Expected int in range [%d, %d], but found %d!", l, r, x);
    throw;
  }
  return x;
}

int up[2], nup[2];

void solve(int tid){
  int n = readInt(1, 100000)
    vector<int> b(n);
  forn(i, n){
    b[i] = readInt(1, 100) - 1;
  }

  forn(i, 2)
    up[i] = 0;

  fore(i, 1, n){
    memset(nup, 0, sizeof nup);
    forn(j, 2){
      forn(k, 2){
        nup[j] = max(nup[j], up[k] + abs((k ? b[i - 1] : 0) - (j ? b[i] : 0)));
      }
    }
    memcpy(up, nup, sizeof nup);
  }
  cout << max(up[0], up[1]) << endl;
}

int main(){
  #ifdef ssu1
  assert(freopen("input.txt", "rt", stdin));
  //assert(freopen("output.txt", "wt", stdout));
  #endif

  int T = readInt(1, 20);
  forn(Ti, T){
    solve(Ti);
  }
  return 0;
}
*/

#include<bits/stdc++.h>
using namespace std;
int ar[100005]={},dp[100005][2]={};
int main()
{
  int t;
  cout << (10ULL^18) << endl;
  cin >> t;
  while(t--)
    {
      int n,i,j;
      cin >> n;
      for(i=0; i<n; i++)
        cin >> ar[i];
      for(i=0; i<n-1; i++)
        {
          //          dp[i+1][0]=max(dp[i][0],dp[i][1]+abs(ar[i]-1));
                    dp[i+1][0]=dp[i][1]+abs(ar[i]-1);
          dp[i+1][1]=max(dp[i][0]+abs(ar[i+1]-1),dp[i][1]+abs(ar[i]-ar[i+1]));
        }
      cout << max(dp[n-1][0],dp[n-1][1]) << endl;
    }
  return 0;
}
