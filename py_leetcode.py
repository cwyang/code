from typing import List
class Solution:
    def stock_sell_once(self, nums: List[int]) -> int:
        low = nums[0]
        profit = 0
        for i in nums[1:]:
            d = i - low
            profit = max(profit, d)
            low = min(low, i)
        return profit
    
def solve() -> None:
    l = list(map(int, input().split()))
    s = Solution()
    print(s.stock_sell_once(l))
    
#solve()

import sys
import io
TESTCASE = '''
310 300 320 330 315
'''
def test() -> None:
    sys.stdin = io.StringIO(TESTCASE.strip())
    solve()
    sys.stdin = sys.__stdin__

test()
