import pandas as pd

def divide(dividend, divisor):
        """
        :type dividend: int
        :type divisor: int
        :rtype: int
        """
        def divide(dd): # the last position that divisor* val <  dd
            s, r = 0, 0
            for i in range(9):
                tmp = s + divisor
                if tmp <= dd:
                    s = tmp
                else:
                    return str(i), str(dd-s)
            return str(9), str(dd-s)
        carrying = 0
        if dividend == 0:
            return 0,carrying
        sign = -1
        if (dividend >0 and divisor >0 ) or (dividend < 0 and divisor < 0):
            sign = 1
        dividend = abs(dividend)
        divisor = abs(divisor)
        if divisor > dividend:
            return 0,carrying
        ans, did, dr = [], str(dividend), str(divisor)
        n = len(dr)
        pre = did[:n-1]
        for i in range(n-1, len(did)):
            dd = pre+did[i]
            carrying += 1
            dd = int(dd)
            v, pre = divide(dd)
            ans.append(v)
             
        ans = int(''.join(ans))*sign

        if ans > (1<<31)-1:
            ans = (1<<31)-1
        return ans, carrying

carryings = [division(lower, upper)[1] for lower in range(100) for upper in range(100) if lower < upper]