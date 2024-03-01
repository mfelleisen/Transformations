def nthMagicalNumber(n: int, a: int, b: int) -> int:
    """
    A positive integer is magical if it is divisible by either a or b.
    Given the three integers n, a, and b, return the nth magical number. Since the answer may be very large, return it modulo 109 + 7.
 
    Example 1:

    Input: n = 1, a = 2, b = 3
    Output: 2

    Example 2:

    Input: n = 4, a = 2, b = 3
    Output: 6

 
    Constraints:

    1 <= n <= 109
    2 <= a, b <= 4 * 104

    """
    def gcd(a, b):
        return a if b == 0 else gcd(b, a % b)
    mod = 1000000007
    lcm = a * b // gcd(a, b)
    left, right = 1, 10**14
    while left < right:
        mid = left + (right - left) // 2
        if (mid // a + mid // b - mid // lcm) < n:
            left = mid + 1
        else:
            right = mid
    return left % mod