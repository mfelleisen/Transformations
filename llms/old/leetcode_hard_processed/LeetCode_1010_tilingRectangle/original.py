def tilingRectangle(n: int, m: int) -> int:
    """
    Given a rectangle of size n x m, return the minimum number of integer-sided squares that tile the rectangle.
 
    Example 1:


    Input: n = 2, m = 3
    Output: 3
    Explanation: 3 squares are necessary to cover the rectangle.
    2 (squares of 1x1)
    1 (square of 2x2)
    Example 2:


    Input: n = 5, m = 8
    Output: 5

    Example 3:


    Input: n = 11, m = 13
    Output: 6

 
    Constraints:

    1 <= n, m <= 13

    """
    if n > m:
        n, m = m, n
    if n == 1:
        return m
    if n == m:
        return 1

    res = float('inf')
    for a in range(1, n // 2 + 1):
        res = min(res, tilingRectangle(a, m) + tilingRectangle(n - a, m))

    for b in range(1, m // 2 + 1):
        res = min(res, tilingRectangle(n, b) + tilingRectangle(n, m - b))

    return res