def getPermutation(n: int, k: int) -> str:
    """
    The set [1, 2, 3, ..., n] contains a total of n! unique permutations.
    By listing and labeling all of the permutations in order, we get the following sequence for n = 3:

    "123"
    "132"
    "213"
    "231"
    "312"
    "321"

    Given n and k, return the kth permutation sequence.
 
    Example 1:
    Input: n = 3, k = 3
    Output: "213"
    Example 2:
    Input: n = 4, k = 9
    Output: "2314"
    Example 3:
    Input: n = 3, k = 1
    Output: "123"

 
    Constraints:

    1 <= n <= 9
    1 <= k <= n!

    """
    factorials = [1]
    nums = list(range(1, n + 1))
    result = []

    for i in range(1, n):
        factorials.append(factorials[-1] * i)

    k -= 1

    for i in range(n, 0, -1):
        index = k // factorials[i - 1]
        k %= factorials[i - 1]
        result.append(str(nums.pop(index)))

    return ''.join(result)