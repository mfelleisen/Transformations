def find_kth_number(n: int, k: int) -> int:
    """
    Given two integers n and k, return the kth lexicographically smallest integer in the range [1, n].
 
    Example 1:

    Input: n = 13, k = 2
    Output: 10
    Explanation: The lexicographical order is [1, 10, 11, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9], so the second smallest number is 10.

    Example 2:

    Input: n = 1, k = 1
    Output: 1

 
    Constraints:

    1 <= k <= n <= 109

    """
    nums = [i for i in range(1, n + 1)]
    nums.sort(key=lambda x: str(x))
    return nums[k - 1]