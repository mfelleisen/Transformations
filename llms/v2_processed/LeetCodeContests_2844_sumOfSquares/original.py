from typing import List

def sumOfSquares(nums: List[int]) -> int:
    """
    You are given a 1-indexed integer array nums of length n.
    An element nums[i] of nums is called special if i divides n, i.e. n % i == 0.
    Return the sum of the squares of all special elements of nums.
    """
    n = len(nums)
    sum_squares = 0
    for i in range(1, n + 1):
        if n % i == 0:
            sum_squares += nums[i - 1] ** 2
    return sum_squares