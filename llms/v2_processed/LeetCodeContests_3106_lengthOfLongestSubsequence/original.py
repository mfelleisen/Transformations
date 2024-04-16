from typing import List

def lengthOfLongestSubsequence(nums: List[int], target: int) -> int:
    """
    You are given a 0-indexed array of integers nums, and an integer target.
    Return the length of the longest subsequence of nums that sums up to target. 
    If no such subsequence exists, return -1.
    A subsequence is an array that can be derived from another array by deleting 
    some or no elements without changing the order of the remaining elements.
    """
    # Dictionary to store (sum, max length of subsequence that produces sum)
    dp = {0: 0}  # Base case: sum of 0 is achieved with a subsequence length of 0
    
    for num in nums:
        # Temporary dictionary to prevent modifying dp during iteration
        temp_dp = {}
        for s, length in dp.items():
            new_sum = s + num
            if new_sum <= target:
                if new_sum in dp:
                    temp_dp[new_sum] = max(dp[new_sum], length + 1)
                else:
                    temp_dp[new_sum] = length + 1
        dp.update(temp_dp)
    
    # Check if we achieved the target sum; if so, return the length of the longest subsequence
    if target in dp:
        return dp[target]
    else:
        return -1

# Example usage
print(lengthOfLongestSubsequence([1,2,3,4,5], 9))  # Output: 3
print(lengthOfLongestSubsequence([4,1,3,2,1,5], 7))  # Output: 4
print(lengthOfLongestSubsequence([1,1,5,4,5], 3))  # Output: -1