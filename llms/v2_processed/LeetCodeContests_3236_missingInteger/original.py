from typing import List

def missingInteger(nums: List[int]) -> int:
    """You are given a 0-indexed array of integers nums.
    A prefix nums[0..i] is sequential if, for all 1 <= j <= i, nums[j] = nums[j - 1] + 1. In particular, the prefix consisting only of nums[0] is sequential.
    Return the smallest integer x missing from nums such that x is greater than or equal to the sum of the longest sequential prefix.
    """
    # First, find the longest sequential prefix
    i = 0
    while i < len(nums) - 1 and nums[i + 1] == nums[i] + 1:
        i += 1
    
    # Calculate the sum of the longest sequential prefix
    sum_prefix = sum(nums[:i+1])
    
    # Find the smallest missing integer greater than or equal to sum_prefix
    x = sum_prefix
    while x in nums:
        x += 1
        
    return x

# Example usage
print(missingInteger([1,2,3,2,5]))  # Output: 6
print(missingInteger([3,4,5,1,12,14,13]))  # Output: 15