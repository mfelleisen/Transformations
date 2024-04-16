from typing import List

def sumCounts(nums: List[int]) -> int:
    """
    Given a 0-indexed integer array nums, this function returns the sum of the squares of distinct
    counts of all subarrays of nums.
    """
    n = len(nums)
    total_sum = 0
    
    # Iterate over each possible starting point of the subarray
    for i in range(n):
        # To track distinct elements and their counts
        seen = {}
        # Iterate over each possible ending point of the subarray starting from i
        for j in range(i, n):
            # If the element is not in seen, add it with count 1
            if nums[j] not in seen:
                seen[nums[j]] = 0
            seen[nums[j]] += 1
            
            # The number of distinct elements is the size of the seen dictionary
            distinct_count = len(seen)
            # Add the square of the distinct count to the total sum
            total_sum += distinct_count ** 2
    
    return total_sum