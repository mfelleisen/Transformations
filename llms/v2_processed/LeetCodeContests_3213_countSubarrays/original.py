from typing import List
from collections import defaultdict

def countSubarrays(nums: List[int], k: int) -> int:
    """
    You are given an integer array nums and a positive integer k.
    Return the number of subarrays where the maximum element of nums appears at least k times in that subarray.
    
    :param nums: List[int] - the input list of integers
    :param k: int - the minimum frequency of the maximum element in the subarray
    :return: int - the count of subarrays satisfying the condition
    """
    max_element = max(nums)
    count = 0
    
    # Iterate over all elements to find subarrays starting from each index
    for i in range(len(nums)):
        freq = defaultdict(int)
        current_max = 0
        
        # Explore all subarrays starting from index i
        for j in range(i, len(nums)):
            num = nums[j]
            freq[num] += 1
            
            # Update the current maximum if necessary
            if num > current_max:
                current_max = num
                
            # Check if the current maximum element appears at least k times
            if current_max == max_element and freq[current_max] >= k:
                count += 1
    
    return count

# Example usage:
print(countSubarrays([1,3,2,3,3], 2))  # Output: 6
print(countSubarrays([1,4,2,1], 3))    # Output: 0