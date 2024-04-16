from typing import List

def incremovableSubarrayCount(nums: List[int]) -> int:
    """You are given a 0-indexed array of positive integers nums.
    A subarray of nums is called incremovable if nums becomes strictly increasing on removing the subarray.
    Return the total number of incremovable subarrays of nums.
    """
    n = len(nums)
    count = 0
    
    def is_strictly_increasing(arr: List[int]) -> bool:
        return all(arr[i] < arr[i+1] for i in range(len(arr) - 1))
    
    # Check each possible subarray
    for start in range(n):
        for end in range(start, n):
            # Create a new list excluding the current subarray
            modified_nums = nums[:start] + nums[end+1:]
            if is_strictly_increasing(modified_nums):
                count += 1
    
    return count

# Test cases
print(incremovableSubarrayCount([1, 2, 3, 4]))  # Output: 10
print(incremovableSubarrayCount([6, 5, 7, 8]))  # Output: 7
print(incremovableSubarrayCount([8, 7, 6, 6]))  # Output: 3