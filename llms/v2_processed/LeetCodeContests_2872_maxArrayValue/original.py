from typing import List

def maxArrayValue(nums: List[int]) -> int:
    """
    You are given a 0-indexed array nums consisting of positive integers.
    You can do the following operation on the array any number of times:
     * Choose an integer i such that 0 <= i < nums.length - 1 and nums[i] <= nums[i + 1]. 
       Replace the element nums[i + 1] with nums[i] + nums[i + 1] and delete the element nums[i] from the array.
    Return the value of the largest element that you can possibly obtain in the final array.
    """
    # We traverse the list from the end to the beginning
    # to simulate the operations described efficiently.
    # This works because combining elements earlier in the process
    # can lead to larger numbers at the end.
    
    # Start from the last element and move backward
    for i in range(len(nums) - 2, -1, -1):
        # We only combine when nums[i] <= nums[i + 1]
        if nums[i] <= nums[i + 1]:
            nums[i] = nums[i] + nums[i + 1]

    # The largest possible value will be the maximum of the modified array
    return max(nums)

# Example usage:
print(maxArrayValue([2,3,7,9,3]))  # Output: 21
print(maxArrayValue([5,3,3]))      # Output: 11