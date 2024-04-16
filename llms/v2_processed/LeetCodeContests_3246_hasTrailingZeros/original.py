from typing import List

def hasTrailingZeros(nums: List[int]) -> bool:
    """
    You are given an array of positive integers nums.
    You have to check if it is possible to select two or more elements in the array such that the bitwise OR of the selected elements 
    has at least one trailing zero in its binary representation.
    
    Parameters:
    nums (List[int]): A list of positive integers.
    
    Returns:
    bool: True if it is possible to select two or more elements whose bitwise OR has trailing zeros, False otherwise.
    """
    # A single number's bitwise OR with itself does not change.
    # We need at least two numbers for their OR to potentially have trailing zeros.
    
    # Use a loop to OR each combination of two elements and check if the result has a trailing zero.
    n = len(nums)
    for i in range(n):
        for j in range(i + 1, n):
            # Bitwise OR of two elements
            or_result = nums[i] | nums[j]
            # Check if there is at least one trailing zero
            if or_result & 1 == 0:
                return True
    
    return False

# Example usage:
print(hasTrailingZeros([1,2,3,4,5]))  # Output: True
print(hasTrailingZeros([2,4,8,16]))   # Output: True
print(hasTrailingZeros([1,3,5,7,9]))  # Output: False