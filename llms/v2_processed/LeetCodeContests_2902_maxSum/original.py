from typing import List

def maxSum(nums: List[int]) -> int:
    """
    This function receives a list of integers and returns the maximum sum of a pair of numbers from the list such that the maximum digit in both numbers are equal.
    If no such pair exists, it returns -1.
    """
    from collections import defaultdict
    
    # Create a dictionary to store the maximum sums and the maximum values seen for each key digit
    max_digit_sum = defaultdict(int)
    max_values_with_digit = defaultdict(int)
    
    # Function to find the maximum single digit in a number
    def max_digit(n: int) -> int:
        return max(int(d) for d in str(n))
    
    # Iterate over each number in the list
    for num in nums:
        digit = max_digit(num)
        
        # If a number with this digit has been seen before, calculate potential new max sum
        if digit in max_values_with_digit:
            max_digit_sum[digit] = max(max_digit_sum[digit], max_values_with_digit[digit] + num)
        
        # Update the maximum value seen for this digit
        max_values_with_digit[digit] = max(max_values_with_digit[digit], num)
    
    # Get the maximum value from the sums calculated (or return -1 if no sums were calculated)
    result = max(max_digit_sum.values(), default=-1)
    return result

# Example usage:
print(maxSum([51, 71, 17, 24, 42]))  # Output: 88
print(maxSum([1, 2, 3, 4]))          # Output: -1