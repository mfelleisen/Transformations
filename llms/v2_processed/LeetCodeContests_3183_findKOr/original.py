from typing import List

def findKOr(nums: List[int], k: int) -> int:
    """
    Given a list of integers `nums` and an integer `k`, returns the K-or of `nums`.
    The K-or is calculated by checking each bit position across all numbers and
    setting that bit in the result if at least `k` numbers have that bit set.
    """
    # Since the maximum value of nums[i] is less than 2^31, we check up to 31 bits.
    max_bits = 31
    result = 0

    # Iterate through each bit position
    for i in range(max_bits):
        count = 0
        # Check the ith bit in each number
        for num in nums:
            if num & (1 << i):
                count += 1
        # If at least k numbers have this bit set, set this bit in the result
        if count >= k:
            result |= (1 << i)

    return result

# Example usage:
print(findKOr([7, 12, 9, 8, 9, 15], 4))  # Output: 9
print(findKOr([2, 12, 1, 11, 4, 5], 6))   # Output: 0
print(findKOr([10, 8, 5, 9, 11, 6, 8], 1))  # Output: 15