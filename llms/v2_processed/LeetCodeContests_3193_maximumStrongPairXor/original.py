from typing import List

def maximumStrongPairXor(nums: List[int]) -> int:
    """
    This function finds the maximum XOR value out of all possible strong pairs in the array nums.
    A strong pair (x, y) satisfies the condition: |x - y| <= min(x, y)
    """
    max_xor = 0
    n = len(nums)

    for i in range(n):
        for j in range(i, n):
            if abs(nums[i] - nums[j]) <= min(nums[i], nums[j]):
                max_xor = max(max_xor, nums[i] ^ nums[j])

    return max_xor

# Examples
print(maximumStrongPairXor([1,2,3,4,5]))  # Output: 7
print(maximumStrongPairXor([10,100]))      # Output: 0
print(maximumStrongPairXor([5,6,25,30]))   # Output: 7