from typing import List

def minIncrementOperations(nums: List[int], k: int) -> int:
    n = len(nums)
    increments_needed = 0

    # We only need to ensure that every window of size 3 has a max element >= k
    for i in range(n - 2):  # Stop at n-2 to create a window of size 3 with i, i+1, i+2
        max_in_window = max(nums[i], nums[i+1], nums[i+2])
        if max_in_window < k:
            increment = k - max_in_window
            increments_needed += increment
            # Increase all elements in the window to ensure max element condition holds in future windows
            nums[i] += increment
            nums[i+1] += increment
            nums[i+2] += increment

    return increments_needed

# Example usage:
print(minIncrementOperations([2, 3, 0, 0, 2], 4))  # Output: 3
print(minIncrementOperations([0, 1, 3, 3], 5))      # Output: 2
print(minIncrementOperations([1, 1, 2], 1))         # Output: 0