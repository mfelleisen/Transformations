from typing import List

def maximumJumps(nums: List[int], target: int) -> int:
    n = len(nums)
    dp = [float('-inf')] * n  # Initialize DP array with negative infinity
    dp[0] = 0  # Start position, no jumps needed

    for i in range(n):
        if dp[i] == float('-inf'):
            continue  # Skip if this index is not reachable
        for j in range(i + 1, n):
            if abs(nums[j] - nums[i]) <= target:
                dp[j] = max(dp[j], dp[i] + 1)

    # If the last index is still negative infinity, it's unreachable
    return dp[-1] if dp[-1] != float('-inf') else -1

# Examples
print(maximumJumps([1, 3, 6, 4, 1, 2], 2))  # Output: 3
print(maximumJumps([1, 3, 6, 4, 1, 2], 3))  # Output: 5
print(maximumJumps([1, 3, 6, 4, 1, 2], 0))  # Output: -1