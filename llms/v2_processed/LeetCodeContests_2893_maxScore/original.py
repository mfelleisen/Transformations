from typing import List

def maxScore(nums: List[int], x: int) -> int:
    """
    Given a list of integers nums and a penalty x, this function calculates the maximum score
    you can achieve starting from position 0 and moving to any higher index. If you move between
    indices with numbers of different parities, you lose 'x' points.
    """
    n = len(nums)
    dp = [float('-inf')] * n
    dp[0] = nums[0]
    
    for i in range(n):
        for j in range(i + 1, n):
            # Calculate score transition without penalty first
            current_score = dp[i] + nums[j]
            # Apply penalty if the parities of nums[i] and nums[j] differ
            if (nums[i] % 2) != (nums[j] % 2):
                current_score -= x
            # Update dp[j] if the new computed score is higher
            dp[j] = max(dp[j], current_score)
    
    # The final answer is the maximum value in the dp array
    return max(dp)

# Example cases
print(maxScore([2,3,6,1,9,2], 5))  # Output: 13
print(maxScore([2,4,6,8], 3))       # Output: 20