from typing import List


def numSquarefulPerms(nums: List[int]) -> int:
    """
    An array is squareful if the sum of every pair of adjacent elements is a perfect square.
    Given an integer array nums, return the number of permutations of nums that are squareful.
    Two permutations perm1 and perm2 are different if there is some index i such that perm1[i] != perm2[i].
 
    Example 1:

    Input: nums = [1,17,8]
    Output: 2
    Explanation: [1,8,17] and [17,8,1] are the valid permutations.

    Example 2:

    Input: nums = [2,2,2]
    Output: 1

 
    Constraints:

    1 <= nums.length <= 12
    0 <= nums[i] <= 109

    """
    from collections import Counter

    def is_square(n):
        return int(n ** 0.5) ** 2 == n

    def dfs(idx):
        if idx == len(nums):
            return 1

        count = 0
        for key in counter.keys():
            if counter[key] and (idx == 0 or is_square(nums[idx - 1] + key)):
                counter[key] -= 1
                nums[idx] = key
                count += dfs(idx + 1)
                counter[key] += 1

        return count

    key_set, counter = set(nums), Counter(nums)
    return dfs(0)