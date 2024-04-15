from typing import List


def maximumGap(nums: List[int]) -> int:
    """
    Given an integer array nums, return the maximum difference between two successive elements in its sorted form. If the array contains less than two elements, return 0.
    You must write an algorithm that runs in linear time and uses linear extra space.
 
    Example 1:

    Input: nums = [3,6,9,1]
    Output: 3
    Explanation: The sorted form of the array is [1,3,6,9], either (3,6) or (6,9) has the maximum difference 3.

    Example 2:

    Input: nums = [10]
    Output: 0
    Explanation: The array contains less than 2 elements, therefore return 0.

 
    Constraints:

    1 <= nums.length <= 105
    0 <= nums[i] <= 109

    """
    if len(nums) < 2:
        return 0

    min_val = min(nums)
    max_val = max(nums)
    length = len(nums)
    bucket_size = max(1, (max_val - min_val) // (length - 1))
    bucket_num = (max_val - min_val) // bucket_size + 1
    buckets = [[float("inf"), float("-inf")] for _ in range(bucket_num)]

    for num in nums:
        idx = (num - min_val) // bucket_size
        buckets[idx][0] = min(buckets[idx][0], num)
        buckets[idx][1] = max(buckets[idx][1], num)

    max_gap = 0
    prev_max = min_val
    for bucket in buckets:
        if bucket[0] == float("inf"):
            continue
        max_gap = max(max_gap, bucket[0] - prev_max)
        prev_max = bucket[1]
    return max_gap