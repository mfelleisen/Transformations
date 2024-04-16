from typing import List

def findIntersectionValues(nums1: List[int], nums2: List[int]) -> List[int]:
    """You are given two 0-indexed integer arrays nums1 and nums2 of sizes n and m, respectively.
    Consider calculating the following values:
    - The number of indices i such that 0 <= i < n and nums1[i] occurs at least once in nums2.
    - The number of indices i such that 0 <= i < m and nums2[i] occurs at least once in nums1.
    Return an integer array answer of size 2 containing the two values in the above order.
    """
    set_nums1 = set(nums1)
    set_nums2 = set(nums2)
    
    # Calculate intersection
    intersection1 = set_nums1 & set_nums2
    intersection2 = set_nums2 & set_nums1
    
    # Count indices in nums1 whose elements are in nums2
    count1 = sum(1 for x in nums1 if x in intersection1)
    # Count indices in nums2 whose elements are in nums1
    count2 = sum(1 for x in nums2 if x in intersection2)
    
    # Since intersection1 and intersection2 are the same, we could simplify:
    # count1 = sum(1 for x in nums1 if x in intersection1)
    # count2 = sum(1 for x in nums2 if x in intersection1)
    
    return [count1, count2]

# Example usage:
print(findIntersectionValues([4,3,2,3,1], [2,2,5,2,3,6]))  # Output: [3, 4]
print(findIntersectionValues([3,4,2,3], [1,5]))  # Output: [0, 0]