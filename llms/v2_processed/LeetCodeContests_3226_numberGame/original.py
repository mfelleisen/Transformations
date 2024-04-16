from typing import List

def numberGame(nums: List[int]) -> List[int]:
    """
    This function simulates a game where Alice and Bob alternately remove the minimum element from the list `nums`.
    Bob appends his element first to the result list `arr`, followed by Alice. This continues until `nums` is empty.
    """
    arr = []
    while nums:
        # Alice removes the minimum element
        min_num = min(nums)
        nums.remove(min_num)
        alice_choice = min_num

        # Bob removes the next minimum element
        min_num = min(nums)
        nums.remove(min_num)
        bob_choice = min_num

        # Bob appends first to arr, then Alice
        arr.append(bob_choice)
        arr.append(alice_choice)
        
    return arr

# Example usage:
nums1 = [5, 4, 2, 3]
print(numberGame(nums1))  # Output: [3, 2, 5, 4]

nums2 = [2, 5]
print(numberGame(nums2))  # Output: [5, 2]