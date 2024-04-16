def minimumPossibleSum(n: int, target: int) -> int:
    MOD = 10**9 + 7
    
    # This will store the elements of the array
    nums = []
    
    # Starting from 1, we look for suitable candidates for the array
    candidate = 1
    
    while len(nums) < n:
        # We can include the candidate if there's no element in the current list
        # such that their sum equals the target
        can_use = True
        for num in nums:
            if num + candidate == target:
                can_use = False
                break
        
        if can_use:
            nums.append(candidate)
        candidate += 1

    # Calculate the minimum possible sum modulo 10**9 + 7
    return sum(nums) % MOD

# Example test cases
print(minimumPossibleSum(2, 3))   # Expected: 4 (nums = [1, 3])
print(minimumPossibleSum(3, 3))   # Expected: 8 (nums = [1, 3, 4])
print(minimumPossibleSum(1, 1))   # Expected: 1 (nums = [1])