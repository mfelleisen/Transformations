def minimumSum(n: int, k: int) -> int:
    """You are given two integers, n and k.
    An array of distinct positive integers is called a k-avoiding array if there does not exist any pair of distinct elements that sum to k.
    Return the minimum possible sum of a k-avoiding array of length n.
    """
    # We'll start filling the array from 1 upwards and skip any number that could form a sum of k with any existing number in the array.
    
    # This set will store the numbers used in the k-avoiding array
    used_numbers = set()
    # This is the current number we are considering to add to the array
    current_number = 1
    # This is to keep track of how many numbers we have added to the array
    count = 0
    # This will hold the sum of the k-avoiding array
    total_sum = 0
    
    while count < n:
        # Check if adding the current number would violate the k-avoiding property
        # We check if (k - current_number) is already in the set and not equal to the current number itself (distinctness)
        if (k - current_number) not in used_numbers or (k - current_number) == current_number:
            # Add current_number to the k-avoiding array
            used_numbers.add(current_number)
            total_sum += current_number
            count += 1
        # Increment the number to consider next
        current_number += 1
    
    return total_sum