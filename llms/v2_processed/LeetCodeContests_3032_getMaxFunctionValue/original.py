from typing import List

def getMaxFunctionValue(receiver: List[int], k: int) -> int:
    """
    You are given a 0-indexed integer array receiver of length n and an integer k.
    This function calculates the maximum value of a function f(x), which is the sum of ids of players
    who receive the ball during k passes, starting from any player x.
    """
    n = len(receiver)
    max_value = float('-inf')
    
    for start in range(n):
        current_id = start
        f_value = start
        for _ in range(k):
            current_id = receiver[current_id]
            f_value += current_id
        
        max_value = max(max_value, f_value)
    
    return max_value

# Example tests
print(getMaxFunctionValue([2, 0, 1], 4))  # Output: 6
print(getMaxFunctionValue([1, 1, 1, 2, 3], 3))  # Output: 10