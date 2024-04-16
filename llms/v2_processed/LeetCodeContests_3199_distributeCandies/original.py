def distributeCandies(n: int, limit: int) -> int:
    """You are given two positive integers n and limit.
    Return the total number of ways to distribute n candies among 3 children such that no child gets more than limit candies.
    """
    count = 0
    for a in range(min(n, limit) + 1):
        for b in range(min(n - a, limit) + 1):
            c = n - a - b
            if c <= limit:
                count += 1
    return count

# Example usage:
print(distributeCandies(5, 2))  # Output: 3
print(distributeCandies(3, 3))  # Output: 10