def countSymmetricIntegers(low: int, high: int) -> int:
    """
    Return the number of symmetric integers in the range [low, high].
    An integer x consisting of 2 * n digits is symmetric if the sum of the first n digits of x is equal to the sum of the last n digits of x.
    Numbers with an odd number of digits are never symmetric.
    """
    def is_symmetric(num):
        s = str(num)
        n = len(s)
        if n % 2 != 0:
            return False
        mid = n // 2
        return sum(int(c) for c in s[:mid]) == sum(int(c) for c in s[mid:])
    
    count = 0
    for num in range(low, high + 1):
        if is_symmetric(num):
            count += 1
            
    return count

# Test the function with example inputs
print(countSymmetricIntegers(1, 100))     # Output: 9
print(countSymmetricIntegers(1200, 1230))  # Output: 4