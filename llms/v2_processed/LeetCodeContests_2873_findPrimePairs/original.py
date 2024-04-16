from typing import List

def is_prime(num: int) -> bool:
    """ Helper function to check if a number is prime. """
    if num <= 1:
        return False
    if num <= 3:
        return True
    if num % 2 == 0 or num % 3 == 0:
        return False
    i = 5
    while i * i <= num:
        if num % i == 0 or num % (i + 2) == 0:
            return False
        i += 6
    return True

def findPrimePairs(n: int) -> List[List[int]]:
    """
    Returns the 2D sorted list of prime number pairs [xi, yi] where:
    - 1 <= x <= y <= n
    - x + y == n
    - x and y are prime numbers
    """
    primes = [i for i in range(1, n+1) if is_prime(i)]
    prime_pairs = []
    
    # Use two pointers to find pairs
    i, j = 0, len(primes) - 1
    while i <= j:
        x, y = primes[i], primes[j]
        if x + y == n:
            prime_pairs.append([x, y])
            i += 1
            j -= 1
        elif x + y < n:
            i += 1
        else:
            j -= 1

    return prime_pairs

# Example usage:
print(findPrimePairs(10))  # Output: [[3, 7], [5, 5]]
print(findPrimePairs(2))   # Output: []