def findMaximumNumber(k: int, x: int) -> int:
    current_sum = 0
    num = 0

    while True:
        num += 1
        bin_rep = bin(num)[2:]  # Get binary representation as a string and strip the '0b' prefix.
        price = 0
        
        # Check set bits at positions that are multiples of x (1-indexed from the right)
        for i in range(len(bin_rep)):
            # i+1 because we index from 1 and from the right
            if (i + 1) % x == 0 and bin_rep[-(i + 1)] == '1':
                price += 1
        
        current_sum += price
        
        # If the sum exceeds k, the previous number is the maximum number we want
        if current_sum > k:
            return num - 1

# Example use case
print(findMaximumNumber(9, 1))  # Output: 6
print(findMaximumNumber(7, 2))  # Output: 9