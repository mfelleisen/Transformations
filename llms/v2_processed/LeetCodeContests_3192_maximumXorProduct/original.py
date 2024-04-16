def maximumXorProduct(a: int, b: int, n: int) -> int:
    MOD = 10**9 + 7
    max_product = 0
    upper_limit = 2 ** n
    
    for x in range(upper_limit):
        product = (a ^ x) * (b ^ x)
        if product > max_product:
            max_product = product
    
    return max_product % MOD