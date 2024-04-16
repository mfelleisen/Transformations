from typing import List

def countBlackBlocks(m: int, n: int, coordinates: List[List[int]]) -> List[int]:
    from collections import defaultdict
    
    # Dictionary to hold the count of black cells for each top-left corner of a block
    block_count = defaultdict(int)
    
    # Populate block_count based on the coordinates of black cells
    for x, y in coordinates:
        # A cell can affect up to 4 potential 2x2 blocks as a part of their corner
        if x > 0 and y > 0:
            block_count[(x-1, y-1)] += 1
        if x > 0 and y < n-1:
            block_count[(x-1, y)] += 1
        if x < m-1 and y > 0:
            block_count[(x, y-1)] += 1
        if x < m-1 and y < n-1:
            block_count[(x, y)] += 1
    
    # Array to hold the result for blocks containing 0, 1, 2, 3, or 4 black cells
    result = [0] * 5
    
    # Calculate the total number of potential blocks
    total_blocks = (m - 1) * (n - 1)
    
    # Blocks counter based on the black cells in each block
    blocks_with_black_cells = defaultdict(int)
    for count in block_count.values():
        blocks_with_black_cells[count] += 1
    
    # Fill the result array based on counts found in blocks_with_black_cells
    for i in range(5):
        if i in blocks_with_black_cells:
            result[i] = blocks_with_black_cells[i]
    
    # Compute the number of blocks with 0 black cells
    result[0] = total_blocks - sum(result[1:5])
    
    return result

# Example Usage
m = 3
n = 3
coordinates = [[0,0], [1,1], [0,2]]
print(countBlackBlocks(m, n, coordinates))  # Output: [0, 2, 2, 0, 0]