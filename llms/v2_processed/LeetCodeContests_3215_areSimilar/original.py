from typing import List

def areSimilar(mat: List[List[int]], k: int) -> bool:
    m, n = len(mat), len(mat[0])
    
    # Helper function to perform right cyclic shift
    def right_shift(row, shifts):
        if shifts == 0:
            return row
        shifts = shifts % len(row)
        return row[-shifts:] + row[:-shifts]
    
    # Helper function to perform left cyclic shift
    def left_shift(row, shifts):
        if shifts == 0:
            return row
        shifts = shifts % len(row)
        return row[shifts:] + row[:shifts]
    
    final_mat = []
    for i, row in enumerate(mat):
        if i % 2 == 0:  # Even indexed row
            new_row = left_shift(row, k)
        else:           # Odd indexed row
            new_row = right_shift(row, k)
        final_mat.append(new_row)
    
    # Compare initial and final matrices
    return final_mat == mat

# Examples to test the function
mat1 = [[1,2,1,2],[5,5,5,5],[6,3,6,3]]
k1 = 2
print(areSimilar(mat1, k1))  # Output: True

mat2 = [[2,2],[2,2]]
k2 = 3
print(areSimilar(mat2, k2))  # Output: True

mat3 = [[1,2]]
k3 = 1
print(areSimilar(mat3, k3))  # Output: False