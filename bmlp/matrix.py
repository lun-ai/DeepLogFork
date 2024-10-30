import pygraphblas as gb
import numpy as np
from .utils import *

# Use python wrapper of GraphBLAS on GPU (BLAS - Basic Linear Algebra Subprograms)
# GraphBLAS supports graph operations via linear algebraic methods (e.g. matrix multiplication) over various semirings
# Documentation: https://graphegon.github.io/pygraphblas/pygraphblas/index.html

# Operator	Description	                    GraphBLAS Type
# A @ B	    Matrix Matrix Multiplication	type default PLUS_TIMES semiring
# v @ A	    Vector Matrix Multiplication	type default PLUS_TIMES semiring
# A @ v	    Matrix Vector Multiplication	type default PLUS_TIMES semiring
# A @= B	In-place Matrix Matrix Multiplication	type default PLUS_TIMES semiring
# v @= A	In-place Vector Matrix Multiplication	type default PLUS_TIMES semiring
# A @= v	In-place Matrix Vector Multiplication	type default PLUS_TIMES semiring
# A | B	    Matrix Union	type default SECOND combiner
# A |= B	In-place Matrix Union	type default SECOND combiner
# A & B	    Matrix Intersection	type default SECOND combiner
# A &= B	In-place Matrix Intersection	type default SECOND combiner
# A + B	    Matrix Element-Wise Union	type default PLUS combiner
# A += B	In-place Matrix Element-Wise Union	type default PLUS combiner
# A - B	    Matrix Element-Wise Union	type default MINUS combiner
# A -= B	In-place Matrix Element-Wise Union	type default MINUS combiner
# A * B	    Matrix Element-Wise Intersection	type default TIMES combiner
# A *= B	In-place Matrix Element-Wise Intersection	type default TIMES combiner
# A / B	    Matrix Element-Wise Intersection	type default DIV combiner
# A /= B	In-place Matrix Element-Wise Intersection	type default DIV combiner
# A == B	Compare Element-Wise Union	type default EQ operator
# A != B	Compare Element-Wise Union	type default NE operator
# A < B	    Compare Element-Wise Union	type default LT operator
# A > B	    Compare Element-Wise Union	type default GT operator
# A <= B	Compare Element-Wise Union	type default LE operator
# A >= B	Compare Element-Wise Union	type default GE operator


# Write graphBLAS matrix as a set of integers in a prolog file
def boolean_matrix_to_integers(matrix, name, path):
    
    with open(path, 'w') as prolog:
        for i in range(0, matrix.nrows):
            
            out = 0
            for j in range(matrix.ncols - 1, -1, -1):
                
                # sparse matrix elements can be empty bits
                element = matrix.get(i, j)
                out = (out << 1) | element if element else (out << 1)
                
            prolog.write('%s(%s).\n' % (name, out))


# From a path to a prolog file containing a boolean matrix
# convert it into a graphBLAS matrix for computation
def integers_to_boolean_matrix(path):
    
    bitcodes, dim = parse_prolog_binary_codes(path)
    matrix = gb.Matrix.sparse(gb.BOOL, dim, dim)
    
    
    for row in range(0, len(bitcodes)):
        for col in range(0, len(bitcodes[row])):
            if bitcodes[row][col]:
                matrix[row, col] = True
    
    return matrix
    

# Create an identity matrix
def identity(dim):
    I = gb.Matrix.sparse(gb.BOOL, dim, dim)
    for i in range(dim):
        I[i, i] = True
    return I


# GraphBLAS version of BMLP-RMS algorithm which performs repeated matrix squaring
def BMLP_RMS(R1, print_matrix=False):

    # the dimensions of the matrix, e.g. the number of nodes in a graph
    dim = R1.nrows
    empty_matrix = gb.Matrix.sparse(gb.BOOL, dim, dim)

    # Add identy to the adjacency matrix
    R = identity(dim) + R1
    if print_matrix:
        print('R = R1 + I = \n'+ str(R) + '\n')

    # Iteratively compute the transitive closure using boolean matrix multiplication
    # Initialise closure matrix with an empty matrix
    R_ = empty_matrix
    while True:
        # R = R x R until no new connections are found
        R_ = R @ R
        if print_matrix:
            print('R2* = \n' + str(R_) + '\n')
        if R_.iseq(R):
            break
        R = R_

    # Multiply to remove redundant diagonal elements
    return R_ @ R1


# GraphBLAS version of BMLP-SMP algorithm which performs vector multiplication
def BMLP_SMP(V, R1, print_matrix=False):
    
    # Push the model subset selection into the summation for improved performance
    V_ = V
    while True:
        # Apply vector mutiplication (selection) to the transitive closure
        V_ = V + V @ R1
        if V_.iseq(V):
            break
        V = V_
    
    # Multiple to remove redundant elements
    return V_ @ R1
