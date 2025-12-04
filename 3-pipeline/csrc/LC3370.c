// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

unsigned int clz(unsigned int x)
{
    int n = 32, c = 16;
    do {
        unsigned int y = x >> c;
        if (y) {
            n -= c;
            x = y;
        }
        c >>= 1;
    } while (c);
    return n - x;
}

int smallestNumber(int n) {
    int bit_len = (1 << (32-clz(n)))-1;
    
    return bit_len;
}

int main()
{
    *(int *) (4) = smallestNumber(1);
    *(int *) (8) = smallestNumber(509);
    *(int *) (12) = smallestNumber(1000);
}
