from collections import defaultdict
from itertools import product

def print_grid(grid):
    for y in range(10):
        print(''.join(str(grid[(x, y)]) for x in range(10)))
    print()

def part1(grid):
    flashes = 0

    for i in range(10):
        for x, y in product(range(10), repeat=2):
            grid[(x, y)] += 1
        changed = True
        flashed = set()
        while changed:
            changed = False
            for x, y in product(range(10), repeat=2):
                if grid[(x, y)] > 9 and (x, y) not in flashed:
                    print(f'flashing: ({x}|{y}) = {grid[(x, y)]}')
                    flashes += 1
                    flashed.add((x, y))
                    changed = True
                    for x_, y_ in product(range(-1, 2), repeat=2):
                        print(f'incrementing ({x+x_}|{y+y_})')
                        grid[(x+x_, y+y_)] += 1
            for x, y in product(range(10), repeat=2):
                if grid[(x, y)] > 9:
                    grid[(x, y)] = 0
        # print(f'After step {i+1}:')
        # print_grid(grid)
    return flashes

if __name__ == '__main__':
    from sys import stdin
    input_ = stdin.readlines()
    grid = defaultdict(lambda: float('-infinity'))
    for x, y in product(range(10), repeat=2):
        grid[(x, y)] = int(input_[y][x])
    
    print('Before any steps:')
    print_grid(grid)

    part1(grid)
