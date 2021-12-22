from collections import defaultdict
from typing import Callable, DefaultDict, Iterable, List, Tuple, TypeVar, Union

T = TypeVar('T')
Point = Tuple[int, int]
Board = Tuple[int, int, DefaultDict[Point, str]]

def interact(f: Callable[[str], str]):
    from sys import stdin, stdout
    stdout.write(f(stdin.read()))

def get_cell(s: str, alg: str):
    return alg[int(s.replace('.', '0').replace('#', '1'), 2)]

def count(x: T, xs: Iterable[T]):
    res = 0
    for i in xs:
        if x == i:
            res += 1
    return res

def enhance(alg : str, image: Union[List[str], Board]) -> Board:
    if isinstance(image, (str, list)):
        # default = '.'
        img = defaultdict(lambda: '.')
        default = get_cell('.' * 9, alg)
        for y, row in enumerate(image):
            for x, cell in enumerate(row):
                img[(x, y)] = cell
            x_range = range(-1, len(image[0]) + 1)
            y_range = range(-1, len(image) + 1)
    else:
        max_x, max_y, image_ = image
        default = get_cell(image_.default_factory() * 9, alg)
        img = defaultdict(lambda: default, image_)
        x_range = range(-1, max_x + 2)
        y_range = range(-1, max_y + 2)
    out = defaultdict(lambda: default)
    for y in y_range:
        for x in x_range:
            s = ''.join(img[(x+dx, y+dy)] for dy in range(-1, 2) for dx in range(-1, 2))
            out[(x+1, y+1)] = get_cell(s, alg)
    return (x_range.stop, y_range.stop, out)

def main(s: str):
    alg, _, *image = s.splitlines()
    part1 = str(count('#', enhance(alg, enhance(alg, image))[-1].values()))
    part2 = image
    for _ in range(50):
        part2 = enhance(alg, part2)
    part2_count = str(count('#', part2[-1].values()))
    return f'{part1}\n{part2_count}\n'

interact(main)
