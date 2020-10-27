def solve() -> None:
    a = [int(n) for n in sys.stdin.readlines()]
    print(a[0] - sum(a[1:]))
    eprint("hi")

INPUT = '''
10
1
1
1
1
1
1
'''

OUTPUT = '''
45
'''

# pytest
import sys                              # noqa: E402
import io                               # noqa: E402
def test_solve(capsys) -> None:         # noqa: E302
    sys.stdin = io.StringIO(INPUT.strip())
    solve()
    sys.stdin = sys.__stdin__
    out, err = capsys.readouterr()
    print(out)
    eprint(err)
    assert out == OUTPUT.lstrip()
def eprint(*args, **kwargs):            # noqa: E302
    print(*args, file=sys.stderr, **kwargs)
