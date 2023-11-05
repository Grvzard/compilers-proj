from pathlib import Path
import subprocess


def test():
    exec_path = Path(__file__).parent.parent / 'build' / 'proj1'
    data_path = Path(__file__).parent / 'p1_code.txt'
    p = subprocess.run([exec_path, data_path], stdout=subprocess.PIPE)
    assert p.returncode == 0
    print(p.stdout.decode())


if __name__ == '__main__':
    test()
