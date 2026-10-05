import argparse
import sys

from .cells import CELLS
from .io import write_dataset
from .seeds import HARNESS_ROOT


def main(argv=None):
    ap = argparse.ArgumentParser(prog="phrf_gen")
    ap.add_argument("--root-kind", required=True, choices=["harness", "pilot", "confirmatory"])
    ap.add_argument("--root-hex", help="64-bit root as hex (pilot/confirmatory only)")
    ap.add_argument("--cells", nargs="+", required=True)
    ap.add_argument("--datasets", type=int, default=1)
    ap.add_argument("--out", required=True)
    a = ap.parse_args(argv)
    if a.root_kind == "harness":
        root = HARNESS_ROOT
    else:
        if not a.root_hex:
            sys.exit("pilot/confirmatory roots must be given explicitly (--root-hex); none exist yet")
        root = int(a.root_hex, 16)
        if root == HARNESS_ROOT:
            sys.exit("the harness root may not be used as a pilot/confirmatory root")
    for cid in a.cells:
        for d in range(a.datasets):
            m = write_dataset(CELLS[cid], root, a.root_kind, d, a.out)
            print(m["file"], m["npz_sha256"])


if __name__ == "__main__":
    main()
