#!/usr/bin/env python3
"""Extract the first two datasets of every frozen CMRG control cell for CI."""
import argparse
import json
import struct
from pathlib import Path
from verify_inputs import verify


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("control", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    receipt, _ = verify(args.control)
    assert receipt["phase"] == "control"
    lines = ["package scalafim.fmri.laws", "",
             "/** Independent R CMRG inputs and dense retained-row GLS oracle; generated from the frozen control packet. */",
             "private[laws] object FglsCmrgFixtures:",
             "  final case class Example(cell: String, replicate: Int, noise: String, expected: String)",
             "  val values: Vector[Example] = Vector("]
    examples = []
    for item in receipt["inputs"]:
        cell = item["cell"]
        count = item["shape"][1] * item["shape"][2]
        with (args.control / item["file"]).open("rb") as stream:
            noise = struct.unpack("<" + "d" * (count * 2), stream.read(count * 2 * 8))
        oracle = json.loads((args.control / (cell + ".oracle.json")).read_text())
        for dataset in range(2):
            selected = [{key: value[dataset] if isinstance(value, list) else value
                         for key, value in voxel.items()} for voxel in oracle]
            csv = ",".join(repr(x) for x in noise[dataset * count:(dataset + 1) * count])
            expected = json.dumps(selected, separators=(",", ":"))
            examples.append(f"    Example({json.dumps(cell)}, {dataset},\n"
                            f"      {json.dumps(csv)},\n      {json.dumps(expected)})")
    args.output.write_text("\n".join(lines) + "\n" + ",\n".join(examples) + "\n  )\n")


if __name__ == "__main__":
    main()
