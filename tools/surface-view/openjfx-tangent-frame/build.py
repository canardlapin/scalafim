#!/usr/bin/env python3
"""Build and check the opt-in OpenJFX 24.0.1 tangent-frame repair."""

import argparse
import hashlib
import json
import shutil
import subprocess
import zipfile
from pathlib import Path


HERE = Path(__file__).resolve().parent
MEMBER = "com/sun/prism/impl/MeshUtil.java"
SOURCE_JAR_SHA = "fb2d327f60ca45af83598aa5edbade751e2373515169d1213515a518d40f9c29"
SOURCE_SHA = "32d3d720b7accbd290cd71388b3e6f87f696ef7e184c7815407ea169d34c9ef8"
MODULE_SHAS = {
    "javafx.base": "0ea913712b982e24be7fb20d1a90b45dafc38403f49dd9d494d1d0d3b43e8794",
    "javafx.graphics": "90b9c1c89ebd0d79a8af6951517516828cd08896def96871af015cf0a09cdf53",
}


def sha(data):
    return hashlib.sha256(data).hexdigest()


def run(argv, output, name, expected=0):
    with (output / (name + ".log")).open("xb") as log:
        result = subprocess.run(argv, stdout=log, stderr=subprocess.STDOUT, timeout=180)
    (output / (name + ".json")).write_text(json.dumps({
        "argv": argv, "exit_code": result.returncode,
    }, indent=2) + "\n")
    if result.returncode != expected:
        raise RuntimeError(f"{name}: exit {result.returncode}; see {output / (name + '.log')}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sources", type=Path, required=True)
    parser.add_argument("--module-path", required=True)
    parser.add_argument("--javac", required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--verify-native", action="store_true", help="Requires a native display and ES2")
    args = parser.parse_args()
    output = args.output.resolve()
    if output.exists():
        raise ValueError(f"Refusing existing output {output}")
    source_bytes = args.sources.read_bytes()
    if sha(source_bytes) != SOURCE_JAR_SHA:
        raise ValueError("Expected the checksum-pinned OpenJFX 24.0.1 source JAR")
    module_paths = [Path(p).resolve() for p in args.module_path.split(":")]
    module_hashes = {str(p): sha(p.read_bytes()) for p in module_paths}
    for name, expected in MODULE_SHAS.items():
        if expected not in module_hashes.values():
            raise ValueError(f"Missing checksum-pinned {name} 24.0.1 mac-aarch64 module")
    javac = Path(shutil.which(args.javac) or args.javac).resolve()
    java = str(javac.with_name("java"))
    module_path = ":".join(map(str, module_paths))
    output.mkdir(parents=True)
    source_root = output / "source"
    source = source_root / MEMBER
    source.parent.mkdir(parents=True)
    with zipfile.ZipFile(args.sources) as archive:
        original = archive.read(MEMBER)
    if sha(original) != SOURCE_SHA:
        raise ValueError("Unexpected MeshUtil source")
    source.write_bytes(original)
    patch = HERE / "MeshUtil.patch"
    run(["git", "apply", "--unsafe-paths", "--directory", str(source_root), str(patch)], output, "apply")
    classes = output / "classes"
    tests = output / "tests"
    classes.mkdir()
    tests.mkdir()
    common = [str(javac), "-Werror", "--release", "22", "--module-path", module_path]
    run(common + ["--patch-module", "javafx.graphics=" + str(source_root),
                  "-d", str(classes), str(source)], output, "compile")
    run(common + ["--patch-module", "javafx.graphics=" + str(HERE),
                  "-d", str(tests), str(HERE / "TangentSpaceProbe.java")], output, "compile-test")
    test_main = ["-m", "javafx.graphics/com.sun.prism.impl.TangentSpaceProbe"]
    # Preserve the real stock failure; the acceptance run below must pass unchanged.
    run([java, "--module-path", module_path, "--patch-module", "javafx.graphics=" + str(tests)]
        + test_main, output, "stock-cpu", expected=1)
    run([java, "--module-path", module_path, "--patch-module",
         "javafx.graphics=" + str(classes) + ":" + str(tests)] + test_main, output, "fixed-cpu")
    jar = output / "openjfx-24.0.1-tangent-frame.jar"
    entries = {str(p.relative_to(classes)): p.read_bytes() for p in classes.rglob("*.class")}
    if set(entries) != {"com/sun/prism/impl/MeshUtil.class"}:
        raise ValueError("Unexpected compiled class set")
    entries["META-INF/source/" + MEMBER] = source.read_bytes()
    entries["META-INF/MeshUtil.patch"] = patch.read_bytes()
    for name in ("LICENSE", "ADDITIONAL_LICENSE_INFO", "ASSEMBLY_EXCEPTION"):
        entries["META-INF/" + name] = (HERE / name).read_bytes()
    entries["META-INF/NOTICE"] = (
        "OpenJFX 24.0.1 MeshUtil tangent-frame compatibility repair.\n"
        "Oracle/OpenJFX copyright and GPLv2 with Classpath Exception apply; see included source and licenses.\n"
        "Opt-in --patch-module component; requires OpenJFX 24.0.1+4.\n"
    ).encode()
    with zipfile.ZipFile(jar, "x", compression=zipfile.ZIP_STORED) as archive:
        for name, data in sorted(entries.items()):
            info = zipfile.ZipInfo(name, (1980, 1, 1, 0, 0, 0))
            info.external_attr = 0o100644 << 16
            archive.writestr(info, data)
    if args.verify_native:
        run(common + ["--add-modules", "javafx.graphics", "-d", str(tests),
                      str(HERE / "PublicEmissionControl.java")], output, "compile-native")
        native = [java, "--enable-native-access=javafx.graphics", "--module-path", module_path,
                  "--add-modules", "javafx.graphics", "-Dprism.order=es2", "-cp", str(tests)]
        run(native + ["PublicEmissionControl", str(output / "stock-native-images")],
            output, "stock-native", expected=1)
        run(native + ["--patch-module", "javafx.graphics=" + str(jar),
                      "PublicEmissionControl", str(output / "fixed-native-images")], output, "fixed-native")
    receipt = {
        "schema": "scalafim.openjfx-tangent-frame.v1", "runtime": "24.0.1+4",
        "sourceJarSha256": SOURCE_JAR_SHA, "sourceSha256": SOURCE_SHA,
        "patchSha256": sha(patch.read_bytes()), "patchedSourceSha256": sha(source.read_bytes()),
        "moduleSha256": module_hashes, "compilerSha256": sha(javac.read_bytes()),
        "compilerVersion": subprocess.check_output([str(javac), "-version"], text=True).strip(),
        "jarSha256": sha(jar.read_bytes()), "cpu": "pass", "stockCpu": "reproduced-failure",
        "native": "pass" if args.verify_native else "not-run",
    }
    (output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print(json.dumps({"jar": str(jar), "sha256": receipt["jarSha256"],
                      "cpu": receipt["cpu"], "native": receipt["native"]}, indent=2))


if __name__ == "__main__":
    main()
