from pathlib import Path
from code_exec_server.code_exec_reqs import exec_test_batched, check_executor_alive


def main(args):
    assert check_executor_alive(
        args.executor), "Executor is not alive. Please start it in code_exec_server"
    for d in Path(args.programs_dir).iterdir():
        for refactored in d.glob("*_refactored"):
            files = list(refactored.glob("*.rkt"))
            contents = [f.read_text() for f in files]
            results = exec_test_batched(args.executor, contents, [
                                        ""] * len(contents), lang="racket")
            for f, (p, out) in zip(files, results):
                if not p or "FAILURE" in out or "ERROR" in out:
                    print(f"Error in {f} -- deleting")
                    f.unlink()


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--programs-dir", type=str, default="./v2_processed")
    parser.add_argument("--executor", type=str,
                        default="http://127.0.0.1:8000")
    args = parser.parse_args()
    main(args)
