import os
from pathlib import Path
from typing import List, Literal, Tuple

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

Classification = Literal["hi", "lo"]


def parse_sexp(text):
    stack = []
    current_list = []
    token = ''
    in_string = False

    i = 0
    while i < len(text):
        char = text[i]

        if char == '"' and (i == 0 or text[i-1] != '\\'):
            if not in_string:
                in_string = True
                token += char
            else:
                in_string = False
                token += char
                current_list.append(token)
                token = ''
        elif in_string:
            token += char
        elif char == '(':
            if token:
                current_list.append(token)
                token = ''
            stack.append(current_list)
            current_list = []
        elif char == ')':
            if token:
                current_list.append(token)
                token = ''
            if stack:
                temp = current_list
                current_list = stack.pop()
                current_list.append(temp)
        elif char.isspace():
            if token:
                current_list.append(token)
                token = ''
        else:
            token += char

        i += 1

    if token:
        current_list.append(token)

    return current_list[0] if current_list else []


class TestCase:
    def __init__(self, original: str, inputs: List[str], output: str):
        self.original = original
        self.inputs = inputs
        self.output = output


class Example:
    def __init__(
            self,
            original: str,
            code: str,
            description: str,
            tests: List[TestCase],
            tests_header: str,
            classification: Classification
    ):
        self.original = original
        self.code = code
        # NOTE: description is in Racket ";;" comments and prefixed with "#lang racket"
        self.description = description
        self.tests = tests
        # NOTE: footer is simply "))"
        self.tests_header = tests_header
        self.classification = classification

    @classmethod
    def from_code(cls, code: str, classification: Classification) -> "Example":
        # step 1: extract tests and parse them out
        og = code
        footer = "(require rackunit)"
        codesplit = code.split(footer)
        code = codesplit[0].strip()
        tests_raw = (footer + codesplit[1]).strip()
        testsplit = tests_raw.split("))")
        tests_header = testsplit[0].strip() + "))"
        tests_raw = "))".join(testsplit[1:-1]).strip()
        print(tests_raw)

        quit()
        # step 2: extract description and clip code
        base_description = "#lang racket\n"
        description = base_description
        nlsplit = code.split("\n")
        for i, line in enumerate(nlsplit):
            if line.startswith(";"):
                description += line + "\n"
            elif description == base_description:  # skip over initial whitespace
                continue
            else:
                description = description.strip()
                code = "\n".join(nlsplit[i:]).strip()
                break
        assert description != base_description, f"Could not find description in {
            code}"

        # step 3: convert tab to spaces, remove any comments (leading and trailing from code) or empty lines
        code = code.replace("\t", "  ")
        nlsplit = code.split("\n")
        newcode = ""
        for i, line in enumerate(nlsplit):
            if line.startswith(";"):
                continue
            else:
                # see if we can find a comment in the line
                comment_idx = line.find(";")
                if comment_idx != -1:
                    line = line[:comment_idx].rstrip()
            if line.strip() != "":
                newcode += line + "\n"

        code = newcode.strip()
        return cls(og, code, description, tests, classification)


class HiLoItem:
    def __init__(self, name: str, his: List[Example], los: List[Example]):
        self.name = name
        self.his = his
        self.los = los


class HiLoDataset:
    def __init__(self, path: Path):
        items = []
        for d in path.iterdir():
            if d.is_dir():
                name = d.stem
                his = []
                los = []
                for hi_path in d.rglob("hi/*.rkt"):
                    his.append(Example.from_code(hi_path.read_text(), "hi"))
                for lo_path in d.rglob("lo/*.rkt"):
                    los.append(Example.from_code(lo_path.read_text(), "lo"))
                if len(his) == 0 or len(los) == 0:
                    continue
                items.append(HiLoItem(name, his, los))
        print(f"Loaded {len(items)} items")
        self.items: List[HiLoDataset] = items


def main(args):
    ds = HiLoDataset(Path(THIS_DIR) / "golden")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    args = parser.parse_args()
    main(args)
