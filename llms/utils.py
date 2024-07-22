import json
import gzip
from pathlib import Path
from typing import Optional, TypeVar, List



def gunzip_json_write(path: Path, data: dict) -> None:
    """
    Write a dictionary to a gzip-compressed JSON file.
    """
    with gzip.open(str(path), "wt") as f:
        json.dump(data, f)


def gunzip_json_read(path: Path) -> Optional[dict]:
    """
    Read a gzip-compressed JSON file to a dictionary.
    """
    try:
        with gzip.open(path, "rt") as f:
            return json.load(f)
    except Exception:
        return None


def jsonl_reader(file_path: Path):
    """
    A generator that reads a JSONL file and yields each line as a dictionary.
    """
    with open(file_path, "r") as f:
        for line in f:
            yield json.loads(line)


def jsonl_writer(file_path: Path, data):
    """
    Writes a list of dictionaries to a JSONL file.
    """
    with open(file_path, "w") as f:
        for line in data:
            f.write(json.dumps(line) + "\n")


def markdown_codeblock_extract(new: str) -> str:
    """
    Extracts the first markdown codeblock from the given string.
    """
    lines = new.split("\n")
    buf = ""
    in_codeblock = False
    for ln in lines:
        if ln.startswith("```"):
            if in_codeblock:
                break
            else:
                in_codeblock = True
        elif in_codeblock:
            buf += ln + "\n"
    return buf

T = TypeVar("T")


def chunkify(lst: List[T], n: int) -> List[List[T]]:
    chunks = []
    for i in range(0, len(lst), n):
        chunk = []
        for j in range(n):
            if i + j < len(lst):
                chunk.append(lst[i + j])
        chunks.append(chunk)
    return chunks