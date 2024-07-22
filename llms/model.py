from vllm import LLM, SamplingParams
from typing import Union, List, Dict
import torch
from abc import ABC, abstractmethod

Prompt = Union[str, List[Dict[str, str]]]

def autodetect_dtype_str() -> str:
    if torch.cuda.is_bf16_supported():
        return "bfloat16"
    else:
        return "auto"


class BaseModel(ABC):
    # init method
    def __init__(self, model_name: str):
        self.model_name = model_name

    @abstractmethod
    def generate(self, prompts: List[Prompt], **kwargs) -> List[str]:
        pass

class HFModel(BaseModel):
    def __init__(self, model_name: str):
        self.model_name = model_name
        self.model = LLM(model_name, dtype=autodetect_dtype_str(), enforce_eager=True)

    def generate(self, prompts: List[Prompt], **kwargs) -> List[str]:
        kwargs = kwargs.copy()
        stop = kwargs.pop("stop", [])
        gens = self.model.generate(
          prompts=prompts,
          sampling_params=SamplingParams(
            top_p=kwargs.pop("top_p", 0.95),
            temperature=kwargs.pop("temperature", 0.0),
            max_tokens=kwargs.pop("max_tokens", 128),
            stop=stop,
          )
        )
        return [gen.outputs[0].text for gen in gens]


def model_factory(name: str, kind: str) -> BaseModel:
    if kind == "hf":
        return HFModel(name)
    else:
        raise ValueError(f"Unknown model kind: {kind}")