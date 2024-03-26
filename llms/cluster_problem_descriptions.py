import numpy as np
from pathlib import Path
import warnings
import torch
import torch.nn.functional as F
from transformers import AutoTokenizer, AutoModel
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from adjustText import adjust_text
from tqdm import tqdm

warnings.filterwarnings("ignore")


def mean_pooling(model_output, attention_mask):
    token_embeddings = model_output[0]
    input_mask_expanded = attention_mask.unsqueeze(
        -1).expand(token_embeddings.size()).float()
    return torch.sum(token_embeddings * input_mask_expanded, 1) / torch.clamp(input_mask_expanded.sum(1), min=1e-9)


def get_embeddings(texts, model_name='nomic-ai/nomic-embed-text-v1.5'):
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    model = AutoModel.from_pretrained(
        model_name, trust_remote_code=True, safe_serialization=True)
    model.eval()
    embeddings = []
    for text in tqdm(texts, desc='Embedding problems...'):
        inputs = tokenizer(text, return_tensors='pt',
                           padding=True, truncation=True, max_length=1024)
        with torch.no_grad():
            outputs = model(**inputs)
        # apply mean pooling to get the embeddings
        pooled_output = mean_pooling(outputs, inputs['attention_mask'])
        # normalize and reduce dimensionality of embeddings
        pooled_output = F.layer_norm(
            pooled_output, normalized_shape=(pooled_output.shape[1],))
        # adjust dimensionality if necessary
        pooled_output = pooled_output[:, :1024]
        embeddings.append(F.normalize(
            pooled_output, p=2, dim=1).squeeze().numpy())
    return np.array(embeddings)


def plot_embeddings(embeddings, labels, problem_names, output_file=None):
    # use PCA to reduce the dimensionality for 2D plotting
    pca = PCA(n_components=2)
    reduced_embeddings = pca.fit_transform(embeddings)

    # plot the embeddings with different colors for different clusters
    plt.figure(figsize=(12, 12))
    texts = []
    unique_labels = set(labels)

    with open(output_file, 'w') if output_file else dummy_context() as f:
        for label in unique_labels:
            index = [i for i, l in enumerate(labels) if l == label]
            plt.scatter(
                reduced_embeddings[index, 0], reduced_embeddings[index, 1], label=f'Cluster {label}')

            # add text labels for each point and write to file if required
            for i in index:
                texts.append(plt.text(
                    reduced_embeddings[i, 0], reduced_embeddings[i, 1], problem_names[i], ha='center', va='center', fontsize=9))
                if f:
                    f.write(f"{problem_names[i]} {reduced_embeddings[i, 0]} {reduced_embeddings[i, 1]} {label}\n")

        # adjust the text so they don't overlap
        adjust_text(texts, arrowprops=dict(arrowstyle='->', color='k', lw=0.5))

    plt.legend()
    plt.show()


def main(args):
    problems = []
    for file in args.path.rglob("original.py"):
        name = file.parent.name
        name = "_".join(name.split("_")[2:])
        code = file.read_text()
        # get docstring
        docstring = code.split('"""')[1]
        # deindent
        docstring = "\n".join([line.strip() for line in docstring.split("\n")])
        problems += [(name, docstring)]

    # print one example
    print(problems[0][1])

    # embed the problems using an encoder-only transformer model
    embeddings = get_embeddings(
        list([docstring for _, docstring in problems]))

    # cluster the embeddings
    kmeans = KMeans(n_clusters=args.k)
    kmeans.fit(embeddings)
    labels = kmeans.labels_

    # plot the embeddings with cluster labels
    problem_names = list([name for name, _ in problems])
    plot_embeddings(embeddings, labels, problem_names, args.output)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--path', type=Path, required=True,
                        help='Path to directory containing code files')
    parser.add_argument('--output', type=str, required=False,
                        help='Output file to write textual representation of clusters')
    parser.add_argument('-k', type=int, default=3,
                        help='Number of clusters to create')
    args = parser.parse_args()
    main(args)
