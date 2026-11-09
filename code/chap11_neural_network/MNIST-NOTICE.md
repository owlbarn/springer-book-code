# MNIST data attribution and provenance

MNIST handwritten digit database: Yann LeCun, Corinna Cortes and Christopher J. C.
Burges. Original description: http://yann.lecun.com/exdb/mnist/ .

This book uses the existing Owl serialization from the official
https://github.com/owlbarn/owl_dataset repository, pinned in `mnist.json`.
The six downloaded files are verified against Git object IDs, compressed SHA256,
and decoded SHA256. Their serialized bytes are unchanged; image normalization is
performed in memory by each example. No CIFAR/news/other dataset is downloaded.

License declaration: the `ylecun/mnist` publisher's dataset card declares MIT,
including its "Licensing Information" section:
https://huggingface.co/datasets/ylecun/mnist/raw/main/README.md .
The accessed card's SHA256 is recorded in the manifest. This statement describes
that publisher metadata and does not claim Owl's code license covers other data.
Retain this attribution and the source declaration with the MNIST files.

Citation: LeCun, Y., Cortes, C., Burges, C. J. C. (2010), MNIST handwritten digit
database. Owl's README documents the matrix/ndarray representations.
