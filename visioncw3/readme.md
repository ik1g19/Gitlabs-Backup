This project implements an image classification system using densely sampled patches and the Liblinear library. The system is trained on a dataset of images, where each image is divided into small patches and these patches are used to train a classifier.

# Methods Used


Dense patch sampling: The system divides each image into small patches and uses these patches as features for training.

K-means clustering: A k-means algorithm is used to cluster the densely sampled patches into visual words, which are then used as input features for the classifier.

Liblinear: A multi-class classification problem is solved using 15 one-vs-all classifiers implemented using the Liblinear library.


# Code Structure

The code is organized into several classes:


- PatchFeature: This class implements a patch feature extractor that densely samples patches from images.
- HardAssigner: This class trains a k-means clustering algorithm to cluster the densely sampled patches into visual words.
- PatchExtractor: This class uses the trained hard assigner and patch feature extractor to create features for classification.
- Evaluator: This class evaluates the performance of the classifier using a variety of metrics.


# Dependencies

This project depends on the following libraries:


- OpenIMAJ
- Java 8

