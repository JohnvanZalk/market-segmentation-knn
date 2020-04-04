# market-segmentation-knn

Targeted marketing relies heavily on customer segmentation. Many companies aquire segmentation (e.g., job levels, organization classifications) from web forms.

However, long web forms can deter customers from getting a product. And, the likelihood that customers complete forms correctly is low because they are trying to get the information behind the form as fast as possible.

This project provides an example of how one segmentation field, job function, can be aquired artificially for improved accuracy and web conversions.

Job functions help pool together people with similar responsibilities. Here are a few examples:

* Information Technology
* Legal
* Media & Communications
* Program & Product Management
* Research
* Sales

The R script in this repository performs a simple ETL process, pulling customers from Salesforce, predicting their job function, and loading them back to Salesforce.

The prediction is done using the k-Nearest Neighbor method where one customer's job function is populated with the most common value from its nearest neighbors (i.e. the customers with the most similar job titles).

<img src= "https://github.com/JohnvanZalk/market-segmentation-knn/blob/master/knn.JPG" width="500">

In the example above, the yellow customer would be assigned the same job function as the blue customers (assuming k=3).
