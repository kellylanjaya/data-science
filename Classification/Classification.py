# -*- coding: utf-8 -*-
"""[Classification] Bluejek Hospital

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1XWmSB5kjiwWcY8eaPS8aUGlINpljlOu7
"""

!apt-get install openjdk-8-jdk-headless -qq > /dev/null
!wget -q https://downloads.apache.org/spark/spark-3.0.2/spark-3.0.2-bin-hadoop2.7.tgz
!tar -xvf spark-3.0.2-bin-hadoop2.7.tgz
!pip install -q findspark
import os
os.environ["JAVA_HOME"] = "/usr/lib/jvm/java-8-openjdk-amd64"
os.environ["SPARK_HOME"] = "/content/spark-3.0.2-bin-hadoop2.7"
import findspark
findspark.init()

from pyspark.sql import SparkSession
from pyspark.sql.functions import when
from pyspark.ml.feature import VectorAssembler, StandardScaler
from pyspark.ml.classification import LogisticRegression
from pyspark.ml.evaluation import BinaryClassificationEvaluator

spark = SparkSession.builder.getOrCreate()

# 1. Load Data
train = spark.read.option("inferSchema", "true").csv("Classification_Train.csv", header = True)
test = spark.read.option("inferSchema", "true").csv("Classification_Test.csv", header = True)

# 2. Select Features
train = train.select("Education Level", "Married", "Salary Income", "Depressed")
test = test.select("Education Level", "Married", "Salary Income", "Depressed")

# 3. Data Preprocessing
train = train.na.drop()
test = test.na.drop()

# 4. Transform Data
train = train.withColumn("Education Level", when(train["Education Level"] == "High", 0).when(train["Education Level"] == "Intermediate", 1).otherwise(2))
train = train.withColumn("Married", when(train["Married"] == "Yes", 0).otherwise(1))
train = train.withColumn("Depressed", when(train["Depressed"] == "Yes", 0).otherwise(1))

test = test.withColumn("Education Level", when(test["Education Level"] == "High", 0).when(test["Education Level"] == "Intermediate", 1).otherwise(2))
test = test.withColumn("Married", when(test["Married"] == "Yes", 0).otherwise(1))
test = test.withColumn("Depressed", when(test["Depressed"] == "Yes", 0).otherwise(1))

# 5. Normalization
col = train.columns
col.remove("Depressed")
train = VectorAssembler(inputCols=col, outputCol="Features").transform(train)
test = VectorAssembler(inputCols=col, outputCol="Features").transform(test)

scaler = StandardScaler(inputCol="Features", outputCol="Normalized")
train = scaler.fit(train).transform(train)
test = scaler.fit(test).transform(test)

# 6. Generate Model
model = LogisticRegression(featuresCol="Normalized", labelCol="Depressed", maxIter=10).fit(train)

prediction = model.transform(test)

# 7. Model Testing dan Evaluation
eval = BinaryClassificationEvaluator(labelCol="Depressed")
print(eval.evaluate(prediction) * 100)

train.show(50)
test.show(50)