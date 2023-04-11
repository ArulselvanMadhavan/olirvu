
# Table of Contents

1.  [Oḷirvu](#org5e37f71)
    1.  [To run the app](#orgdc66ecc)
    2.  [Visualizations](#orgd14b49b)
        1.  [Heaps](#org8bdc296)
        2.  [Dynamic Programming](#org077cba5)
        3.  [Quantization](#org8fc4ddb)
    3.  [Known Issues](#org2873036)


<a id="org5e37f71"></a>

# Oḷirvu


<a id="orgdc66ecc"></a>

## To run the app

    make run


<a id="orgd14b49b"></a>

## Visualizations


<a id="org8bdc296"></a>

### Heaps

1.  Red Black Tree

    ![img](./images/rbt.png)

2.  Leftist Heap

    ![img](./images/leftist_heap.png)

3.  Binomial Heap

    ![img](./images/bin_heap.png)


<a id="org077cba5"></a>

### Dynamic Programming

1.  Coin change

    ![img](./images/coin_change.png)


<a id="org8fc4ddb"></a>

### Quantization

![img](./images/quant.png)


<a id="org2873036"></a>

## Known Issues

1.  If the Tree is not rendered with a default tree, the
    layout hides the label of the root node. So, we always start the
    visualization with some defaults
2.  Binomial heap viz - The children of heap are not displayed in
    increasing order of rank.
3.  Binomial heap viz - Multiple roots problem was avoided by
    introducing a root node called "heap"

