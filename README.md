
# Table of Contents

1.  [Oḷirvu](#org6950940)
    1.  [To run the app](#org5d7a7fb)
    2.  [Visualizations](#org99a5633)
        1.  [Heaps](#org5a7126b)
        2.  [Dynamic Programming](#org97a7616)
    3.  [Known Issues](#org844a558)


<a id="org6950940"></a>

# Oḷirvu


<a id="org5d7a7fb"></a>

## To run the app

    make run


<a id="org99a5633"></a>

## Visualizations


<a id="org5a7126b"></a>

### Heaps

1.  Red Black Tree

    ![img](./images/rbt.png)

2.  Leftist Heap

    ![img](./images/leftist_heap.png)

3.  Binomial Heap

    ![img](./images/bin_heap.png)


<a id="org97a7616"></a>

### Dynamic Programming

1.  Coin change

    ![img](./images/coin_change.png)


<a id="org844a558"></a>

## Known Issues

1.  If the Tree is not rendered with a default tree, the
    layout hides the label of the root node. So, we always start the
    visualization with some defaults
2.  Binomial heap viz - The children of heap are not displayed in
    increasing order of rank.
3.  Binomial heap viz - Multiple roots problem was avoided by
    introducing a root node called "heap"

