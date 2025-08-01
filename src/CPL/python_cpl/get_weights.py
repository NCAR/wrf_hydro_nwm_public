def get_weights(weights):
    print("Python side received:", weights)
    return [w + 1 for w in weights]
