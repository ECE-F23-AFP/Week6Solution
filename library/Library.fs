﻿namespace library

module Math =
    let rec fact = function
        | 1 -> 1
        | n -> n * fact (n-1)

