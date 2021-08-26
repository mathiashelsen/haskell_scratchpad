doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallerNumber x = if x > 100
    then x
    else 2*x
