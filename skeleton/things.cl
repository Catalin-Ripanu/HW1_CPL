class Product {
    name : String;
    model : String;
    price : Int;

    init(n : String, m: String, p : Int):SELF_TYPE {{
        name <- n;
        model <- m;
        price <- p;
        self;
    }};

    getName():String {
        name
    };

    getprice():Int{ price * 119 / 100 };

    toString():String {
        {
        type_name().concat("(").concat(name).concat(",").concat(model).concat(")");
        }
    };
};

class Edible inherits Product {

    getprice():Int { price * 109 / 100 };
};

class Soda inherits Edible {

    getprice():Int {price * 109 / 100 + 20};
};

class Coffee inherits Edible {

    getprice():Int {price * 119 / 100};
};

class Laptop inherits Product {

    getprice():Int {price * 119 / 100 + 499};
};

class Router inherits Product {};

class Rank {
    name : String;

    init(n : String):SELF_TYPE {
        {
        name <- n;
        self;
        }
    };

    toString():String {
        {
            type_name().concat("(").concat(name).concat(")");
        }
    };
};

class Private inherits Rank {};

class Corporal inherits Private {};

class Sergent inherits Corporal {};

class Officer inherits Sergent {};