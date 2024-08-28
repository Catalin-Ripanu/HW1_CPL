class ListStringTokenizer inherits IO{
    curr_count : Int;
    max_count : Int;
    elem : String;
    next : ListStringTokenizer;

    init(e : String, n : ListStringTokenizer, curr_c : Int, max_c : Int) : ListStringTokenizer {
        {
            elem <- e;
            next <- n;
            curr_count <- curr_c;
            max_count <- max_c;
            self;
        }
    };

    getNext():ListStringTokenizer {
        next
    };

    getElem():String {
        elem
    };

    setMax(arg : Int):Object {
        max_count <- arg
    };

    setCount(arg : Int):Object {
        curr_count <- arg
    };

    getMax():Int {
        max_count
    };

    getCount():Int {
        curr_count
    };

    toString():IO {
        {
            if curr_count = max_count then out_string("[ ") else out_string("") fi;
                out_string(elem);
            if curr_count = 1 then out_string(" ") else out_string(", ") fi;
            if (isvoid next) then out_string("]\n") else next.toString() fi;
        }
    };

    split(str : String, delim : String):ListStringTokenizer {
        let res : ListStringTokenizer <- new ListStringTokenizer,
            elem_list : ListStringTokenizer <- res,
            empty : ListStringTokenizer,
            curr_count : Int,
            max_count : Int,
            iter : Int <- 1,
            i : Int,
            index : Int,
            final_index : Int <- 1
        in
            {
                if (not delim.length() = 1) then res else
                    while index < str.length() loop
                        if str.substr(index, final_index) = " " then {
                            max_count <- max_count + 1;
                            curr_count <- curr_count + 1;
                            if curr_count = 1 then res.init(str.substr(i, index - i), new ListStringTokenizer, curr_count, 0)
                                            else {  elem_list <- elem_list.getNext();
                                                    elem_list.init(str.substr(i, index - i), new ListStringTokenizer, curr_count, 0);
                                                 } fi;
                            i <- index + 1;
                            index <- index + 1;
                        } else index <- index + 1 fi
                    pool fi;

                curr_count <- curr_count + 1;
                max_count <- max_count + 1;

                if 1 < curr_count then {
                    elem_list <- elem_list.getNext();
                    elem_list.init(str.substr(i, index - i), empty, curr_count, 0);
                    i <- 1;
                    elem_list <- res;

                    while i <= max_count loop
                        {
                            elem_list.setMax(max_count);
                            elem_list.setCount(curr_count);
                            elem_list <- elem_list.getNext();
                            i <- i + 1;
                            curr_count <- curr_count - 1;
                        } pool;

                } else
                        elem_list.init(str.substr(i, index - i), empty, curr_count, max_count) fi;
                res;
            }
    };
};

class A2I {
    i2c(i : Int) : String {
    if i = 0 then "0" else
    if i = 1 then "1" else
    if i = 2 then "2" else
    if i = 3 then "3" else
    if i = 4 then "4" else
    if i = 5 then "5" else
    if i = 6 then "6" else
    if i = 7 then "7" else
    if i = 8 then "8" else
    if i = 9 then "9" else
    { abort(); ""; }
        fi fi fi fi fi fi fi fi fi fi
     };

    i2a(i : Int) : String {
    if i = 0 then "0" else 
        if 0 < i then i2a_aux(i) else
          "-".concat(i2a_aux(i * ~1)) 
        fi fi
    };

    i2a_aux(i : Int) : String {
        if i = 0 then "" else 
        (let next : Int <- i / 10 in
        i2a_aux(next).concat(i2c(i - next * 10))
        )
        fi
    };

    c2i(char : String) : Int {
        if char = "0" then 0 else
        if char = "1" then 1 else
        if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        if char = "a" then 97 else
        if char = "b" then 98 else
        if char = "c" then 99 else
        if char = "d" then 100 else
        if char = "e" then 101 else
        if char = "f" then 102 else
        if char = "g" then 103 else
        if char = "h" then 104 else
        if char = "i" then 105 else
        if char = "j" then 106 else
        if char = "k" then 107 else
        if char = "l" then 108 else
        if char = "m" then 109 else
        if char = "n" then 110 else
        if char = "o" then 111 else
        if char = "p" then 112 else
        if char = "q" then 113 else
        if char = "r" then 114 else
        if char = "s" then 115 else
        if char = "t" then 116 else
        if char = "u" then 117 else
        if char = "v" then 118 else
        if char = "w" then 119 else
        if char = "x" then 120 else
        if char = "y" then 121 else
        if char = "z" then 122 else
        { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi 
        fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi 
        fi fi
     };

    a2i(s : String) : Int {
        if s.length() = 0 then 0 else
        if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
           a2i_aux(s)
        fi fi fi
     };

     a2i_aux(s : String) : Int {
        (let int : Int <- 0 in
           {
               (let j : Int <- s.length() in
                  (let i : Int <- 0 in
                    while i < j loop
                        {
                            int <- int * 10 + c2i(s.substr(i,1));
                            i <- i + 1;
                        }
                    pool
                  )
               );
              int;
            }
        )
     };

};

class Comparator {
    compareTo(o1 : Object, o2 : Object):Int {{ abort(); 0; }};
};

class PriceComparator inherits Comparator{
    compareTo(o1 : Object, o2 : Object):Int {
            let val1 : Int <- case o1 of
                        product_case : Product => product_case.getprice();
                        object_case : Object => { abort(); 0; };
                    esac,
                val2 : Int <- case o2 of
                        product_case : Product => product_case.getprice();
                        object_case : Object => { abort(); 0; };
                    esac
            in
                val1 - val2
    };
};

class RankComparator inherits Comparator{
    compareTo(o1 : Object, o2 : Object):Int {
        let val1 : String <- o1.type_name(),
            res : Int,
            val2 : String <- o2.type_name()
            in {
                if val1 = "Private" then if val2 = "Private" then res <- 0 else res <- 0-1 fi
                else
                    if val1 = "Corporal" then {
                        if val2 = "Private" then res <- 1 else
                        if val2 = "Corporal" then res <- 0 else res <- 0-1 fi fi;
                    } else
                            if val1 = "Sergent" then {
                                if val2 = "Private" then res <- 1 else
                                if val2 = "Corporal" then res <- 1 else
                                if val2 = "Sergent" then res <- 0 else res <- 0-1 fi fi fi;
                    } else
                            if val1 = "Officer" then if val2 = "Officer" then res <- 0 else res <- 1 fi
                else { abort(); 0; } fi fi fi fi;
                res;
            }
    };
};

class AlphabeticComparator inherits Comparator{
    compareTo(o1 : Object, o2 : Object):Int {
        let val1 : String <- case o1 of
                        string_case : String => string_case;
                        object_case : Object => { abort(); ""; };
                    esac,
            val2 : String <- case o2 of
                        string_case : String => string_case;
                        object_case : Object => { abort(); ""; };
                    esac,
                    res : Int
            in {
                if val1 < val2 then res <- 0-1 else if val1 = val2 then res <- 0 else res <- 1 fi fi;
                res;
            }
    };
};
class Filter {
    filter(o : Object):Bool {{ abort(); false; }};
};

class ProductFilter inherits Filter {
    filter(o : Object) : Bool { 
       case o of
            product_case : Product => true;
            object_case : Object => false;
        esac
    };
};

class RankFilter inherits Filter {
    filter(o : Object) : Bool { 
        case o of
            rank_case : Rank => true;
            object_case : Object => false;
        esac
    };
};

class SamePriceFilter inherits Filter {
    filter(o : Object) : Bool { 
        case o of
            product_case : Product => {
                if product_case.getprice() = 
                    product_case@Product.getprice() then true else false fi;
            };
            object_case : Object => false;
        esac
    };
};