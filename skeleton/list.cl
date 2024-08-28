class List inherits IO {
    curr_count : Int;
    max_count : Int;
    elem : Object;
    flag : Int;
    next : List;

    init(e : Object, n : List, curr_c : Int, max_c : Int) : List {
        {
            elem <- e;
            next <- n;
            curr_count <- curr_c;
            max_count <- max_c;
            self;
        }
    };

    getNext():List {
        next
    };

    setNext(arg : List):Object {
        next <- arg
    };

    getElem():Object {
        elem
    };

    setElem(arg : Object):Object {
        elem <- arg
    };

    setMax(arg : Int):Object {
        max_count <- arg
    };

    setCount(arg : Int):Object {
        curr_count <- arg
    };

    setFlag(arg : Int):Object {
        flag <- arg
    };

    getFlag():Int {
        flag
    };

    getMax():Int {
        max_count
    };

    getCount():Int {
        curr_count
    };

    toString():IO { 
        let str : String <-
                    case elem of
                        string_case : String => "String(".concat(string_case).concat(")");
                        int_case : Int => "Int(".concat((new A2I).i2a(int_case)).concat(")");
                        bool_case : Bool => if bool_case = false then "Bool(".concat("false").concat(")") 
                                                                    else "Bool(".concat("true").concat(")") fi;
                        io_case : IO => "IO()";
                        rank_case : Rank => rank_case.toString();
                        product_case : Product => product_case.toString();
                        object_case : Object => {abort(); "";};
                    esac
            in 
                {
                    if curr_count = max_count then out_string("[ ") else out_string("") fi;
                    if getFlag() = 1 then out_string("") else {out_string(str);} fi;
                    if (isvoid next) then out_string(" ]") else {
                        if next.getFlag() = 1 then out_string("") else out_string(", ") fi; 
                        next.toString();
                    } fi;
                }
    };

    merge(other : List):List {
        let i : Int <- 1,
            dec : Bool,
            sec_dec : Bool,
            num : Int,
            max : Int,
            curr_list : List <- self
            in
                {
                    while i <= self.getMax() loop {
                        if curr_list.getFlag() = 1 then dec <- true else dec <- false fi;
                        curr_list <- curr_list.getNext();
                        i <- i + 1;
                    } pool;

                    i <- 1;
                    curr_list <- other;

                    while i <= other.getMax() loop {
                        if curr_list.getFlag() = 1 then sec_dec <- true else sec_dec <- false fi;
                        curr_list <- curr_list.getNext();
                        i <- i + 1;
                    } pool;

                    curr_list <- self;
                    i <- 1;

                    if dec = true then {
                        while i <= self.getMax() - 2 loop {
                            curr_list <- curr_list.getNext();
                            i <- i + 1;
                    } pool;

                    curr_list.setNext(other);
                    curr_list <- self;

                    if sec_dec = false then num <- curr_list.getMax() - 1 + other.getMax()
                        else num <- curr_list.getMax() - 1 + other.getMax() - 1 fi;

                    max <- num;
                    i <- max;

                    while 0 < i loop {
                        curr_list.setCount(num);
                        curr_list.setMax(max);
                        curr_list <- curr_list.getNext();
                        i <- i - 1;
                        num <- num - 1;
                } pool;

                } else {
                        while i < self.getMax() loop {
                        curr_list <- curr_list.getNext();
                        i <- i + 1;
                        } pool;

                        curr_list.setNext(other);
                        curr_list <- self;

                        if sec_dec = false then num <- curr_list.getMax() + other.getMax()
                            else num <- curr_list.getMax() + other.getMax() - 1 fi;

                        max <- num;
                        i <- max;

                        while 0 < i loop {
                            curr_list.setCount(num);
                            curr_list.setMax(max);
                            curr_list <- curr_list.getNext();
                            i <- i - 1;
                            num <- num - 1;
                        } pool;
                }fi;
                    self;
            }
    };

    filterBy(f : Filter):List {
        let res : List <- new List,
            curr_count : Int,
            curr_res : List <- res,
            curr_list : List <- self,
            empty : List,
            valid : Bool,
            iter : Int <- 1,
            max_count : Int
            in
                {
                    while iter <= self.getMax() - 1 loop {
                        valid <- f.filter(curr_list.getElem());
                        if valid = true then {
                            curr_count <- curr_count + 1;
                            max_count <- max_count + 1;
                            curr_res.init(curr_list.getElem(), new List, curr_count, 0);
                            curr_res <- curr_res.getNext();
                            curr_list <- curr_list.getNext();
                        } else curr_list <- curr_list.getNext() fi;

                        iter <- iter + 1;

                    }pool;

                    if 1 < curr_count then {
                        curr_count <- curr_count + 1;
                        max_count <- max_count + 1;
                        curr_res.init("", empty, curr_count, 0);
                        curr_res.setFlag(1);
                        iter <- 1;
                        curr_res <- res;

                        while iter <= max_count loop {
                            curr_res.setMax(max_count);
                            curr_res.setCount(curr_count);
                            curr_res <- curr_res.getNext();
                            iter <- iter + 1;
                            curr_count <- curr_count - 1;
                        } pool;

                        } else {
                            curr_count <- curr_count + 1;
                            max_count <- max_count + 1;
                            res.setMax(max_count);
                            res.setCount(curr_count);
                            curr_count <- curr_count - 1;

                            if curr_count = 0 then curr_count <- 1 else curr_count fi;
                            curr_res.init("", empty, curr_count, max_count);
                            curr_res.setFlag(1); }
                            fi;
                    res;
        }
    };

    sortBy(sort : Comparator, option : Int):List {
        let curr_list : List <- self,
            dec : Int,
            aux : Object,
            again : Bool <- true,
            iter : Int <- 1
            in
                {
                    while again = true loop {
                        again <- false;

                        while iter <= self.getMax() - 2 loop {
                            dec <- sort.compareTo(curr_list.getElem(), curr_list.getNext().getElem());
                            if option = 0 then {
                               if dec <= 0 then {
                                    curr_list <- curr_list.getNext();
                               } else {
                                    aux <- curr_list.getElem();
                                    curr_list.setElem(curr_list.getNext().getElem());
                                    curr_list.getNext().setElem(aux);
                                    curr_list <- curr_list.getNext();
                                    again <- true;
                               } fi;
                        } else {
                            if dec < 0 then {
                                    aux <- curr_list.getElem();
                                    curr_list.setElem(curr_list.getNext().getElem());
                                    curr_list.getNext().setElem(aux);
                                    curr_list <- curr_list.getNext();
                                    again <- true;
                               } else {
                                    curr_list <- curr_list.getNext();
                               }fi;
                         }fi;
                        iter <- iter + 1;
                    }pool;
                        iter <- 1;
                        curr_list <- self;
                }pool;
                self;
        }
    };
};