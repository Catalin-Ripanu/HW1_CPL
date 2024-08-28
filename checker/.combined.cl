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
};class Main inherits IO{
    lists : List;
    next_list : List;
    lists_num : Int;
    max_num : Int;
    tokenizer : ListStringTokenizer;
    looping : Bool <- true;
    somestr : String;

    helper():Object {
            {
                let curr_count : Int,
                    max_count : Int,
                    convert : A2I <- new A2I,
                    res : List <- new List,
                    elem_list_res : List <- res,
                    tokens_list : ListStringTokenizer,
                    empty : List
                in
                  {
                    while (not somestr = "END") loop {
                        tokens_list <- tokenizer.split(somestr, " ");
                        if curr_count = 0 then {
                            if tokens_list.getElem() = "Soda" then {
                             let product_case : Product <- (new Soda).init(tokens_list.getNext().getElem(), 
                                tokens_list.getNext().getNext().getElem(), 
                                convert.a2i(tokens_list.getNext().getNext().getNext().getElem()))
                                in
                                    res.init(product_case, new List, curr_count, 0);
                        } else
                            if tokens_list.getElem() = "Coffee" then {
                                let product_case : Product <- (new Coffee).init(tokens_list.getNext().getElem(), 
                                    tokens_list.getNext().getNext().getElem(), 
                                    convert.a2i(tokens_list.getNext().getNext().getNext().getElem()))
                                in
                                    res.init(product_case, new List, curr_count, 0);
                        } else
                            if tokens_list.getElem() = "Laptop" then {
                                let product_case : Product <- (new Laptop).init(tokens_list.getNext().getElem(), 
                                    tokens_list.getNext().getNext().getElem(), 
                                    convert.a2i(tokens_list.getNext().getNext().getNext().getElem()))
                                in 
                                    res.init(product_case, new List, curr_count, 0);
                        } else
                            if tokens_list.getElem() = "Router" then {
                                let product_case : Product <- (new Router).init(tokens_list.getNext().getElem(), 
                                    tokens_list.getNext().getNext().getElem(), 
                                    convert.a2i(tokens_list.getNext().getNext().getNext().getElem()))
                                in
                                    res.init(product_case, new List, curr_count, 0);
                        } else
                            if tokens_list.getElem() = "Officer" then {
                                let rank_case : Rank <- (new Officer).init(tokens_list.getNext().getElem())

                                in 
                                    res.init(rank_case, new List, curr_count, 0);
                        } else
                            if tokens_list.getElem() = "Sergent" then {
                                let rank_case : Rank <- (new Sergent).init(tokens_list.getNext().getElem())

                                in 
                                    res.init(rank_case, new List, curr_count, 0);
                        } else
                            if tokens_list.getElem() = "Corporal" then {
                                let rank_case : Rank <- (new Corporal).init(tokens_list.getNext().getElem())

                                in
                                    res.init(rank_case, new List, curr_count, 0);
                        } else
                            if tokens_list.getElem() = "Private" then {
                                let rank_case : Rank <- (new Private).init(tokens_list.getNext().getElem())

                                in
                                    res.init(rank_case, new List, curr_count, 0);
                        } else
                            if tokens_list.getElem() = "IO" then res.init(new IO, new List, curr_count, 0) 
                          else
                            if tokens_list.getElem() = "Int" then res.init(convert.a2i(tokens_list.getNext().getElem()), new List, curr_count, 0)
                          else
                            if tokens_list.getElem() = "String" then res.init(tokens_list.getNext().getElem(), new List, curr_count, 0)
                          else
                            if tokens_list.getElem() = "Bool" then {
                                if tokens_list.getNext().getElem() = "true" then res.init(true, new List, curr_count, 0)
                          else
                            res.init(false, new List, curr_count, 0) fi;
                        } else {out_string("Invalid type\n"); abort(); } fi fi fi fi fi fi fi fi fi fi fi fi;
                        } else {
                            if tokens_list.getElem() = "Soda" then {
                                let product_case : Product <- (new Soda).init(tokens_list.getNext().getElem(), 
                                    tokens_list.getNext().getNext().getElem(), 
                                    convert.a2i(tokens_list.getNext().getNext().getNext().getElem()))
                                in 
                                {
                                    elem_list_res <- elem_list_res.getNext();
                                    elem_list_res.init(product_case, new List, curr_count, 0);
                                };
                        } else
                            if tokens_list.getElem() = "Coffee" then {
                                let product_case : Product <- (new Coffee).init(tokens_list.getNext().getElem(), 
                                    tokens_list.getNext().getNext().getElem(), 
                                    convert.a2i(tokens_list.getNext().getNext().getNext().getElem()))
                                in
                                {
                                    elem_list_res <- elem_list_res.getNext();
                                    elem_list_res.init(product_case, new List, curr_count, 0);
                                };
                        } else
                            if tokens_list.getElem() = "Laptop" then {
                                let product_case : Product <- (new Laptop).init(tokens_list.getNext().getElem(), 
                                    tokens_list.getNext().getNext().getElem(), 
                                    convert.a2i(tokens_list.getNext().getNext().getNext().getElem()))
                                in
                                {
                                    elem_list_res <- elem_list_res.getNext();
                                    elem_list_res.init(product_case, new List, curr_count, 0);
                                };
                        } else
                            if tokens_list.getElem() = "Router" then {
                                let product_case : Product <- (new Router).init(tokens_list.getNext().getElem(), 
                                    tokens_list.getNext().getNext().getElem(), 
                                    convert.a2i(tokens_list.getNext().getNext().getNext().getElem()))
                                in
                                {
                                    elem_list_res <- elem_list_res.getNext();
                                    elem_list_res.init(product_case, new List, curr_count, 0);
                                };
                        } else
                            if tokens_list.getElem() = "Officer" then {
                                let rank_case : Rank <- (new Officer).init(tokens_list.getNext().getElem())
                                in
                                {
                                    elem_list_res <- elem_list_res.getNext();
                                    elem_list_res.init(rank_case, new List, curr_count, 0);
                                };
                        } else
                            if tokens_list.getElem() = "Sergent" then {
                                let rank_case : Rank <- (new Sergent).init(tokens_list.getNext().getElem())
                                in
                                {
                                    elem_list_res <- elem_list_res.getNext();
                                    elem_list_res.init(rank_case, new List, curr_count, 0);
                                };
                        } else
                            if tokens_list.getElem() = "Corporal" then {
                                let rank_case : Rank <- (new Corporal).init(tokens_list.getNext().getElem())
                                in
                                {
                                    elem_list_res <- elem_list_res.getNext();
                                    elem_list_res.init(rank_case, new List, curr_count, 0);
                                };
                        } else
                            if tokens_list.getElem() = "Private" then {
                                let rank_case : Rank <- (new Private).init(tokens_list.getNext().getElem())
                                in
                                {
                                    elem_list_res <- elem_list_res.getNext();
                                    elem_list_res.init(rank_case, new List, curr_count, 0);
                                };
                        } else
                            if tokens_list.getElem() = "IO" then {
                                elem_list_res <- elem_list_res.getNext();
                                elem_list_res.init(new IO, new List, curr_count, 0);
                        } else
                            if tokens_list.getElem() = "Int" then {
                                elem_list_res <- elem_list_res.getNext();
                                elem_list_res.init(convert.a2i(tokens_list.getNext().getElem()), new List, curr_count, 0);
                        } else
                            if tokens_list.getElem() = "String" then {
                                elem_list_res <- elem_list_res.getNext();
                                elem_list_res.init(tokens_list.getNext().getElem(), new List, curr_count, 0);
                        } else
                            if tokens_list.getElem() = "Bool" then {
                                if tokens_list.getNext().getElem() = "true" then {
                                    elem_list_res <- elem_list_res.getNext();
                                    elem_list_res.init(true, new List, curr_count, 0);
                                }
                          else
                                {
                                    elem_list_res <- elem_list_res.getNext();
                                    elem_list_res.init(false, new List, curr_count, 0);
                                } fi;
                        } else {out_string("Invalid type\n"); abort(); } fi fi fi fi fi fi fi fi fi fi fi fi;
                    } fi;

                    max_count <- max_count + 1;
                    curr_count <- curr_count + 1;
                    somestr <- in_string();

                } pool;

                curr_count <- curr_count + 1;
                max_count <- max_count + 1;

                if 1 < curr_count then {
                    elem_list_res <- elem_list_res.getNext();
                    elem_list_res.init("", empty, curr_count, 0);
                    elem_list_res.setFlag(1);
                    let i : Int
                        in {
                            elem_list_res <- res;
                    while i < max_count - 1 loop
                        {
                            elem_list_res.setMax(max_count);
                            elem_list_res.setCount(curr_count);
                            elem_list_res <- elem_list_res.getNext();
                            i <- i + 1;
                            curr_count <- curr_count - 1;
                        } pool;
                    };
                } else
                        {
                            elem_list_res.setMax(max_count);
                            elem_list_res.setCount(curr_count);
                        } fi;
                lists_num <- lists_num + 2;

                if lists_num = 2 then lists.init(res, new List, lists_num, 0) else
                    {
                        lists_num <- lists_num - 1;
                        next_list.setFlag(0);
                        next_list <- next_list.getNext();
                        next_list.init(res, new List, lists_num, 0);
                        next_list.setFlag(1);
                    } fi;
            };
        }
    };

    createList():Object {
        {
            if 1 <= lists_num then {somestr <- in_string(); helper();} else helper() fi;
        }
    };

    decision():Object {
        {
            let token_list : ListStringTokenizer <- tokenizer.split(somestr, " "),
                convert : A2I <- new A2I
                in
                    {
                        if token_list.getElem() = "help" then 
                        out_string("Available commands: exit, load, print, merge, filterBy, sortBy\n") else
                        if lists_num = 0 then if token_list.getElem() = "" then {out_string("Invalid command\n"); abort(); } else createList() fi else
                        if token_list.getElem() = "load" then {
                        if 1 < token_list.getMax() then {out_string("Invalid command\n"); abort(); } else createList() fi;
                    } else
                        if token_list.getElem() = "print" then {
                            if 2 < token_list.getMax() then {out_string("Invalid command\n"); abort(); } else {
                                if token_list.getMax() = 1 then { if lists_num = 0 then {out_string("No objects in list\n"); } else
                                    let i : Int <- 0,
                                        curr_list : List <- lists
                                    in
                                        while i < lists_num - 1 loop {
                                            out_string(convert.i2a(i + 1).concat(": "));
                                            case curr_list.getElem() of
                                                list_case : List => {
                                                    list_case.toString();
                                                    out_string("\n");
                                                    curr_list <- curr_list.getNext();
                                                    };
                                                object_case : Object => {abort(); "";}; 
                                            esac;

                                            i <- i + 1;
                                        } pool
                                    fi;
                                } else
                                    if lists_num - 1 < convert.a2i(token_list.getNext().getElem()) 
                                        then {out_string("Index out of bounds\n"); abort(); }
                                            else
                                                if convert.a2i(token_list.getNext().getElem()) <= 0 
                                                    then {out_string("Index out of bounds\n"); abort(); }
                                            else
                                                let i : Int <- 1,
                                                    curr_list : List <- lists
                                                in {
                                                    while i <= convert.a2i(token_list.getNext().getElem())  - 1 loop {
                                                        curr_list <- curr_list.getNext();
                                                        i <- i + 1;
                                                    } pool;

                                                    case curr_list.getElem() of
                                                        list_case : List => { 
                                                        list_case.toString();
                                                        out_string("\n");
                                                    };
                                                        object_case : Object => {abort(); "";};
                                                    esac;
                                                }
                                    fi fi fi;
                                } fi;
                    } else
                        if token_list.getElem() = "merge" then {
                            if 3 = token_list.getMax() then {
                                if lists_num - 1 < convert.a2i(token_list.getNext().getElem()) 
                                    then {out_string("Index out of bounds\n"); abort(); }
                                        else
                                            if convert.a2i(token_list.getNext().getElem()) <= 0 
                                                then {out_string("Index out of bounds\n"); abort(); }
                            else
                                if lists_num - 1 < convert.a2i(token_list.getNext().getNext().getElem()) 
                                    then {out_string("Index out of bounds\n"); abort(); }
                                        else
                                            if convert.a2i(token_list.getNext().getNext().getElem()) <= 0 
                                                then {out_string("Index out of bounds\n"); abort(); }
                            else
                                if convert.a2i(token_list.getNext().
                                                getNext().getElem()) <= convert.a2i(token_list.getNext().getElem())
                                    then {out_string("Invalid arguments for 'merge'\n"); abort(); }
                            else
                                let list1 : List <- lists,
                                    list2 : List <- lists,
                                    elem_l1 : List,
                                    elem_l2 : List,
                                    start_list : List <- lists,
                                    end_list : List <- lists,
                                    prev : List <- lists,
                                    curr_list : List,
                                    empty : List,
                                    iter1 : Int <- 1,
                                    iter2 : Int <- 1
                                in {
                                    while iter1 <= convert.a2i(token_list.getNext().getElem())  - 1 loop{
                                        list1 <- list1.getNext();
                                        iter1 <- iter1 + 1;
                                    }pool;

                                    while iter2 <= convert.a2i(token_list.getNext().getNext().getElem())  - 1 loop{
                                        list2 <- list2.getNext();
                                        iter2 <- iter2 + 1;
                                    }pool;

                                    case list1.getElem() of
                                        list_case : List => { 
                                            elem_l1 <- list_case;
                                        };
                                        object_case : Object => {abort(); "";};
                                    esac;

                                    case list2.getElem() of
                                        list_case : List => { 
                                            elem_l2 <- list_case;
                                        };
                                        object_case : Object => {abort(); "";};
                                    esac;

                                    elem_l1 <- elem_l1.merge(elem_l2);
                                    list1.setElem(elem_l1);
                                    lists_num <- lists_num - 1;
                                    iter1 <- 1;

                                    while (not start_list = list1) loop{
                                        prev <- start_list;
                                        start_list <- start_list.getNext();
                                    }pool;

                                    while iter1 <= lists_num - 1 loop{
                                        end_list <- end_list.getNext();
                                        iter1 <- iter1 + 1;
                                    }pool;

                                    if list2.getFlag() = 0 then {
                                        list1.setNext(empty);
                                        prev.setNext(list2.getNext());
                                        list1.setFlag(1);
                                        end_list.setFlag(0);
                                        end_list.setNext(list1);
                                        list2.setNext(empty);
                                        curr_list <- lists;
                                    } else {
                                         list1.setNext(empty);
                                         list2.setNext(empty);
                                         curr_list <- lists;
                                         list1.setFlag(1);
                                    }fi;

                                    while iter1 <= lists_num - 1 loop{
                                        curr_list.setCount(lists_num);
                                        curr_list <- curr_list.getNext();
                                    }pool;
                                } fi fi fi fi fi;
                            } else {out_string("Invalid command\n"); abort(); } fi;
                    } else
                        if token_list.getElem() = "filterBy" then {
                            if 3 = token_list.getMax() then {
                                if lists_num - 1 < convert.a2i(token_list.getNext().getElem()) 
                                    then {out_string("Index out of bounds\n"); abort(); }
                                        else
                                            if convert.a2i(token_list.getNext().getElem()) <= 0 
                                                then {out_string("Index out of bounds\n"); abort(); }
                            else
                                if token_list.getNext().getNext().getElem() = "ProductFilter" then {
                                    let i : Int <- 1,
                                        curr_list : List <- lists
                                    in {
                                        while i <= convert.a2i(token_list.getNext().getElem())  - 1 loop {
                                        curr_list <- curr_list.getNext();
                                        i <- i + 1;
                                        } pool;

                                        case curr_list.getElem() of
                                            list_case : List => { 
                                            list_case <- list_case.filterBy(new ProductFilter);
                                            curr_list.setElem(list_case);
                                            };
                                            object_case : Object => {abort(); "";};
                                        esac;
                                    };
                                } else
                                    if token_list.getNext().getNext().getElem() = "RankFilter" then {
                                        let i : Int <- 1,
                                            curr_list : List <- lists
                                        in {
                                            while i <= convert.a2i(token_list.getNext().getElem())  - 1 loop {
                                            curr_list <- curr_list.getNext();
                                            i <- i + 1;
                                            } pool;

                                            case curr_list.getElem() of
                                                list_case : List => { 
                                                    list_case <- list_case.filterBy(new RankFilter);
                                                    curr_list.setElem(list_case);
                                                };
                                                object_case : Object => {abort(); "";};
                                            esac;
                                            };
                                } else 
                                    if token_list.getNext().getNext().getElem() = "SamePriceFilter" then {
                                        let i : Int <- 1,
                                            curr_list : List <- lists
                                        in {
                                            while i <= convert.a2i(token_list.getNext().getElem())  - 1 loop {
                                                curr_list <- curr_list.getNext();
                                                i <- i + 1;
                                            } pool;

                                            case curr_list.getElem() of
                                                list_case : List => { 
                                                    list_case <- list_case.filterBy(new SamePriceFilter);
                                                    curr_list.setElem(list_case);
                                                };
                                                object_case : Object => {abort(); "";};
                                            esac;
                                            };
                                    } else {out_string("Invalid command\n"); abort(); } fi fi fi fi fi;
                            } else {out_string("Invalid command\n"); abort(); } fi;
                    } else
                        if token_list.getElem() = "sortBy" then {
                            if 4 = token_list.getMax() then {
                                if lists_num - 1 < convert.a2i(token_list.getNext().getElem()) 
                                    then {out_string("Index out of bounds\n"); abort(); }
                                        else
                                            if convert.a2i(token_list.getNext().getElem()) <= 0 
                                                then {out_string("Index out of bounds\n"); abort(); }
                            else
                                if token_list.getNext().getNext().getElem() = "PriceComparator" then {
                                    let i : Int <- 1,
                                        curr_list : List <- lists
                                    in {
                                        while i <= convert.a2i(token_list.getNext().getElem())  - 1 loop {
                                            curr_list <- curr_list.getNext();
                                            i <- i + 1;
                                        } pool;

                                        case curr_list.getElem() of
                                            list_case : List => {
                                                if token_list.getNext().getNext().getNext().getElem() = "ascendent" then
                                                    list_case <- list_case.sortBy(new PriceComparator, 0)
                                                else 
                                                    if token_list.getNext().getNext().getNext().getElem() = "descendent" then
                                                        list_case <- list_case.sortBy(new PriceComparator, 1) 
                                                else {out_string("Invalid command\n"); abort(); } fi fi;
                                                    curr_list.setElem(list_case);
                                            };
                                            object_case : Object => {abort(); "";};
                                        esac;
                                    };
                                } else
                                    if token_list.getNext().getNext().getElem() = "RankComparator" then {
                                        let i : Int <- 1,
                                            curr_list : List <- lists
                                        in {
                                            while i <= convert.a2i(token_list.getNext().getElem())  - 1 loop {
                                                curr_list <- curr_list.getNext();
                                                i <- i + 1;
                                            } pool;

                                            case curr_list.getElem() of
                                                list_case : List => { 
                                                    if token_list.getNext().getNext().getNext().getElem() = "ascendent" then
                                                        list_case <- list_case.sortBy(new RankComparator, 0)
                                                            else 
                                                                if token_list.getNext().getNext().getNext().getElem() = "descendent" then
                                                                    list_case <- list_case.sortBy(new RankComparator, 1) 
                                                                    else {out_string("Invalid command\n"); abort(); } fi fi;
                                                                  curr_list.setElem(list_case);
                                                };
                                                object_case : Object => {abort(); "";};
                                            esac;
                                            };
                                } else
                                    if token_list.getNext().getNext().getElem() = "AlphabeticComparator" then {
                                        let i : Int <- 1,
                                            curr_list : List <- lists
                                        in {
                                            while i <= convert.a2i(token_list.getNext().getElem())  - 1 loop {
                                                curr_list <- curr_list.getNext();
                                                i <- i + 1;
                                            } pool;

                                            case curr_list.getElem() of
                                                list_case : List => { 
                                                    if token_list.getNext().getNext().getNext().getElem() = "ascendent" then
                                                        list_case <- list_case.sortBy(new AlphabeticComparator, 0)
                                                    else
                                                        if token_list.getNext().getNext().getNext().getElem() = "descendent" then
                                                            list_case <- list_case.sortBy(new AlphabeticComparator, 1) 
                                                    else {out_string("Invalid command\n"); abort(); } fi fi;
                                                    curr_list.setElem(list_case);
                                                };
                                                object_case : Object => {abort(); "";};
                                            esac;
                                            };
                                } else {out_string("Invalid command\n"); abort(); } fi fi fi fi fi;
                        } else {out_string("Invalid command\n"); abort(); } fi;
                    } else
                        if token_list.getElem() = "exit" then {out_string("Exiting...\n"); abort();} else
                            {abort(); } fi fi fi fi fi fi fi fi;
            };
        }
    };

    main():Object {
    {
        lists <- new List;
        next_list <- lists;
        tokenizer <- new ListStringTokenizer;

        while looping loop {
            somestr <- in_string();
            decision();
        } pool;
    }
    };
};class Product {
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

class Officer inherits Sergent {};class ListStringTokenizer inherits IO{
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