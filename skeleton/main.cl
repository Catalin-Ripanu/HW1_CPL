class Main inherits IO{
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
};