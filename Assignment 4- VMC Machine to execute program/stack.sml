signature Stack =
sig
    type 'a Stack
    exception EmptyStack
    exception Error of string
    (* val create: unit -> 'a Stack There is a problem there; to be confirmed *)
    val create : 'a Stack
    val push : 'a * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val empty: 'a Stack -> bool
    val poptop : 'a Stack -> ('a * 'a Stack) option
    val nth : 'a Stack * int -> 'a
    val drop : 'a Stack * int -> 'a Stack
    val depth : 'a Stack -> int
    val app : ('a -> unit) -> 'a Stack -> unit
    val map : ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find : ('a -> bool) -> 'a Stack -> 'a option
    val filter : ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists : ('a -> bool) -> 'a Stack -> bool
    val all : ('a -> bool) -> 'a Stack -> bool
    val list2Stack : 'a list -> 'a Stack (* Convert a list into a Stack *)
    val Stack2list: 'a Stack -> 'a list (* Convert a Stack into a list *)
    val toString: ('a -> string) -> 'a Stack -> string
end

structure FunStack :> Stack =
struct
    type 'a Stack ='a list
    exception EmptyStack
    exception Error of string
    val create = []
    fun push(x : 'a, stack : 'a Stack) : 'a Stack = x :: stack
    fun pop(stack : 'a Stack) : 'a Stack = if length(stack) = 0 then raise EmptyStack else tl(stack)
    fun top(stack : 'a Stack) : 'a  = if length(stack) = 0 then raise EmptyStack else hd(stack)
    fun empty(stack : 'a Stack) : bool = if length(stack) = 0 then true else false
    fun poptop(stack : 'a Stack) : ('a * 'a Stack) option = 
                                let val tail  = pop(stack)
                                    val head = top(stack)
                                in
                                    SOME( (head,tail))
                                end 
    fun nth( stack : 'a Stack, n : int) : 'a = if length(stack) > n andalso n >= 0 then List.nth(stack,n) else raise Error "Index out of bound"
    fun drop( stack : 'a Stack, i :int) : 'a Stack = if length(stack) > i andalso i >= 0 then List.drop(stack,i) else raise Error "Index out of bound"
    fun depth(stack : 'a Stack) = length(stack)
    fun app f stack = List.app f stack
    fun map f stack = List.map f stack
    fun mapPartial f stack = List.mapPartial f stack
    fun find f stack = List.find f stack
    fun filter f stack  = List.filter f stack
    fun foldr f init stack = List.foldr f init stack
    fun foldl f init stack = List.foldl f init stack
    fun exists f stack = List.exists f stack
    fun all f stack = List.all f stack
    (*Here in the conversion of list to stack 1st element of list is the bottom of the stack*)
    fun list2Stack(elements : 'a list) : 'a Stack = 
                                    let val temp = create
                                        fun getStack([], stack : 'a Stack) = stack
                                        | getStack (head :: tail, stack : 'a Stack) = getStack(tail, push(head,stack))
                                    in
                                        getStack(elements, temp)
                                    end
    (*Here the top element of the stack would be the last element of the list*)
    fun Stack2list(stack : 'a Stack) : 'a list = 
                                    let 
                                        fun getList(lis, stack) = if depth(stack) = 0 then lis else getList(top(stack) :: lis, pop(stack))
                                    in
                                        getList([],stack)
                                    end
    (*This will convert the stack into string by applying the function f*)
    (*Here the order of the elements of the string is 1st of string is the top of stack*)
    fun toString f stack = 
                        let val lis = Stack2list(stack)
                            fun concat([],str) = str
                            | concat(head :: tail, str) = concat(tail, f(head) ^ str)
                        in 
                            concat(lis, "")
                        end
end
