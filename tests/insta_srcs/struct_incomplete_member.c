struct A {
    struct A*before; 
    int value;
    struct A* after;
} global_doubly_linked_list;

int main() {
    struct A a1;
    struct A a2;
    global_doubly_linked_list.before = &a1;
    global_doubly_linked_list.value = 2;
    global_doubly_linked_list.after = &a2;
}