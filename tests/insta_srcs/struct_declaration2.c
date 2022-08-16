int main()
{
    struct my_struct_tag
    {
        char a;
        int b;
        char c;
    } mine;
    return sizeof(mine); // -> 12
}