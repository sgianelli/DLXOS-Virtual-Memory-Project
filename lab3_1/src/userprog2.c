/* This test program touches a character pointer in 13 different pages..
   This will generate 13 page faults and the  output should display " You have run out of memory, killing the process"
*/

main()
{
    int i;
    char  Sum=0;
    char * charPtr=(char *)0x7000;

    for(i=0;i<13;i++)
          *(charPtr + 8192*i)=(char )i;

    for(i=0;i<13;i++)
        Sum+= *(charPtr + 8192 * i);

    Printf("\n in Userprog2, Sum : %d",Sum);
}
