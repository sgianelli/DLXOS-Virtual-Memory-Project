/* This test program touches a character pointer in 12 different pages..
   This will generate 12 page faults and the output should display as  Sum = 66.
*/

main()
{
    int i, sem;
    char  Sum=0;
    char * charPtr=(char *)0x7000 ;
    //Printf("USERPROG1: After initialization\n");
 
    for(i=0;i<12;i++) {
          //Printf("USERPROG1: For loop 1, iteration %d\n", i);
          *(charPtr + 8192*i)=(char )i;
    }
    for(i=0;i<12;i++)
        Sum+= *(charPtr + 8192 * i);


    Printf("\n\nIn Userprog1, Sum : %d\n\n",Sum);

}
