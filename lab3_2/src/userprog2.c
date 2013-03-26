// This test case should generate 5 page faults and display output as Sum=210.
main()
{
    int i,j;
    int  Sum=0;
    int nArray[10000];


    for(i=0,j=1;i<10000;i+=500)
        nArray[i]=j++;
    for(i=0;i<10000;i+=500)
        Sum+= nArray[i];

    Printf("\nIn Userprog2, Sum : %d\n\n",Sum);
      
}
