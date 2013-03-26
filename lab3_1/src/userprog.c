#include "lab4.h"

main(int argc, char *argv[])
{
    int TestcaseId=0;
    switch(argc)
    {
      case 2:
           TestcaseId=dstrtol(argv[1],NULL,10);
           Printf("\nTesting case %d\n",TestcaseId);
           break;
      default:
           Printf("\nUsage: ");
           Printf(argv[0]);
           Printf(" [case id]\n\n");
           exit();
    }

    switch(TestcaseId)
    {
        case 1: process_create("userprog1.dlx.obj",NULL);
                break;
        case 2: process_create("userprog2.dlx.obj",NULL);
                break;
        default:
                Printf("Test case %d not supported\n", TestcaseId);
                break;
    }
}
