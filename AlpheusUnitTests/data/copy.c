#include <stdio.h>
#include <stdlib.h>

// Compiled binary for this code is "copy_prog" that is placed in source control near this source file
// It is compiled on linux with "cc copy.c -o copy_prog"
// This code is used for compiling utility that is stored in source control as a compiled binary
// The program copyies the source file into the destination

int main(int argc, char** argv) {
       FILE *fp;
       FILE *fp2;
       const int bufSize = 1024;
       char buffer[bufSize];

       if(argc<3) {
            printf("You need to specify source and distanation as arguments\r\n");
            exit(1);
       }
       const char* source = argv[1];
       const char* dest = argv[2];
       printf("Coping %s to %s\r\n",source,dest);
       
       fp = fopen(source, "rb");
       printf("Source is open for reading\r\n");
       fp2 = fopen(dest,"wb+");
       printf("Destination is open for writing\r\n");
       int offset = 0;
       while(!feof(fp)) {
           int read = fread(buffer, 1, bufSize,fp);
           printf("Read %d bytes\r\n",read);
           fwrite(buffer,1,read,fp2);
           printf("wrote %d bytes\r\n",read);
       }

       fclose(fp);
       fclose(fp2);
       printf("Successfuly copied.\r\n");
       return(0);
}
