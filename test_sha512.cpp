#include <stdio.h>
#include <openssl/evp.h>
#include <cstring>

int  main(int argc, char *argv[])
{
    EVP_MD_CTX *mdctx;
    const EVP_MD *md;
    char mess1[3];;
    mess1[0] = 0xff;
    mess1[1] = 0xff;
    mess1[2] = 0xff;
    unsigned char md_value[EVP_MAX_MD_SIZE];
    unsigned int md_len, i;
    
    OpenSSL_add_all_digests();
    
    if(!argv[1]) {
           printf("Usage: mdtest digestname\n");
           exit(1);
    }
    
    md = EVP_get_digestbyname(argv[1]);
    
    if(!md) {
           printf("Unknown message digest %s\n", argv[1]);
           exit(1);
    }
    
    mdctx = EVP_MD_CTX_create();
    EVP_DigestInit_ex(mdctx, md, NULL);
    EVP_DigestUpdate(mdctx, mess1, strlen(mess1));
    EVP_DigestFinal_ex(mdctx, md_value, &md_len);
    EVP_MD_CTX_destroy(mdctx);
    
    printf("Digest is: ");
    for(i = 0; i < md_len; i++)
           printf("%02x", md_value[i]);
    printf("\n");
    
    /* Call this once before exit. */
    EVP_cleanup();
    exit(0);
}

