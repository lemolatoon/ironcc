#ifndef __DIRENT_H
#define __DIRENT_H

/* This is the data type of directory stream objects.
   The actual structure is opaque to users.  */
typedef struct dirent DIR;

typedef char __UNSIGNED_LONG_INT[8];
typedef char __SIGNED_LONG_INT[8];
typedef __UNSIGNED_LONG_INT __ino_t;
// #include <stdio.h>  // typedef `__off_t`
typedef void *__off_t;

struct dirent {
  __ino_t d_ino;
  __off_t d_off;
  /* unsigned short int */ char d_reclen[2];
  /* unsigned char */ char d_type;
  char d_name[256]; /* We must not include limits.h! */
};

extern DIR *opendir(const char *__name);
extern int closedir(DIR *__dirp);
extern struct dirent *readdir(DIR *__dirp);

#endif
