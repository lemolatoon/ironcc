
#ifndef _STDIO_H
#define _STDIO_H 1

#include <stdarg.h>
#include <stddef.h>

typedef struct _IO_FILE FILE;
typedef void _IO_lock_t;
// typedef void *__off_t;
#include <dirent.h>

typedef void *__dummy_struct_IO_maker_ptr;

struct _IO_FILE {
  int _flags; /* High-order word is _IO_MAGIC; rest is flags. */

  /* The following pointers correspond to the C++ streambuf protocol. */
  char *_IO_read_ptr;   /* Current read pointer */
  char *_IO_read_end;   /* End of get area. */
  char *_IO_read_base;  /* Start of putback+get area. */
  char *_IO_write_base; /* Start of put area. */
  char *_IO_write_ptr;  /* Current put pointer. */
  char *_IO_write_end;  /* End of put area. */
  char *_IO_buf_base;   /* Start of reserve area. */
  char *_IO_buf_end;    /* End of reserve area. */

  /* The following fields are used to support backing up and undo. */
  char *_IO_save_base;   /* Pointer to start of non-current get area. */
  char *_IO_backup_base; /* Pointer to first valid character of backup area */
  char *_IO_save_end;    /* Pointer to end of non-current get area. */

  __dummy_struct_IO_maker_ptr _markers;

  struct _IO_FILE *_chain;

  int _fileno;
  int _flags2;

  __off_t _old_offset; /* This used to be _offset but it's too small.  */

  /* 1+column number of pbase(); 0 is unknown. */
  // unsigned short _cur_column;
  // signed char _vtable_offset;
  int _cur_column;
  char _vtable_offset;
  char _shortbuf[1];

  _IO_lock_t *_lock;
};

int printf(const char *fmt, ...);
int fprintf(FILE *stream, const char *fmt, ...);
int vfprintf(FILE *stream, const char *format, va_list arg);
FILE *fopen(const char *filename, const char *mode);
char *fgets(char *string, int n, FILE *stream);
void perror(const char *s);
int putchar(int c);
int snprintf(char *buffer, size_t n, const char *fmt, ...);

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;
#endif