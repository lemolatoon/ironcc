#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  DIR *dir;
  struct dirent *ent;
  char *directory;

  if (argc == 1) {
    // If no directory specified, use current working directory
    directory = ".";
  } else {
    directory = argv[1];
  }

  // Open directory stream
  dir = opendir(directory);
  if (dir != NULL) {
    // Print all files and directories within the directory
    while ((ent = readdir(dir)) != NULL) {
      printf("%s\n", ent->d_name);
    }
    closedir(dir);
  } else {
    // Could not open directory
    perror("ls: cannot access");
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
