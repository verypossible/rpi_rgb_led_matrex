#include "led-matrix.h"

#include <err.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <poll.h>
#include <stdio.h>
#include <string>

void write_back(char *msg);
int poll_input();
void read_in(char *buffer, int len);
int to_read_length();
void write_fixed(char *msg, int len, char *reason);

#define MAX_READ 14

using rgb_matrix::RGBMatrix;

int main(int argc, char *argv[]) {
  RGBMatrix::Options my_defaults;
  my_defaults.hardware_mapping = "adafruit-hat";
  my_defaults.chain_length = 1;
  rgb_matrix::RuntimeOptions runtime_defaults;
  RGBMatrix *matrix = rgb_matrix::CreateMatrixFromFlags(&argc, &argv,
                                                        &my_defaults,
                                                        &runtime_defaults);
  if (matrix == NULL) {
    PrintMatrixFlags(stderr, my_defaults, runtime_defaults);
    return 1;
  }

  char buffer[MAX_READ + 1];
  char command;
  std::string command_string;
  char x_coord_s[2];
  char y_coord_s[2];
  char r_val_s[3];
  char g_val_s[3];
  char b_val_s[3];
  int x_coord, y_coord, r_val, g_val, b_val;

  while(1) {
    int res = poll_input();
    if(res > 0) {
      int len = to_read_length();
      if (len > MAX_READ) {err(EXIT_FAILURE, "Too large message to read.");}

      // len being less than zero indicates STDIN has been closed - exit
      if (len < 0) {return 1;}

      read_in(buffer, len);
      command = buffer[0];
      command_string = std::string(buffer);

      switch (command) {
        case '0':
          command_string.copy(x_coord_s, 2, 10);
          x_coord = std::stoi (x_coord_s);

          command_string.copy(y_coord_s, 2, 12);
          y_coord = std::stoi (y_coord_s);

          command_string.copy(r_val_s, 3, 1);
          r_val = std::stoi (r_val_s);

          command_string.copy(g_val_s, 3, 4);
          g_val = std::stoi (g_val_s);

          command_string.copy(b_val_s, 3, 7);
          b_val = std::stoi (b_val_s);

          matrix->SetPixel(x_coord, y_coord, r_val, g_val, b_val);
          break;
        case '1':
          command_string.copy(r_val_s, 3, 1);
          r_val = std::stoi (r_val_s);

          command_string.copy(g_val_s, 3, 4);
          g_val = std::stoi (g_val_s);

          command_string.copy(b_val_s, 3, 7);
          b_val = std::stoi (b_val_s);

          matrix->Fill(r_val, g_val, b_val);
          break;
        case '2':
          matrix->Clear();
          break;
        default:
          errx(EXIT_FAILURE, "Unknown mode '%c'", command);
          break;
        }
    }
  }

  delete matrix;
}

/**
 * Write a len characters, pointed to by msg, to STDIN. The reason is used
 * as debug information should the write fail.
 */
void write_fixed(char *msg, int len, char *reason) {
  int written = 0;
  while(written < len) {
    int this_write = write(STDOUT_FILENO,  msg + written, len - written);
    if (this_write <= 0 && errno != EINTR) {
      err(EXIT_FAILURE, "%s: %d", reason, this_write);
    }
    written += this_write;
  }
}

/**
 * Send the zero-terminated msg back the BEAM by writing to stdout.
 */
void write_back(char *msg) {
  unsigned long len = strlen(msg);
  char size_header[2] = {(len >> 8 & 0xff), (len & 0xff)};
  char header_write[13] = "header write";
  char data_write[11] = "data write";
  write_fixed(size_header, 2, header_write);
  write_fixed(msg, len, data_write);
}

/**
 * Reads len chars from STDIN to buffer. Returns len if successful, or -1 if STDIN has been closed
 *
 */
int read_fixed(char *buffer, int len) {
  int read_count = 0;
  while(read_count < len) {
    int this_read = read(STDIN_FILENO, buffer + read_count, len - read_count);

    // 0 is returned from read if EOF is STDIN has been closed.
    if (this_read == 0) {
      return -1;
    }

    // errno is set to EINTR if interrrupted by a signal before any data is sent.
    if(this_read < 0 && errno != EINTR) {
      err(EXIT_FAILURE, "read failed");
    }
    read_count += this_read;
  }
  return len;
}

/* *
 * Read len number of bytes from the stream and populate the buffer. Zero-terminates the buffer.
 **/
void read_in(char *buffer, int len) {
  int read_count = 0;
  do {
    int this_read = read(STDIN_FILENO, buffer + read_count, len - read_count);
    if(this_read < 0 && errno != EINTR) {
      err(EXIT_FAILURE, "read failed");
    }
    read_count += this_read;
  } while(read_count < len);
  buffer[len] = '\0';
}

/**
 * The first two bytes indicates the length of the message, with the first byte being most significant.
 * Read this and return as an int. Returns -1 if STDIN is closed
 **/
int to_read_length() {
  unsigned char size_header[2];
  int r = read_fixed((char*) size_header, 2);
  if(r < 0) {
    return -1;
  }

  return (size_header[0] << 8) | size_header[1];
}

/**
 * See http://man7.org/linux/man-pages/man2/poll.2.html
 * Also based on https://stackoverflow.com/questions/4656388/poll-stdin-and-other-fds
 *
 * Waits for input to be available on stdin. If it is return value is greater than zero.
 * Times out every 5 seconds.
**/
int poll_input() {
  int timeout = 5000;
  struct pollfd fd;
  fd.fd = STDIN_FILENO;
  fd.events = POLLIN;
  fd.revents = 0;
  return poll(&fd, 1, timeout);
}
