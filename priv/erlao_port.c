/* Copyright (C) 2011 Romain "Artefact2" Dalmaso <artefact2@gmail.com>
 * 
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see
 *  <http://www.gnu.org/licenses/>.
 */

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ao/ao.h>

struct ao_handle_s {
  ao_device* device;
  ao_option* options;
  ao_sample_format* fmt;
};

typedef struct ao_handle_s ao_handle;
typedef unsigned char byte;

static int32_t read_int32(const byte* const array, size_t offset) {
  return (int32_t)(
		   (array[offset] << 24) | 
		   (array[offset + 1] << 16) | 
		   (array[offset + 2] << 8) | 
		   array[offset + 3]);
}

static void write_int32(int32_t value, byte* array, size_t offset) {
  array[offset] = (value >> 24) & 0xFF;
  array[offset + 1] = (value >> 16) & 0xFF;
  array[offset + 2] = (value >> 8) & 0xFF;
  array[offset + 3] = value & 0xFF;  
}

static int read_all(int fd, byte* buf, size_t nbyte) {
  size_t bytes_read = 0;
  ssize_t s;
  while(bytes_read != nbyte) {
    s = read(fd, buf + bytes_read, nbyte - bytes_read);
    if(s <= 0) return 0;
    else bytes_read += (size_t)s;
  }
  return 1;
}

static int write_all(int fd, const byte* const buf, size_t nbyte) {
  size_t bytes_written = 0;
  ssize_t s;
  while(bytes_written != nbyte) {
    s = write(fd, buf + bytes_written, nbyte - bytes_written);
    if(s <= 0) return 0;
    else bytes_written += (size_t)s;
  }
  return 1;
}

static int get_message(size_t* length, byte** buffer) {
  byte s_buffer[4];
  byte* m_buffer;
  size_t message_size;

  if(!read_all(3, s_buffer, 4)) return 0;

  message_size = (size_t)read_int32(s_buffer, 0);
  m_buffer = malloc(message_size);

  if(!read_all(3, m_buffer, message_size)) return 0;

  *length = message_size;
  *buffer = m_buffer;
  return 1;
}

static void free_message(byte* message) {
  free(message);
}

static ao_sample_format* parse_ao_sample_format(const byte* const m_data, size_t offset) {
  ao_sample_format* fmt = malloc(sizeof(ao_sample_format));
  
  fmt->bits = read_int32(m_data, offset);
  fmt->rate = read_int32(m_data, offset + 4);
  fmt->channels = read_int32(m_data, offset + 8);
  fmt->byte_format = read_int32(m_data, offset + 12);
  if(m_data[offset + 16] == '\0') fmt->matrix = NULL;
  else fmt->matrix = strdup((char*)m_data + 16);
  
  return fmt;
}

static ao_option* parse_ao_options(const byte* const m_data, size_t opt_offset) {
  ao_option* options = NULL;
  uint32_t num_options = read_int32(m_data, opt_offset);
  
  if(num_options > 0) {
    uint32_t j;
    for(j = 0; j < num_options; ++j) {
      ao_append_option(&options, 
		       (char*)m_data + opt_offset + 4 + num_options * 8
		       + read_int32(m_data, opt_offset + 4 + j * 8),
		       (char*)m_data + opt_offset + 4 + num_options * 8
		       + read_int32(m_data, opt_offset + 4 + j * 8 + 4));
    }
  }

  return options;
}

static byte* format_ao_info(const ao_info* const d_info, size_t* length) {
  int i;
  size_t l_name = strlen(d_info->name);
  size_t l_sname = strlen(d_info->short_name);
  size_t l_comment = strlen(d_info->comment);
  size_t total_size = 16 /* integer values */
    + 4 + l_name
    + 4 + l_sname
    + 4 + l_comment;
  
  for(i = 0; i < d_info->option_count; ++i) {
    total_size += strlen(d_info->options[i]) + 1; /* +1 for '\0' */
  }

  byte* output = malloc(total_size);

  write_int32(d_info->type, output, 0);

  write_int32(l_name, output, 4);
  memcpy(output + 8, d_info->name, l_name);
	
  write_int32(l_sname, output, 8 + l_name);
  memcpy(output + 12 + l_name, d_info->short_name, l_sname);
  
  write_int32(l_comment, output, 12 + l_name + l_sname);
  memcpy(output + 16 + l_name + l_sname, d_info->comment, l_comment);
  
  write_int32(d_info->preferred_byte_format, output, 16 + l_name + l_sname + l_comment);
  write_int32(d_info->priority, output, 20 + l_name + l_sname + l_comment);
  
  write_int32(d_info->option_count, output, 24 + l_name + l_sname + l_comment);
  
  size_t o_offset = 28 + l_name + l_sname + l_comment;
  size_t o_length;
  for(i = 0; i < d_info->option_count; ++i) {
    o_length = strlen(d_info->options[i]) + 1;
    memcpy(output + o_offset, d_info->options[i], o_length);
    o_offset += o_length;
  }

  *length = total_size;
  return output;
}

int main(void) {
  size_t m_length;
  byte* m_data;

  ao_initialize();

  while(get_message(&m_length, &m_data)) {
    if(m_data[0] == 1) {
      /* ao_is_big_endian */
      byte output[6];
      write_int32(2, output, 0);
      output[4] = 1;
      output[5] = (byte)ao_is_big_endian();
      write_all(4, output, 6);
    }

    else if(m_data[0] == 2) {
      /* ao_append_global_option */
      byte output[6];
      size_t val_offset = read_int32(m_data, 1);

      write_int32(2, output, 0);
      output[4] = 2;
      output[5] = (byte)ao_append_global_option((char*)(m_data + 5), (char*)(m_data + val_offset));
      write_all(4, output, 6);
    }

    else if(m_data[0] == 3) {
      /* ao_open_live */
      ao_device* device;
      int32_t driver_id = read_int32(m_data, 1);
      ao_sample_format* fmt = parse_ao_sample_format(m_data, 9);
      ao_option* options = parse_ao_options(m_data, read_int32(m_data, 5));

      device = ao_open_live(driver_id, fmt, options);
      if(device == NULL) {
	byte output[10];

	ao_free_options(options);
	free(fmt->matrix);
	free(fmt);

	write_int32(6, output, 0);
	output[4] = 3;
	output[5] = 0;
	write_int32(errno, output, 6);
	write_all(4, output, 10);
      } else {
	ao_handle* handle = malloc(sizeof(ao_handle));
	byte* output = malloc(sizeof(ao_handle*) + 6);

	handle->device = device;
	handle->options = options;
	handle->fmt = fmt;

	write_int32(2 + sizeof(ao_handle*), output, 0);
	output[4] = 3;
	output[5] = 1;
	memcpy(output + 6, &handle, sizeof(ao_handle*));
	write_all(4, output, 6 + sizeof(ao_handle*));
	free(output);
      }
    }

    else if(m_data[0] == 4) {
      /* ao_open_file */
      ao_device* device;
      int32_t driver_id = read_int32(m_data, 1);
      byte overwrite = m_data[5];
      char* filename = (char*)m_data + 14;
      ao_sample_format* fmt = parse_ao_sample_format(m_data, read_int32(m_data, 6));
      ao_option* options = parse_ao_options(m_data, read_int32(m_data, 10));

      device = ao_open_file(driver_id, filename, (int)overwrite, fmt, options);
      if(device == NULL) {
	byte output[10];

	ao_free_options(options);
	free(fmt->matrix);
	free(fmt);

	write_int32(6, output, 0);
	output[4] = 4;
	output[5] = 0;
	write_int32(errno, output, 6);
	write_all(4, output, 10);	
      } else {
	ao_handle* handle = malloc(sizeof(ao_handle));
	byte* output = malloc(sizeof(ao_handle*) + 6);

	handle->device = device;
	handle->options = options;
	handle->fmt = fmt;

	write_int32(2 + sizeof(ao_handle*), output, 0);
	output[4] = 4;
	output[5] = 1;
	memcpy(output + 6, &handle, sizeof(ao_handle*));
	write_all(4, output, 6 + sizeof(ao_handle*));
	free(output);
      }
    }

    else if(m_data[0] == 5) {
      /* ao_play */
      byte output[6];
      ao_handle* handle;
      memcpy(&handle, m_data + 5, sizeof(ao_handle*));
      output[5] = ao_play(handle->device, (char*)m_data + 5 + sizeof(ao_handle*), (uint32_t)read_int32(m_data, 1));
      
      write_int32(2, output, 0);
      output[4] = 5;
      write_all(4, output, 6);
    }

    else if(m_data[0] == 6) {
      /* ao_close */
      byte output[6];
      ao_handle* handle;
      memcpy(&handle, m_data + 1, sizeof(ao_handle*));
      output[5] = ao_close(handle->device);
      ao_free_options(handle->options);
      free(handle->fmt->matrix);
      free(handle->fmt);

      write_int32(2, output, 0);
      output[4] = 6;
      write_all(4, output, 6);
    }

    else if(m_data[0] == 7) {
      /* ao_driver_id */
      byte output[9];
      write_int32(5, output, 0);
      output[4] = 7;
      write_int32(ao_driver_id((char*)m_data + 1), output, 5);
      write_all(4, output, 9);
    }

    else if(m_data[0] == 8) {
      /* ao_default_driver_id */
      byte output[9];
      write_int32(5, output, 0);
      output[4] = 8;
      write_int32(ao_default_driver_id(), output, 5);
      write_all(4, output, 9);
    }

    else if(m_data[0] == 9) {
      /* ao_driver_info */
      const ao_info* const d_info = ao_driver_info(read_int32(m_data, 1));
      if(d_info == NULL) {
	byte output[6];
	write_int32(2, output, 0);
	output[4] = 9;
	output[5] = 0;
	write_all(4, output, 6);
      } else {
	byte header[6];
	byte* output;
	size_t s_size;
	
	output = format_ao_info(d_info, &s_size);

	write_int32(s_size + 2, header, 0);
	header[4] = 9;
	header[5] = 1;

	write_all(4, header, 6);
	write_all(4, output, s_size);
	free(output);
      }
    }

    else if(m_data[0] == 10) {
      /* ao_driver_info_list */
      int count, i;
      byte header[5];
      ao_info** drivers = ao_driver_info_list(&count);
      byte** formatted = malloc(sizeof(byte*) * count);
      size_t* sizes = malloc(sizeof(size_t) * count);
      size_t size, t_size = 0;

      for(i = 0; i < count; ++i) {
	formatted[i] = format_ao_info(drivers[i], &size);
	sizes[i] = size;
	t_size += size;
      }

      write_int32(t_size + 1, header, 0);
      header[4] = 10;

      write_all(4, header, 5);
      for(i = 0; i < count; ++i) {
	write_all(4, formatted[i], sizes[i]);
	free(formatted[i]);
      }
      free(formatted);
      free(sizes);
    }

    else if(m_data[0] == 11) {
      /* ao_file_extension */
      char* extension = NULL;

      /*
	ao_file_extension seems to be missing from the library, yet
	it is still documented...
      */
      /*
	extension = ao_file_extension(read_int32(m_data, 1));
      */
      if(extension == NULL) {
	byte output[6];
	write_int32(2, output, 0);
	output[4] = 11;
	output[5] = 0;
	write_all(4, output, 6);
      } else {
	size_t ext_length = strlen(extension);
	byte* output = malloc(6 + ext_length);
	write_int32(2 + ext_length, output, 0);
	output[4] = 11;
	output[5] = 1;
	memcpy(output + 6, extension, ext_length);

	write_all(4, output, 6 + ext_length);
	free(output);
      }
    }

    else {
      /* unknown_method */
      byte output[4] = {0, 0, 0, 0};
      write_all(4, output, 4);
    }
    
    free_message(m_data);
  }

  ao_shutdown();
  return 0;
}
